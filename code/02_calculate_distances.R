
library(tidyverse)
library(sf)
library(geosphere)
library(ggmap)
library(googleway)
library(mapview)

mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldImagery"))


# Get centroids of Medstat regions

load(file.path("workspace", "center_most_pop_place.RData"))


# Read Medstat shapefile

MS_spdf <- st_read(file.path("data", "MedStat_GIS", "MedStat_Base_2011_region.shp"), 
                   as_tibble = TRUE, 
                   options = "ENCODING=WINDOWS-1252", stringsAsFactors = FALSE)


# Get GP and SCI centers/clinics addresses -----------------------------------


# GP

gp_addresses <- read_excel("data/gp_addresses.xlsx") %>% 
  mutate_all(str_trim) %>% 
  mutate(PLZ = as.numeric(PLZ)) %>% 
  mutate(GP_addr_string = str_c(street, PLZ, Ort, "Schweiz", sep = " + "))


# SCI

SCI_centers <- tibble(
  place = c("Plein Soleil", "SPZ", "Crr Suva", "Balgrist", "REHAB", "Ente Ospedaliero Cantonale"), 
  street = c("Ch. de la Cigale 3", 
             "Guido A. Zäch Str. 1", 
             "Avenue du Grand-Champsec 90", 
             "Forchstrasse 340",
             "Im Burgfelderhof 40",
             "Viale Officina 3"),
  PLZ = c(1010, 6207, 1950, 8008, 4055, 6500),
  Ort =  c("Lausanne", "Nottwil", "Sion", "Zürich", "Basel", "Bellinzona")) %>%
  
  mutate(center_addr_string = str_c(street, PLZ, Ort, "Schweiz", sep = " + "))


if(TRUE) {
  
  ggmap::register_google("AIzaSyATu6r_ZkcS672g5B9T9HqdFktaKN_shhk")
  
  GP_coordinates <- geocode(gp_addresses$GP_addr_string, output = "latlon", source = "google")
  save(GP_coordinates, file = file.path("workspace", "coordinates_GPs.RData"))
  
  
  cent_coordinates <- geocode(SCI_centers$center_addr_string, output = "latlon", source = "google")
  save(cent_coordinates, file = file.path("workspace", "cent_coordinates.RData"))
  
}

load(file.path("workspace", "coordinates_GPs.RData"))
load(file.path("workspace", "cent_coordinates.RData"))


# Bind coordinates to addresses

gp_addresses <- bind_cols(gp_addresses, GP_coordinates)
cent_addresses <- bind_cols(SCI_centers, cent_coordinates)


# Transform addresses/coordinates into a Spatial Points object

GP_addresses.sp <- st_as_sf(x = gp_addresses, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")
cent_addresses.sp <- st_as_sf(x = cent_addresses, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

save(GP_addresses.sp, file = file.path("workspace", "GP_addr.sp.Rdata"))
save(cent_addresses.sp, file = file.path("workspace", "cent_addresses.sp.Rdata"))


# Check whether the GPs/centers are at the correct place

if(TRUE) {
  mapview(GP_addresses.sp)
  mapview(cent_addresses.sp)
}


# Prepare Medstat dataset -------------------------------------------------


# Transform coordinates system of population centroids (hectare with highest population) to
# google coordinates system

MS_coordinates <- do.call(rbind, center_most_pop_place) %>% 
  add_column(MedStat = names(center_most_pop_place), .before = 1) %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  select(-B12BTOT) %>% 
  add_column(lon = st_coordinates(MS_coordinates)[, "X"]) %>% 
  add_column(lat = st_coordinates(MS_coordinates)[, "Y"])

if(TRUE) {mapview(MS_coordinates)}
  


# Bind Fürstentum Lichtenstein as separate row into Medstat centroid dataset
# For Fürstentum Lichtenstein we had no population centroid, therefore we choose Vaduz

FL00 <- tibble(MedStat = "FL00", 
               lon = 9.520915, lat = 47.141091) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>% 
  add_column(lon = 9.520915, lat = 47.141091)

MS_coordinates <- rbind(MS_coordinates, FL00)

tail(MS_coordinates)


# Bind GP/SCI center coordinates to Medstat centroid dataset -------------------------------


# For that we need to replicate the Medstat coordinates dataset 10 times (10 GPs)
# And the GP dataset 705 times (number of Medstat regions).

# GP

n_rows_MS <- nrow(MS_coordinates)
n_rows_GP <- nrow(gp_addresses)

MS_coordinates_GP <- MS_coordinates %>% 
  as_tibble() %>% 
  uncount(n_rows_GP) %>% 
  rename(lon_MS = lon, lat_MS = lat)

gp_addresses <- do.call("rbind", replicate(n_rows_MS, gp_addresses, simplify = FALSE)) %>% 
  rename(lon_GP = lon, lat_GP = lat)

dist_GP <- bind_cols(MS_coordinates_GP, gp_addresses)


# Center SCI

n_rows_MS <- nrow(MS_coordinates)
n_rows_cent <- nrow(cent_addresses)

MS_coordinates_cent <- MS_coordinates %>% 
  as_tibble() %>% 
  uncount(n_rows_cent) %>% 
  rename(lon_MS = lon, lat_MS = lat)

cent_addresses <- do.call("rbind", replicate(n_rows_MS, cent_addresses, simplify = FALSE)) %>% 
  rename(lon_cent = lon, lat_cent = lat)

dist_cent <- bind_cols(MS_coordinates_cent, cent_addresses)



# Get distance from GP to Medstat population center -----------------------


# We aim for a 50 km beeline distance

dist_beeline <- function(lat_a, lon_a, lat_b, lon_b) { 
  
  if(anyNA(c(lat_a, lon_a, lat_b, lon_b))) return(NA) 
  
  distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = geosphere::distHaversine) / 1000
  
}


# GP

dist_GP_50_km <- dist_GP %>% 
  mutate(distance_km_beeline = pmap_dbl(list(lat_MS, lon_MS, lat_GP, lon_GP), dist_beeline)) %>% 
  filter(distance_km_beeline < 50)


# Centers

dist_cent_50_km <- dist_cent %>% 
  mutate(distance_km_beeline = pmap_dbl(list(lat_MS, lon_MS, lat_cent, lon_cent), dist_beeline)) %>% 
  filter(distance_km_beeline < 50)


# Check the regions

# GP

border_50_km <- dist_GP_50_km %>% 
  split(.$GP_addr_string) %>% 
  map("MedStat")


region <- MS_spdf %>%
  filter(MEDSTAT04 %in% unlist(border_50_km)) %>% 
  group_by(MEDSTAT04) %>% 
  summarise()

if(TRUE) { mapview(list(GP_addresses.sp, region)) }


# Centers

border_50_km <- dist_cent_50_km %>% 
  split(.$center_addr_string) %>% 
  map("MedStat")


region <- MS_spdf %>%
  filter(MEDSTAT04 %in% unlist(border_50_km)) %>% 
  group_by(MEDSTAT04) %>% 
  summarise()

if(TRUE) { mapview(list(cent_addresses.sp, region)) }





if(TRUE) { # Calculate driving times GP -------------------------------------------------
  
  # Prepare lat/lon coordinates
  
  # GP
  
  GP_distances <- list()
  
  # Medstat
  
  GP_distances[["MS_lat_lon"]] <- dist_GP_50_km %>% 
    select(lat_MS, lon_MS) %>% 
    split(seq(nrow(.))) %>% 
    set_names(dist_GP_50_km$MedStat) %>% 
    map(unlist)
  
  # GP
  
  GP_distances[["GP_lat_lon"]] <- dist_GP_50_km %>% 
    select(lat_GP, lon_GP) %>% 
    split(seq(nrow(.))) %>% 
    set_names(dist_GP_50_km$MedStat) %>% 
    map(unlist)
  
  
  # SCI centers
  
  cent_distances <- list()
  
  # Medstat
  
  cent_distances[["MS_lat_lon"]] <- dist_cent_50_km %>% 
    select(lat_MS, lon_MS) %>% 
    split(seq(nrow(.))) %>% 
    set_names(dist_cent_50_km$MedStat) %>% 
    map(unlist)
  
  
  # GP
  
  cent_distances[["cent_lat_lon"]] <- dist_cent_50_km %>% 
    select(lat_cent, lon_cent) %>% 
    split(seq(nrow(.))) %>% 
    set_names(dist_cent_50_km$MedStat) %>% 
    map(unlist)

  
  # We calculate the driving times using the google directions API ----------
  
  google_distance_delayed <- function(x, y) {
    
    pb$tick()$print()
    
    Sys.sleep(0.5)
    
    key <- "AIzaSyATu6r_ZkcS672g5B9T9HqdFktaKN_shhk"
    
    drv_dist <- googleway::google_distance(
      origins = x, 
      destinations = y, 
      key = key, 
      mode = "driving")
    
    # From some places like Zermatt it is not possible to drive by car, therefore we calculate the 
    # travel distance by public transport
    
    if(unlist(drv_dist$rows$elements)["status"] == "ZERO_RESULTS") {
      
      drv_dist <- google_distance(origins = x, destinations = y, key = key, 
                                  mode = "transit")
      
    }
    
    drv_dist_df <- data.frame(
      
      origin = drv_dist$origin_addresses,
      destination = drv_dist$destination_addresses,
      
      lapply(distance_elements(drv_dist), function(x) {
        
        data.frame(
          duration = mean(x[["duration"]][["value"]], na.rm = TRUE),
          distance = mean(x[["distance"]][["value"]], na.rm = TRUE)
        )
        
      }
      ) %>% bind_rows()
      
      , stringsAsFactors = FALSE)
    
    return(drv_dist_df)
    
  }
  
  #!! Take care, if travel time by car cannot be calculated as the place is not reachable by car
  #!! then the traval time by public transport is being calculated. For this, the time now is used
  #!! this means that if at the time when this script is run there are no train connections, then the 
  #!! travel times will be incorrect.
  
  
  # GP distances
  
  pb <- progress_estimated(length(GP_distances[["MS_lat_lon"]])) 
  
  drv_dist_GP <- map2_dfr(GP_distances[["MS_lat_lon"]], 
                          GP_distances[["GP_lat_lon"]], 
                          google_distance_delayed) %>% 
    
    mutate(duration_min = duration / 60, distance_km = distance / 1000)
  
  save(drv_dist_GP, file = file.path("workspace", "drv_dist_GP.RData"))
  
  
  # SCI center distances
  
  pb <- progress_estimated(length(cent_distances[["MS_lat_lon"]]))
  
  drv_dist_cent <- map2_dfr(
    cent_distances[["MS_lat_lon"]], 
    cent_distances[["cent_lat_lon"]], 
    google_distance_delayed) %>% 
    
    mutate(duration_min = duration / 60, distance_km = distance / 1000)
  
  save(drv_dist_cent, file = file.path("workspace", "drv_dist_centers.RData"))
  
}


load(file.path("workspace", "drv_dist_GP.RData"))
load(file.path("workspace", "drv_dist_centers.RData"))

# We combine the dataset with the Medstat information and the travel distances --------

dist_GP_50_km <- bind_cols(dist_GP_50_km, drv_dist_GP)
dist_cent_50_km <- bind_cols(dist_cent_50_km, drv_dist_cent)


# This variables might be useful when we want to manually check the driving times in google maps

# GP

dist_GP_50_km <- mutate(dist_GP_50_km, 
                        MS_address_string = str_c(lat_MS, lon_MS, sep = ", "),
                        GP_address_string = str_c(lat_GP, lon_GP, sep = ", "))

# SCI centers

dist_cent_50_km <- mutate(dist_cent_50_km, 
                          MS_address_string = str_c(lat_MS, lon_MS, sep = ", "),
                          cent_address_string = str_c(lat_cent, lon_cent, sep = ", "))


# We select the variables we want

# GP

dist_GP_50_km <- select(dist_GP_50_km, 
                        MedStat, Ort, duration_min, distance_km, distance_km_beeline, 
                        MS_address_string, GP_addr_string, GP_address_string, 
                        origin, destination) %>% 
  arrange(Ort, duration_min)


# SCI centers

dist_cent_50_km <- select(dist_cent_50_km, 
                          MedStat, Ort, duration_min, distance_km, distance_km_beeline, 
                          MS_address_string, center_addr_string, cent_address_string, 
                          origin, destination) %>% 
  arrange(Ort, duration_min)


# We keep only regions from which a GP can be reached within 25 minutes

dist_MS_GP_25_min <- filter(dist_GP_50_km, duration_min < 26) %>% 
  arrange(Ort)

dist_MS_cent_25_min <- filter(dist_cent_50_km, duration_min < 26) %>% 
  arrange(Ort)


# We save the dataset and clear the workspace -----------------------------

save(dist_MS_GP_25_min, file = file.path("workspace", "dist_MS_GP_25_min.RData"))
save(dist_MS_cent_25_min, file = file.path("workspace", "dist_MS_cent_25_min.RData"))

rm("cent_addresses", "cent_addresses.sp", "cent_coordinates", 
  "center_most_pop_place", "dist_beeline", "drv_dist_cent", "drv_dist_GP", 
  "FL00", "gp_addresses", "GP_addresses.sp", "GP_coordinates", 
  "GP_distances", "MS_spdf", "n_rows_cent", "n_rows_GP", "SCI_centers")
