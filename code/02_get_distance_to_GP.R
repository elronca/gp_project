
library(tidyverse)
library(sf)
library(mapview)
library(geosphere)
library(googleway)


# Get centroids of Medstat regions

load(file.path("workspace", "pop_centroids_most_pop_place.RData"))

# Read Medstat shapefile

MS_spdf <- st_read(file.path("data", "MedStat_GIS", "MedStat_Base_2011_region.shp"), 
                   as_tibble = TRUE, options = "ENCODING=WINDOWS-1252")


# Get GP addresses --------------------------------------------------------

GP_addr <- read_csv2(file.path("data", "gp_addresses.csv")) %>% 
  mutate_all(str_trim) %>% 
  mutate(PLZ = as.numeric(PLZ)) %>% 
  mutate(GP_addr_string = str_c(street, PLZ, Ort, "Schweiz", sep = " + "))


# If FALSE, do not run code

if(FALSE) {
  
  register_google("AIzaSyCs6xFuSic9PpwWinOPA1TDaGXD_HZbQdc")
  
  GP_coordinates <- geocode(GP_addr$GP_addr_string, output = "latlon", source = "google")
  save(GP_coordinates, file = file.path("workspace", "coordinates_GPs.RData"))

  }

load(file.path("workspace", "coordinates_GPs.RData"))

# Bind GP coordinates to GP addresses

GP_addr <- bind_cols(GP_addr, GP_coordinates)

# Transform GP_addr into a Spatial Points object

GP_addr.sp <- st_as_sf(x = GP_addr, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

# Check whether the GPs are at the correct place

mapview(GP_addr.sp)



# Prepare Medstat dataset -------------------------------------------------


# For Fürstentum Lichtenstein we had no population centroid, therefore we choose Vaduz

FL00 <- tibble(MedStat = "FL00", lon = 9.520915, lat = 47.141091) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>% 
  add_column (lon = 9.520915, lat = 47.141091)
  

# Transform coordinates system of population centroids (hectare with highest population) to
# google coordinates system

MS_coordinates <- pop_centroids_most_pop_place %>% 
  bind_rows(.id = "MedStat") %>% 
  st_as_sf(coords = c("X_Koord", "Y_Koord"), crs = st_crs(MS_spdf)$proj4string) %>% 
  st_transform("+proj=longlat +datum=WGS84")

# CHeck Coordinates on map

mapview(MS_coordinates)


# Get longitude and latitude as separate columns

MS_coordinates <- MS_coordinates %>% 
  add_column(lon = st_coordinates(MS_coordinates)[, 1]) %>% 
  add_column(lat = st_coordinates(MS_coordinates)[, 2]) 


# Bind Fürstentum Lichtenstein as separate row into Medstat centroid dataset
  
MS_coordinates <- rbind(MS_coordinates, FL00)


# Now we need to bring the Medstat centroid dataset together with the GP addresses
# For that we need to replicate the Medstat coordinates dataset 10 times (10 GPs)
# And the GP dataset 705 times (number of Medstat regions).

n_rows_MS <- nrow(MS_coordinates)
n_rows_GP <- nrow(GP_addr)

MS_coordinates <- MS_coordinates %>% 
  mutate(freq = n_rows_GP) %>% 
  uncount(freq) %>% 
  rename(lon_MS = lon, lat_MS = lat)

GP_addr <- do.call("rbind", replicate(n_rows_MS, GP_addr, simplify = FALSE)) %>% 
  rename(lon_GP = lon, lat_GP = lat)


# Now we bind the two datasets together

dist_GP <- bind_cols(MS_coordinates, GP_addr)



# Get distance from GP to Medstat population center -----------------------


# We aim for a 50 km beeline distance

if(TRUE) {
  

dist_geo <- function(lat_a, lon_a, lat_b, lon_b) { 
  
  if(anyNA(c(lat_a, lon_a, lat_b, lon_b))) return(NA) 
  
  round(distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)/1000, 2) 
  
  } 

dist_GP$distance_km_beeline <- mapply(
  
  lat_a = dist_GP$lat_MS, 
  lon_a = dist_GP$lon_MS, 
  lat_b = dist_GP$lat_GP, 
  lon_b = dist_GP$lon_GP, 
  
  FUN = dist_geo)

dist_50_km <- filter(dist_GP, distance_km_beeline < 50)

dist_GP <- dist_50_km

}


# We have a look at these regions, either separately (not in the code) or toghether

border_50_km <- dist_GP %>% 
  split(.$GP_addr_string) %>% 
  map(. %>% pull(MedStat))


region <- MS_spdf %>%
  filter(MEDSTAT04 %in% unlist(border_50_km)) %>% 
  group_by(MEDSTAT04) %>% 
  summarise()

mapview(list(GP_addr.sp, region))


## Calculate driving times

if(FALSE) {
  
  # Prepare lat/lon coordinates
  
  MS_lat_lon <- dist_GP %>% 
    select(lat_MS, lon_MS) %>% 
    split(seq(nrow(.))) %>% 
    set_names(dist_GP$MedStat) %>% 
    lapply(unlist)
  
  GP_lat_lon <- dist_GP %>% 
    select(lat_GP, lon_GP) %>% 
    split(seq(nrow(.))) %>% 
    set_names(dist_GP$Ort) %>% 
    lapply(unlist)
  

# We calculate the driving times using the google directions API ----------

  google_distance_delayed <- function(x, y) {
    
    pb$tick()$print()
    
    Sys.sleep(0.5)
    
    key <- "AIzaSyCs6xFuSic9PpwWinOPA1TDaGXD_HZbQdc"
    
    drv_dist <- google_distance(origins = x, destinations = y, key = key)
    
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
  
  pb <- progress_estimated(length(MS_lat_lon)) 
  
  drv_dist <- map2_dfr(MS_lat_lon, GP_lat_lon, google_distance_delayed) %>% 
    mutate(duration_min = duration / 60, distance_km = distance / 1000)
  
  save(drv_dist, file = file.path("workspace", "drv_dist_all.RData"))
  
}

load(file.path("workspace", "drv_dist_all.RData"))


# We combine the dataset with the Medstat information and the travel distances --------

dist_MS_GP <- bind_cols(dist_GP, drv_dist)


# This variables might be useful when we want to manually check the driving times in google maps

dist_MS_GP <- mutate(dist_MS_GP, 
                  MS_address_string = str_c(lat_MS, lon_MS, sep = ", "),
                  GP_address_string = str_c(lat_GP, lon_GP, sep = ", "))


# We select the variables we want

dist_MS_GP <- select(dist_MS_GP, 
                     MedStat, Ort, duration_min, distance_km, distance_km_beeline, 
                     MS_address_string, GP_addr_string, GP_address_string, 
                     origin, destination) %>% 
  arrange(Ort, duration_min)


# We keep only regions from which a GP can be reached within 25 minutes

dist_MS_GP_25_min <- filter(dist_MS_GP, duration_min < 26) %>% 
  arrange(Ort)


# We save the dataset and clear the workspace -----------------------------

save(dist_MS_GP_25_min, file = file.path("workspace", "dist_MS_GP_25_min.RData"))

rm("border_50_km", "dist_50_km", "dist_geo", "dist_GP", "dist_MS_GP", 
     "dist_MS_GP_25_min", "drv_dist", "FL00", "GP_addr", "GP_addr.sp", 
     "GP_coordinates", "MS_coordinates", "MS_spdf", "n_rows_GP", "n_rows_MS", 
     "pop_centroids_most_pop_place", "region")
