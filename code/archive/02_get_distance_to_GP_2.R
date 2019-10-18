
library(tidyverse)
library(sf)
library(ggmap)
library(mapview)
library(readxl)
library(geosphere)
library(googleway)


# Get centroids of Medstat regions

load(file.path("workspace", "k_means_centroid.RData"))
load(file.path("workspace", "pop_centroids_most_pop_place.RData"))

# Read Medstat shapefile

MS_spdf <- st_read(file.path("data", "MedStat_GIS", "MedStat_Base_2011_region.shp"), 
                   as_tibble = TRUE, options = "ENCODING=WINDOWS-1252")


# Get GP addresses --------------------------------------------------------

GP_addr <- read_csv2(file.path("data", "gp_addresses.csv")) %>% 
  mutate_all(str_trim) %>% 
  mutate(PLZ = as.numeric(PLZ)) %>% 
  mutate(GP_addr_string = str_c(street, PLZ, Ort, "Schweiz", sep = " + "))


# Register google API using API key

register_google("AIzaSyCs6xFuSic9PpwWinOPA1TDaGXD_HZbQdc")

# Do not run code

if(FALSE) {
  
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


FL00 <- tibble(MedStat = "FL00", lon = 9.520915, lat = 47.141091) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>% 
  add_column (lon = 9.520915, lat = 47.141091)
  

MS_coordinates <- pop_centroids_most_pop_place %>% 
  bind_rows(.id = "MedStat") %>% 
  st_as_sf(coords = c("X_Koord", "Y_Koord"), crs = st_crs(MS_spdf)$proj4string) %>% 
  st_transform("+proj=longlat +datum=WGS84")

mapview(MS_coordinates)

MS_coordinates <- MS_coordinates %>% 
  add_column(lon = st_coordinates(MS_coordinates)[, 1]) %>% 
  add_column(lat = st_coordinates(MS_coordinates)[, 2]) 
  
MS_coordinates <- rbind(MS_coordinates, FL00)

n_rows_MS <- nrow(MS_coordinates)
n_rows_GP <- nrow(GP_addr)


MS_coordinates <- MS_coordinates %>% 
  mutate(freq = n_rows_GP) %>% 
  uncount(freq) %>% 
  rename(lon_MS = lon, lat_MS = lat)

GP_addr <- do.call("rbind", replicate(n_rows_MS, GP_addr, simplify = FALSE)) %>% 
  rename(lon_GP = lon, lat_GP = lat)

dist_GP <- bind_cols(MS_coordinates, GP_addr)




if(TRUE) {
  

dist_geo <- function(lat_a, lon_a, lat_b, lon_b) { 
  
  if(anyNA(c(lat_a, lon_a, lat_b, lon_b))) return(NA) 
  
  round(distm(c(lon_a, lat_a), c(lon_b, lat_b), fun = distHaversine)/1000,2) 
  
  } 

dist_GP$distance_km <- mapply(
  
  lat_a = dist_GP$lat_MS, 
  lon_a = dist_GP$lon_MS, 
  lat_b = dist_GP$lat_GP, 
  lon_b = dist_GP$lon_GP, 
  
  FUN = dist_geo)

dist_50_km <- filter(dist_GP, distance_km < 50)

dist_GP <- dist_50_km

}

border_50_km <- dist_GP %>% split(.$GP_addr_string) %>% map(. %>% pull(MedStat))

function(x) {x %>% filter(Med)}

names(border_50_km)
GP_addr.sp$Ort

region <- MS_spdf %>%
  filter(MEDSTAT04 %in% unlist(border_50_km)) %>% 
  group_by(MEDSTAT04) %>% 
  summarise()

mapview(list(GP_addr.sp, region))


dist_GP <- mutate(dist_GP, 
       MS_address_string = str_c(lat_MS, lon_MS, sep = ", "),
       GP_address_string = str_c(lat_GP, lon_GP, sep = ", "))



## Calculate driving times

if(FALSE) {
  
  key <- "AIzaSyCs6xFuSic9PpwWinOPA1TDaGXD_HZbQdc"
  
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
  
  google_distance_delayed <- function(x,y) {
    
    pb$tick()$print()
    
    Sys.sleep(0.5)
    
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

load(file.path("workspace", "drv_dist.RData"))

dist_MS_GP <- bind_cols(dist_GP, drv_dist)

dist_GP <- rename(dist_MS_GP, distance_km_beeline = distance_km) 

dist_GP <- select(dist_GP, MedStat, Ort, GP_addr_string, MS_address_string, GP_address_string, origin, destination, duration_min, distance_km, distance_km_beeline)



names(dist_MS_GP)
