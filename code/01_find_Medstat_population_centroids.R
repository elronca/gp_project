## Find center of each Medstat region based on the density of the local population using K-Means

library(tidyverse)
library(sf)
library(ggspatial)


# Identify places of highest population density in each Medstat region --------


# Read Medstat polygons

MS_spdf <- st_read(file.path("data", "MedStat_GIS", "MedStat_Base_2011_region.shp"), 
                   as_tibble = TRUE, options = "ENCODING=WINDOWS-1252",
                   stringsAsFactors = FALSE) %>% 
  filter(KT != "FL")




# Read number of persons by hectare

pop_hectare <- read_csv2(file.path("data", "STATPOP2012B.csv"))


# Hectares with 1 to 3 inhabitants are coded with 3 inhabitans for data securty reasons.
# To get a more appropirate picture of real the number of inhabitants we replace every 3 with a random number of 1 to 3.

(n_threes <- sum(pop_hectare$B12BTOT == 3))

set.seed(84)

replacement_values <- sample(1:3, n_threes, replace = TRUE)

table(replacement_values)

pop_hectare[(pop_hectare$B12BTOT == 3), ]$B12BTOT <- replacement_values

rm(n_threes, replacement_values)



# Do the spatial stuff ----------------------------------------------------


# Retrieve coordinate reference system from Medstat spatial polygon

crs_MS_spdf <- st_crs(MS_spdf)[["proj4string"]]


# Transform coordinates of persons per hectare file into spatial points using coordinates of Medstat spatial polygon file

pop_hectare_sp <- st_as_sf(x = pop_hectare, coords = c("X_KOORD", "Y_KOORD"), crs = crs_MS_spdf) %>% 
  select(RELI, B12BTOT, geometry)


# Get a sample of the very large persons per hectare spatial points file

pop_hectare_sp_sample <- pop_hectare_sp[sample(nrow(pop_hectare_sp), 10000), ]


# Plot points into Medstat polygons (they fit nicely into the polygons)
# Hence we use the same coordinates system for the medstat regions and the plot

plot(st_geometry(MS_spdf), col = "yellow")
plot(st_geometry(pop_hectare_sp_sample), type = "p", size = 0.1, col = "blue", add = TRUE)

# If the blue points are located on the yellow map of Switzerland the coordinates system tranformation worked well


rm(pop_hectare_sp_sample)



# which points (hectares and related numbers of people) fall inside which polygon (Medstat regions)

intersects <- st_intersects(MS_spdf, pop_hectare_sp)


# Using different methods to find the center of the Medstat shapefiles --------

get_pop_centroids <- function(i, my_intersects, my_crs, pop_dens_file, map_file) {
  
  
  if(FALSE) { # This is for debugging not part of the actual loop
    
    i = 1
    my_intersects <- intersects
    my_crs <- crs_MS_spdf
    pop_dens_file <- pop_hectare_sp
    map_file <- MS_spdf
    
  } # Debugging ends here
  
  pb$tick()$print()
  
  # i = the nth Medstat region
  
  name_MS_region_i <- map_file %>% pull(MEDSTAT04) %>% pluck(i)
  
  intersects_region_i <- my_intersects %>% pluck(i)
  
  coords_MS_region_i <- pop_dens_file %>% 
    slice(intersects_region_i) %>% 
    st_coordinates() %>% 
    as_tibble()
  
  B12BTOT <- pop_dens_file %>% 
    slice(intersects_region_i) %>% 
    pull(B12BTOT)
  
  coords_pop <- coords_MS_region_i %>% 
    add_column(B12BTOT = B12BTOT)
  
  
  
  
  # Create unweighted population density centroid (yellow circle) -----------
  
  center_crude <- coords_MS_region_i %>% 
    summarize_all(mean) %>%
    st_as_sf(coords = c("X", "Y"), crs = my_crs)
  
  
  
  # Create weighted population density centroid (blue circle) ---------------
  
  pop_weights <- pop_dens_file %>% 
    slice(intersects_region_i) %>% 
    mutate(weight = B12BTOT / sum(B12BTOT)) %>% 
    pull(weight)
  
  center_pop_weighted <- coords_MS_region_i %>% 
    map_dfr(weighted.mean, w = pop_weights) %>% 
    st_as_sf(coords = c("X", "Y"), crs = my_crs)
  
  
  
  # Calculate K Means clusters (red star) -----------------------------------
  
  center_k_means <- coords_pop %>% 
    uncount(B12BTOT) %>% 
    kmeans(centers = 1) %>% 
    .$centers %>% 
    as_tibble() %>% 
    st_as_sf(coords = c("X", "Y"), crs = my_crs)
  
  # with center = 1, this is the same as the weighted population density centroid.
  
  
  
  # Find coordinates of region with highest number of inhabitants (green square) --------
  
  center_most_pop_place <- coords_pop %>% 
    filter(B12BTOT == max(B12BTOT)) %>% 
    filter(row_number() == 1) %>% 
    st_as_sf(coords = c("X", "Y"), crs = my_crs)
  
  
  # Plot the centers on the Medstat region ------------------------------------
  
  MS_plot <- ggplot() +
    geom_sf(data = map_file[i, ])+
    geom_point(data = coords_pop, aes(x = X, y = Y, size = B12BTOT)) +
    geom_sf(data = center_k_means, colour = "red", shape = 8, stroke = 2, size = 6) +
    geom_sf(data = center_most_pop_place, colour = "green", shape = 0, stroke = 2, size = 6) +
    geom_sf(data = center_crude, colour = "yellow", size = 5, stroke = 2, shape = 21) +
    geom_sf(data = center_pop_weighted, colour = "blue", size = 5, stroke = 2, shape = 21) +
    annotation_scale(location = "br") +
    ggtitle(name_MS_region_i)
  
  return(list(name_MS_region = name_MS_region_i, 
              center_crude = center_crude, 
              center_pop_weighted = center_pop_weighted, 
              center_k_means = center_k_means, 
              center_most_pop_place = center_most_pop_place, 
              MS_plot = MS_plot))
  
}

# Run function over all MedStat regions


pb <- progress_estimated(nrow(MS_spdf))

my_results <- map(.x = seq_len(nrow(MS_spdf)), .f = get_pop_centroids, my_intersects = intersects, my_crs = crs_MS_spdf, pop_dens_file = pop_hectare_sp, map_file = MS_spdf)


# Plot different Medstat centers ------------------------------------------

plots_list <- map(my_results, "MS_plot")

if(FALSE) {
  
  
  # Save plots
  
  GG_save_pdf = function(list, filename) {
    
    #start pdf
    
    pdf(filename)
    
    #loop
    
    for (p in list) {
      print(p)
    }
    
    #end pdf
    
    dev.off()
    
    invisible(NULL)
    
  }
  
  GG_save_pdf(plots_list, file.path("output", "centroids.pdf"))
  
}


# Save Medstat centers that will be used in further analysis ----------------------

center_most_pop_place <- map(my_results, "center_most_pop_place")

save(center_most_pop_place, file = file.path("workspace", "center_most_pop_place.RData"))


# Clear workspace ---------------------------------------------------------

rm("center_most_pop_place", "crs_MS_spdf", "get_pop_centroids", 
     "intersects", "MS_spdf", "my_results", "pb", "plots_list", "pop_hectare", 
     "pop_hectare_sp")

