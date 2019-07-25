## Find center of each Medstat region based on the density of the local population using K-Means

library(tidyverse)
library(sf)


# Identify places of highest population density in each Medstat region --------


# Read Medstat polygons

MS_spdf <- st_read(file.path("data", "MedStat_GIS", "MedStat_Base_2011_region.shp"), as_tibble = TRUE, options = "ENCODING=WINDOWS-1252") %>% 
  filter(KT != "FL")


# Read number of persons by hectare

pop_hectare <- read_csv2(file.path("data", "STATPOP2012B.csv"))

# Hectares with 1 to 3 inhabitants are coded with 3 inhabitans for data securty reasons.
# The get a more appropirate picture of the number of inhabitants we replace every 3 with a number of 1 to 3.

(n_threes <- sum(pop_hectare$B12BTOT == 3))

set.seed(84)
replacement_values <- sample(1:3, n_threes, replace = TRUE)

table(replacement_values)

pop_hectare[(pop_hectare$B12BTOT == 3), ]$B12BTOT <- replacement_values

rm(n_threes, replacement_values)


# Retrieve coordinate reference system from Medstat spatial polygon

crs_MS_spdf <- st_crs(MS_spdf)[["proj4string"]]

# Transform coordinates into spatial points

pop_hectare_sp <- st_as_sf(x = pop_hectare, coords = c("X_KOORD", "Y_KOORD"), crs = crs_MS_spdf) %>% 
  select(RELI, B12BTOT, geometry)


# Sample from the coordinates

pop_hectare_sp_sample <- pop_hectare_sp[sample(nrow(pop_hectare_sp), 10000), ]


# Plot points into Medstat polygons (they fit nicely into the polygons)
# Hence we use the same coordinates system for the medstat regions and the plot

plot(st_geometry(MS_spdf), col = "red")
plot(st_geometry(pop_hectare_sp_sample), type = "p", size = 0.1, add = TRUE)

rm(pop_hectare_sp_sample)


# which points (hectares and related numbers of people) fall inside which polygon (Medstat regions)

my_intersects <- st_intersects(MS_spdf, pop_hectare_sp)

get_pop_centroids <- function(i, intersects) {
  
  pb$tick()$print()
  
  index_region <- i
  
  name_MedStat <- MS_spdf$MEDSTAT04[index_region]
  
  my_intersects <- my_intersects[[index_region]]
  
  my_region <- pop_hectare_sp[my_intersects, ]
  
  
  # Create unweighted population density centroid (yellow circle)
  
  my_region <- st_coordinates(pop_hectare_sp[my_intersects, ])
  x <- mean(my_region[, 1])
  y <- mean(my_region[, 2])
  
  center_crude <- tibble(x, y)
  center_crude_sp <- st_as_sf(x = center_crude, coords = c("x", "y"), crs = crs_MS_spdf)
  
  
  # Create weighted population density centroid (blue circle)
  
  my_region <- pop_hectare_sp[my_intersects, ]
  my_region$weight <- my_region$B12BTOT / sum(my_region$B12BTOT)
  
  x <- weighted.mean(st_coordinates(my_region)[, 1], my_region$weight)
  y <- weighted.mean(st_coordinates(my_region)[, 2], my_region$weight)
  
  center_weighted <- tibble(x, y)
  center_weighted_sp <- st_as_sf(x = center_weighted, coords = c("x", "y"), crs = crs_MS_spdf)
  
  
  # Calculate K Means clusters (red star)
  
  coords <- st_coordinates(pop_hectare_sp[my_intersects, ])
  data <- st_set_geometry(pop_hectare_sp[my_intersects, ], NULL)
  
  my_tibble <- as_tibble(data.frame(data, coords)) %>% 
    rename(X_Koord = X, Y_Koord = Y)
  
  weighted_tib_for_k_means = tibble(
    X_Koord = rep(my_tibble$X_Koord, my_tibble$B12BTOT),
    Y_Koord = rep(my_tibble$Y_Koord, my_tibble$B12BTOT)
  )
  
  clust_info <- kmeans(weighted_tib_for_k_means, 2)
  
  k_means_center <- data.frame(X_Koord = clust_info$centers[1,1], Y_Koord = clust_info$centers[1,2])
  
  
  # Find coordinates of region with highest number of inhabitants (green square)
  
  most_pop_place <- my_tibble %>% 
    filter(B12BTOT == max(my_tibble$B12BTOT)) %>% 
    filter(row_number()==1) %>% 
    select(X_Koord, Y_Koord)
  
  my_plot <- ggplot() +
    geom_sf(data = MS_spdf[index_region, ])+
    geom_point(data = my_tibble, aes(x = X_Koord, y = Y_Koord, size = B12BTOT)) +
    geom_point(data = k_means_center, aes(x = X_Koord, y = Y_Koord), colour = "red", shape = 8, stroke = 2, size = 6) +
    geom_point(data = most_pop_place, aes(x = X_Koord, y = Y_Koord), colour = "green", shape = 0, stroke = 2, size = 6) +
    geom_sf(data = center_crude_sp, colour = "yellow", size = 5, stroke = 2, shape = 21) +
    geom_sf(data = center_weighted_sp, colour = "blue", size = 5, stroke = 2, shape = 21) +
    ggtitle(name_MedStat)
  
  
  return(list(name_MedStat, center_crude_sp, center_weighted_sp, k_means_center, most_pop_place, my_plot))
  
}

# Run function over all MedStat regions


pb <- progress_estimated(705)
my_results <- map(1:705, get_pop_centroids, intersects = my_intersects)

# Get Medstat names

name_MedStat <- map(my_results, 1)


# Get population centroids

pop_centroids_avg_crd <- map(my_results, 2)

pop_centroids_avg_wgt <- map(my_results, 3)

pop_centroids_km <- map(my_results, 4)

pop_centroids_most_pop_place <- map(my_results, 5)


# Rename list of population centroids

names(pop_centroids_avg_crd) <- name_MedStat
names(pop_centroids_avg_wgt) <- name_MedStat
names(pop_centroids_km) <- name_MedStat
names(pop_centroids_most_pop_place) <- name_MedStat

# Get all plots of population centroids in Medstat spatial polygons

plots_list <- map(my_results, 6)


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


# The blue circle is the weighted population centroid 

GG_save_pdf(plots_list, file.path("output", "centroids.pdf"))

save(pop_centroids_km, file = file.path("workspace", "k_means_centroid.RData"))
save(pop_centroids_most_pop_place, file = file.path("workspace", "pop_centroids_most_pop_place.RData"))

rm("crs_MS_spdf", "get_pop_centroids", "GG_save_pdf", "MS_spdf", 
  "my_intersects", "my_results", "name_MedStat", "pb", "plots_list", 
  "pop_centroids_avg_crd", "pop_centroids_avg_wgt", "pop_centroids_km", 
  "pop_centroids_most_pop_place", "pop_hectare", "pop_hectare_sp")
