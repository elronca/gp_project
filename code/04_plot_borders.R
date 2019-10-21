## Plot persons per region and borders

library(tidyverse)
library(sf)
library(nngeo)
library(RColorBrewer)
library(gridExtra)


# Personen pro Region um Hausarzt

load(file = file.path("workspace", "pop_MS.RData"))


# Shapefiles von MedStat Regionen und Schweizer Seen

ms_sp_raw <- st_read(file.path("data", "MedStat_GIS", "MedStat_Base_2011_region.shp"), 
                     as_tibble = TRUE, options = "ENCODING=WINDOWS-1252",
                     stringsAsFactors = FALSE)

lakes <- rbind(
  
  st_read(file.path("data", "shapfiles_lakes", "k4seenyyyymmdd11_ch2007Poly.shp"), stringsAsFactors = FALSE),
  st_read(file.path("data", "shapfiles_lakes", "k4seenyyyymmdd22_ch2007Poly.shp"), stringsAsFactors = FALSE)
  
)

# Hausärzteadressen

load(file.path("workspace", "GP_addr.sp.Rdata"))
load(file.path("workspace", "cent_addr.sp.Rdata"))

GP_addresses.sp <- st_transform(GP_addresses.sp, crs = st_crs(ms_sp_raw)$proj4string)


# Bereite Anzahl Personen pro Region vor

ppr <- pop_MS %>% 
  filter(Ort %in% c("Chur", "Klosters", "Promotogno", "Reichenbach", "Sargans",
                    "St. Gallen", "Thun", "Weinfelden", "Yverdon", "Meiringen") | str_detect(Ort, "Maria")) %>% 
  
  select(MedStat, n_sci) %>% 
  
  distinct(MedStat, .keep_all = TRUE) %>% 
  
  mutate(n_sci = as.integer(n_sci), 
         n_sci = if_else(MedStat == "FL00", NA_integer_, n_sci)) %>% 
  mutate(n_sci_cat = cut(n_sci,
                         breaks = c(-Inf, 0.99, 2, 5, 10, Inf),
                         labels = c("0", "1-2", "3-5", "6-10", "10-17")))

ms_sp <- left_join(ms_sp_raw, ppr, by = c("MEDSTAT04" = "MedStat"))



# Get 25-minutes radius around GP polygons ----------

GP_MS <- pop_MS %>% 
  filter(Ort %in% c("Chur", "Klosters", "Promotogno", "Reichenbach", "Sargans",
                    "St. Gallen", "Thun", "Weinfelden", "Yverdon", "Meiringen") | str_detect(Ort, "Maria")) %>% 
  split(.$Ort) %>% 
  map("MedStat")


# Get 25-minutes radius around specialist services providers ----------

spec_MS <- pop_MS %>% 
  filter(Ort %in% c("REHAB", "Amb_Bellinzona", "Plein soleil", "SPZ", "Crr Suva", "Balgrist")) %>% 
  split(.$Ort) %>% 
  map("MedStat")


# Extrahiere die 25-Minuten Grenzen um die Hausärzte und Spezialisten

get_borders <- function(x) {
  ms_sp_raw %>%
    filter(MEDSTAT04 %in% x) %>% 
    st_union() %>% 
    nngeo::st_remove_holes()
  
}

GP_borders <- map(GP_MS, get_borders)
spec_borders <- map(spec_MS, get_borders)



# Get the outer rings of the 25 minutes borders around the GPs as one polygon

all_GP_borders <- do.call(c, GP_borders)
if(FALSE) {all_GP_borders %>% ggplot() + geom_sf()}

# Get the outer rings of the 25 minutes borders around the clinics as one polygon

all_spec_borders <- do.call(c, spec_borders)


# PSCICO people want Bellinzona separately for whatever reason

spec_borders_excl_Bellinzona <- spec_borders
spec_borders_excl_Bellinzona[["Amb_Bellinzona"]] <- NULL
spec_borders_excl_Bellinzona <- do.call(c, spec_borders_excl_Bellinzona)

if(FALSE) {all_spec_borders %>% ggplot() + geom_sf()}
if(FALSE) {spec_borders_excl_Bellinzona %>% ggplot() + geom_sf()}


# Get all the Medstat regions inside the 25 minutes borders.
# They will be later used to plot

GP_regions <- ms_sp_raw %>% filter(MEDSTAT04 %in% unlist(GP_MS))
if(FALSE) {GP_regions %>% ggplot() + geom_sf()}


# Get the border of Switzerland

Swiss_border <- ms_sp_raw %>% st_union %>% nngeo::st_remove_holes()
if(FALSE) {Swiss_border %>% ggplot() + geom_sf()}


# Check with an example

if(FALSE) {
  GP_borders[["Meiringen"]] %>% ggplot() + geom_sf()
  ms_sp_raw %>% filter(MEDSTAT04 %in% GP_MS[["Meiringen"]]) %>% ggplot() + geom_sf()
}



# Plot number of individuals with SCI around GPs --------------------------

my_colors <- brewer.pal(n = length(levels(ms_sp$n_sci_cat)), name = 'BuPu')
my_colors[1] <- "grey90"


# Define theme

my_theme <- theme(
  panel.grid = element_blank(),
  line = element_blank(),
  rect = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank())


catchment_areas_GPs <- ggplot() + 
  
  geom_sf(data = lakes, fill = "darkblue", colour = NA, show.legend = FALSE) +
  
  geom_sf(data = ms_sp, mapping = aes(fill = n_sci_cat), colour = NA) +
  
  scale_fill_manual(values = my_colors, 
                    name = "Anzahl Personen", 
                    na.value = "white",
                    labels = c(levels(ms_sp$n_sci_cat), ""), 
                    guide = guide_legend(reverse = TRUE, override.aes = list(shape = NA)))+
  
  geom_sf(data = GP_regions, fill = NA, colour = "grey90", show.legend = FALSE) +
  
  geom_sf(data = all_GP_borders, fill = NA, colour = "darkgreen", size = 0.5) +
  
  geom_sf(data = spec_borders_excl_Bellinzona, fill = NA, colour = "red", size = 0.5) +
  geom_sf(data = spec_borders[["Amb_Bellinzona"]], fill = NA, colour = "#E69F00", size = 0.5) +
  
  geom_sf(data = Swiss_border, fill = NA, colour = "black", show.legend = FALSE) +
  
  geom_sf(data = GP_addresses.sp, size = 4, stroke = 1, color = "black", fill = "lawngreen", alpha = 0.8, shape = 23) +
  
  geom_sf(data = cent_addresses.sp %>% filter(place != "Bellinzona"), size = 4, stroke = 1, color = "black", fill = "red", alpha = 0.8, shape = 21) +
  geom_sf(data = cent_addresses.sp %>% filter(place == "Bellinzona"), size = 4, stroke = 1, color = "black", fill = "#E69F00", alpha = 0.8, shape = 21) +
  
  coord_sf(datum = NA) + my_theme +
  
  labs(title = "Anzahl Personen mit Querschnittlähmung in 25 min Fahrdistanz von Hausarzt", 
       caption = "Roter Punkt und Grenze = Spezialisierte Zentren\nGelber Punkt und Grenze = Ambulatorium Bellinzona\n Grüne Vierecke = Hausärzte")

ggsave(file.path("output", "map_all_GPs.pdf"), 
       plot = catchment_areas_GPs, 
       width = 25,
       height = 13,
       units = "cm")


# Plot GP regions individually --------------------------------------------

plot_GPs_individually <- function(x) {
  
  my_GP <- ms_sp %>% filter(MEDSTAT04 %in% GP_MS[[x]])
  names(my_colors) <- levels(ms_sp$n_sci_cat)
  
  my_color_sel <- my_colors[levels(ms_sp$n_sci_cat) %in% my_GP$n_sci_cat]
  my_levels <- levels(ms_sp$n_sci_cat)[levels(ms_sp$n_sci_cat) %in% my_GP$n_sci_cat]
  
  ggplot() + 
    
    geom_sf(data = lakes, fill = "darkblue", colour = NA, show.legend = FALSE) +
    
    geom_sf(data = my_GP, mapping = aes(fill = n_sci_cat), colour = NA) +
    
    scale_fill_manual(values = my_color_sel, 
                      name = "Anzahl Personen", 
                      na.value = "white",
                      labels = c(my_levels, ""), 
                      guide = guide_legend(reverse = TRUE, override.aes = list(shape = NA))) +
    
    geom_sf(data = GP_regions %>% filter(MEDSTAT04 %in% GP_MS[[x]]), fill = NA, colour = "grey90", show.legend = FALSE) +
    
    geom_sf(data = GP_borders[[x]], fill = NA, colour = "black", size = 1) +
    
    geom_sf(data = all_spec_borders, fill = NA, colour = "red", size = 0.5) +
    
    geom_sf(data = Swiss_border, fill = NA, colour = "black", show.legend = FALSE) +
    
    geom_sf(data = GP_addresses.sp %>% filter(Ort == x), 
            size = 7, stroke = 2, color = "black", fill = "lawngreen", alpha = 0.5, shape = 23) +
    
    geom_sf(data = cent_addr.sp, size = 4, stroke = 1, color = "black", fill = "red", alpha = 0.8, shape = 21) +
    
    
    coord_sf(datum = NA) + my_theme +
    
    ggtitle(x) + theme(plot.title = element_text(size = 40, face = "bold")) + 
    
    theme(
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 18)
    )
  
}

my_plots <- map(names(GP_borders), plot_GPs_individually)

combined_plots <- cowplot::plot_grid(plotlist = my_plots, ncol = 2)

ggsave(file.path("output", "GPs_regions.pdf"), 
       plot = combined_plots, 
       height = 100, width = 70,
       units = "cm")


# Clear workspace and end project

rm("all_GP_borders", "all_spec_borders", "catchment_areas_GPs", 
  "cent_addr.sp", "combined_plots", "get_borders", "GP_addresses.sp", 
  "GP_borders", "GP_MS", "GP_regions", "lakes", "ms_sp", "ms_sp_raw", 
  "my_colors", "my_plots", "my_theme", "plot_GPs_individually", 
  "pop_MS", "ppr", "spec_borders", "spec_MS", "Swiss_border")

dev.off()

