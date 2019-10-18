
## Calculate persons with sCI per regions as reported by SwiSCI

library(tidyverse)

load(file.path("workspace", "dist_MS_GP_25_min.RData"))
load(file.path("workspace", "dist_MS_cent_25_min.RData"))



# Get number of persons with SCI per Medstat region -----------------------

sci_2012 <- read_csv2(file.path("data", "SwiSCI-Data_2015_02_09.csv")) %>% 
  select(id = `SwiSCI-ID`, MedStat) %>% 
  mutate(year = 2012)


sci_2017 <- read_csv2(file.path("data", "2018-C-006_Fragebogen2_2019_02_27.csv")) %>% 
  
  select(
    id = id_swisci,
    MedStat = medstat) %>% 
  
  mutate(year = 2017)

sci <- bind_rows(sci_2012, sci_2017) %>% 
  arrange(id, desc(year)) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  drop_na(MedStat)

rm(sci_2012, sci_2017)


# Calculate numbers of persons with SCI near GPs or specialists --------------------------------

dist_providers <- bind_rows(dist_MS_GP_25_min, dist_MS_cent_25_min) %>% 
  
  select(MedStat, Ort) %>% 
  
  mutate(Ort = case_when(Ort == "Basel" ~ "REHAB",
                         Ort == "Bellinzona" ~ "Amb_Bellinzona",
                         Ort == "Lausanne" ~ "Plein soleil",
                         Ort == "Nottwil" ~ "SPZ",
                         Ort == "Sion" ~ "Crr Suva",
                         Ort == "Zürich" ~ "Balgrist",
                         TRUE ~ as.character(Ort)))


pop_MS <- sci %>% 
  group_by(MedStat) %>% 
  summarize(n_sci = n()) %>% 
  right_join(dist_providers, by = "MedStat") %>%
  mutate(n_sci = replace_na(n_sci, 0)) %>% 
  ungroup() %>% 
  arrange(MedStat)

save(pop_MS, file = file.path("workspace", "pop_MS.RData"))


# Find competing providers for each region

competitors <- pop_MS %>% 
  split(.$MedStat) %>% 
  map(`[`, "Ort") %>% 
  map_chr(function(x) {str_c(pull(x), collapse = "; ")}) %>% 
  enframe(name = "MedStat", value = "competitors")

comp_region <- pop_MS %>% select(MedStat, n_sci) %>% 
  left_join(competitors, by = "MedStat") %>% 
  arrange(MedStat)


# Remove regions from where only a specialist can be reached

GP_locations <- c("Chur", "Klosters", "Promotogno", "Reichenbach", "Sargans",
                  "St. Gallen", "Sta. Maria V.M.", "Thun", "Weinfelden", "Yverdon")

specialist_locations <- c("REHAB", "Amb_Bellinzona", "Plein soleil", "SPZ", "Crr Suva", "Balgrist")


# Remove specialists only regions

specialists_only <- comp_region %>% 
  filter(competitors %in% specialist_locations) %>% 
  pull(MedStat) %>% 
  unique()

comp_region <- comp_region %>% filter(!MedStat %in% specialists_only)


# Remove duplicates

comp_region <- comp_region %>% distinct(.keep_all = FALSE)


# Find regions from where not only a GP but also a specialist can be reached

specialist_string <- str_c(specialist_locations, collapse = "|")

comp_region <- mutate(comp_region, spec_cent = map_int(competitors, ~ str_detect(., specialist_string)))


# Create some summery tables

# 1) Region characteristics

region_characteristics <- comp_region %>% 
  
  summarize(total_regions = n(),
            regions_spec_nearby = sum(spec_cent),
            regions_GP_exclusively = n() - sum(spec_cent)) %>% 
  
  t() %>% as_tibble(rownames = "id") %>% set_names(c("characteristics", "n"))

write_csv2(region_characteristics, file.path("output", "characteristics_Medstat_regions.csv"))


# 2) Individuals characteristics

individuals_characteristics <- comp_region %>% 
  
  summarize(total_pers_in_regions = sum(n_sci),
            lost_pers_due_to_spec = sum(comp_region[spec_cent == 1, ]$n_sci),
            pers_left_for_study = total_pers_in_regions - lost_pers_due_to_spec) %>% 
  
  t() %>% as_tibble(rownames = "id") %>% set_names(c("characteristics", "n"))

write_csv2(individuals_characteristics, file.path("output", "indiv_characteristics_Medstat_regions.csv"))


# 3) Potential patients per region or group of regions including specialists

shared_patients_w_spec <- comp_region %>% 
  group_by(competitors) %>% 
  summarize(n_sci = sum(n_sci)) %>% 
  filter(n_sci != 0)

write.csv2(shared_patients_w_spec, file.path("output", "shared_patients_w_spec.csv"), row.names = FALSE)


# Potential patients for each GP without specialist interference

pot_pat_GP <- pop_MS %>% 
  filter(!Ort %in% specialist_locations) %>% 
  group_by(Ort) %>% 
  summarize(n_sci_potential = sum(n_sci)) %>% 
  add_column("Potential patients for each GP without specialists interference" = "")

write.csv2(pot_pat_GP, file.path("output", "pot_pat_GP.csv"), row.names = FALSE)


# 5) Persons with SCI per region or group of regions excluding specialists
# When there is only one name these regions have the patients exclusively

shared_patients_w_o_spec <- comp_region %>% 
  filter(spec_cent == 0) %>% 
  group_by(competitors) %>% 
  summarize(n_sci = sum(n_sci)) %>% 
  filter(n_sci != 0) %>% 
  add_column("Persons with SCI per region or group of regions excluding specialists" = "")


# Clear workspace

rm("comp_region", "dist_MS_cent_25_min", "dist_MS_GP_25_min", 
     "dist_providers", "GP_locations", "individuals_characteristics", 
     "pop_MS", "pot_pat_GP", "region_characteristics", "sci", "shared_patients_w_o_spec", 
     "shared_patients_w_spec", "specialist_locations", "specialist_string", 
     "specialists_only", "competitors")
