
## Calculate persons with sCI per regions as reported by SwiSCI

library(tidyverse)

load(file.path("workspace", "dist_MS_GP_25_min.RData"))

swisci <- read_csv2(file.path("data", "SwiSCI-Data_2015_02_09.csv")) %>% 
  select(id = `SwiSCI-ID`, MedStat)


# Number of persons with SCI per Medstat region

pop_MS <- swisci %>% 
  group_by(MedStat) %>% 
  summarize(n_sci = n()) %>% 
  right_join(dist_MS_GP_25_min, by = "MedStat") %>% 
  select(MedStat, Ort, duration_min, n_sci) %>% 
  mutate(n_sci = replace_na(n_sci, 0)) %>% 
  ungroup()


# MedStat regions from which multiple GP practices can be reaced within 25 minutes

common_MedStats <- pop_MS %>% 
  count(MedStat) %>% 
  filter(n > 1) %>% 
  pull(MedStat)


# Define regions where GPs have the monopoly over a specific region

pop_MS <- pop_MS %>% 
  mutate(n_sci_exclusiv = if_else(MedStat %in% common_MedStats, 0, n_sci))


# Determine which GPs can be reached from which region

MS_which_reach_which_GP <- pop_MS %>% 
  filter(MedStat %in% common_MedStats) %>% 
  split(.$MedStat) %>% 
  map(2) %>% 
  map_chr(str_flatten, collapse = " & ") %>% 
  enframe(name = "MedStat", value = "GP_in_25_min")

pop_MS <- pop_MS %>% 
  left_join(MS_which_reach_which_GP, by = "MedStat") %>% 
  mutate(GP_in_25_min = if_else(is.na(GP_in_25_min), Ort, GP_in_25_min)) %>% 
  select(Ort, everything())

# Determine number of persons with SCI per GP
# - In total
# - Exclusively
# - Shared with other GPs that possibly take part in the project

n_SCI_per_region <- pop_MS %>% 
  group_by(Ort) %>% 
  summarize(n_sci_exclusiv = sum(n_sci_exclusiv),
            n_sci_possible = sum(n_sci),
            competing_for = n_sci_possible - n_sci_exclusiv)


get_competitors <- function(x) {
  
  pop_MS %>% 
    filter(Ort == x) %>% 
    pull(GP_in_25_min) %>% 
    str_split(" & ") %>% 
    unlist() %>% 
    unique() %>% 
    setdiff(x)
  
}

competitors <- map(unique(pop_MS$Ort), get_competitors) %>% 
  set_names(unique(pop_MS$Ort)) %>% 
  enframe(name = "Ort", value = "competing_with")

competitors$competing_with[competitors$competing_with %>% map_dbl(length) == 0] <- "-"
competitors$competing_with <- map_chr(competitors$competing_with, str_flatten, collapse = " & ")

n_SCI_per_region <- full_join(n_SCI_per_region, competitors, by = "Ort")




