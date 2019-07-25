
## Calculate persons with sCI per regions as reported by SwiSCI

load(file.path("workspace", "dist_MS_GP_25_min.RData"))

swisci <- read_csv2(file.path("data", "SwiSCI-Data_2015_02_09.csv")) %>% 
  select(id = `SwiSCI-ID`, MedStat)

pop_MS <- swisci %>% 
  group_by(MedStat) %>% 
  summarize(n_sci = n()) %>% 
  right_join(dist_MS_GP_25_min, by = "MedStat") %>% 
  select(MedStat, Ort, duration_min, n_sci) %>% 
  mutate(n_sci = replace_na(n_sci, 0)) %>% 
  ungroup()

pop_near_GP <- pop_MS %>% 
  group_by(Ort) %>% 
  summarize(n_sci = sum(n_sci))

pop_MS %>% n_distinct(MedStat)
  


  




