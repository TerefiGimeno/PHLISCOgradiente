library(tidyverse)

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  select(-c(weight_mg, d15N_leaf)) %>% 
  mutate(ratio_CN_leaf = C_perc_leaf/N_perc_leaf)

d13CbasePh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_base_phloem.csv") %>% 
  select(-c(d15N_base_phloem))


d13CtreeRing <- read.csv("gradienteData/isotopes_gradiente_2023/Tabla_S2025-3401_mod.csv") %>% 
  filter(year == 2023) %>% 
  select(-c(year, perc_C)) %>%
  rename(d13C_ring23 = d13C_permil) %>% 
  mutate(campaign = "winter24")

d13CstemPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_stem_phloem.csv")


trees <- read.csv("gradienteData/isotopes_gradiente_2023/Tabla_S2025-3401_mod.csv") %>%
  right_join(read.csv("gradienteData/alturas_individuos/dbh_height.csv"),
             by = c("site", "tree")) %>% 
  

  
    

