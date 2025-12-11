library(tidyr)
library(dplyr)

trees <- read.csv("gradienteData/isotopes_gradiente_2023/Tabla_S2025-3401_mod.csv") %>%
  right_join(read.csv("gradienteData/alturas_individuos/dbh_height.csv"),
             by = c("site", "tree")) %>% 
  filter(year == 2023) %>% 
  select(-c(year, perc_C)) %>%
  rename(d13C_ring23 = d13C_permil) %>% 
  mutate(campaign = c(rep("winter24", times = nrow(trees))))
  
    
d13Cphloem <-
  read.csv("gradienteData/isotopes_gradiente_2023/table01.csv") %>% 
  filter(stringr::str_detect(id, "_")) %>% 
  separate(id, into = paste0("col" , 1:6), sep = "_",
           fill = "right", extra = "drop")

write.csv(d13Cphloem, file = "gradienteData/isotopes_gradiente_2023/kk.csv",
          row.names = F)


