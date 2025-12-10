library(tidyr)
library(dplyr)

d13Cphloem <-
  read.csv("gradienteData/isotopes_gradiente_2023/table01.csv") %>% 
  filter(stringr::str_detect(id, "_")) %>% 
  separate(id, into = paste0("col" , 1:6), sep = "_",
           fill = "right", extra = "drop")

write.csv(d13Cphloem, file = "gradienteData/isotopes_gradiente_2023/kk.csv",
          row.names = F)


