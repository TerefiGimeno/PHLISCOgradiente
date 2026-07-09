library(tidyr)
library(dplyr)

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  select(-c(weight_mg, d15N_leaf)) %>% 
  mutate(ratio_CN_leaf = C_perc_leaf/N_perc_leaf)

msa <- d13Cleaf %>% 
  filter(site == "MSA") %>% 
  mutate(campaign2 = ifelse(sampling_date >= 20230831, 'late_summer23', campaign))

hist(msa$d13C_leaf)
hist(msa$C_perc_leaf)
hist(msa$N_perc_leaf)
hist(msa$ratio_CN_leaf)

summary(aov(C_perc_leaf ~ canopy_position * campaign2, data = msa))
summary(aov(ratio_CN_leaf ~ canopy_position * campaign, data = msa))

crap <- meteo_art
crap$myTmean <- (crap$TmaxDay_C + crap$TminDay_C)*0.5
plot(crap$TmeanDay_C ~ crap$myTmean)
summary(lm(TmeanDay_C ~ myTmean, data = crap))
plot(crap$TmaxDay_C ~ crap$date, xlim = c(as.Date("1992-1-1"), max(crap$date)),
     ylim =c(min(crap$TminDay_C, na.rm =T), max(crap$TmaxDay_C, na.rm =T)), col = "red", pch = 19)
points(crap$TminDay_C ~ crap$date, pch = 19, col = "blue")
points(crap$TmeanDay_C ~ crap$date, pch = 19, col = "green")
