library(tidyverse)

####1. build the database####

d13CtreeRing <- read.csv("gradienteData/isotopes_gradiente_2023/Tabla_S2025-3401_mod.csv") %>% 
  full_join(read.csv("gradienteData/alturas_individuos/dbh_height.csv"),
            by = c("site", "tree")) %>% 
  filter(year == 2023) %>% 
  select(-c(year, perc_C)) %>%
  rename(d13C_ring23 = d13C_permil)

d13CbasePh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_base_phloem.csv") %>% 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_base_phloem, sampling_date)) 

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  mutate(ratio_CN_leaf = C_perc_leaf/N_perc_leaf) %>% 
  filter(canopy_position == "shade_low") %>% 
  filter(sampling_date <= 20230731 | sampling_date >= 20230827) %>% 
  select(-c(weight_mg, d15N_leaf, canopy_position, canopy_position2, sampling_date))

d13CstemPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_stem_phloem.csv") %>%
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  filter(canopy_position == "shade_low") %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_stem_phloem, canopy_position, canopy_position2, sampling_date))

wp <- read.csv("gradienteData/wp_gradiente_2023/lwp_gradiente_2023.csv") %>% 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) %>% 
  mutate(wp_md = (wp_midday_1 + wp_midday_2)/2)

gradient <- d13CtreeRing %>% 
  full_join(wp, by = c("site", "tree")) %>% 
  relocate(d13C_ring23, .after = wp_md) %>% 
  full_join(d13CbasePh, by = c("site", "tree", "campaign")) %>% 
  full_join(d13Cleaf, by = c("site", "tree", "campaign")) %>% 
  full_join(d13CstemPh, by = c("site", "tree", "campaign")) %>% 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU")))

gradient <- subset(gradient, campaign == "spring23" | campaign == "summer23")

####2. Explore correlations####

summary(lm(d13C_base_phloem ~ d13C_stem_phloem * site * campaign,
           data = gradient))
anova(lm(d13C_base_phloem ~ d13C_stem_phloem * site * campaign,
           data = gradient))
ggplot(gradient, aes(x = d13C_base_phloem, y = d13C_stem_phloem,
                     shape = site, color = campaign)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", color = "black") +
  labs(title = "",
       x = expression("Stem phloem " * delta^13 * C~"(\u2030)"),
       y = expression("Base phloem " * delta^13 * C~"(\u2030)"),
       shape = "Site", color = "Campaign") +
  theme_minimal()
