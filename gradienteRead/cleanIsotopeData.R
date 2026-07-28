library(emmeans)
library(multcomp)
library(multcompView)
library(tidyverse)

####1. Prepare the data####

# selection of data from the various campaigns and canopy positions is based on
# the results of the statistical analyses detailed in "quick_stats_plots"

d13CtreeRing <- read.csv("gradienteData/isotopes_gradiente_2023/Tabla_S2025-3401_mod.csv") %>% 
  full_join(read.csv("gradienteData/alturas_individuos/dbh_height.csv"),
             by = c("site", "tree")) %>% 
  filter(year == 2023) %>% 
  select(-c(year, perc_C)) %>%
  rename(d13C_ring23 = d13C_permil) %>% 
  relocate(d13C_ring23, .after = h_m)

d13CbasePh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_base_phloem.csv") %>% 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_base_phloem)) |> 
  rename(d13C_stem_ph = d13C_base_phloem)

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  mutate(ratio_CN_leaf = C_perc_leaf/N_perc_leaf) %>% 
  filter(canopy_position == "shade_low") %>% 
  filter(sampling_date <= 20230731 | sampling_date >= 20230827) %>% 
  select(-c(weight_mg, canopy_position, canopy_position2, sampling_date))

d13CstemPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_stem_phloem.csv") %>%
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  filter(canopy_position == "shade_low") %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_stem_phloem, canopy_position, canopy_position2, sampling_date)) |> 
  rename(d13C_branch_ph = d13C_stem_phloem)

d13C_gradiente <- full_join(d13CstemPh, d13CbasePh, by = c("site", "tree", "campaign")) |> 
  full_join(d13Cleaf, by = c("site", "tree", "campaign")) |> 
  full_join(d13CtreeRing, by = c("site", "tree")) |> 
  relocate(c(dbh_cm, h_m, d13C_ring23), .after = campaign) |> 
  relocate(sampling_date, .after = campaign) |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO")))

####2. Analyses####
#####2.1. d15N#####
hist(d13C_gradiente$d15N_leaf)
modeld15N <- lm(d15N_leaf ~ site * campaign, data = d13C_gradiente)
plot(modeld15N)
summary(modeld15N)
anova(modeld15N)
modeld15N_means <- emmeans(modeld15N, ~ site)
model_means_cld <- cld(modeld15N_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
ggplot(d13C_gradiente, aes(x = site, y = d15N_leaf, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("Leaf" * delta^15 * "N (\u2030)"),
    fill = "Campaign"
  ) +
  theme_minimal()
#####2.1. Nitrogen#####
hist(d13C_gradiente$N_perc_leaf)
modelN <- lm(N_perc_leaf ~ site * campaign, data = d13C_gradiente)
plot(modelN)
summary(modelN)
anova(modelN)
modelN_means <- emmeans(modelN, ~ site)
model_means_cld <- cld(modelN_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
ggplot(d13C_gradiente, aes(x = site, y = N_perc_leaf, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("["*N[leaf]*"]"~("%")),
    fill = "Campaign"
  ) +
  theme_minimal()

#####2.1. C:N#####
hist(d13C_gradiente$ratio_CN_leaf)
modelratio <- lm(ratio_CN_leaf ~ site * campaign, data = d13C_gradiente)
plot(modelratio)
summary(modelratio)
anova(modelratio)
modelratio_means <- emmeans(modelratio, ~ site)
model_means_cld <- cld(modelratio_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
ggplot(d13C_gradiente, aes(x = site, y = ratio_CN_leaf, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("["*C[leaf]*"]/["*N[leaf]*"]"),
    fill = "Campaign"
  ) +
  theme_minimal()
