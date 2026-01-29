library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  select(-c(weight_mg, d15N_leaf)) %>% 
  mutate(ratio_CN_leaf = C_perc_leaf/N_perc_leaf)

d13Cleaf_short <- subset(d13Cleaf, sampling_date <= 20230731 |
                           sampling_date >= 20230827)

hist(d13Cleaf_short$d13C_leaf)
summary(lm(d13C_leaf ~ site * campaign, data = d13Cleaf_short))
anova(lm(d13C_leaf ~ site * campaign, data = d13Cleaf_short))
TukeyHSD(aov(d13C_leaf ~ site * campaign, data = d13Cleaf_short))
one <- ggplot(d13Cleaf_short, aes(x = site, y = d13C_leaf, fill = campaign)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Leaf" ~ delta^13 * "C (\u2030)"),
    fill = "Campaign"
  ) +
  theme_minimal()


d13CbasePh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_base_phloem.csv") %>% 
  select(-c(d15N_base_phloem))
d13CbasePh <- subset(d13CbasePh, campaign == "spring23" | campaign == "summer23")
hist(d13CbasePh$d13C_base_phloem)
summary(lm(d13C_base_phloem ~ site * campaign, data = d13CbasePh))
anova(lm(d13C_base_phloem ~ site * campaign, data = d13CbasePh))
TukeyHSD(aov(d13C_base_phloem ~ site, data = d13CbasePh))
two <- ggplot(d13CbasePh, aes(x = site, y = d13C_base_phloem, fill = campaign)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Phloem " * delta^13 * "C (\u2030)"),
    fill = "Campaign"
  ) +
  theme_minimal()

d13CtreeRing <- read.csv("gradienteData/isotopes_gradiente_2023/Tabla_S2025-3401_mod.csv")
hist(d13CtreeRing$d13C_permil)
summary(lm(d13C_permil ~ site, data = d13CtreeRing))
anova(lm(d13C_permil ~ site, data = d13CtreeRing))
TukeyHSD(aov(d13C_permil ~ site, data = d13CtreeRing))
three <- ggplot(d13CtreeRing, aes(x = site, y = d13C_permil)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Ring " * delta^13 * "C (\u2030)"),
    fill = "Campaign"
  ) +
  theme_minimal()

cowplot::plot_grid(one, two, three, labels = "AUTO", ncol = 3)


trees <- read.csv("gradienteData/isotopes_gradiente_2023/Tabla_S2025-3401_mod.csv") %>%
  right_join(read.csv("gradienteData/alturas_individuos/dbh_height.csv"),
             by = c("site", "tree")) %>% 
  filter(year == 2023) %>% 
  select(-c(year, perc_C)) %>%
  rename(d13C_ring23 = d13C_permil) %>% 
  mutate(campaign = c(rep("winter24", times = nrow(trees))))

  
    

