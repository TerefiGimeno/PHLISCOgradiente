library(emmeans)
library(multcomp)
library(multcompView)
library(tidyverse)
library(ggsignif)

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
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) |> 
  mutate(campaign = ifelse(campaign == "spring23", "Spring", "Summer"))

####2. Analyses####
#####2.1. Leaf stoichiometry#####
######2.1.1. d15N######
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
######2.1.2. Nitrogen######
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

######2.1.3. C:N######
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

#####2.2. d13C#####
######2.2.1 Leaf d13C######
hist(d13C_gradiente$d13C_leaf)
modeld13C_leaf <- lm(d13C_leaf ~ site * campaign, data = d13C_gradiente)
plot(modeld13C_leaf)
summary(modeld13C_leaf)
anova(modeld13C_leaf)
modeld13Cleaf_means <- emmeans(modeld13C_leaf, ~ site)
model_means_cld <- cld(modeld13Cleaf_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
ggplot(d13C_gradiente, aes(x = site, y = d13C_leaf, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  geom_signif(y_position=c(-27.1, -27.1, -26.5), xmin=c(0.65, 3.5, 0.65), 
              xmax=c(3.45, 5.5, 5.5), annotation=c("ns", "ns", "*"),
              tip_length=0.06, vjust = -0.2, textsize = 6, color="grey40") +
  scale_fill_manual(values=c("#EF476F", "#FFD166")) +
  ylim(-35, -26)+
  labs(
    x = "",
    y = expression("Bulk " * delta^13*C[leaf]~"(\u2030)"),
    fill = "Campaign"
  ) +
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = .5),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = c(0.98, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid = element_blank()
  )

######2.2.2 Branch phloem d13C######
hist(d13C_gradiente$d13C_branch_ph)
modeld13C_branch_ph <- lm(d13C_branch_ph ~ site * campaign, data = d13C_gradiente)
plot(modeld13C_branch_ph)
summary(modeld13C_branch_ph)
anova(modeld13C_branch_ph)
modeld13C_branch_ph_means <- emmeans(modeld13C_branch_ph, ~ site * campaign)
model_means_cld <- cld(modeld13C_branch_ph_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
ggplot(d13C_gradiente, aes(x = site, y = d13C_branch_ph, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("#EF476F", "#FFD166")) +
  ylim(-35, -26)+
  labs(
    x = "",
    y = expression("Phloem " * delta^13*C[branch]~"(\u2030)"),
    fill = "Campaign"
  ) +
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = .5),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = c(0.98, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid = element_blank()
  )

######2.2.3 Stem phloem d13C######
hist(d13C_gradiente$d13C_stem_ph)
modeld13C_stem_ph <- lm(d13C_stem_ph ~ site * campaign, data = d13C_gradiente)
plot(modeld13C_stem_ph)
summary(modeld13C_stem_ph)
anova(modeld13C_stem_ph)
modeld13C_stem_ph_means <- emmeans(modeld13C_stem_ph, ~ site * campaign)
model_means_cld <- cld(modeld13C_stem_ph_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
ggplot(d13C_gradiente, aes(x = site, y = d13C_stem_ph, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("#EF476F", "#FFD166")) +
  ylim(-35, -24)+
  labs(
    x = "",
    y = expression("Phloem " * delta^13*C[stem]~"(\u2030)"),
    fill = "Campaign"
  ) +
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = .5),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = c(0.98, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid = element_blank()
  )

######2.2.4 Ring cellulose d13C######
hist(d13C_gradiente$d13C_ring23)
modeld13C_ring <- lm(d13C_ring23 ~ site, data = d13C_gradiente)
plot(modeld13C_ring)
summary(modeld13C_ring)
anova(modeld13C_ring)
modeld13C_ring_means <- emmeans(modeld13C_ring, ~ site)
model_means_cld <- cld(modeld13C_ring_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
ggplot(d13C_gradiente, aes(x = site, y = d13C_ring23, fill = "#06D6A0")) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  ylim(-35, -24.3)+
  labs(
    x = "",
    y = expression("Cellulose " * delta^13*C[ring]~"(\u2030)"),
    ) +
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = .5),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 12),
    )




