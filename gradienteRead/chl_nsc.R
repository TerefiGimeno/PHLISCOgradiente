library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)

chl <- read.csv("gradienteData/chl_gradiente_2023/pigm_gradiente_2023_updated.csv") |> 
  filter(canopy_position == "shade_low") |>
  filter(date <= 20230731 | date >= 20230825) |> 
  mutate(campaign = ifelse(date >= 20230621, "summer23", "spring23")) |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) |> 
  rename(tree = id_plant) |> 
  mutate(chla_and_chlb = chla_ug_ml + chlb_ug_ml)

chl <- chl[, c("site", "campaign", "tree", "chla_ug_ml", "chlb_ug_ml",
               "chla_chlb", "chla_and_chlb")]

hist(chl$chla_ug_ml)
summary(aov(chla_ug_ml ~ site * campaign, data = chl))
TukeyHSD(aov(chla_ug_ml ~ site, data = chl))
summary(aov(chlb_ug_ml ~ site * campaign, data = chl))
TukeyHSD(aov(chlb_ug_ml ~ site, data = chl))
summary(aov(chla_and_chlb ~ site * campaign, data = chl))
TukeyHSD(aov(chla_and_chlb ~ site, data = chl))
summary(aov(chla_chlb ~ site * campaign, data = chl))
TukeyHSD(aov(chla_chlb ~ site, data = chl))

model <- lm(sla ~ site * campaign, data = sla)
summary(model)
anova(lm(sla ~ site * campaign, data = sla))
model_means2 <- emmeans(model, ~ site)
model_means_cld2 <- cld(model_means2, adjust = "sidak",
                        Letters = c("a", "b", "c", "d", "e", "f", "g"),
                        alpha = 0.05, sort = FALSE)

chla <- ggplot(chl, aes(x = site, y = chla_ug_ml, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  ylim(1, 8) +
  labs(
    x = "",
    y = expression("Chl a ("*mu*g~ml^-1*")"),
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE) 

chlb <- ggplot(chl, aes(x = site, y = chlb_ug_ml, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  ylim(1, 8) +
  labs(
    x = "",
    y = expression("Chl b ("*mu*g~ml^-1*")"),
    fill = "Campaign"
  ) +
  theme_minimal() +
  guides(fill = FALSE) 

chlSum <- ggplot(chl, aes(x = site, y = chla_and_chlb, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("[Chla] + [Chlb] ("*mu*g~ml^-1*")"),
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE) 

chlRatio <- ggplot(chl, aes(x = site, y = chla_chlb, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = "[Chla]:[Chlb]",
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE) 

cowplot::plot_grid(chla, chlb, chlSum, chlRatio, ncol = 2)

nsc <- read.csv("gradienteData/sugars_gradiente_2023/nsc_grad_2023_updated.csv") |>
  mutate(canopy_position = ifelse(is.na(canopy_position), "crap", canopy_position)) |> 
  filter(canopy_position != "sun") |>
  filter(canopy_position != "shade") |> 
  mutate(campaign = factor(campaign, levels = c("spring23", "summer23", "late_summer23"))) 
  
# explore differences among campaigns within HMO and MSA

summary(aov(umol_suc_ml ~ campaign,
            data = subset(nsc, site == "MSA" & sample_type == "SPH")))
TukeyHSD(aov(umol_suc_ml ~ campaign,
              data = subset(nsc, site == "MSA" & sample_type == "SPH")))
summary(aov(umol_suc_ml ~ campaign,
            data = subset(nsc, site == "MSA" & sample_type == "LPH")))
summary(aov(umol_suc_ml ~ campaign,
            data = subset(nsc, site == "MSA" & sample_type == "BPH")))
TukeyHSD(aov(umol_suc_ml ~ campaign,
             data = subset(nsc, site == "MSA" & sample_type == "BPH")))

branchSuc <- ggplot(subset(nsc, site == "MSA" & sample_type == "SPH"),
                    aes(x = campaign, y = umol_suc_ml)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  ylim(0, 2) +
  labs(
    x = "",
    y = expression("[Sucrose]"[branch]~"("*mu*mol~ml^-1*")"),
    ) +
  theme_minimal()+
  guides(fill = FALSE) 

leafSuc <- ggplot(subset(nsc, site == "MSA" & sample_type == "LPH"),
                  aes(x = campaign, y = umol_suc_ml)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  ylim(0, 2) +
  labs(
    x = "",
    y = expression("[Sucrose]"[leaf]~"("*mu*mol~ml^-1*")"),
  ) +
  theme_minimal()+
  guides(fill = FALSE)

trunkSuc <- ggplot(subset(nsc, site == "MSA" & sample_type == "BPH"),
                  aes(x = campaign, y = umol_suc_ml)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  ylim(0, 2) +
  labs(
    x = "",
    y = expression("[Sucrose]"[stem]~"("*mu*mol~ml^-1*")"),
  ) +
  theme_minimal()+
  guides(fill = FALSE)

cowplot::plot_grid(branchSuc, leafSuc, trunkSuc, ncol = 3)

summary(aov(umol_suc_ml ~ campaign,
            data = subset(nsc, site == "HMO" & sample_type == "BPH")))
TukeyHSD(aov(umol_suc_ml ~ campaign,
             data = subset(nsc, site == "HMO" & sample_type == "BPH")))

ggplot(subset(nsc, site == "HMO" & sample_type == "BPH"),
                   aes(x = campaign, y = umol_suc_ml)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  ylim(0, 2) +
  labs(
    x = "",
    y = expression("[Sucrose]"[stem]~"("*mu*mol~ml^-1*")"),
  ) +
  theme_minimal()+
  guides(fill = FALSE)

# MSA: lower brahcn and trunk [Sucrose] in spring than in summer,
# lower Branch [Sucrose] in late summer than in summer.
# No seasonal differences for leaf [Sucrose].
# HMO: lower trunk [Sucrose] in late summer, no differnces between sprign and summer

# keep late summer campaigns for consistency

dimensions <- read.csv("gradienteData/wd_gradiente_2023/wd_gradiente_2023_updated.csv") |> 
  mutate(surface_cm2 = pi*diameter_wd_cm*length_wd_cm)
dimensions$sample_type <- c(rep("SPH", times = nrow(dimensions)))
dimensions <- dimensions[, c("date", "id_plant", "canopy_position", "sample_type", "surface_cm2")]

nsc <- read.csv("gradienteData/sugars_gradiente_2023/nsc_grad_2023_updated.csv") |>
  mutate(canopy_position = ifelse(is.na(canopy_position), "crap", canopy_position)) |>
  left_join(dimensions, by = c("date", "id_plant", "canopy_position", "sample_type")) |> 
  mutate(surface_cm2 = ifelse(sample_type == "BPH", pi*(0.25^2)*5, surface_cm2)) |>
  mutate(DW_g = rep(NA, times = nrow(nsc))) |>
  mutate(volume_mL = ifelse(sample_type == "LPH", 0.5, 10)) |> 
  mutate(umol_suc_cm2 = umol_suc_ml * volume_mL/surface_cm2) |> 
  mutate(umol_suc_g = umol_suc_ml * volume_mL/DW_g) |> 
  filter(canopy_position != "sun") |>
  filter(canopy_position != "shade") |>
  filter(date <= 20230630 | date >= 20230827) |> 
  mutate(campaign = ifelse(date >= 20230622, "summer23", "spring23")) |>
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) |> 
  rename(tree = id_plant)

nsc <- nsc[, c("site", "campaign", "tree", "sample_type",
               "umol_suc_ml", "umol_suc_cm2", "umol_suc_g")]
nsc <- nsc |> 
  pivot_wider(names_from = sample_type, values_from = c(umol_suc_ml, umol_suc_cm2, umol_suc_g))
nsc <- nsc[, c(1:6, 8:10)] |> 
  rename(leafSuc_umol_ml = umol_suc_ml_LPH) |> 
  rename(branchSuc_umol_ml = umol_suc_ml_SPH) |> 
  rename(trunkSuc_umol_ml = umol_suc_ml_BPH) |>
  rename(leafSuc_umol_g = umol_suc_g_LPH) |> 
  rename(branchSuc_umol_cm2 = umol_suc_cm2_SPH) |> 
  rename(trunkSuc_umol_cm2 = umol_suc_cm2_BPH)

hist(nsc$leafSuc_umol_ml)
subset(nsc, leafSuc_umol_ml >= 0.8)
# there is an outlier that looks like a a measurement error -> discard
nsc[which(nsc$leafSuc_umol_ml >= 0.8), "leafSuc_umol_ml"] <- NA

hist(nsc$branchSuc_umol_ml)
hist(nsc$trunkSuc_umol_ml)

summary(aov(leafSuc_umol_ml ~ site * campaign, data = nsc))
plot(aov(leafSuc_umol_ml ~ site * campaign, data = nsc))
model_means_leaf <- emmeans(lm(branchSuc_umol_ml ~ site, data = nsc),
                              ~ site)
model_means_cld <- cld(model_means_leaf, adjust = "sidak",
                       Letters = c("A", "B", "C", "D", "E", "F", "G"),
                       alpha = 0.05, sort = FALSE)

summary(aov(branchSuc_umol_ml ~ site * campaign, data = nsc))
plot(aov(branchSuc_umol_ml ~ site * campaign, data = nsc))
model_means_branch <- emmeans(lm(branchSuc_umol_ml ~ site * campaign, data = nsc),
                              ~ site * campaign)
model_means_cld <- cld(model_means_branch, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
                       alpha = 0.05, sort = FALSE)

summary(aov(trunkSuc_umol_ml ~ site * campaign, data = nsc))
plot(aov(trunkSuc_umol_ml ~ site * campaign, data = nsc))
model_means_trunk <- emmeans(lm(trunkSuc_umol_ml ~ site * campaign, data = nsc),
                              ~ site * campaign)
model_means_cld <- cld(model_means_trunk, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
                       alpha = 0.05, sort = FALSE)

leafSuc <- ggplot(nsc, aes(x = site, y = leafSuc_umol_ml, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  ylim(-0.022, 1.61) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("[Sucrose]"[leaf]~"("*mu*mol~mL^-1*")"),
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE)

branchSuc <- ggplot(nsc, aes(x = site, y = branchSuc_umol_ml, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  ylim(-0.022, 1.61) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("[Sucrose]"[branch]~"("*mu*mol~mL^-1*")"),
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE)

trunkSuc <- ggplot(nsc, aes(x = site, y = trunkSuc_umol_ml, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  ylim(-0.022, 1.61) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("[Sucrose]"[trunk]~"("*mu*mol~mL^-1*")"),
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE)

cowplot::plot_grid(leafSuc, branchSuc, trunkSuc, ncol = 3)
