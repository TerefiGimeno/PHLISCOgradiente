library(emmeans)
library(multcomp)
library(multcompView)
library(tidyverse)

#### Chlorophyl ####

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

#### NSC ####

dimensions <- read.csv("gradienteData/sugars_gradiente_2023/dimensions_gradiente_2023_updated.csv") |> 
  mutate(surface_cm2 = pi*diameter_cm*length_cm)
dimensions$sample_type <- c(rep("SPH", times = nrow(dimensions)))
dimensions <- dimensions[, c("date", "id_plant", "canopy_position", "sample_type", "surface_cm2")]

sla <- read.csv("gradienteData/sla_gradiente_2023/sla_gradiente_2023_updated.csv") |> 
  mutate(SLA = ifelse(SLA == 9999, NA, SLA))
sla_summary <- sla |> 
  group_by(site, campaign, canopy_position) |> 
  summarise(slaAvg = mean(SLA, na.rm = T), slaSD = sd(SLA, na.rm =T))
sla <- sla |> 
  left_join(sla_summary, by = c("site", "campaign", "canopy_position")) |> 
  mutate(slaGapFilled = ifelse(is.na(SLA), slaAvg, SLA))

dwLeaves <- read.csv("gradienteData/sugars_gradiente_2023/lph_dw_gradiente_2023.csv") |> 
  left_join(sla[, c("campaign", "id_plant", "canopy_position" , "slaGapFilled")],
            by = c("campaign", "id_plant", "canopy_position")) |>
  mutate(la_cm2 = dw_lph_g * slaGapFilled)
dwLeaves$sample_type <- c(rep("LPH", times = nrow(dwLeaves)))
dwLeaves <- dwLeaves[, c("date", "id_plant", "canopy_position", "sample_type",
                         "dw_lph_g", "la_cm2")]
nsc <- read.csv("gradienteData/sugars_gradiente_2023/nsc_grad_2023_updated.csv") |>
  mutate(canopy_position = ifelse(is.na(canopy_position), "crap", canopy_position)) |>
  left_join(dimensions, by = c("date", "id_plant", "canopy_position", "sample_type")) |> 
  left_join(dwLeaves, by = c("date", "id_plant", "canopy_position", "sample_type")) |>
  mutate(surface_cm2 = ifelse(sample_type == "BPH", pi*(0.25^2)*5, surface_cm2)) |>
  mutate(surface_cm2 = ifelse(sample_type == "LPH", la_cm2, surface_cm2)) |> 
  mutate(volume_mL = ifelse(sample_type == "LPH", 0.5, 10)) |> 
  mutate(umol_suc_cm2 = umol_suc_ml * volume_mL/surface_cm2) |> 
  mutate(umol_suc_g = umol_suc_ml * volume_mL/dw_lph_g) |> 
  filter(canopy_position != "sun") |>
  filter(canopy_position != "shade") |>
  mutate(campaign = factor(campaign, levels = c("spring23", "summer23", "late_summer23")))

# explore differences among campaigns within HMO and MSA

summary(aov(log(umol_suc_cm2) ~ campaign,
            data = subset(nsc, site == "MSA" & sample_type == "SPH")))
TukeyHSD(aov(log(umol_suc_cm2) ~ campaign,
             data = subset(nsc, site == "MSA" & sample_type == "SPH")))
summary(aov(umol_suc_g ~ campaign,
            data = subset(nsc,site == "MSA" & sample_type == "LPH"
                          & campaign != "late_summer")))
summary(aov(umol_suc_cm2 ~ campaign,
            data = subset(nsc,site == "MSA" & sample_type == "LPH"
                          & campaign != "late_summer")))
summary(aov(log(umol_suc_cm2) ~ campaign,
            data = subset(nsc, site == "MSA" & sample_type == "BPH")))
TukeyHSD(aov(log(umol_suc_cm2) ~ campaign,
             data = subset(nsc, site == "MSA" & sample_type == "BPH")))

branchSuc <- ggplot(subset(nsc, site == "MSA" & sample_type == "SPH"),
                    aes(x = campaign, y = umol_suc_cm2)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  #ylim(0, 15) +
  labs(
    x = "",
    y = expression("[Sucrose]"[branch]~"("*mu*mol~cm^-2*")"),
  ) +
  theme_minimal()

leafSuc <- ggplot(subset(nsc, site == "MSA" & sample_type == "LPH"),
                  aes(x = campaign, y = umol_suc_cm2)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  #ylim(0, 15) +
  labs(
    x = "",
    y = expression("[Sucrose]"[leaf]~"("*mu*mol~cm^-2*")"),
  ) +
  theme_minimal()

trunkSuc <- ggplot(subset(nsc, site == "MSA" & sample_type == "BPH"),
                   aes(x = campaign, y = umol_suc_cm2)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  #ylim(0, 15) +
  labs(
    x = "",
    y = expression("[Sucrose]"[stem]~"("*mu*mol~cm^-2*")"),
  ) +
  theme_minimal()

cowplot::plot_grid(leafSuc, branchSuc, trunkSuc, ncol = 3)

summary(aov(umol_suc_cm2 ~ campaign,
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

# MSA: lower branch and trunk [Sucrose] in spring than in summer,
# lower branch [Sucrose] in late summer than in summer
# but higher leaf [Sucrose] in spring than in summer.
# HMO: lower trunk [Sucrose] in late summer, no differences between spring and summer

# keep late summer campaigns for consistency,
# but not for MSA leaf phloem, keep MSA leaf phloem from summer
# I lack measurements of leaf dry mass for late summer

nscCheat <- subset(nsc, campaign == "summer23" & site == "MSA" & sample_type == "LPH")
nsc <- nsc |>
  filter(date <= 20230630 | date >= 20230827) |> 
  mutate(campaign = ifelse(date >= 20230622, "summer23", "spring23"))
nsc <- nsc[-which(is.na(nsc$surface_cm2)),]
nsc <- rbind(nsc, nscCheat)
rm(nscCheat, dimensions, dwLeaves, sla_summary)
nsc <- nsc |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) |> 
  rename(tree = id_plant)

nsc <- nsc[, c("site", "campaign", "tree", "sample_type",
               "umol_suc_ml", "umol_suc_cm2", "umol_suc_g")]
nsc <- nsc |> 
  pivot_wider(names_from = sample_type, values_from = c(umol_suc_ml, umol_suc_cm2, umol_suc_g))
nsc <- nsc[, c(1:10)] |> 
  rename(leafSuc_umol_ml = umol_suc_ml_LPH) |> 
  rename(branchSuc_umol_ml = umol_suc_ml_SPH) |> 
  rename(trunkSuc_umol_ml = umol_suc_ml_BPH) |>
  rename(leafSuc_umol_g = umol_suc_g_LPH) |> 
  rename(leafSuc_umol_cm2 = umol_suc_cm2_LPH) |> 
  rename(branchSuc_umol_cm2 = umol_suc_cm2_SPH) |> 
  rename(trunkSuc_umol_cm2 = umol_suc_cm2_BPH)

hist(nsc$leafSuc_umol_g)
subset(nsc, leafSuc_umol_g >= 0.7)
# there is an outlier that looks like a a measurement error -> discard
nsc[which(nsc$leafSuc_umol_ml >= 0.8),
    c("leafSuc_umol_ml", "leafSuc_umol_cm2", "leafSuc_umol_g")] <- NA
hist(nsc$leafSuc_umol_g)
hist(log(nsc$leafSuc_umol_g))
hist(nsc$leafSuc_umol_cm2)
hist(log(nsc$leafSuc_umol_cm2))
hist(nsc$branchSuc_umol_cm2)
hist(log(nsc$branchSuc_umol_cm2))
hist(nsc$trunkSuc_umol_cm2)

summary(aov(log(leafSuc_umol_cm2) ~ site * campaign, data = nsc))
summary(aov(log(leafSuc_umol_g) ~ site * campaign, data = nsc))
plot(aov(log(leafSuc_umol_cm2) ~ site * campaign, data = nsc))
model_means_leaf <- emmeans(lm(log(leafSuc_umol_cm2) ~ site * campaign, data = nsc),
                              ~ site * campaign)
model_means_cld <- cld(model_means_leaf, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
                       alpha = 0.05, sort = FALSE)

summary(aov(log(branchSuc_umol_cm2) ~ site * campaign, data = nsc))
plot(aov(log(branchSuc_umol_ml) ~ site * campaign, data = nsc))
model_means_branch <- emmeans(lm(log(branchSuc_umol_cm2) ~ site * campaign, data = nsc),
                              ~ site * campaign)
model_means_cld <- cld(model_means_branch, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
                       alpha = 0.05, sort = FALSE)

summary(aov(trunkSuc_umol_cm2 ~ site * campaign, data = nsc))
plot(aov(trunkSuc_umol_cm2 ~ site * campaign, data = nsc))
model_means_trunk <- emmeans(lm(trunkSuc_umol_cm2 ~ site * campaign, data = nsc),
                              ~ site * campaign)
model_means_cld <- cld(model_means_trunk, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
                       alpha = 0.05, sort = FALSE)

leafSuc <- ggplot(nsc, aes(x = site, y = log(leafSuc_umol_cm2), fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  #ylim(-0.022, 1.61) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("Log ([Sucrose]"[leaf]~"("*mu*mol~cm^-2*"))"),
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE)

branchSuc <- ggplot(nsc, aes(x = site, y = log(branchSuc_umol_cm2), fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  #ylim(-0.022, 1.61) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("Log ([Sucrose]"[branch]~"("*mu*mol~cm^-2*"))"),
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE)

trunkSuc <- ggplot(nsc, aes(x = site, y = trunkSuc_umol_cm2, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  #ylim(-0.022, 1.61) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("[Sucrose]"[trunk]~"("*mu*mol~cm^-2*")"),
    fill = "Campaign"
  ) +
  theme_minimal()+
  guides(fill = FALSE)

cowplot::plot_grid(leafSuc, branchSuc, trunkSuc, ncol = 1)
