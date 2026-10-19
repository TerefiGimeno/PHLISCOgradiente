library(ggplot2)
library(tidyverse)
library(patchwork)

####1. build the D13C database####

d13CtrunkPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_base_phloem.csv") %>% 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_base_phloem)) |> 
  filter(site != "HMO") |> 
  rename(d13C_trunk_ph = d13C_base_phloem) |> 
  mutate(site = droplevels(as.factor(site))) |> 
  select(-c(sampling_date))

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  filter(canopy_position == "shade_low") %>% 
  filter(sampling_date <= 20230731 | sampling_date >= 20230827) %>% 
  select(-c(weight_mg, canopy_position, canopy_position2, sampling_date,
            N_perc_leaf, d15N_leaf, C_perc_leaf))

d13CbranchPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_stem_phloem.csv") %>%
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  filter(canopy_position == "shade_low") %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_stem_phloem, canopy_position, canopy_position2, sampling_date)) |> 
  rename(d13C_branch_ph = d13C_stem_phloem)

d13CleafPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf_phloem.csv") |> 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(sampling_date)) |> 
  filter(site != "HMO") |> 
  mutate(site = droplevels(as.factor(site)))

gradiente <- full_join(d13CbranchPh, d13CtrunkPh, by = c("site", "tree", "campaign")) |> 
  full_join(d13CleafPh, by = c("site", "tree", "campaign")) |> 
  full_join(d13Cleaf, by = c("site", "tree", "campaign")) |>
  mutate(d13Camb_month = ifelse(campaign == "spring23", -8.75, -8.64)) |> 
  mutate(D13C_leaf = (d13Camb_month - d13C_leaf)/(1+d13C_leaf*0.001)) |> 
  mutate(D13C_leaf_ph = (d13Camb_month - d13C_leaf_ph)/(1+d13C_leaf_ph*0.001)) |> 
  mutate(D13C_branch_ph = (d13Camb_month - d13C_branch_ph)/(1+d13C_branch_ph*0.001)) |>
  mutate(D13C_trunk_ph = (d13Camb_month - d13C_trunk_ph)/(1+d13C_trunk_ph*0.001)) |>
  select(-c(starts_with("d13C", ignore.case = FALSE))) |> 
  mutate(site = factor(as.factor(site), levels = c("ART", "BER", "ITU", "MSA", "DIU")))

gradient_summ <- gradiente %>% 
  group_by(site, campaign) %>% 
  summarise(D13leaf = mean(D13C_leaf, na.rm = T),
            D13leaf_se = sd(D13C_leaf, na.rm = T)/sqrt(length(D13C_leaf)),
            D13leafPh = mean(D13C_leaf_ph, na.rm = T),
            D13leafPh_se = sd(D13C_leaf_ph, na.rm = T)/sqrt(length(D13C_leaf_ph)),
            D13branchPh = mean(D13C_branch_ph, na.rm = T),
            D13branchPh_se = sd(D13C_branch_ph, na.rm = T)/sqrt(length(D13C_branch_ph)),
            D13trunkPh = mean(D13C_trunk_ph, na.rm = T),
            D13trunkPh_se = sd(D13C_trunk_ph, na.rm = T)/sqrt(length(D13C_trunk_ph)))

####2. make correlation plots####

#####2.1. D13C of leaf ph vs leaf bulk#####

fig2a <- ggplot(gradiente, aes(y = D13C_leaf_ph, x = D13C_leaf)) +
  geom_smooth(aes(group = campaign, color = campaign),
              method = "lm", se = FALSE) +
  scale_color_manual(values=c("spring23" = "#EF476F", "summer23" = "#FFD166")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(fill = campaign, shape = site), size = 2, alpha = 0.5) +
  geom_errorbar(data = gradient_summ, aes(y = D13leafPh, x = D13leaf,
                                         ymin = D13leafPh - D13leafPh_se,
                                         ymax = D13leafPh + D13leafPh_se),
  width = 0, linewidth = 0.7, color = "black") +
  geom_errorbar(data = gradient_summ, aes(y = D13leafPh, x = D13leaf,
                                         xmin = D13leaf - D13leaf_se,
                                         xmax = D13leaf + D13leaf_se),
  width = 0, linewidth = 0.7, color = "black") +
  geom_point(data = gradient_summ, aes(y = D13leafPh, x = D13leaf,
                                      shape = site, fill = campaign),
             size = 4, color = "black", stroke = 0.75) +
  scale_shape_manual(values = c("ART" = 21, "BER" = 22, "ITU" = 23,
                                "MSA" = 24, "DIU" = 25)) +
  scale_fill_manual(values=c("spring23" = "#EF476F", "summer23" = "#FFD166")) +
  ylim(16, 26) +
  xlim(16, 26) +
  labs(title = "(a)", y = expression(Delta^13*C[leaf~PH]~"(\u2030)"), x = "") +
  guides(fill = "none", #color = guide_legend(title = NULL, order = 2, override.aes = list(linewidth = 1.2)),
         color = "none", shape = guide_legend(title = NULL, override.aes =
                                list(fill = NA, color = "black", size = 3))) +
  theme(panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.x  = element_text(size = 12.5),
    axis.text.y  = element_text(size = 12.5),
    title = element_text(size = 15, face = "bold"),
    legend.position = c(0.1, 0.65),
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    legend.spacing.y = unit(2, "pt")
  )

#####2.2. D13C of branch ph vs leaf bulk#####

fig2b <- ggplot(gradiente, aes(y = D13C_branch_ph, x = D13C_leaf)) +
  geom_smooth(aes(group = campaign, color = campaign),
              method = "lm", se = FALSE) +
  scale_color_manual(values=c("spring23" = "#EF476F", "summer23" = "#FFD166")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(fill = campaign, shape = site), size = 2, alpha = 0.5) +
  geom_errorbar(data = gradient_summ, aes(y = D13branchPh, x = D13leaf,
                                          ymin = D13branchPh - D13branchPh_se,
                                          ymax = D13branchPh + D13branchPh_se),
                width = 0, linewidth = 0.7, color = "black") +
  geom_errorbar(data = gradient_summ, aes(y = D13branchPh, x = D13leaf,
                                          xmin = D13leaf - D13leaf_se,
                                          xmax = D13leaf + D13leaf_se),
                width = 0, linewidth = 0.7, color = "black") +
  geom_point(data = gradient_summ, aes(y = D13branchPh, x = D13leaf,
                                       shape = site, fill = campaign),
             size = 4, color = "black", stroke = 0.75) +
  scale_shape_manual(values = c("ART" = 21, "BER" = 22, "ITU" = 23,
                                "MSA" = 24, "DIU" = 25)) +
  scale_fill_manual(values=c("spring23" = "#EF476F", "summer23" = "#FFD166")) +
  ylim(16, 26) +
  xlim(16, 26) +
  labs(title = "(b)",y = expression(Delta^13*C[branch~PH]~"(\u2030)"), x = "") +
  theme(panel.background = element_blank(),
        plot.background  = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.line = element_blank(),
        title = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x  = element_text(size = 12.5),
        axis.text.y  = element_text(size = 12.5),
        legend.position = "none"
        
  )

#####2.3. D13C of trunk ph vs leaf bulk#####

fig2c <- ggplot(gradiente, aes(y = D13C_trunk_ph, x = D13C_leaf)) +
  geom_smooth(aes(group = campaign, color = campaign),
              method = "lm", se = FALSE) +
  scale_color_manual(values=c("spring23" = "#EF476F", "summer23" = "#FFD166")) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(fill = campaign, shape = site), size = 2, alpha = 0.5) +
  geom_errorbar(data = gradient_summ, aes(y = D13trunkPh, x = D13leaf,
                                          ymin = D13trunkPh - D13trunkPh_se,
                                          ymax = D13trunkPh + D13trunkPh_se),
                width = 0, linewidth = 0.7, color = "black") +
  geom_errorbar(data = gradient_summ, aes(y = D13trunkPh, x = D13leaf,
                                          xmin = D13leaf - D13leaf_se,
                                          xmax = D13leaf + D13leaf_se),
                width = 0, linewidth = 0.7, color = "black") +
  geom_point(data = gradient_summ, aes(y = D13trunkPh, x = D13leaf,
                                       shape = site, fill = campaign),
             size = 4, color = "black", stroke = 0.75) +
  scale_shape_manual(values = c("ART" = 21, "BER" = 22, "ITU" = 23,
                                "MSA" = 24, "DIU" = 25)) +
  scale_fill_manual(values=c("spring23" = "#EF476F", "summer23" = "#FFD166")) +
  ylim(16, 26) +
  xlim(16, 26) +
  labs(title = "(c)",y = expression(Delta^13*C[trunk~PH]~"(\u2030)"),
       x = expression(Delta^13*C[Bulk~leaf]~"(\u2030)")) +
  theme(panel.background = element_blank(),
        plot.background  = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.line = element_blank(),
        title = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x  = element_text(size = 12.5),
        axis.text.y  = element_text(size = 12.5),
        legend.position = "none"
        
  )


####3. build the sucrose database####

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
  mutate(umol_suc_m2 = umol_suc_ml * volume_mL/(surface_cm2/10000)) |> 
  mutate(umol_suc_g = umol_suc_ml * volume_mL/dw_lph_g) |> 
  filter(canopy_position != "sun") |>
  filter(canopy_position != "shade") |>
  mutate(campaign = factor(campaign, levels = c("spring23", "summer23", "late_summer23"))) |> 
  filter(site != "HMO") |> 
  mutate(site = droplevels(as.factor(site)))

# MSA: lower branch and trunk [Sucrose] in spring than in summer,
# lower branch [Sucrose] in late summer than in summer
# but higher leaf [Sucrose] in spring than in summer.

# keep late summer campaigns for consistency,
# but not for MSA leaf phloem, keep MSA leaf phloem from summer
# I lack measurements of leaf dry mass for late summer

nscCheat <- subset(nsc, campaign == "summer23" & site == "MSA" & sample_type == "LPH")
nsc <- nsc |>
  filter(date <= 20230630 | date >= 20230827) |> 
  mutate(campaign = ifelse(date >= 20230630, "summer23", "spring23"))
nsc <- nsc[-which(is.na(nsc$surface_cm2)),]
nsc <- rbind(nsc, nscCheat)
rm(nscCheat, dimensions, dwLeaves, sla_summary)
nsc <- nsc |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) |> 
  rename(tree = id_plant)

nsc <- nsc[, c("site", "campaign", "tree", "sample_type",
               "umol_suc_ml", "umol_suc_m2", "umol_suc_g")]
nsc <- nsc |> 
  pivot_wider(names_from = sample_type, values_from = c(umol_suc_ml, umol_suc_m2, umol_suc_g))
nsc <- nsc[, c(1:10)] |> 
  rename(leafSuc_umol_ml = umol_suc_ml_LPH) |> 
  rename(branchSuc_umol_ml = umol_suc_ml_SPH) |> 
  rename(trunkSuc_umol_ml = umol_suc_ml_BPH) |>
  rename(leafSuc_umol_g = umol_suc_g_LPH) |> 
  rename(leafSuc_umol_m2 = umol_suc_m2_LPH) |> 
  rename(branchSuc_umol_m2 = umol_suc_m2_SPH) |> 
  rename(trunkSuc_umol_m2 = umol_suc_m2_BPH)

####4. sucrose graphs####

leafSuc <- ggplot(nsc, aes(x = site, y = log(leafSuc_umol_g*1000), fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("#EF476F", "#FFD166"), name = NULL) +
  labs(x = "", title = "(d)",
       y = expression("Lg(Leaf PH [Suc] ("*mu*mol~mg^-1*"))")) +
  theme(panel.background = element_blank(),
        plot.background  = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.line = element_blank(),
        title = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x  = element_text(size = 12.5),
        axis.text.y  = element_text(size = 12.5),
        legend.position = c(0.13, 0.85),
        legend.background = element_rect(fill = NA, color = NA),
        legend.key = element_blank(),
        legend.text = element_text(size = 11),
        legend.spacing.y = unit(2, "pt")
        
  )

branchSuc <- ggplot(nsc, aes(x = site, y = branchSuc_umol_m2*0.001, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("#EF476F", "#FFD166"), name = NULL) +
  labs(x = "", title = "(e)",
       y = expression("Branch PH [Suc] ("*mmol~m^-2*")")) +
  theme(panel.background = element_blank(),
        plot.background  = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.line = element_blank(),
        title = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x  = element_text(size = 12.5),
        axis.text.y  = element_text(size = 12.5),
        legend.position = "none"
        
  )
trunkSuc <- ggplot(nsc, aes(x = site, y = trunkSuc_umol_m2*0.001, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("#EF476F", "#FFD166"), name = NULL) +
  labs(x = "Site", title = "(f)",
       y = expression("Trunk PH [Suc] ("*mmol~m^-2*")")) +
  theme(panel.background = element_blank(),
        plot.background  = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.line = element_blank(),
        title = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x  = element_text(size = 12.5),
        axis.text.y  = element_text(size = 12.5),
        legend.position = "none"
        
  )

####4. plot everything####

combined_plot <- (fig2a | leafSuc) /
  (fig2b | branchSuc) /
  (fig2c | trunkSuc)

ggsave(filename = "gradienteOutput/fig2.tiff", plot = combined_plot,
  device = "tiff",
  width = 24,
  height = 18,
  units = "cm",
  dpi = 300,
  compression = "lzw",
  bg = "none"
)