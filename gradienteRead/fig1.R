library(ggplot2)
library(tidyverse)

D13CtreeRing <- read.csv("gradienteData/isotopes_gradiente_2023/Tabla_S2025-3401_mod.csv") %>% 
  filter(year == 2023) %>% 
  filter(tree != "MSA7") |>
  mutate(D13C = (-8.64 - d13C_permil)/(1+d13C_permil*0.001)) |>
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) |> 
  select(-c(year, perc_C, d13C_permil, tree))
D13CtreeRing$campaign <- rep("Whole year", times = nrow(D13CtreeRing))
D13CtreeRing$tissue <- rep("Ring", times = nrow(D13CtreeRing))
D13CtreeRing <- D13CtreeRing[, c("site", "campaign", "tissue", "D13C")]
  
d13CtrunkPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_base_phloem.csv") %>% 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_base_phloem)) |> 
  rename(d13C_trunk_ph = d13C_base_phloem)

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  filter(canopy_position == "shade_low") %>% 
  filter(sampling_date <= 20230731 | sampling_date >= 20230827) %>% 
  select(-c(weight_mg, canopy_position, canopy_position2, sampling_date,
            N_perc_leaf, d15N_leaf))

d13CbranchPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_stem_phloem.csv") %>%
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  filter(canopy_position == "shade_low") %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_stem_phloem, canopy_position, canopy_position2, sampling_date)) |> 
  rename(d13C_branch_ph = d13C_stem_phloem)

d13CleafPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf_phloem.csv") |> 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(sampling_date))

D13C_gradiente <- full_join(d13CbranchPh, d13CtrunkPh, by = c("site", "tree", "campaign")) |> 
  full_join(d13Cleaf, by = c("site", "tree", "campaign")) |>
  full_join(d13CleafPh, by = c("site", "tree", "campaign")) |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) |> 
  mutate(d13Camb_month = ifelse(campaign == "spring23", -8.75, -8.64)) |> 
  mutate(D13C_leaf = (d13Camb_month - d13C_leaf)/(1+d13C_leaf*0.001)) |> 
  mutate(D13C_leaf_ph = (d13Camb_month - d13C_leaf_ph)/(1+d13C_leaf_ph*0.001)) |> 
  mutate(D13C_branch_ph = (d13Camb_month - d13C_branch_ph)/(1+d13C_branch_ph*0.001)) |>
  mutate(D13C_trunk_ph = (d13Camb_month - d13C_trunk_ph)/(1+d13C_trunk_ph*0.001)) |>
  select(c(site, campaign, c(starts_with("D13C_", ignore.case = F)))) |> 
  pivot_longer(cols = starts_with("D13C_"), names_to = "tissue", values_to = "D13C") |> 
  mutate(tissue = as.factor(tissue)) |> 
  mutate(campaign = as.factor(campaign))

D13C_gradiente$campaign <- recode_factor(D13C_gradiente$campaign,
                                         spring23 = "Spring", summer23 = "Summer")
D13C_gradiente$tissue <- recode_factor(D13C_gradiente$tissue,
                                       D13C_branch_ph = "Branch PH", D13C_leaf = "Bulk leaf",
                                         D13C_leaf_ph = "Leaf PH", D13C_trunk_ph = "Trunk PH")
D13C_gradiente$tissue <- factor(D13C_gradiente$tissue, levels = c("Bulk leaf", "Leaf PH", "Branch PH", "Trunk PH"))

D13C_gradiente <- rbind(D13C_gradiente, D13CtreeRing)

site_labels <- D13C_gradiente %>% distinct(site)

fig1 <- ggplot(D13C_gradiente, aes(x = tissue, y = D13C, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("#EF476F", "#FFD166", "deepskyblue")) +
  ylim(16.5, 26.2)+
  labs(
    x = "",
    y = expression(Delta^13*C~"(\u2030)"),
    fill = "Campaign"
  ) +
  facet_wrap(~site) +
  geom_text(data = site_labels, aes(x = Inf, y = Inf, label = site),
    hjust = 1.1, vjust = 1.5, size = 7,
    inherit.aes = FALSE) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = .5),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 14),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = c(0.835, 0.325),
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid = element_blank()
  )
