library(ggplot2)
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

d13CtrunkPh <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_base_phloem.csv") %>% 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  select(-c(d15N_base_phloem)) |> 
  rename(d13C_trunk_ph = d13C_base_phloem)

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  mutate(ratio_CN_leaf = C_perc_leaf/N_perc_leaf) %>% 
  filter(canopy_position == "shade_low") %>% 
  filter(sampling_date <= 20230731 | sampling_date >= 20230827) %>% 
  select(-c(weight_mg, canopy_position, canopy_position2, sampling_date))

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

gradiente <- full_join(d13CbranchPh, d13CtrunkPh, by = c("site", "tree", "campaign")) |> 
  full_join(d13Cleaf, by = c("site", "tree", "campaign")) |>
  full_join(d13CleafPh, by = c("site", "tree", "campaign")) |> 
  full_join(d13CtreeRing, by = c("site", "tree")) |> 
  relocate(c(dbh_cm, h_m, d13C_ring23), .after = campaign) |> 
  relocate(sampling_date, .after = campaign) |> 
  # remove a value of tree ring d13C where we have no other records
  filter(tree != "MSA7") |>
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) |> 
  mutate(d13Camb_month = ifelse(campaign == "spring23", -8.75, -8.64)) |> 
  mutate(d13Camb_year = -8.64) |> 
  mutate(D13C_leaf = (d13Camb_month - d13C_leaf)/(1+d13C_leaf*0.001)) |> 
  mutate(D13C_leaf_ph = (d13Camb_month - d13C_leaf_ph)/(1+d13C_leaf_ph*0.001)) |> 
  mutate(D13C_branch_ph = (d13Camb_month - d13C_branch_ph)/(1+d13C_branch_ph*0.001)) |>
  mutate(D13C_trunk_ph = (d13Camb_month - d13C_trunk_ph)/(1+d13C_trunk_ph*0.001)) |>
  mutate(D13C_ring23 = (d13Camb_year - d13C_ring23)/(1+d13C_ring23*0.001)) |> 
  full_join(read.csv("gradienteOutput/clean_df/wp.csv"), by = c("site", "campaign", "tree")) |> 
  full_join(read.csv("gradienteOutput/clean_df/sla.csv"), by = c("site", "campaign", "tree")) |> 
  full_join(read.csv("gradienteOutput/clean_df/wd.csv"), by = c("site", "campaign", "tree")) |> 
  full_join(read.csv("gradienteOutput/clean_df/chl.csv"), by = c("site", "campaign", "tree")) |> 
  full_join(read.csv("gradienteOutput/clean_df/sucrose.csv"), by = c("site", "campaign", "tree")) |> 
  select(-c(site, tree, campaign, sampling_date)) |> 
  select(-c(starts_with("d13C", ignore.case = FALSE))) |> 
  # leave out variable that gave low correlations in the first screening
  select(-c(d15N_leaf, C_perc_leaf, ratio_CN_leaf, chla_ug_ml, chlb_ug_ml, chla_chlb))

corr <- cor(gradiente, use = "pairwise.complete.obs")
corrplot::corrplot(corr, method = "square", order = "FPC", type = "lower", diag = F)

gradiente_deltas <- gradiente |> 
  select(c(starts_with("D13C", ignore.case = FALSE)))

corr <- cor(gradiente_deltas, use = "pairwise.complete.obs")
corrplot::corrplot(corr, )


D13C_summary <- d13C_gradiente %>%
  group_by(campaign, site) %>%
  summarise(
    D13C_leaf_mean = mean(D13C_leaf, na.rm = TRUE),
    D13C_leaf_ph_mean = mean(D13C_leaf_ph, na.rm = TRUE),
    D13C_branch_ph_mean = mean(D13C_branch_ph, na.rm = TRUE),
    D13C_trunk_ph_mean = mean(D13C_trunk_ph, na.rm = TRUE),
    D13C_ring23_mean = mean(D13C_ring23, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  left_join(read.csv("gradienteData/summary_meteo_campaigns.csv"), by = c("site", "campaign"))

corr3 <- cor(D13C_summary[, -c(1:2)], use = "pairwise.complete.obs")
