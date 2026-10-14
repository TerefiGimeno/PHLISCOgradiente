library(jmuOutlier)
library(corrplot)
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

gradienteFull <- full_join(d13CbranchPh, d13CtrunkPh, by = c("site", "tree", "campaign")) |> 
  full_join(d13Cleaf, by = c("site", "tree", "campaign")) |>
  full_join(d13CleafPh, by = c("site", "tree", "campaign")) |> 
  full_join(d13CtreeRing, by = c("site", "tree")) |> 
  relocate(c(dbh_cm, h_m, d13C_ring23), .after = campaign) |> 
  relocate(sampling_date, .after = campaign) |> 
  # remove a value of tree ring d13C where we have no other records
  filter(tree != "MSA7") |>
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
  mutate(lg_leafSuc_umol_mg = log(leafSuc_umol_g*1000)) |> 
  select(-c(sampling_date, leafSuc_umol_g)) |> 
  select(-c(starts_with("d13C", ignore.case = FALSE))) |> 
  # leave out variable that gave low correlations in the first screening
  select(-c(d15N_leaf, C_perc_leaf, ratio_CN_leaf, chla_ug_ml, chlb_ug_ml, chla_chlb)) |> 
  mutate(site = factor(as.factor(site), levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO")))

gradiente <- gradienteFull |> 
  filter(site != "HMO") |> 
  mutate(site = droplevels(site))

myOrder <- c("site", "tree", "campaign", "sla", "wd_g_cm3",
             "N_perc_leaf", "wp_md", "chla_and_chlb", "lg_leafSuc_umol_mg",
             "branchSuc_umol_m2", "trunkSuc_umol_m2","D13C_leaf", "D13C_leaf_ph",
             "D13C_branch_ph", "D13C_trunk_ph","D13C_ring23")

gradiente <- gradiente[, myOrder]

####2. Calcualte and plot correlation matrices####

sites <- levels(gradiente$site)
gradienteList <- list()
for(i in 1:length(sites)){
  gradienteList[[i]] <- subset(gradiente, site == sites[i])[, 4:ncol(gradiente)]
}

corrList <- list()
for(i in 1:length(gradienteList)){
  corrList[[i]] <- cor(gradienteList[[i]], use = "pairwise.complete.obs")
}

# windows(18,8)
# par(mfrow=c(2,3))
# 
# corrPlots <- list()
# for(i in 1:length(corrList)){
#   corrPlots[[i]] <- corrplot::corrplot(corrList[[i]],
#                                        method = "square", order = "original",
#                                        type = "lower", diag = F, main = sites[i])
# }


####3. Significant p-values####

method <- "pearson"  # or "spearman"
nperm <- 20000
p <- ncol(gradienteList[[1]])
# this is slow for each site, I do it one by one

#####3.1 Artikutza#####

# Create an empty matrix for permutation P-values
datArt <- gradienteList[[1]]
p_matrixArt <- matrix(NA_real_, nrow = p, ncol = p,
  dimnames = list(names(datArt), names(datArt)))
set.seed(123)  # makes results reproducible

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    
    # Keep only rows with both measurements present
    ok <- complete.cases(datArt[[i]], datArt[[j]])
    x <- datArt[[i]][ok]
    y <- datArt[[j]][ok]
    
    # Test only if enough observations and both variables vary
    if (length(x) >= 3 &&
        length(unique(x)) > 1 &&
        length(unique(y)) > 1) {
      
      test <- jmuOutlier::perm.cor.test(x = x, y = y,
        method = method, num.sim = nperm, alternative = "two.sided")
      p_matrixArt[i, j] <- test$p.value
      p_matrixArt[j, i] <- test$p.value
    }
  }
}

diag(p_matrixArt) <- NA_real_

# Create a table of significant correlations
resultsArt <- data.frame(Variable1 = character(),  Variable2 = character(),
  r = numeric(), P_value = numeric())

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    resultsArt <- rbind(resultsArt,
      data.frame(Variable1 = names(datArt)[i], Variable2 = names(datArt)[j],
        r = corrList[[1]][i, j], P_value = p_matrixArt[i, j]))}}
write.csv(resultsArt, file = "gradienteOutput/pairwise_correlations_results/resultsArt.csv", row.names = F)

#####3.2 Bertiz#####

# Create an empty matrix for permutation P-values
datBer <- gradienteList[[2]]
p_matrixBer <- matrix(NA_real_, nrow = p, ncol = p,
                      dimnames = list(names(datBer), names(datBer)))
set.seed(123)  # makes results reproducible

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    
    # Keep only rows with both measurements present
    ok <- complete.cases(datBer[[i]], datBer[[j]])
    x <- datBer[[i]][ok]
    y <- datBer[[j]][ok]
    
    # Test only if enough observations and both variables vary
    if (length(x) >= 3 &&
        length(unique(x)) > 1 &&
        length(unique(y)) > 1) {
      
      test <- jmuOutlier::perm.cor.test(x = x, y = y,
                                        method = method, num.sim = nperm, alternative = "two.sided")
      p_matrixBer[i, j] <- test$p.value
      p_matrixBer[j, i] <- test$p.value
    }
  }
}

diag(p_matrixBer) <- NA_real_

# Create a table of significant correlations
resultsBer <- data.frame(Variable1 = character(),  Variable2 = character(),
                         r = numeric(), P_value = numeric())

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    resultsBer <- rbind(resultsBer,
                        data.frame(Variable1 = names(datBer)[i], Variable2 = names(datBer)[j],
                                   r = corrList[[2]][i, j], P_value = p_matrixBer[i, j]))}}
write.csv(resultsBer, file = "gradienteOutput/pairwise_correlations_results/resultsBer.csv", row.names = F)

#####3.3 Iturrieta#####

# Create an empty matrix for permutation P-values
datItu <- gradienteList[[3]]
p_matrixItu <- matrix(NA_real_, nrow = p, ncol = p,
                      dimnames = list(names(datItu), names(datItu)))
set.seed(123)  # makes results reproducible

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    
    # Keep only rows with both measurements present
    ok <- complete.cases(datItu[[i]], datItu[[j]])
    x <- datItu[[i]][ok]
    y <- datItu[[j]][ok]
    
    # Test only if enough observations and both variables vary
    if (length(x) >= 3 &&
        length(unique(x)) > 1 &&
        length(unique(y)) > 1) {
      
      test <- jmuOutlier::perm.cor.test(x = x, y = y,
                                        method = method, num.sim = nperm, alternative = "two.sided")
      p_matrixItu[i, j] <- test$p.value
      p_matrixItu[j, i] <- test$p.value
    }
  }
}

diag(p_matrixItu) <- NA_real_

# Create a table of significant correlations
resultsItu <- data.frame(Variable1 = character(),  Variable2 = character(),
                         r = numeric(), P_value = numeric())

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    resultsItu <- rbind(resultsItu,
                        data.frame(Variable1 = names(datItu)[i], Variable2 = names(datItu)[j],
                                   r = corrList[[3]][i, j], P_value = p_matrixItu[i, j]))}}
write.csv(resultsItu, file = "gradienteOutput/pairwise_correlations_results/resultsItu.csv", row.names = F)

#####3.4 Monte Santiago#####

# Create an empty matrix for permutation P-values
datMsa <- gradienteList[[4]]
p_matrixMsa <- matrix(NA_real_, nrow = p, ncol = p,
                      dimnames = list(names(datMsa), names(datMsa)))
set.seed(123)  # makes results reproducible

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    
    # Keep only rows with both measurements present
    ok <- complete.cases(datMsa[[i]], datMsa[[j]])
    x <- datMsa[[i]][ok]
    y <- datMsa[[j]][ok]
    
    # Test only if enough observations and both variables vary
    if (length(x) >= 3 &&
        length(unique(x)) > 1 &&
        length(unique(y)) > 1) {
      
      test <- jmuOutlier::perm.cor.test(x = x, y = y,
                                        method = method, num.sim = nperm, alternative = "two.sided")
      p_matrixMsa[i, j] <- test$p.value
      p_matrixMsa[j, i] <- test$p.value
    }
  }
}

diag(p_matrixMsa) <- NA_real_

# Create a table of significant correlations
resultsMsa <- data.frame(Variable1 = character(),  Variable2 = character(),
                         r = numeric(), P_value = numeric())

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    resultsMsa <- rbind(resultsMsa,
                        data.frame(Variable1 = names(datMsa)[i], Variable2 = names(datMsa)[j],
                                   r = corrList[[4]][i, j], P_value = p_matrixMsa[i, j]))}}
write.csv(resultsMsa, file = "gradienteOutput/pairwise_correlations_results/resultsMsa.csv", row.names = F)

#####3.5 Diustes#####

# Create an empty matrix for permutation P-values
datDiu <- gradienteList[[5]]
p_matrixDiu <- matrix(NA_real_, nrow = p, ncol = p,
                      dimnames = list(names(datDiu), names(datDiu)))
set.seed(123)  # makes results reproducible

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    
    # Keep only rows with both measurements present
    ok <- complete.cases(datDiu[[i]], datDiu[[j]])
    x <- datDiu[[i]][ok]
    y <- datDiu[[j]][ok]
    
    # Test only if enough observations and both variables vary
    if (length(x) >= 3 &&
        length(unique(x)) > 1 &&
        length(unique(y)) > 1) {
      
      test <- jmuOutlier::perm.cor.test(x = x, y = y,
                                        method = method, num.sim = nperm, alternative = "two.sided")
      p_matrixDiu[i, j] <- test$p.value
      p_matrixDiu[j, i] <- test$p.value
    }
  }
}

diag(p_matrixDiu) <- NA_real_

# Create a table of significant correlations
resultsDiu <- data.frame(Variable1 = character(),  Variable2 = character(),
                         r = numeric(), P_value = numeric())

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    resultsDiu <- rbind(resultsDiu,
                        data.frame(Variable1 = names(datDiu)[i], Variable2 = names(datDiu)[j],
                                   r = corrList[[5]][i, j], P_value = p_matrixDiu[i, j]))}}
write.csv(resultsDiu, file = "gradienteOutput/pairwise_correlations_results/resultsDiu.csv", row.names = F)


# Show significance on corrplot figure (only significant correlations are plotted)

par(mfrow=c(2,3))

corrplot(corrList[[1]], method = "color", order = "original",
                       type = "lower", diag = F, tl.col = "black", tl.srt = 45,
                       p.mat = p_matrixArt, sig.level = 0.05, insig = "blank")

corrplot(corrList[[2]], method = "color", order = "original",
         type = "lower", diag = F, tl.col = "black", tl.srt = 45,
         p.mat = p_matrixBer, sig.level = 0.05, insig = "blank")

corrplot(corrList[[3]], method = "color", order = "original",
         type = "lower", diag = F, tl.col = "black", tl.srt = 45,
         p.mat = p_matrixItu, sig.level = 0.05, insig = "blank")

corrplot(corrList[[4]], method = "color", order = "original",
         type = "lower", diag = F, tl.col = "black", tl.srt = 45,
         p.mat = p_matrixMsa, sig.level = 0.05, insig = "blank")

corrplot(corrList[[5]], method = "color", order = "original",
         type = "lower", diag = F, tl.col = "black", tl.srt = 45,
         p.mat = p_matrixDiu, sig.level = 0.05, insig = "blank")




####4. Correlations with climate####

climate <- read.csv("gradienteOutput/clean_df/summary_meteo_campaigns.csv")# |> 
  #select(-c(starts_with("P_", ignore.case = FALSE))) |> 
  #select(-c(starts_with("Tmin", ignore.case = FALSE)))


D13C_summary <- gradienteFull %>%
  group_by(campaign, site) %>%
  summarise(
    D13C_leaf_mean = mean(D13C_leaf, na.rm = TRUE),
    D13C_leaf_ph_mean = mean(D13C_leaf_ph, na.rm = TRUE),
    D13C_branch_ph_mean = mean(D13C_branch_ph, na.rm = TRUE),
    D13C_trunk_ph_mean = mean(D13C_trunk_ph, na.rm = TRUE),
    D13C_ring23_mean = mean(D13C_ring23, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  left_join(climate, by = c("site", "campaign"))


library(jmuOutlier)

# Data frame containing averages across all sites
dat <- D13C_summary[, 3:ncol(D13C_summary)]

# Select only the pairs you want to test
Deltas <- c(paste0("D13C_", c("leaf", "leaf_ph", "branch_ph", "trunk_ph", "ring23"), "_mean"))
climVars <- names(climate)[2:61]
pairs_of_interest <- expand.grid(
  Variable1 = Deltas,
  Variable2 = climVars
)

method <- "pearson"
nperm <- 20000

set.seed(123)

# Run permutation tests only for the selected pairs
results <- lapply(seq_len(nrow(pairs_of_interest)), function(i) {
  x_name <- pairs_of_interest$Variable1[i]
  y_name <- pairs_of_interest$Variable2[i]
  # Remove missing values for this pair
  ok <- complete.cases(dat[, c(x_name, y_name)])
  x <- dat[[x_name]][ok]
  y <- dat[[y_name]][ok]
    if (length(x) < 3 ||
      length(unique(x)) < 2 ||
      length(unique(y)) < 2) {
        return(data.frame(
      Variable1 = x_name,
      Variable2 = y_name,
      n = length(x),
      r = NA_real_,
      P_value = NA_real_
    ))
  }
  
  test <- jmuOutlier::perm.cor.test(
    x = x,
    y = y,
    method = method,
    num.sim = nperm,
    alternative = "two.sided"
  )
  
  data.frame(
    Variable1 = x_name,
    Variable2 = y_name,
    n = length(x),
    r = unname(cor(x, y, method = method)),
    P_value = test$p.value
  )
})

results <- do.call(rbind, results)



results


corrClimate <- cor(D13C_summary[, -c(1:2)], use = "pairwise.complete.obs")

method <- "pearson"  # or "spearman"
nperm <- 20000
datClim <- D13C_summary[, -c(1:2)]
p <- ncol(datClim)

# Create an empty matrix for permutation P-values
p_matrixClim <- matrix(NA_real_, nrow = p, ncol = p,
                      dimnames = list(names(datClim), names(datClim)))
set.seed(123)  # makes results reproducible

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    
    # Keep only rows with both measurements present
    ok <- complete.cases(datClim[[i]], datClim[[j]])
    x <- datClim[[i]][ok]
    y <- datClim[[j]][ok]
    
    # Test only if enough observations and both variables vary
    if (length(x) >= 3 &&
        length(unique(x)) > 1 &&
        length(unique(y)) > 1) {
      
      test <- jmuOutlier::perm.cor.test(x = x, y = y,
                                        method = method, num.sim = nperm, alternative = "two.sided")
      p_matrixClim[i, j] <- test$p.value
      p_matrixClim[j, i] <- test$p.value
    }
  }
}

diag(p_matrixClim) <- NA_real_

# Create a table of significant correlations
resultsClim <- data.frame(Variable1 = character(),  Variable2 = character(),
                         r = numeric(), P_value = numeric())

for (i in 1:(p - 1)) {
  for (j in (i + 1):p) {
    resultsClim <- rbind(resultsClim,
                        data.frame(Variable1 = names(datClim)[i], Variable2 = names(datClim)[j],
                                   r = corrClimate[i, j], P_value = p_matrixClim[i, j]))}}
write.csv(resultsArt, file = "gradienteOutput/pairwise_correlations_results/resultsClim.csv", row.names = F)




corrClimateShort <- corrClimate[6:nrow(corrClimate), 1:5]
corrP <- corrClimateShort[startsWith(rownames(corrClimateShort), "P"), , drop = FALSE]
rownames(corrP)[1] <- "P_1d"
ndays <- as.numeric(str_extract(rownames(corrP), "\\d+"))
corrP <- as.data.frame(corrP)
corrP$ndays <- ndays

corrPlong <- corrP |> 
  pivot_longer(cols = starts_with("D13C", ignore.case = F),
               names_to = "tissue", values_to = "D13C")

precip <- ggplot(data = corrPlong, aes(x = ndays, y = D13C, color = tissue, group = tissue))+
  geom_line()+
  geom_point(size =2)+
  labs(x = "No. Days", y = expression(Delta^13*C~"(\u2030)"),
       title = "Precipitation", color = "Tissue")+
  theme_minimal()

corrTmean <- corrClimateShort[startsWith(rownames(corrClimateShort), "Tmean"), , drop = FALSE]
rownames(corrTmean)[1] <- "TemanDay_C_1d"
ndays <- as.numeric(str_extract(rownames(corrTmean), "\\d+"))
corrTmean <- as.data.frame(corrTmean)
corrTmean$ndays <- ndays

corrTmeanlong <- corrTmean |> 
  pivot_longer(cols = starts_with("D13C", ignore.case = F),
               names_to = "tissue", values_to = "D13C")

Tmean <- ggplot(data = corrTmeanlong, aes(x = ndays, y = D13C, color = tissue, group = tissue))+
  geom_line()+
  geom_point(size =2)+
  labs(x = "No. Days", y = expression(Delta^13*C~"(\u2030)"),
       title = "Tmean", color = "Tissue")+
  theme_minimal()

combined_plot <- precip + Tmean
precip + Tmean +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")