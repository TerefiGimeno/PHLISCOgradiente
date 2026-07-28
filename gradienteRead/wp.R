library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)

wp <- read.csv("gradienteData/wp_gradiente_2023/lwp_gradiente_2023.csv") %>% 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) %>% 
  mutate(wp_md = rowMeans(across(starts_with("wp")), na.rm = T))

plot(wp$wp_md, pch = 19, col ="blue", ylim = c(-3.5, -0.2))
points(wp$wp_midday_1, pch = 19, col = "red")
points(wp$wp_midday_2, pch = 19, col = "green")

# some differences between replicates within leaves, but within range.
# We do not discard any values.

wp <- wp[, c("site", "campaign", "tree", "wp_md")]
  
wp_summ <- wp %>% 
  group_by(site, campaign) %>% 
  summarise(wp_mean = mean(wp_md, na.rm =T), wp_se = sd(wp_md, na.rm = T)/sqrt(length(which(!is.na(wp_md)))))
#write.csv(wp_summ, file = "kk.csv")

hist(wp$wp_md)
model <- lm(wp_md ~ site * campaign, data = wp)
summary(model)
anova(lm(wp_md ~ site * campaign, data = wp))
model_means <- emmeans(model, ~ site * campaign)
model_means_cld <- cld(model_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
model_means2 <- emmeans(model, ~ site)
model_means_cld2 <- cld(model_means2, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)

# more negative overall WP in summer

ggplot(wp, aes(x = site, y = wp_md, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression(Psi[md]~"(MPa)"),
    fill = "Campaign"
  ) +
  theme_minimal()

# differences between seasons: more negative wp_md in summer
# differences among sites: (ART = BER)ab = (DIU)b < ITUab
# BER shows the opposite pattern between summer and spring (rainy day)

