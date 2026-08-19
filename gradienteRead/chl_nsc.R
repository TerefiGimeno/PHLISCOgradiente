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
