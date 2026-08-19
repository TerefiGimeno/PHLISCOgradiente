library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)

sla <- read.csv("gradienteData/sla_gradiente_2023/sla_gradiente_2023_updated.csv") |> 
  mutate(SLA = ifelse(SLA == 9999, NA, SLA)) |> 
  filter(canopy_position == "shade_low") |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) |> 
  rename(tree = id_plant) |> 
  rename(sla = SLA)

sla <- sla[, c("site", "campaign", "tree", "sla")]

hist(sla$sla)
model <- lm(sla ~ site * campaign, data = sla)
summary(model)
anova(lm(sla ~ site * campaign, data = sla))
model_means2 <- emmeans(model, ~ site)
model_means_cld2 <- cld(model_means2, adjust = "sidak",
                        Letters = c("a", "b", "c", "d", "e", "f", "g"),
                        alpha = 0.05, sort = FALSE)


ggplot(sla, aes(x = site, y = sla, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression("SLA (g "*cm^-2*")"),
    fill = "Campaign"
  ) +
  theme_minimal()
