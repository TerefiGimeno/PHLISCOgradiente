library(emmeans)
library(multcomp)
library(multcompView)
library(tidyverse)

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

wd <- read.csv("gradienteData/wd_gradiente_2023/wd_gradiente_2023_updated.csv") |> 
  filter(canopy_position == "shade_low") |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) |> 
  rename(tree = id_plant)
hist(log(wd$wd_g_cm3*1000))
model_wd <- lm(wd_g_cm3 ~ site * campaign, data = wd)
summary(model_wd)
anova(model_wd)
model_means2 <- emmeans(model_wd, ~ site * campaign)
model_means_cld2 <- cld(model_means2, adjust = "sidak",
                        Letters = c("a", "b", "c", "d", "e", "f", "g"),
                        alpha = 0.05, sort = FALSE)
ggplot(wd, aes(x = site, y = wd_g_cm3, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = expression(rho[wood]~"(g "*cm^-3*")"),
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
    axis.title.x = element_blank(),
    axis.text.x  = element_text(size = 12.5),
    axis.text.y  = element_text(size = 12.5),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.9),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_blank(),
    legend.text = element_text(size = 11),
    legend.spacing.y = unit(2, "pt")
  )

