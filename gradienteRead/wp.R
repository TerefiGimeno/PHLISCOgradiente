library(tidyverse)

wp <- read.csv("gradienteData/wp_gradiente_2023/lwp_gradiente_2023.csv") %>% 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) %>% 
  mutate(wp_md = (wp_midday_1 + wp_midday_2)/2)

plot(wp$wp_md, pch = 19, col ="blue", ylim = c(-3.5, -0.2))
points(wp$wp_midday_1, pch = 19, col = "red")
points(wp$wp_midday_2, pch = 19, col = "green")

# some differences between replicates within leaves, but within range.
# We do not discard any values.

hist(wp$wp_md)
summary(lm(wp_md ~ site * campaign, data = wp))
anova(lm(wp_md ~ site * campaign, data = wp))
TukeyHSD(aov(wp_md ~ site, data = wp))
TukeyHSD(aov(wp_md ~ site *campaign, data = wp))

ggplot(wp, aes(x = site, y = wp_md)) +
  geom_boxplot(position = position_dodge(width = 0.8))

ggplot(wp, aes(x = site, y = wp_md, fill = campaign)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression(Psi[md]~"(MPa)"),
    fill = "Campaign"
  ) +
  theme_minimal()

# differences between seasons: more negative wp_md in summer
# differences among sites: (ART = BER)ab = (DIU)b < ITUab

