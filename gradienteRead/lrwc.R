library(tidyverse)

lrwc <- read.csv("gradienteData/lrwc_gradiente_2023/lrwc_gradiente_2023.csv") %>% 
  rename(campaign = season) |> 
  rename(sampling_date = date) |>
  mutate(site = recode_values(site, "art" ~ "ART", "ber" ~ "BER", "diu" ~ "DIU",
                              "itu" ~ "ITU", "ms" ~ "MSA")) |> 
  mutate(tree = paste0(site, id_plant)) |> 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  filter(light_exposure == "shade_low") |> 
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) %>%
  mutate(lrwc = (fw -dw)/(sw - dw)) |> 
  select(-c(light_exposure, id_plant, fw, sw, dw))

hist(lrwc$lrwc)
# there is a leaf that contains more than 100% water -> discard
lrwc[which(lrwc$lrwc > 1), "lrwc"] <- NA

lrwc_summ <- lrwc %>% 
  group_by(site, campaign) %>% 
  summarise(lrwc_mean = mean(lrwc, na.rm =T), wp_se = sd(lrwc, na.rm = T)/sqrt(length(which(!is.na(lrwc)))))

hist(lrwc$lrwc)
summary(lm(lrwc ~ site * campaign, data = lrwc))
anova(lm(lrwc ~ site * campaign, data = lrwc))
# more negative overall LRWC in SPRING!!!
TukeyHSD(aov(lrwc ~ site, data = lrwc))
# BER has lower LRWC than ART, ITU and DIU
TukeyHSD(aov(lrwc ~ site *campaign, data = lrwc))
# lrwc is HIGHER in summer in ART, in the othe sites no within site sig. diff.

ggplot(lrwc*100, aes(x = site, y = lrwc)) +
  geom_boxplot(position = position_dodge(width = 0.8))

ggplot(lrwc, aes(x = site, y = lrwc, fill = campaign)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = "LRWC (%)",
    fill = "Campaign"
  ) +
  theme_minimal()

# differences between seasons: more negative wp_md in summer
# differences among sites: (ART = BER)ab = (DIU)b < ITUab
# BER shows the opposite pattern between summer and spring (rainy day)