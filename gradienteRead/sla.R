library(tidyverse)

sla <- read.csv("gradienteData/leaf_area/sla_gradiente_2023.csv") %>% 
  rename(campaign = season) |> 
  mutate(site = recode_values(site, "art" ~ "ART", "ber" ~ "BER", "diu" ~ "DIU",
                              "itu" ~ "ITU")) |> 
  mutate(tree = paste0(site, id_plant)) |> 
  mutate(campaign = ifelse(campaign == "spring", "spring23", "summer23")) |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) %>%
  select(-c(light_exposure, X, id_comb, fw_l_lma, dw_l_lma, tla_suma, id_plant))

hist(lrwc$lrwc)
# there is a leaf that contains more than 100% water -> discard
lrwc[which(lrwc$lrwc > 1), "lrwc"] <- NA

lrwc_summ <- lrwc %>% 
  group_by(site, campaign) %>% 
  summarise(lrwc_mean = mean(lrwc, na.rm =T), wp_se = sd(lrwc, na.rm = T)/sqrt(length(which(!is.na(lrwc)))))

hist(lrwc$lrwc)
summary(lm(lrwc ~ site * campaign, data = subset(lrwc, site != "ART")))
anova(lm(lrwc ~ site * campaign, data = lrwc))
# more negative overall LRWC in SPRING!!!
TukeyHSD(aov(lrwc ~ site, data = lrwc))
# BER has lower LRWC than ART, ITU and DIU
TukeyHSD(aov(lrwc ~ site *campaign, data = lrwc))
# lrwc is HIGHER in summer in ART, in the othe sites no within site sig. diff.

ggplot(lrwc*100, aes(x = site, y = lrwc)) +
  geom_boxplot(position = position_dodge(width = 0.8))


ggplot(lrwc, aes(x = site, y = lrwc*100, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("magenta1", "orange")) +
  labs(
    x = "",
    y = "LRWC (%)",
    fill = "Campaign"
  ) +
  theme_minimal()

# differences between seasons: more negative wp_md in summer
# differences among sites: (ART = BER)ab = (DIU)b < ITUab
# BER shows the opposite pattern between summer and spring (rainy day)