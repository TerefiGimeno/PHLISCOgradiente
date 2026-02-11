library(cowplot)

source("gradienteRead/cleanIsotopeData.R")

####1. d13C of the leaf####

# look in detail at differences among canopy position in MSA
# select only campaigns for which we sampled multiple canopy positions
d13Cleaf_MSA <- subset(d13Cleaf, site == "MSA") %>% 
  mutate(campaign2 = ifelse(sampling_date >= 20230831, "xlater_summer23", campaign))
hist(d13Cleaf_MSA$d13C_leaf)
summary(lm(d13C_leaf ~ canopy_position * campaign,
           data = subset(d13Cleaf_MSA, sampling_date <= 20230827)))
anova(lm(d13C_leaf ~ canopy_position * campaign,
         data = subset(d13Cleaf_MSA, sampling_date <= 20230827)))
TukeyHSD(aov(d13C_leaf ~ canopy_position,
             data = subset(d13Cleaf_MSA, sampling_date <= 20230827)))
summary(lm(d13C_leaf ~ canopy_position2 * campaign,
           data = subset(d13Cleaf_MSA, sampling_date <= 20230827)))

# shade and shade_low are not significantly different from each other
# sun is significantly different from shade_low and from pooled shade and shade_low

# assess differences among campaigns (spring, summer, late summer)
# only in MSA and excluding sun leaves
d13Cleaf_MSA_2 <- subset(d13Cleaf_MSA, canopy_position != "sun")
summary(lm(d13C_leaf ~ campaign2, data = d13Cleaf_MSA_2))
TukeyHSD(aov(d13C_leaf ~ campaign2, data = d13Cleaf_MSA_2))
summary(lm(d13C_leaf ~ campaign2,
           data = subset(d13Cleaf_MSA_2, canopy_position == "shade_low")))
TukeyHSD(aov(d13C_leaf ~ campaign2,
           data = subset(d13Cleaf_MSA_2, canopy_position == "shade_low")))

# Significant differences between spring and late summer, but not with summer
# irrespective of whether shade and shale_low are pooled together or not

MSA_leaf <- ggplot(d13Cleaf_MSA,
                   aes(x = campaign2, y = d13C_leaf, fill = canopy_position)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Leaf" ~ delta^13 * "C (\u2030)"),
    fill = "Position"
  ) +
  theme_minimal()

# for leaf d13C the gradient study:
# (1) Drop the summer23 campaign (22-24 Aug) and keep spring and later summer
# these dates are closer to those of the other sites
# (2) select only shade low leaves for consistency


d13Cleaf_short <- subset(d13Cleaf, canopy_position == "shade_low" &
                           sampling_date <= 20230731 | sampling_date >= 20230827)
d13Cleaf_short <- d13Cleaf %>% 
  filter(canopy_position == "shade_low") %>% 
  filter(sampling_date <= 20230731 | sampling_date >= 20230827) %>%
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU")))
  
hist(d13Cleaf_short$d13C_leaf)
summary(lm(d13C_leaf ~ site * campaign, data = d13Cleaf_short))
anova(lm(d13C_leaf ~ site * campaign, data = d13Cleaf_short))
TukeyHSD(aov(d13C_leaf ~ site * campaign, data = d13Cleaf_short))
TukeyHSD(aov(d13C_leaf ~ site, data = d13Cleaf_short))
one <- ggplot(d13Cleaf_short, aes(x = site, y = d13C_leaf, fill = campaign)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Leaf" ~ delta^13 * "C (\u2030)"),
    fill = "Campaign"
  ) +
  theme_minimal()

# overall discrimination was higher in summer than in spring (more negative d13C), 
# sites differed in leaf d13C: (ART = BER = ITU) < (MSA = DIU)


####2. d13C of base phloem####

# look in detail at differences among campaigns in MSA and HMO

d13CbasePh_MSA <- d13CbasePh %>% 
  filter(site == "MSA")
hist(d13CbasePh_MSA$d13C_base_phloem)
summary(lm(d13C_base_phloem ~ campaign, data = d13CbasePh_MSA))
anova(lm(d13C_base_phloem ~ campaign, data = d13CbasePh_MSA))
TukeyHSD(aov(d13C_base_phloem ~ campaign, data = d13CbasePh_MSA))

ggplot(d13CbasePh_MSA, aes(x = campaign, y = d13C_base_phloem)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Base phloem" ~ delta^13 * "C (\u2030)")
  ) +
  theme_minimal()

# There are differences among campaigns in base phloem d13C at MSA,
# more negative d13C in both summer campaigns than in spring
# no differences between summer campaigns
# discard the first summer campaigns for consistency

d13CbasePh_HMO <- d13CbasePh %>% 
  filter(site == "HMO") %>% 
  mutate(campaign = factor(campaign,
                           levels = c("spring23", "summer23", "late_summer23")))
hist(d13CbasePh_HMO$d13C_base_phloem)
summary(lm(d13C_base_phloem ~ campaign, data = d13CbasePh_HMO))
anova(lm(d13C_base_phloem ~ campaign, data = d13CbasePh_HMO))

ggplot(d13CbasePh_HMO, aes(x = campaign, y = d13C_base_phloem)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Base phloem" ~ delta^13 * "C (\u2030)")
  ) +
  theme_minimal()

# no differences among campaigns in base phloem d13C at HMO.
# select only late summer, closer in date to the rest of the sampling

d13CbasePh <- d13CbasePh %>% 
  filter(sampling_date <= 20230701 | sampling_date >= 20230827) %>%
  mutate(campaign = ifelse(sampling_date <= 20230701, "spring23", "summer23")) %>% 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO")))
hist(d13CbasePh$d13C_base_phloem)
summary(lm(d13C_base_phloem ~ site * campaign, data = d13CbasePh))
anova(lm(d13C_base_phloem ~ site * campaign, data = d13CbasePh))
TukeyHSD(aov(d13C_base_phloem ~ site, data = d13CbasePh))
two <- ggplot(d13CbasePh, aes(x = site, y = d13C_base_phloem, fill = campaign)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Base phloem " * delta^13 * C~"(\u2030)"),
    fill = "Campaign"
  ) +
  theme_minimal()

# no differences between sampling campaigns.
# significant differences among sites: (ART = BER = ITU) < (MSA = HMO)
# DIU is not significantly different from any (although DIU-MSA p = 0.07)

####3. d13C of the tree ring####

d13CtreeRing <- d13CtreeRing %>% 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO")))

hist(d13CtreeRing$d13C_ring23)
summary(lm(d13C_ring23 ~ site, data = d13CtreeRing))
anova(lm(d13C_ring23 ~ site, data = d13CtreeRing))
TukeyHSD(aov(d13C_ring23 ~ site, data = d13CtreeRing))
three <- ggplot(d13CtreeRing, aes(x = site, y = d13C_ring23)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Ring " * delta^13 * "C (\u2030)"),
    fill = "Campaign"
  ) +
  theme_minimal()

# there are significant differences among sites in d13C:
# (ITU = ART) < (MSA = DIU = HMO), BER does not differ from any

####4.d13C of stem phloem

# look in detail at differences among canopy position in MSA
# select only campaigns for which we sampled multiple canopy positions
# we only have one value of stem phloem d13C for sun branches in spring
# drop sun branches

d13CstemPh_MSA <- d13CstemPh %>% 
  filter(site == "MSA") %>% 
  filter(canopy_position2 == "shade") %>% 
  mutate(campaign = ifelse(sampling_date >= 20230831, "late_summer", campaign)) %>% 
  mutate(campaign = factor(campaign, levels = c("spring23", "summer23", "late_summer")))

hist(d13CstemPh_MSA$d13C_stem_phloem)
summary(aov(d13C_stem_phloem ~ canopy_position * campaign,
           data = subset(d13CstemPh_MSA, sampling_date <= 20230827)))

ggplot(subset(d13CstemPh_MSA, sampling_date <= 20230827),
       aes(x = canopy_position, y = d13C_stem_phloem, fill = campaign)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Stem phloem " * delta^13 * "C (\u2030)"),
    fill = "Campaign"
  ) +
  theme_minimal()

# there are no differences in stem phloem d13C between spring and summer 23
# marginally significant (p = 0.07) differences in d13C between
# shade_low and shade

# assess differences among campaigns in MSA for shade_low
summary(lm(d13C_stem_phloem ~ campaign,
           data = subset(d13CstemPh_MSA, canopy_position == "shade_low")))
summary(aov(d13C_stem_phloem ~ campaign,
           data = subset(d13CstemPh_MSA, canopy_position == "shade_low")))
ggplot(subset(d13CstemPh_MSA, canopy_position == "shade_low"),
       aes(x = campaign, y = d13C_stem_phloem)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("Stem phloem " * delta^13 * "C (\u2030)")
  ) +
  theme_minimal()
# no significant differences among campaigns, although d13C of the stem phloem
# tends to become more negative over time

cowplot::plot_grid(one, two, three, labels = "AUTO", ncol = 3)

