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
write.csv(wp, file = "gradienteOutput/clean_df/wp.csv", row.names = F)
  
wp_summ <- wp %>% 
  group_by(site, campaign) %>% 
  summarise(wp_mean = mean(wp_md, na.rm =T), wp_se = sd(wp_md, na.rm = T)/sqrt(length(which(!is.na(wp_md)))))

hist(wp$wp_md)
model <- lm(wp_md ~ site * campaign, data = wp)
summary(model)
anova(lm(wp_md ~ site * campaign, data = wp))
model_means <- emmeans(model, ~ site * campaign)
model_means_cld <- cld(model_means, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
model_means2 <- emmeans(model, ~ site*campaign)
model_means_cld2 <- cld(model_means2, adjust = "sidak",
                       Letters = c("a", "b", "c", "d", "e", "f", "g"),
                       alpha = 0.05, sort = FALSE)
# more negative overall WP in summer

dodge <- position_dodge2(width = 0.8, preserve = "single", padding = 0)
wpFig <- ggplot(wpSumm, aes(x = site, y = wp_mean, fill = campaign))+
  geom_bar(position=dodge, stat= "identity", color = "black")+
  geom_errorbar(aes(ymax = wp_mean + wp_se, ymin = wp_mean - wp_se), width = 0.2,
                position = dodge)+
  scale_fill_manual(name = "", values = c("#EF476F", "#FFD166"))+
  ylab(expression(Psi[md]~"(MPa)"))+
  xlab("")+
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = .5),
    axis.line = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = c(0.2, 0.1),
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.grid = element_blank()
  )
  


ggplot(rwcSumm, aes(x = Site, y = VWC, fill = soilDepth)) +
  geom_bar(position="dodge", stat="identity", colour = 'black') +
  geom_errorbar(aes(ymax = VWC + VWC_se, ymin = VWC - VWC_se), width = 0.2,
                position=position_dodge(0.9)) +
  ggsignif::geom_signif(y_position=c(72, 67), xmin=c(0.6, 1.6), 
                        xmax=c(3.4, 3.4), annotation=c("***", "*"),
                        tip_length=0.06, vjust = 0.4, textsize = 14, color="grey40") +
  scale_fill_manual(name= "Soil depth", values=c("#56B4E9", "#CC79A7", "#D55E00"),
                    labels = c('0-5 cm', '20-25 cm', '40-45 cm')) +
  scale_y_continuous(limits=c(0, 75)) +
  facet_grid(~ Campaign) +
  ylab("VWC (%)") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 34),
        panel.border = element_rect(size=0.5, fill=NA),
        axis.ticks = element_line(color="black"),
        axis.title.x = element_blank(),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 28))

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

