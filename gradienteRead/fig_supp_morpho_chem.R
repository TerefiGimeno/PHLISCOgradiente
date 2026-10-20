library(tidyverse)

sla <- read.csv("gradienteData/sla_gradiente_2023/sla_gradiente_2023_updated.csv") |> 
  mutate(SLA = ifelse(SLA == 9999, NA, SLA)) |> 
  filter(canopy_position == "shade_low") |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) |> 
  rename(tree = id_plant) |> 
  rename(sla = SLA)

sla <- sla[, c("site", "campaign", "tree", "sla")]

slaPlot <- ggplot(sla, aes(x = site, y = sla, fill = campaign)) +
  geom_boxplot(
    position = position_dodge2(width = 0.8, preserve = "single")) +
  scale_fill_manual(values=c("#EF476F", "#FFD166")) +
  labs(x = "", y = expression("SLA (g "*cm^-2*")"), fill = "") +
  theme(strip.background = element_blank(),
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
        legend.position = c(0.2, 0.07),
        legend.justification = c("right", "bottom"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.grid = element_blank())

wd <- read.csv("gradienteData/wd_gradiente_2023/wd_gradiente_2023_updated.csv") |> 
  filter(canopy_position == "shade_low") |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU", "HMO"))) |> 
  rename(tree = id_plant)

wd <- subset(wd, site != "HMO")

wdPlot <- ggplot(wd, aes(x = site, y = wd_g_cm3, fill = campaign)) +
  geom_boxplot(position = position_dodge2(width = 0.8)) +
  scale_fill_manual(values=c("#EF476F", "#FFD166")) +
  guides(fill = "none")+
  labs(x = "", y = expression(rho[wood]~"(g "*cm^-3*")"), fill = "") +
  theme(strip.background = element_blank(),
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
        panel.grid = element_blank())

chl <- read.csv("gradienteData/chl_gradiente_2023/pigm_gradiente_2023_updated.csv") |> 
  filter(canopy_position == "shade_low") |>
  filter(date <= 20230731 | date >= 20230825) |> 
  mutate(campaign = ifelse(date >= 20230621, "summer23", "spring23")) |> 
  mutate(site = factor(site, levels = c("ART", "BER", "ITU", "MSA", "DIU"))) |> 
  rename(tree = id_plant) |> 
  mutate(chla_and_chlb = chla_ug_ml + chlb_ug_ml)

chl <- chl[, c("site", "campaign", "tree", "chla_ug_ml", "chlb_ug_ml",
               "chla_chlb", "chla_and_chlb")]

chlPlot <- ggplot(chl, aes(x = site, y = chla_and_chlb, fill = campaign)) +
  geom_boxplot(position = position_dodge2(width = 0.8)) +
  scale_fill_manual(values=c("#EF476F", "#FFD166")) +
  guides(fill = F)+
  labs(x = "", y = expression("[Chla] + [Chlb] ("*mu*g~ml^-1*")"), fill = "") +
  theme(strip.background = element_blank(),
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
        panel.grid = element_blank())

d13Cleaf <- read.csv("gradienteData/isotopes_gradiente_2023/isotopes_leaf.csv") %>% 
  filter(canopy_position == "shade_low") %>% 
  filter(sampling_date <= 20230731 | sampling_date >= 20230827) %>% 
  select(-c(weight_mg, canopy_position, canopy_position2, sampling_date))

NleafPlot <- ggplot(d13Cleaf, aes(x = site, y = N_perc_leaf, fill = campaign)) +
  geom_boxplot(position = position_dodge2(width = 0.8)) +
  scale_fill_manual(values=c("#EF476F", "#FFD166")) +
  guides(fill = F)+
  labs(x = "", y = expression("[N]"[leaf]~"(%)"), fill = "") +
  theme(strip.background = element_blank(),
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
        panel.grid = element_blank())

cowplot::plot_grid(slaPlot, wdPlot, chlPlot, NleafPlot, ncol =2)
