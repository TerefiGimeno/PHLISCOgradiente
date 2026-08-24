# Install packages if needed
# install.packages(c("sf", "ggplot2", "viridis", "rnaturalearth", "rnaturalearthdata", "dplyr"))

library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)


fagus <- st_read("gradienteData/Fagus_sylvatica_shp/Fagus_sylvatica_plg_clip.shp")

st_crs(fagus)

europe <- ne_countries(
  continent = "Europe",
  scale = "medium",
  returnclass = "sf"
)

df <- read.csv("gradienteData/sites_Pannual.csv")

points_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)

xmin <- -10   
xmax <- 5    
ymin <- 35    
ymax <- 45    

fagus_crop <- st_crop(fagus, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

europe_crop <- st_crop(europe, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

ggplot() +
  
  # Europe
  geom_sf(data = europe_crop, fill = "white", color = "black", linewidth = 0.3) +
  
  # Fagus sylvatica distribution
  geom_sf(data = fagus_crop, fill = "darkgrey", color = NA, alpha = 0.65) +
  
  # Your sampling locations
  geom_sf(data = points_sf, aes(fill = P_annual), size = 4, shape = 21) +
  
  # Continuous colour scale
  scale_fill_viridis_c(name = "MAP (mm)", direction = -1) +
  
  # Geographic extent
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  
  labs(title =  " ", x = " ", y = " ") +
  
  theme_minimal() +
  
  theme(
    panel.grid.major = element_line(
      color = "grey85",
      linetype = "dashed"
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    legend.position = "right"
  )
  
  