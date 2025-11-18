# Install packages if needed
# install.packages(c("sf", "ggplot2", "viridis", "rnaturalearth", "rnaturalearthdata", "dplyr"))

library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# ---- Your Data (example; replace with your own) ----
df <- read.csv("gradienteData/sites_Pannual.csv")
# df <- data.frame(
#   name  = c("A", "B", "C", "D", "E"),
#   lon   = c(-3.7038, -5.98, -0.12, -1.13, 2.17),
#   lat   = c(40.4168, 37.39, 39.47, 43.36, 41.39),
#   value = c(10, 30, 50, 70, 90)
# )

points_sf <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)

# ---- Get Spain boundary ----
spain <- ne_countries(
  country = "Spain",
  scale = "large",
  returnclass = "sf"
)

# ---- Optional: clip points to Spain only ----
# points_sf <- st_intersection(points_sf, spain)

# ---- Plot ----
ggplot() +
  geom_sf(data = spain, fill = "gray95", color = "gray60") +
  geom_sf(data = points_sf, aes(color = P_annual), size = 4) +
  scale_color_viridis(option = "plasma", direction = 1) +
  coord_sf(xlim = st_bbox(spain)[c("xmin","xmax")],
           ylim = st_bbox(spain)[c("ymin","ymax")],
           expand = FALSE) +
  theme_minimal() +
  labs(
    title = " ",
    color = "Annual P (mm)"
  ) +
  theme(
    panel.grid = element_line(color = "gray90"),
    panel.background = element_rect(fill = "white")
  )
