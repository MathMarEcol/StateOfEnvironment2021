library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(patchwork)
library(lubridate)

# lonlatCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# lonlatCRS_no <- 4326


# Download and process world outline
world <- ne_countries(scale = "medium", returnclass = "sf")
# world_sf <- st_transform(world, crs = st_crs(robCRS)) # Convert to different CRS

  gg <- ggplot() +
    geom_sf(data = world, size = 0.05, fill = "grey20") +
    scale_x_continuous(expand = c(0, 0), limits = c(100, 170)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-50, 0)) +
    theme_bw()

gg

graphics.off()
x11(width = 8, height = 6)
ggsave("Figures/HABs_CaseStudyMap.png", dpi = 500)




