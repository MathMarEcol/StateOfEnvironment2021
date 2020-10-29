library(tidyverse)
library(raster)
library(sf)
library(rnaturalearth)
library(patchwork)
library(lubridate)

# lonlatCRS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# lonlatCRS_no <- 4326

## Create NRS details

## Zooplankton
download.file("https://raw.githubusercontent.com/jaseeverett/IMOS_Toolbox/master/Plankton/Output/NRS_Indices.csv", paste0("Data",.Platform$file.sep,"NRS_Indices.csv"))

NRS <- read_csv(paste0("Data",.Platform$file.sep,"NRS_Indices.csv")) %>%
  group_by(Station) %>%
  mutate(Year_Start = min(Year),
         Year_Latest = max(Year)) %>%
  ungroup() %>%
  distinct(Station, .keep_all = TRUE) %>%
  dplyr::select(Longitude, Latitude, NRScode, Station, StationDepth_m, Year_Start, Year_Latest) %>%
  mutate(NRScode = stringr::str_extract(NRScode, "^.{3}"))

# Download and process world outline
world <- ne_countries(scale = "medium", returnclass = "sf")
# world_sf <- st_transform(world, crs = st_crs(robCRS)) # Convert to different CRS

NRS_sf <- st_as_sf(NRS, coords = c("Longitude", "Latitude"), crs = 4326)


gg <- ggplot() +
  geom_sf(data = world, size = 0.05, fill = "grey20") +
  geom_sf(data = NRS_sf, colour = "red") +
  geom_sf_text(data = NRS_sf, aes(label = Station), colour = "red", nudge_y = -1) +
  scale_x_continuous(expand = c(0, 0), limits = c(100, 170)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-50, 0)) +
  theme_bw() +
  theme(axis.title = element_blank())

graphics.off()
x11(width = 8, height = 6)
gg
ggsave("Figures/HABs_CaseStudyMap.png", dpi = 500)




