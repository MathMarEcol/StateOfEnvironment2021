# HABS bloom analysis for STAR
# Steve Brett's data
# Last update: 6 November 2020

suppressPackageStartupMessages({
  library(lubridate)
  library(tidyverse)
})

#### 1. PREAMBLE ####
KeyGenera = c("Dinophysis", "Pseudo-nitzschia")
KeySpp <- c("Alexandrium pacificum", "Alexandrium minutum")
SppToPlot <- list(Plot1 = c("Alexandrium australiense", "Alexandrium minutum", "Alexandrium ostenfeldii",
                            "Alexandrium pacificum", "Gymnodinium catenatum", "Karenia mikimotoi"), #, "Pseudo-nitzschia spp."),
                  Plot2 = c("Dinophysis acuminata", "Dinophysis acuta", "Dinophysis caudata", "Dinophysis fortii",
                            "Dinophysis tripos", "Prorocentrum lima"))

## Bring in latest HABS data
dat <- read_csv("Data/Brett_data_nov2020.csv") %>%
  rename(ID = SAMPLE_ID, Sample = SAMPLE_SITE, Latitude = LATITUDE,
         Longitude = LONGITUDE, Date = SAMPLE_DATE,
         Species = TAXON_NAME, Abundance = ABUNDANCE) %>%
  filter(!is.na(Longitude)) %>% # Remove rows with no Lat and Lon
  mutate(Genus = str_split_fixed(Species, " ", n = 2)[,1]) %>%
  filter(Genus %in% KeyGenera | Species %in% KeySpp ) %>% # Only keep Genera and species that relate to the toxins for closure information
  mutate(Date = ymd(Date),
         Year = year(Date),
         Toxin = 'DST',
         Toxin = replace(Toxin, Genus == 'Pseudo-nitzschia', 'AST'),
         Toxin = replace(Toxin, Genus == 'Alexandrium', 'PST')) ## relate toxin to the correct species.


## Bring in closures data
closures <-
  read_csv("Data/closures_data.csv") %>% filter(ClosureDays > 0)

closuresPlots <-  ggplot(closures) +
  geom_bar(aes(x=Year, y=ClosureWeeks), stat='identity') +
  facet_grid(Toxin ~., scales = "free")

x11(width = 6, height = 8)
closuresPlots
ggsave("Figures/Closures.png", dpi = 400)


abundplots <- ggplot(dat) +
  geom_line(aes(x = Date, y = Abundance)) +
  facet_grid(Toxin ~., scales = "free") +
  scale_x_date(breaks = scales::date_breaks("1 year"), date_labels = '%Y') +
  labs( x = "Time", y = bquote("Abundance m"^3)) +
  theme_bw()

x11(width = 6, height = 8)
abundplots
ggsave("Figures/Abundances.png", dpi = 400)

