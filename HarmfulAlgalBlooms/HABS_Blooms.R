# HABS bloom analysis for STAR
# Steve Brett's data
# Last update: 6 November 2020

suppressPackageStartupMessages({
  library(lubridate)
  library(sp)
  library(maptools)
  library(patchwork)
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
         Mon = month(Date),
         Toxin = 'DST Producers',
         Toxin = replace(Toxin, Genus == 'Pseudo-nitzschia', 'AST Producers'),
         Toxin = replace(Toxin, Genus == 'Alexandrium', 'PST Producers')) ## relate toxin to the correct species.

## work out Temperate East region & South East region
dat_te <- dat %>% filter(Latitude > -36 & Latitude < -24.5) #14440 records
dat_se <- dat %>% filter(Latitude > -47.12 & Latitude < -36) #9590 records

## Climatology
datm_te <- dat_te %>% group_by(Mon, Toxin) %>% summarise(meanAbundance = mean(Abundance, na.rm = TRUE))
datm_se <- dat_se %>% group_by(Mon, Toxin) %>% summarise(meanAbundance = mean(Abundance, na.rm = TRUE))

## Bring in closures data
closures <-
  read_csv("Data/closures_data.csv") %>% filter(ClosureDays > 0)

closuresPlots <-  ggplot(closures) +
  geom_bar(aes(x=Year, y=ClosureWeeks), stat='identity') +
  facet_grid(Toxin ~., scales = "free")

x11(width = 6, height = 8)
closuresPlots
ggsave("Figures/Closures.png", dpi = 400)

text <- data.frame(x=as.Date("2006-01-01"), y = 24000000, lab = "Temperate East",
                   Toxin = factor('AST Producers', levels = c('AST Producers', 'DST Producers', 'PST Producers')))

abund_te <- ggplot() + geom_rect(data = data.frame(xmin = as.Date(c("2016-01-01")),
                                                        xmax = as.Date(c("2019-12-31")),
                                                        ymin = -Inf,
                                                        ymax = Inf),
                                      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                      fill = "light blue", alpha = 0.5) +
  geom_line(data = dat_te, aes(x = Date, y = Abundance)) +
  geom_text(aes(x = x, y = y,  label=lab, group=NULL), data = text) +
  facet_grid(Toxin ~., scales = "free") +
  scale_x_date(breaks = scales::date_breaks("4 years"), date_labels = '%Y-%b') +
  labs( x = "", y = bquote("Abundance m"^3)) +
  theme_bw(base_size = 12) + theme(strip.background = element_blank(),
                     strip.text = element_blank()
                    )

climat_te <- ggplot(datm_te) +
  geom_line(aes(x = Mon, y = meanAbundance)) +
  geom_smooth(aes(x = Mon, y = meanAbundance)) +
  facet_grid(Toxin ~., scales = "free") +
  scale_x_continuous(breaks = seq(1,12,1), labels= c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  labs( x = "", y = "") +
  theme_bw(base_size = 12)  + theme(strip.background = element_blank())

text_se <- data.frame(x=as.Date("2006-01-01"), y = 12000000, lab = "South East",
                   Toxin = factor('AST Producers', levels = c('AST Producers', 'DST Producers', 'PST Producers')))

abund_se <- ggplot() + geom_rect(data = data.frame(xmin = as.Date(c("2016-01-01")),
                                                   xmax = as.Date(c("2019-12-31")),
                                                   ymin = -Inf,
                                                   ymax = Inf),
                                 aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                                 fill = "light blue", alpha = 0.5) +
  geom_line(data = dat_se, aes(x = Date, y = Abundance)) +
  geom_text(aes(x = x, y = y,  label=lab, group=NULL), data = text_se) +
  facet_grid(Toxin ~., scales = "free") +
  scale_x_date(breaks = scales::date_breaks("4 years"), date_labels = '%Y-%b') +
  labs( x = "Time", y = bquote("Abundance m"^3)) +
  theme_bw(base_size = 12) + theme(strip.background = element_blank(),
                     strip.text = element_blank()
  )

climat_se <- ggplot(datm_se) +
  geom_line(aes(x = Mon, y = meanAbundance)) +
  geom_smooth(aes(x = Mon, y = meanAbundance)) +
  facet_grid(Toxin ~., scales = "free") +
  scale_x_continuous(breaks = seq(1,12,1), labels= c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  labs( x = "Time", y = "") +
  theme_bw(base_size = 12)  + theme(strip.background = element_blank())

x11(width = 10, height = 8)
plot <- (abund_te + climat_te) / (abund_se + climat_se)
plot
ggsave("Figures/RegionHABS.png", plot, dpi = 600)

