# Code to plot trends in the Integrated Marine Observing System (IMOS) NRS data for SOE2021
#
# Jason Everett (UQ/CSIRO/UNSW)
# Last Updated: 11th January 2021

library(lubridate)
library(tidymodels)
library(patchwork)
library(tidyverse)
library(raster)
library(rgdal)

source("fHarmonic.R")
source("../../PlanktonTrendAnalysis/PlanktonTrends_HelpR.R")


recrop <- FALSE
reprocess <- FALSE
re_down <- FALSE # Should we download the files again
pt_size <- 1
t_size <- 3


mb <- c("North", "Coral Sea",  "North-west", "Temperate East", "South-west", "South-east")

mb_names <- c("a) North Bioregion","b) North Bioregion",
              "c) Coral Sea Bioregion","d) Coral Sea Bioregion",
              "e) North-west Bioregion", "f) North-west Bioregion",
              "g) Temperate East Bioregion","h) Temperate East Bioregion",
              "i) South-west Bioregion", "j) South-west Bioregion",
              "k) South-east Bioregion", "l) South-east Bioregion")


# Load Bioregion
bioregion <- readOGR("/Users/jason/GitHub/IMOS_Toolbox/General/Shapefiles/marine_regions_2012/marine_regions.shp")

# Crop elevation data by extent of IMOS Region
if (recrop == TRUE){
  s <- fSatellite_StackNASA()
  sc <- crop(s, extent(bioregion))
  writeRaster(sc, "../../PlanktonTrendAnalysis/Output/Chl_Cropped")
} else{
  sc <- stack("../../PlanktonTrendAnalysis/Output/Chl_Cropped.grd")
}

# Load data
if (reprocess == TRUE){
  sat <- tibble(Bioregion = character(), MeanChl = vector(), Date = date())
  sat$Date = date(sat$Date) # I don't why I need this but it works....

  for (i in 1:length(mb)){
    print(mb[i])
    sm <- mask(sc, bioregion[bioregion$REGION == mb[i],])

    temp <- tibble(BioRegion = mb[i],
                   MeanChl = raster::cellStats(sm, mean, na.rm = TRUE),
                   Date = ymd(str_replace(names(sm), "X", ""))) %>%
      mutate(Month = month(Date),
             logMeanChl = log10(MeanChl),
             DOY = yday(Date),
             HarmDOY = (DOY/365)*2*pi) # Convert to radians)

    write_rds(temp, paste0("../../PlanktonTrendAnalysis/Output/Bioregion_",mb[i],".rds"))

    sat <- bind_rows(sat, temp)

    write_rds(sat, "../../PlanktonTrendAnalysis/Output/Bioregion_Sat.rds")
    rm(temp, sm)
  }
}
sat <- read_rds("../../PlanktonTrendAnalysis/Output/Bioregion_Sat.rds")


## Get the measured Zooplankton Biomass
file = "https://raw.githubusercontent.com/jaseeverett/IMOS_Toolbox/master/Plankton/Output/CPR_Indices.csv"
out_file = "Data/CPR_Indices.csv"

if (re_down==TRUE){
  download.file(file, out_file, method = "auto", quiet=FALSE)
}

dat <- read_csv(out_file)

min_val <- min(dat$ZoopAbundance_m3[dat$ZoopAbundance_m3>0], na.rm = TRUE) / 2 # Half min value
min_val2 <- min(dat$Biomass_mgCarbon_m3[dat$Biomass_mgCarbon_m3>0], na.rm = TRUE) / 2 # Half min value

dat <- dat %>%
  mutate(ZoopAbundance_m3 = log10(ZoopAbundance_m3 + min_val),
         Biomass_mgCarbon_m3 = log10(Biomass_mgCarbon_m3 + min_val2),
         DOY = yday(SampleDateUTC),
         SampleDateUTC = date(SampleDateUTC),
         HarmDOY = (DOY/365)*2*pi) %>%  # Convert to radians
  dplyr::select(Route, Region, Longitude, Latitude, BioRegion, DOY, SampleDateUTC, HarmDOY, ZoopAbundance_m3, Biomass_mgCarbon_m3)

counter <- 1
myplots <- list()
for (i in 1:length(mb)){

  temp <- sat %>%
    filter(Bioregion == mb[i])

  myplots[[counter]] <- fTrendAnalysis(temp, "MeanChl", "Date", "HarmDOY") +
    annotate("text", x = ymd("2001-12-15"), y = Inf, label = mb_names[counter], hjust = 0, vjust = 1.2, size = t_size)

  if (i == 6) {
    myplots[[counter]] <- myplots[[counter]] +
      scale_x_date(labels = NULL,
                   date_labels = "%Y",
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2001-12-1"), as.Date("2020-09-30")),
                   expand = c(0,0))
  } else {
    myplots[[counter]] <- myplots[[counter]] +
      scale_x_date(labels = NULL,
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2001-12-1"), as.Date("2020-06-30")),
                   expand = c(0,0))
  }

  counter <- counter + 1
  rm(temp)

  temp <- dat %>%
    filter(BioRegion == mb[i])

  myplots[[counter]] <- fTrendAnalysis(temp, "Biomass_mgCarbon_m3", "SampleDateUTC", "HarmDOY") +
    scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
    annotate("text", x = ymd("2008-12-15"), y = Inf, label = mb_names[counter], hjust = 0, vjust = 1.2, size = t_size)


  if (i == 6) {
    myplots[[counter]] <- myplots[[counter]] +
      scale_x_date(labels = NULL,
                   date_labels = "%Y",
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2011-12-1"), as.Date("2020-09-30")),
                   expand = c(0,0))
  } else {
    myplots[[counter]] <- myplots[[counter]] +
      scale_x_date(labels = NULL,
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2011-12-1"), as.Date("2020-06-30")),
                   expand = c(0,0))
  }

  counter <- counter + 1
  rm(temp)
  }

myplots[[2]] <- ggplot() + geom_blank() + theme_minimal()
myplots[[6]] <- ggplot() + geom_blank() + theme_minimal()

myplots[[7]] <- myplots[[7]] +
  ylab(expression(paste("Chlorophyll "*italic(a)," Biomass (mg m"^{-3},")"))) +
  theme(axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = 0, vjust = 1))

myplots[[10]] <- myplots[[10]] +
  ylab(expression(paste("Zooplankton Biomass log"[10],"(mg C m"^{-3},")"))) +
  theme(axis.title.y = element_text(color = "black", hjust = 1.35, size = 12))


graphics.off()
fig <- wrap_plots(myplots, ncol = 2, nrow = 6)
ggsave(paste0('Figures',.Platform$file.sep,'WaterColumnAssess_OffshoreBiomass.png'), dpi=300)

saveRDS(myplots,"Figures/gg_Offshore.rds")



