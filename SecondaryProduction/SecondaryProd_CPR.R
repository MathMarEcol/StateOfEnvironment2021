# Code to plot trends in the Integrated Marine Observing System (IMOS) NRS data for SOE2021
#
# Jason Everett (UQ/CSIRO/UNSW)
# Last Updated: 10th November 2020

library(lubridate)
library(tidymodels)
library(patchwork)
library(tidyverse)

source("fHarmonic.R")

re_down <- 1 # Should we download the files again
pt_size <- 1
t_size <- 3

## Plot the measured Phytoplankton Biomass
file = "https://raw.githubusercontent.com/jaseeverett/IMOS_Toolbox/master/Plankton/Output/CPR_Indices.csv"
out_file = paste0('Data',.Platform$file.sep,'CPR_Indices.csv')

if (re_down==1){
  download.file(file, out_file, method = "auto", quiet=FALSE)
}

dat <- read_csv(out_file) %>%
  mutate(
    ZoopAbundance_m3 = log10(ZoopAbundance_m3),
    SiteCode = str_sub(NRScode,1, 3),
    DOY = yday(SampleDateLocal),
    SampleDateLocal = date(SampleDateLocal),
    HarmDOY = (DOY/365)*2*pi) %>%  # Convert to radians)
  select(SiteCode, DOY, SampleDateLocal, HarmDOY, ZoopAbundance_m3, ShannonCopepodDiversity)


# Load Bioregion
bioregion <- readOGR("/Users/jason/GitHub/IMOS_Toolbox/Plankton/Shape/marine_regions_2012/shape/marine_regions.shp")

# Crop elevation data by extent of state subset
sc <- crop(s, extent(bioregion))
# As a final step, you need to identify those pixels of your elevation raster that lie within the borders of the given state polygons. Use the 'mask' function for that.

mb <- c("North", "Temperate East", "North-west", "South-west", "South-east", "Coral Sea")



https://gis.stackexchange.com/questions/272447/equivalent-of-sp-packages-point-in-polygon-overing-using-sf

st_join(pts, poly, join = st_intersects)