# HABS bloom analysis for STAR
# Steve Brett's data
# Anthony J. Richardson and Claire Davies
# Last update: 16/01/2020

suppressPackageStartupMessages({
  library(ggplot2)
  library(rgdal)
  library(sf)
  library(ozmaps)
  library(patchwork)
  library(lubridate)
  library(tidyverse)
})

########################################
# CPR timeseries for regions where data is plentiful

cprNoct <- read_csv("noct_regions.csv") %>%
  mutate(SampleDate = ymd_hms(SAMPLE_DATE),
         year = year(SampleDate),
         mon = month(SampleDate)) %>%
  rename(latitude = LATITUDE, longitude = LONGITUDE, region = BIOREGION, CellsL = CELLS_L) %>%
  select(-SAMPLE_DATE) %>%
  filter(region %in% c("South-east", "Temperate East")) %>% # remove regions with no data trends
  mutate(region = as.factor(region),
         region = factor(region, levels(region)[c(2,1)]))

## take the mean abundance of each segment for each month
CPRts <- cprNoct %>% group_by(region, year, mon) %>% summarise(CellsL = mean(CellsL, na.rm = TRUE)) %>%
  mutate(Date = ymd(paste0(year, '-', mon, '-01')))

CPRNoctTS <- ggplot(data = CPRts, aes(Date, CellsL)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  facet_grid(region ~., scales = "free") +
  scale_x_date(breaks = scales::date_breaks("2 years"), date_labels = '%Y') +
  theme_bw(base_size = 14) + labs(x = 'Time', y = bquote("Noctiluca (Cells L"^-1*")")) +
  geom_text(aes(x = ymd("2007-06-01"), y = Inf, vjust = 2, label=region, group=NULL)) +
  theme(strip.background = element_blank(),strip.text.y = element_blank())
CPRNoctTS

## take the monthly climatology
CPRmc <- cprNoct %>% group_by(region, mon) %>% summarise(CellsL = mean(CellsL, na.rm = TRUE))

y1 = CPRmc %>% group_by(region) %>% summarise(y1=max(CellsL, na.rm = TRUE))

stat <- data.frame(x = c(2,2), y = y1$y1, region = y1$region)

CPRNoctmc <- ggplot(data = CPRmc, aes(mon, CellsL)) +
  geom_point() +
  #geom_line() +
  geom_smooth() +
  facet_grid(region ~., scales = "free") +
  scale_x_continuous(breaks = seq(1,12,1), labels= c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  theme_bw(base_size = 14) + labs(x = 'Time of Year', y ="") +
  geom_text(aes(x = 2, y = Inf, vjust = 2, label=region, group=NULL)) +
  theme(strip.background = element_blank(),strip.text.y = element_blank())
CPRNoctmc

x11(width = 11, height = 6)
plots <- CPRNoctTS + CPRNoctmc
plots

#############################################
#noctiluca map
#############################################
noc <- read_csv("noct_all.csv", na = "(null)", col_types = cols(PROJECT = col_character())) %>%
  mutate(Date = as.Date(SAMPLE_DATE),
         year = year(Date),
         period = '1950-1979',
         period = replace(period, year < 1950, '1860-1949'),
         period = replace(period, year > 2009, '2010-2019'),
         period = replace(period, year > 1979 & year < 1990, '1980-1989'),
         period = replace(period, year > 1989 & year < 2000, '1990-1999'),
         period = replace(period, year > 1999 & year < 2010, '2000-2009'),
         cells_l = as.factor('Yes')) %>%
  filter(!is.na(LATITUDE)) %>%
  select(LATITUDE, LONGITUDE, Date, year, period, cells_l)

noca <- read_csv("noccy_abs.csv", na = "(null)") %>%
  mutate(Date = ymd_hms(SAMPLE_DATE),
         year = year(Date),
         period = '1950-1979',
         period = replace(period, year < 1950, '1860-1949'),
         period = replace(period, year > 2009, '2010-2019'),
         period = replace(period, year > 1979 & year < 1990, '1980-1989'),
         period = replace(period, year > 1989 & year < 2000, '1990-1999'),
         period = replace(period, year > 1999 & year < 2010, '2000-2009'),
         cells_l = as.factor('No')) %>%
  select(LATITUDE, LONGITUDE, Date, year, period, cells_l)

nocgh <- read_csv("gh_noc_absences.csv", na = "(null)") %>%
  mutate(Date = dmy_hm(SAMPLE_DATE),
         year = year(Date),
         period = '1980-1989',
         cells_l = as.factor('No')) %>%
  select(LATITUDE, LONGITUDE, Date, year, period, cells_l)

sh <- data.frame(-34.11758, 151.21815, "1860-01-01",1860 , "1860-1949",  as.factor('Yes')) # GH pers comms
colnames(sh)=c("LATITUDE", "LONGITUDE", "Date",  "year","period", "cells_l")

pas <- read_csv("noc_extras.csv") %>% #add steve bretts paspaley and defence data and Was Hojas
  mutate(Date = dmy(SAMPLE_DATE)) %>%
  select(LATITUDE, LONGITUDE, Date, year, period, cells_l)

all <- rbind(noc,noca,sh, nocgh, pas)

all <- all[order(all$cells_l, decreasing=T), ]

cols <- c("#3366FF", 'azure4')
names(cols) <- levels(all$cells_l)
colscale <- scale_colour_manual(name = 'Present', values = cols)

pchs <- c(19, 1)
names(pchs) <- levels(all$cells_l)
pchscale <- scale_shape_manual(name = 'Present', values = pchs)

sizes <- c(2, 0.5)
names(sizes) <- levels(all$cells_l)
sizescale <- scale_size_manual(name = 'Present', values = sizes)


xlab <- expression(text=''^o*'Longitude')
ylab <- expression(text=''^o*'Latitude')

oz <- ozmap_states
map <- ggplot() + geom_sf(data = oz) +
  geom_point(data=all, aes(x=LONGITUDE, y=LATITUDE, color=cells_l, size=cells_l, pch=cells_l)) +
  facet_wrap(~period) + theme_bw(base_size=12) + labs(x="", y="") +
  theme(strip.background = element_blank(), legend.position = "bottom") +
  scale_x_continuous(breaks = seq(110,160,20), limits = c(110, 160)) +
  colscale + pchscale + sizescale + ylim(-55,-5)
map

x11(width = 11, height = 11)
plotall <-  map / (CPRNoctTS + CPRNoctmc)
plotall
ggsave("Noctiluca.png", plotall, dpi=600)
