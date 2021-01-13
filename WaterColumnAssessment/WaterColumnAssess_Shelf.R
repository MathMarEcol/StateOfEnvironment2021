# Code to plot trends in the Integrated Marine Observing System (IMOS) NRS data for SOE2021
#
# Jason Everett (UQ/CSIRO/UNSW)
# Last Updated: 11th Jan 2021

library(lubridate)
library(tidymodels)
library(patchwork)
library(tidyverse)

source("fHarmonic.R")
source("../../PlanktonTrendAnalysis/PlanktonTrends_HelpR.R")

re_down <- FALSE # Should we download the files again
pt_size <- 1
t_size <- 3

file = "https://raw.githubusercontent.com/jaseeverett/IMOS_Toolbox/master/Plankton/Output/NRS_Indices.csv"
out_file = paste0('Data',.Platform$file.sep,'NRS_Indices.csv')

if (re_down==TRUE){
  download.file(file, out_file, method = "auto", quiet=FALSE)
}


dat <- read_csv(out_file)
min_val <- min(dat$ZoopAbundance_m3[dat$ZoopAbundance_m3>0], na.rm = TRUE) / 2 # Half min value
min_val2 <- min(dat$Biomass_mgm3[dat$Biomass_mgm3>0], na.rm = TRUE) / 2 # Half min value

dat <- dat %>%
  mutate(ZoopAbundance_m3 = log10(ZoopAbundance_m3 + min_val),
         Biomass_mgm3 = log10(Biomass_mgm3 + min_val2),
    SiteCode = str_sub(NRScode,1, 3),
    DOY = yday(SampleDateLocal),
    SampleDateLocal = date(SampleDateLocal),
    HarmDOY = (DOY/365)*2*pi) %>%  # Convert to radians)
  select(SiteCode, DOY, SampleDateLocal, HarmDOY, Biomass_mgm3, Chla_mgm3)

## Now do larval fish

KAI <- read_csv("Data/KAI.csv")
MAI <- read_csv("Data/MAI.csv")
NSI <- read_csv("Data/NSI.csv")
PHB <- read_csv("Data/PHB.csv")
ROT <- read_csv("Data/ROT.csv")

lf <- bind_rows(KAI, MAI, NSI, PHB, ROT) %>%
  mutate(LFAbund = rowSums(select(., "Acanthuridae_37437000":"Phycidae_Gadiopsurus.spp._37226000"), na.rm = TRUE)/Volume_m3) %>%
  select(-c("Acanthuridae_37437000":"Phycidae_Gadiopsurus.spp._37226000")) %>%
  mutate(Date = dmy(Date),
         DOY = yday(Date),
         HarmDOY = (DOY/365)*2*pi,  # Convert to radians)
         SiteCode = str_sub(Sample,1,3)) # Get site code


## Plotting
sites <- c("DAR", "YON", "NSI", "ROT", "PHB", "KAI", "MAI")
site_names <- c("a) Darwin Harbour","b) Darwin Harbour", "c) Darwin Harbour",
                "d) Yongala","e) Yongala", "f) Yongala",
                "g) North Stradbroke Island", "h) North Stradbroke Island","i) North Stradbroke Island",
                "j) Rottnest Island","k) Rottnest Island","l) Rottnest Island",
                "m) Port Hacking", "n) Port Hacking","o) Port Hacking",
                "p) Kangaroo Island", "q) Kangaroo Island", "r) Kangaroo Island",
                "s) Maria Island", "t) Maria Island", "u) Maria Island")


myplots <- list() # Initialise plot list

Abun_slope <- NULL
Chl_slope <- NULL

counter <- 1

for (i in 1:length(sites)) {
  m_dat <- dat %>%
    filter(SiteCode==sites[i])

  ref_date = dmy('01-1-2009')
  data_length <- as.numeric(difftime(m_dat$SampleDateLocal[length(m_dat$SampleDateLocal)], m_dat$SampleDateLocal[1]))
  decade_length <- as.numeric(difftime(dmy("01-01-2020"), ref_date))



  ## Do Phytoplankton
  mdl <- lm(Chla_mgm3 ~ SampleDateLocal + fHarmonic(HarmDOY, k = 1), data = m_dat)
  pval <- anova(mdl)$'Pr(>F)'[1]
  tmdl <- tidy(mdl)
  Chl_slope[i] <- tmdl$estimate[tmdl$term=="SampleDateLocal"] # slope is the change per day.

  if (Chl_slope[i]>0 & pval <= 0.05){
    clr <- "red"
  } else if (Chl_slope[i]<0 & pval <= 0.05){
    clr <- "blue"
  } else {
    clr <- "black"
  }

  gg <- ggplot() +
    geom_point(data = m_dat, aes(x=SampleDateLocal, y=Chla_mgm3), size = pt_size) +
    stat_smooth(data = mdl, method = "lm", col = clr, fill = clr,
                aes(x = SampleDateLocal, y = Chla_mgm3)) +
    theme_bw() +
    scale_y_continuous(expand = expansion(mult = 0.2)) +
    annotate("text", x = ymd("2008-12-15"), y = Inf, label = site_names[counter], hjust = 0, vjust = 1.2, size = t_size) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())

  if (i == 7) {
    gg <- gg +
      scale_x_date(labels = NULL,
                   date_labels = "%Y",
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2008-12-1"), as.Date("2020-09-30")),
                   expand = c(0,0))
  } else {
    gg <- gg +
      scale_x_date(labels = NULL,
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2008-12-1"), as.Date("2020-06-30")),
                   expand = c(0,0))
  }

  myplots[[counter]] <- gg
  counter <- counter + 1
  rm(gg, val, mdl, tmdl)


  myplots[[counter]] <- fTrendAnalysis(m_dat, "Biomass_mgm3", "SampleDateLocal", "HarmDOY") +
    scale_y_continuous(expand = expansion(mult = 0.2), position = "left") +
    annotate("text", x = ymd("2008-12-15"), y = Inf, label = site_names[counter], hjust = 0, vjust = 1.2, size = t_size)


  if (i == 7) {
    myplots[[counter]] <- myplots[[counter]] +
      scale_x_date(labels = NULL,
                   date_labels = "%Y",
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2008-12-1"), as.Date("2020-09-30")),
                   expand = c(0,0))
  } else {
    myplots[[counter]] <- myplots[[counter]] +
      scale_x_date(labels = NULL,
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2008-12-1"), as.Date("2020-06-30")),
                   expand = c(0,0))
  }

  counter <- counter + 1
  rm(temp)


  lf2 <- lf %>%
    filter(SiteCode==sites[i])

  if (dim(lf2)[1] > 0){
    myplots[[counter]] <- fTrendAnalysis(lf2, "LFAbund", "Date", "HarmDOY") +
      scale_y_continuous(expand = expansion(mult = 0.2), position = "left") +
      annotate("text", x = ymd("2013-12-15"), y = Inf, label = site_names[counter], hjust = 0, vjust = 1.2, size = t_size)
  } else {
    myplots[[counter]] <- ggplot() + geom_blank() + theme_minimal()
  }

  if (i == 7) {
    myplots[[counter]] <- myplots[[counter]] +
      scale_x_date(labels = NULL,
                   date_labels = "%Y",
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2013-12-1"), as.Date("2020-09-30")),
                   expand = c(0,0))
  }

  if (i >= 3 & i <= 6) {
    myplots[[counter]] <- myplots[[counter]] +
      scale_x_date(labels = NULL,
                   date_breaks = "2 years",
                   date_minor_breaks = "years",
                   limits = c(as.Date("2013-12-1"), as.Date("2020-06-30")),
                   expand = c(0,0))
  }

  counter <- counter + 1
  rm(temp)


}

myplots[[1]] <- myplots[[1]] +
  labs(title = expression(paste("Chlorophyll "*italic(a)," Biomass")),
          subtitle = expression(paste("log"[10],"(mg m"^{-3},")"))) +
  theme(plot.title = element_text(color = "black", size = 8, face = "bold", hjust = 0.5, vjust = -1),
        plot.subtitle = element_text(color = "black", size = 8, face = "bold", hjust = 0.5, vjust = 2))

myplots[[2]] <- myplots[[2]] +
  labs(title = expression(paste("Zooplankton Biomass")),
       subtitle = expression(paste("log"[10],"(mg m"^{-3},")"))) +
  theme(plot.title = element_text(color = "black", size = 8, face = "bold", hjust = 0.5, vjust = -1),
        plot.subtitle = element_text(color = "black", size = 8, face = "bold", hjust = 0.5, vjust = 2))

myplots[[3]] <- myplots[[3]] +
  labs(title = expression(paste("Larval Fish Abundance")),
       subtitle = expression(paste("log"[10],"(ind. m"^{-3},")"))) +
  theme(plot.title = element_text(color = "black", size = 8, face = "bold", hjust = 0.5, vjust = -1),
        plot.subtitle = element_text(color = "black", size = 8, face = "bold", hjust = 0.5, vjust = 2))


graphics.off()
fig <- wrap_plots(myplots, ncol = 3, nrow = 7, clip = FALSE)
ggsave(paste0('Figures',.Platform$file.sep,'WaterColumnAssess_ShelfBiomass.png'), dpi=300)

saveRDS(myplots,"Figures/gg_Shelf.rds")

