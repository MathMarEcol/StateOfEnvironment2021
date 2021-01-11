# Code to plot trends in the Integrated Marine Observing System (IMOS) NRS data for SOE2021
#
# Jason Everett (UQ/CSIRO/UNSW)
# Last Updated: 11th Jan 2021

library(lubridate)
library(tidymodels)
library(patchwork)
library(tidyverse)

source("fHarmonic.R")

re_down <- 0 # Should we download the files again
pt_size <- 1
t_size <- 3

file = "https://raw.githubusercontent.com/jaseeverett/IMOS_Toolbox/master/Plankton/Output/NRS_Indices.csv"
out_file = paste0('Data',.Platform$file.sep,'NRS_Indices.csv')

if (re_down==1){
  download.file(file, out_file, method = "auto", quiet=FALSE)
}

dat <- read_csv(out_file) %>%
  mutate(ZoopAbundance_m3 = log10(ZoopAbundance_m3),
    SiteCode = str_sub(NRScode,1, 3),
    DOY = yday(SampleDateLocal),
    SampleDateLocal = date(SampleDateLocal),
    HarmDOY = (DOY/365)*2*pi) %>%  # Convert to radians)
  select(SiteCode, DOY, SampleDateLocal, HarmDOY, ZoopAbundance_m3, Chla_mgm3)

## Plotting
sites <- c("DAR", "YON", "NSI", "ROT", "PHB", "KAI", "MAI")
site_names <- c("a) Darwin Harbour","b) Darwin Harbour",
                "c) Yongala","d) Yongala",
                "e) North Stradbroke Island", "f) North Stradbroke Island",
                "g) Rottnest Island","h) Rottnest Island",
                "i) Port Hacking", "j) Port Hacking",
                "k) Kangaroo Island", "l) Kangaroo Island",
                "m) Maria Island", "n) Maria Island")


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
    annotate("text", x = ymd("2008-12-15"), y = Inf, label = site_names[counter], hjust = 0, vjust = 1, size = t_size) +
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


  # Do Zooplankton Abundance
  mdl <- lm(ZoopAbundance_m3 ~ SampleDateLocal + fHarmonic(HarmDOY, k= 1), data = m_dat)
  val <- anova(mdl)$'Pr(>F)'[1]
  tmdl <- tidy(mdl)
  Abun_slope[i] <- tmdl$estimate[tmdl$term=="SampleDateLocal"] # Save the model slope

  if (Abun_slope[i]>0 & val <= 0.05){
    clr <- "red"
  } else if (Abun_slope[i]<0 & val <= 0.05){
    clr <- "blue"
  } else {
    clr <- "black"
  }

  gg <- ggplot() +
    geom_point(data = m_dat, aes(x=SampleDateLocal, y=ZoopAbundance_m3), size = pt_size) +
    stat_smooth(data = mdl, method = "lm", col = clr, fill = clr,
                aes(x = SampleDateLocal, y = ZoopAbundance_m3)) +
    theme_bw() +
    scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
    annotate("text", x = ymd("2008-12-15"), y = Inf, label = site_names[counter], hjust = 0, vjust = 1, size = t_size) +
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

}


myplots[[9]] <- myplots[[9]] +
  ylab(expression(paste("Chlorphyll "*italic(a)," Biomass (mg m"^{-3},")"))) +
  theme(axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = 0, vjust = 1))

myplots[[10]] <- myplots[[10]] +
  ylab(expression(paste("Zooplankton Abundance log"[10],"(ind. m"^{-3},")"))) +
  theme(axis.title.y = element_text(color = "black", hjust = 1, size = 12))

graphics.off()
fig <- wrap_plots(myplots, ncol = 2, nrow = 7, clip = FALSE)
ggsave(paste0('Figures',.Platform$file.sep,'WaterColumnAssess_Shelf.png'), dpi=300)

saveRDS(myplots,"Figures/gg_Shelf.rds")

