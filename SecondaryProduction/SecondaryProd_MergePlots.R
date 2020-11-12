# Code to merge plots for SOE2021
#
# Jason Everett (UQ/CSIRO/UNSW)
# Last Updated: 12th November 2020

library(lubridate)
library(tidymodels)
library(patchwork)
library(tidyverse)

source("fHarmonic.R")

gg_NRS <- readRDS("Figures/gg_NRS.rds")
gg_CPR <- readRDS("Figures/gg_CPR.rds")

gg_abund <- gg_NRS
gg_abund[[1]] <- gg_abund[[1]] + ggtitle("Coastal") + theme(plot.title = element_text(hjust = 0.5, vjust = 0))
gg_abund[[2]] <- ggplot() + geom_blank() + theme_bw() + ggtitle("Offshore") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0), panel.border = element_blank()) # Darwin Harbour
gg_abund[[4]] <- gg_CPR[[3]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) # Yongala
gg_abund[[6]] <- ggplot() + geom_blank() + theme_void() # North Stradbroke Island
gg_abund[[8]] <- gg_CPR[[9]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) # Rottnest Island
gg_abund[[10]] <- gg_CPR[[7]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
  ylab(expression(paste("Zooplankton Abundance log"[10],"(ind. m"^{-3},")"))) +
  theme(axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = 1, vjust = 0)) # Port Hacking
gg_abund[[12]] <- ggplot() + geom_blank() + theme_void() # Kangaroo Island
gg_abund[[14]] <- gg_CPR[[11]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) # Maria Island

graphics.off()
x11(width = 6, height = 8)
(fig <- wrap_plots(gg_abund, ncol = 2, nrow = 7) + plot_annotation(theme = theme(plot.margin = margin())))
ggsave(paste0('Figures',.Platform$file.sep,'SecondaryProdnTrends_Abundance.png'), dpi=300)


#### ####

gg_diversity <- list()
gg_diversity[[1]] <- gg_NRS[[2]] + ggtitle("Coastal") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0)) +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "left")
gg_diversity[[2]] <- ggplot() + geom_blank() + theme_bw() + ggtitle("Offshore") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0), panel.border = element_blank()) # Darwin Harbour
gg_diversity[[3]] <- gg_NRS[[4]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "left")
gg_diversity[[4]] <- gg_CPR[[4]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) # Yongala
gg_diversity[[5]] <- gg_NRS[[6]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "left")
gg_diversity[[6]] <- ggplot() + geom_blank() + theme_void() # North Stradbroke Island
gg_diversity[[7]] <- gg_NRS[[8]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "left") +
  ylab("Copepod Diversity") +
  theme(axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = 0, vjust = 1))

gg_diversity[[8]] <- gg_CPR[[10]] + # Rottnest Island
  scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
  ylab("Copepod Diversity") +
  theme(axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = 1, vjust = 1))

gg_diversity[[9]] <- gg_NRS[[10]] +
scale_y_continuous(expand = expansion(mult = 0.2), position = "left") +
  theme(axis.title.y = element_blank())


gg_diversity[[10]] <- gg_CPR[[8]] + # Port Hacking
scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

gg_diversity[[11]] <- gg_NRS[[12]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "left")
gg_diversity[[12]] <- ggplot() + geom_blank() + theme_void() # Kangaroo Island
gg_diversity[[13]] <- gg_NRS[[14]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "left")
gg_diversity[[14]] <- gg_CPR[[12]] +
  scale_y_continuous(expand = expansion(mult = 0.2), position = "right") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) # Maria Island

graphics.off()
x11(width = 6, height = 8)
(fig <- wrap_plots(gg_diversity, ncol = 2, nrow = 7) +
    plot_annotation(theme = theme(plot.margin = margin())))
ggsave(paste0('Figures',.Platform$file.sep,'SecondaryProdnTrends_Diversity.png'), dpi=300)










