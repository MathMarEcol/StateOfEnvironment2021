## Using NRS data for timeseries for water quality - Secchi and TSS
## Compare with globcolour output
## by Claire Davies
## Date created: 8-04-2021

TSS <- read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/BGC_TSS.csv", na = c("(null)", NaN)) %>% 
  rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, TSS_mgL = TSS_MGL, Secchi_m = SECCHIDEPTH_M) %>%
  mutate(Station = substr(TripCode, 4, 6),
         Station = factor(Station, levels = c("DAR","YON","NSI","PHB","ROT","MAI")),
         Date = ymd(paste0(substr(TripCode, 7, 10),"-",substr(TripCode, 11, 12),"-",substr(TripCode, 13, 14))), 
         Year = year(Date),
         Month = month(Date), 
         MonR = Month/12 * 2 * base::pi) %>%
  filter(!is.na(Station))

ChlRot <-  read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/BGC_Pigments.csv", na = c("(null)", NaN)) %>% 
  rename(TripCode = TRIP_CODE, SampleDepth_m = SAMPLEDEPTH_M, Chla = DV_CPHL_A_AND_CPHL_A) %>%
  filter(grepl('ROT', TripCode)) %>%
  mutate(Date = ymd(paste0(substr(TripCode, 4, 7),"-",substr(TripCode, 8, 9),"-",substr(TripCode, 10, 11))), 
         Year = year(Date),
         Month = month(Date), 
         MonR = Month/12 * 2 * base::pi) %>%
  group_by(Date, Year, MonR) %>% summarise(Chla = mean(Chla, na.rm = TRUE),
                               .groups = 'drop') 

Sec2016 <- TSS #%>% filter(Station != 'KAI') %>% droplevels() #%>% filter(Year < 2017)
Sec2020 <- TSS %>% filter(Year > 2015) #%>% filter(Station != 'KAI') %>% droplevels() 

TSSmean <- TSS %>% group_by(Station) %>% summarise(TSSmean = mean(TSS_mgL, na.rm = TRUE),
                                                   sd2 = 1.4*sd(TSS_mgL, na.rm = TRUE),
                                                   factor = TSSmean + sd2, 
                                                   .groups = "drop") %>%
  inner_join(TSS, by = "Station")


TSS2016 <- TSSmean %>% filter(TSS_mgL < factor & TSS_mgL > 0 & TSSFLAG %in% c(0,1)) %>% group_by(Date, Year, MonR, Station) %>% 
  summarise(TSS_mgL = mean(TSS_mgL, na.rm = TRUE), .groups = "drop") %>% 
  drop_na(TSS_mgL)
TSS2020 <- TSSmean %>% filter(Year > 2015 & TSS_mgL < factor & TSS_mgL > 0 & TSSFLAG %in% c(0,1)) %>% group_by(Date, Year, MonR, Station) %>% 
  summarise(TSS_mgL = mean(TSS_mgL, na.rm = TRUE), .groups = "drop") %>% 
  drop_na(TSS_mgL)

plotParam(Sec2016, "Secchi_m", ymd("2009-01-01"), ymd("2020-12-31"))
plotParam(Sec2020, "Secchi_m", ymd("2016-01-01"), ymd("2020-12-31"))
plotParam(TSS2016, "TSS_mgL", ymd("2009-01-01"), ymd("2020-12-31"))
plotParam(TSS2020, "TSS_mgL", ymd("2016-01-01"), ymd("2020-12-31"))

x11()
layout <- c(patchwork::area(1,1,1,1),
            patchwork::area(2,1,2,1),
            patchwork::area(3,1,3,1),            
            patchwork::area(4,1,4,1),
            patchwork::area(5,1,5,1),
            patchwork::area(6,1,6,1),
            patchwork::area(7,1,7,1),
            patchwork::area(1,2,6,2),
            patchwork::area(1,3,6,3))
Secchi <- myplots_Secchi_m[[1]] + myplots_Secchi_m[[6]] + myplots_Secchi_m[[2]] + plot_spacer() + myplots_Secchi_m[[4]] + 
  myplots_Secchi_m[[5]] + myplots_Secchi_m[[3]] + ggSecchi_m2009 + ggSecchi_m2016 + 
  plot_layout(design = layout, widths = c(5,5,2.5))
Secchi
ggsave("Secchi_ts_bio.png", Secchi)
TSSplot <- myplots_TSS_mgL[[1]] + myplots_TSS_mgL[[6]] + myplots_TSS_mgL[[2]] + plot_spacer() + myplots_TSS_mgL[[4]] + 
  myplots_TSS_mgL[[5]] + myplots_TSS_mgL[[3]] +
  ggTSS_mgL2009 + ggTSS_mgL2016 + 
  plot_layout(design = layout, widths = c(5,5,2.5))
TSSplot
ggsave("TSS_ts_bio.png", TSSplot)


### For Rottnest only
SecRot <- TSS %>% filter(Station == 'ROT')
TSSRot <- TSSmean %>% filter(TSS_mgL < factor & TSS_mgL > 0 & TSSFLAG %in% c(0,1) & Station == 'ROT') %>% group_by(Date, Year, MonR, Station) %>% 
  summarise(TSS_mgL = mean(TSS_mgL, na.rm = TRUE), .groups = "drop") %>% 
  drop_na(TSS_mgL) 

plotParamRot(SecRot, "Secchi_m", ymd("2009-01-01"), ymd("2020-12-31"))
plotParamRot(TSSRot, "TSS_mgL", ymd("2009-01-01"), ymd("2020-12-31"))
plotParamRot(ChlRot, "Chla", ymd("2009-01-01"), ymd("2020-12-31"))

#x11()
Fig2 <- RotPlotSecchi_m[[1]] + ggtitle('Secchi (m)') + theme(plot.title = element_text(size=10)) + 
        RotPlotTSS_mgL[[1]] + ggtitle(expression(paste('TSS (mg m'^{-3},')'))) + theme(plot.title = element_text(size=10)) + 
        RotPlotChla[[1]] + ggtitle(expression(paste('log'[10],' Chlorophyll (mg m'^{-3},')'))) + theme(plot.title = element_text(size=10)) + 
        RotPlotSecchi_m[[2]] + RotPlotTSS_mgL[[2]] + RotPlotChla[[2]] + 
        myplots_Secchi[[4]] + myplots_TSS[[4]] + myplots_chl[[4]]
Fig2
ggsave("Figures/Figure2.png", Fig2)
