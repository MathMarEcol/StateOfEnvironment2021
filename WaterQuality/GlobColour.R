## download files from globcolour ftp request

#url<- "ftp://ftp.hermes.acri.fr/374557225/" # MODIS 2011 - 2020 for ZSD
url <- "ftp://ftp.hermes.acri.fr/967413973/" # MODIS 2011 - 2015 for ZSD
userpwd <- "ftp_hermes:hermes%"
destination <- "~/Documents"
filenames <- getURL(url, userpwd="ftp_hermes:hermes%", 
                    ftp.use.epsv = FALSE, dirlistonly = TRUE)
files <- unlist(strsplit(filenames, '\r\n'))

h <- new_handle()
handle_setopt(h, userpwd = "ftp_hermes:hermes%")

lapply(files, function(filename){
  curl_download(paste(url, filename, sep = ""), destfile = filename, handle = h)
})

source("functions.R")

#### looking at trend in bioregions

zsdStack <- fStackSecchi() # for Secchi
TSSStack <- fStackTSS() # for TSS

bioregion <- readOGR("C:/Users/dav649/Documents/GitHub/PlanktonTrendAnalysis/marine_regions_2012/shape")
mb <- c("North", "Temperate East", "North-west", "South-west", "South-east", "Coral Sea")

bioCS <- readRDS("C:/Users/dav649/Documents/GitHub/PlanktonTrendAnalysis/output/Bioregion_Sat.rds")

myplots_chl <- list()
for (i in 1:length(mb)){
  
  temp <- bioCS %>% 
    filter(Bioregion == mb[i])
  myplots_chl[[i]] <- fTrendAnalysis(temp, "logMeanChl", "Date", "HarmDOY")
  myplots_chl[[i]] <- myplots_chl[[i]] + labs(subtitle = mb[i]) + theme(plot.subtitle = element_text(size=8))
}

bioregionTrend(zsdStack, "Secchi", bioregion)
bioregionTrend(TSSStack, "TSS", bioregion)

Fig1 <- myplots_Secchi[[3]] + ggtitle('Secchi (m)') + theme(plot.title = element_text(size=10)) + 
  myplots_TSS[[3]] + ggtitle(expression(paste('TSS (mg m'^{-3},')'))) + theme(plot.title = element_text(size=10)) + 
  myplots_chl[[3]] + ggtitle(expression(paste('log'[10],' Chlorophyll (mg m'^{-3},')'))) + theme(plot.title = element_text(size=10)) + 
  myplots_Secchi[[1]] + myplots_TSS[[1]] + myplots_chl[[1]] +
  myplots_Secchi[[6]] + myplots_TSS[[6]] + myplots_chl[[6]] +
  myplots_Secchi[[2]] + myplots_TSS[[2]] + myplots_chl[[2]] +
  myplots_Secchi[[5]] + myplots_TSS[[5]] + myplots_chl[[5]] + 
  myplots_Secchi[[4]] + myplots_TSS[[4]] + myplots_chl[[4]] +
  plot_layout(ncol = 3) 
Fig1
ggsave("Figure1.png", Fig1)

expression(paste("log"[10],"(mg m"^{-3},")"))
### SECCHI
## create 5 year mean for previous years
fileyearAll <- list.files("~/", "L3m_")
YearBrickListAll <- sapply(fileyearAll, raster)
YearBrickAll <- brick(YearBrickListAll)
meanAll <- stackApply(YearBrickAll, indices = rep(1,nlayers(YearBrickAll)), fun = "mean", na.rm = TRUE)
rm(YearBrickAll, YearBrickListAll)

## Actual plot
plotlist <- list()
years <- as.factor(seq(2016, 2020, 1))

for (i in 1:nlevels(years)){
  year = years[[i]] %>% droplevels()
  fileyear <- list.files("~/", paste0("L3m_", year))
  YearBrickList <- sapply(fileyear, raster)
  YearBrick <- brick(YearBrickList)
  #mean <- calc(YearBrick, fun = mean, na.rm = T)
  mean <- stackApply(YearBrick, indices = rep(1,nlayers(YearBrick)), fun = "mean", na.rm = TRUE)
  ras_df <- raster::as.data.frame(mean, xy = TRUE) 
  aus <- ne_countries(country = "Australia", scale = "medium", returnclass = "sf")
  cols <- c("#0000FF", "#007FFF", "#00FFFF", "#7FFF7F", "#FFFF00", "#FF7F00", "#FF0000", "#7F0000")
  
  gg <-   ggplot() + geom_tile(data = ras_df, aes(x=x, y=y, fill = index_1)) +
    #geom_sf(data = aus, fill = 'grey', size = .2, inherit.aes=FALSE) +
    #scale_fill_distiller(palette = "Spectral", name = "Secchi Depth m") +
    scale_fill_gradientn(colours = cols, name = "Secchi Depth m", na.value = "grey") +
    scale_x_continuous(limits = c(105, 160), expand = c(0,0)) +
    scale_y_continuous(limits = c(-45, -10), expand = c(0,0)) +
    labs(x = "", y = "", title = year) 
  
  plotlist[[i]] <- gg
}

## aligning colour guides so can collect guides
    maxdf <- data.frame(maxi = NA)
    mindf <- data.frame(mini = NA)
    
    for (i in 1:length(plotlist)){
      maxi <- max(summary(plotlist[[i]][["layers"]][[1]][["data"]][["index_1"]])[["Max."]])
      mini <- min(summary(plotlist[[i]][["layers"]][[1]][["data"]][["index_1"]])[["Min."]])
      maxdf <- rbind(maxdf, maxi)
      mindf <- rbind(mindf, mini)
    }
    maxv <- max(maxdf$maxi, na.rm = TRUE)
    minv <- min(mindf$mini, na.rm = TRUE)
    
    for (i in 1:length(plotlist)){
      plotlist[[i]][["layers"]][[1]][["data"]][["index_1"]][[449]] <- maxv
      plotlist[[i]][["layers"]][[1]][["data"]][["index_1"]][[450]] <- minv
    }

x11(width=10, height = 5)
p <- plotlist[[1]] + plotlist[[2]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] +
  guide_area() +
  plot_layout(guides = 'collect')   
p
ggsave("Secchi.png", p)

## Anomaly plot
plotlist <- list()
years <- as.factor(seq(2016, 2020, 1))

for (i in 1:nlevels(years)){
  year = years[[i]] %>% droplevels()
  fileyear <- list.files("~/", paste0("L3m_", year))
  YearBrickList <- sapply(fileyear, raster)
  YearBrick <- brick(YearBrickList)
  #mean <- calc(YearBrick, fun = mean, na.rm = T)
  mean <- stackApply(YearBrick, indices = rep(1,nlayers(YearBrick)), fun = "mean", na.rm = TRUE)
  anomaly <- mean - meanAll
  ras_df <- raster::as.data.frame(anomaly, xy = TRUE) 
  aus <- ne_countries(country = "Australia", scale = "medium", returnclass = "sf")
  #cols <- c("#0000FF", "#007FFF", "#00FFFF", "#7FFF7F", "#FFFF00", "#FF7F00", "#FF0000", "#7F0000")
  cols <- c("red", "white", "blue")
  
  gg <-   ggplot() + geom_tile(data = ras_df, aes(x=x, y=y, fill = layer)) +
    #geom_sf(data = aus, fill = 'grey', size = .2, inherit.aes=FALSE) +
    #scale_fill_distiller(palette = "Spectral", name = "Secchi Depth m") +
    scale_fill_gradientn(colours = cols, name = "Secchi Depth Anomaly m", na.value = "grey") +
    scale_x_continuous(limits = c(105, 160), expand = c(0,0)) +
    scale_y_continuous(limits = c(-45, -10), expand = c(0,0)) +
    labs(x = "", y = "", title = year) 
  
  plotlist[[i]] <- gg
}

maxdf <- data.frame(maxi = NA)
mindf <- data.frame(mini = NA)

for (i in 1:length(plotlist)){
  maxi <- max(summary(plotlist[[i]][["layers"]][[1]][["data"]][["layer"]])[["Max."]])
  mini <- min(summary(plotlist[[i]][["layers"]][[1]][["data"]][["layer"]])[["Min."]])
  maxdf <- rbind(maxdf, maxi)
  mindf <- rbind(mindf, mini)
}
maxv <- max(maxdf$maxi, na.rm = TRUE)
minv <- min(mindf$mini, na.rm = TRUE)

for (i in 1:length(plotlist)){
  plotlist[[i]][["layers"]][[1]][["data"]][["layer"]][[449]] <- maxv
  plotlist[[i]][["layers"]][[1]][["data"]][["layer"]][[450]] <- minv
}

x11(width=10, height = 5)
p <- plotlist[[1]] + plotlist[[2]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] +
  guide_area() +
  plot_layout(guides = 'collect')   
p
ggsave("Secchi_anomaly.png", p)







