## download files from globcolour ftp request

#url<- "ftp://ftp.hermes.acri.fr/400385808/" # MODIS 2016 - 2020 for ZSD
#url <- "ftp://ftp.hermes.acri.fr/576122810/" # AVVIR
#url <- "ftp://ftp.hermes.acri.fr/968836016/" # MODIS 2011 - 2015 for ZSD
url <- "ftp://ftp.hermes.acri.fr/348532863/" # MODIS 2015 - 2020 for TSS
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

bioregionTrend(zsdStack, bioregion)
bioregionTrend(TSSStack, bioregion)


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







