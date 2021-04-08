suppressPackageStartupMessages({
  library(RCurl)
  library(curl)
  library(rnaturalearth)
  library(rgdal)
  library(ncdf4)
  library(lubridate)
  library(patchwork)
  library(raster)
  library(broom)
  library(tidyverse)
})

# Cyclic function for Month, to ensure Jan follows Dec
Harm <- function (theta, k = 4) {
  X <- matrix(0, length(theta), 2 * k)
  nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
  dimnames(X) <- list(names(theta), nam)
  m <- 0
  for (j in 1:k) {
    X[, (m <- m + 1)] <- cos(j * theta)
    X[, (m <- m + 1)] <- sin(j * theta)
  }
  X
}

# plotting function
plotParam <-  function(data, parameter, start, end){

  slopes <- data.frame(Station = NA, slope = NA, pval = NA)
    for (i in 1:nlevels(data$Station)){
      station <- levels(data$Station)[[i]]
      dat <-  data %>% filter(Station == station)
      form <- as.formula(paste(parameter, "~ Year + Harm(MonR, 2)"))
      m1 <- lm(form, data = dat)
      tmdl <- tidy(m1)
      slope <- tmdl$estimate[tmdl$term=="Year"]
      pval <- anova(m1)$'Pr(>F)'[1]
      df <- data.frame(Station = station, slope = slope, pval = pval)
      slopes <- slopes %>% rbind(df) %>% drop_na()
    }
    
    slopes <- slopes %>% mutate(clr = ifelse(slope>0 & pval <= 0.05, 'red',
                                                 ifelse(slope<0 & pval <= 0.05, 'blue', 'black')))
    
    clr <- slopes$clr
    
    y <- unlist(data %>% select(!!as.symbol(parameter)) %>% as.list())
    gg <- ggplot(data) +
      geom_point(aes(Date, y), size = 1) +
      stat_smooth(aes(Date, y, colour = Station), method = "lm") +
      facet_grid(Station~., scales = "free") + 
      theme_bw() + theme(legend.position = "none") +
      #scale_y_continuous(expand = expansion(mult = 0.2)) +
      scale_x_date(limits = c(start, end), expand = expansion(mult = 0), 
                   breaks = seq(start, end,"2 years"), labels = date_format("%Y")) +
      scale_color_manual(values = clr) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    gg
    
    Year = year(start)

    funclist <- list("gg" = gg)
    names(funclist) <- paste("gg", parameter, Year, sep = '')
    list2env(funclist, .GlobalEnv)
    
  }


#Stack GlobColour data
fStackTSS <- function(){
  rlist <- as.list(sort(list.files("C:/Users/dav649/Documents/GitHub/StateOfEnvironment2021/WaterQuality/GlobCol", "OLA_TSM", full.names = TRUE)))
  TSSStack <- stack(rlist)
  names(TSSStack) <- seq(ymd('2016-04-01'), ymd('2020-12-01'), by = 'months')
}
fStackSecchi <- function(){
  rlist <- as.list(sort(list.files("C:/Users/dav649/Documents/GitHub/StateOfEnvironment2021/WaterQuality/GlobCol", "MOD_ZSD", full.names = TRUE)))
  zsdtack <- stack(rlist)
  names(zsdStack) <- seq(ymd('2011-01-01'), ymd('2020-12-01'), by = 'months')
}

# function for model
fTrendAnalysis <- function(dat, y, time, season){
  
  library(tidymodels)
  pt_size <- 1
  t_size <- 3
  
  # Do Model
  fm <- as.formula(paste(y, "~", time, "+ Harm(", season, ", k = 1)"))
  # y ~ time + fHarmonic(season, k= 1)
  
  mdl <- lm(fm, data = dat)
  pval <- anova(mdl)$'Pr(>F)'[1]
  tmdl <- tidy(mdl)
  slope <- tmdl$estimate[tmdl$term==time] # Save the model slope
  
  print(paste0("Slope = ",(slope)))
  print(paste0("p = ",round(pval, 4)))
  
  # ptemp <- predict(pmdl)
  # ppred <- ptemp[c(1,length(ptemp))]
  # p_Abun_decade_change[i] <- ((diff(10^ppred))/pdata_length)*decade_length
  # p_AbunPerc_decade_change[i] <- p_Abun_decade_change[i]/(10^ppred[1])
  # 
  if (slope>0 & pval <= 0.05){
    clr <- "red"
  } else if (slope<0 & pval <= 0.05){
    clr <- "blue"
  } else {
    clr <- "black"
  }
  
  gg <- ggplot() +
    geom_point(data = dat, aes_string(x=time, y=y), size = pt_size) +
    stat_smooth(data = mdl, method = "lm", col = clr, fill = clr, 
                aes_string(x = time, y = y)) +
    theme_bw() + 
    scale_y_continuous(expand = expansion(mult = 0)) +
    #annotate("text", x = ymd("2008-12-15"), y = Inf, label = site_names[counter], hjust = 0, vjust = 1, size = t_size) + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
  
  return(gg)
}

bioregionTrend <- function(Stack, bioregion) {
      mb <- c("North", "Temperate East", "North-west", "South-west", "South-east", "Coral Sea")
      
      sat <- tibble(Bioregion = character(), MeanVar = vector(), Date = date())
      sat$Date = date(sat$Date) # I don't why I need this but it works....
      
      for (i in 1:length(mb)){
        print(mb[i])
        sm <- mask(Stack, bioregion[bioregion$REGION == mb[i],])
        
        temp <- tibble(Bioregion = mb[i], 
                       MeanVar = raster::cellStats(sm, mean, na.rm = TRUE),
                       Date = ymd(str_replace(names(sm), "X", ""))) %>%  
          mutate(Month = month(Date),
                 DOY = yday(Date),
                 HarmDOY = (DOY/365)*2*pi) # Convert to radians)
        
        sat <- bind_rows(sat, temp)
        
        rm(temp, sm)
      }


    myplots <- list()
    for (i in 1:length(mb)){
      
      temp <- sat %>% 
        filter(Bioregion == mb[i])
      myplots[[i]] <- fTrendAnalysis(temp, "MeanVar", "Date", "HarmDOY")
      myplots[[i]] <- myplots[[i]] + ggtitle(mb[i]) + theme(plot.title = element_text(size=8))
    }
    
    Year = year(start)
    
    funclist <- list("myplots" = myplots)
    names(funclist) <- paste("myplots_", parameter, sep = '')
    list2env(funclist, .GlobalEnv)
    
}


