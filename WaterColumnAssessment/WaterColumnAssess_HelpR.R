fTrendAnalysis <- function(dat, y, time, season){

  library(tidymodels)
  pt_size <- 1
  t_size <- 3

  # Do Model
  fm <- as.formula(paste(y, "~", time, "+ fHarmonic(", season, ", k = 1)"))
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
    scale_y_continuous(expand = expansion(mult = 0.2)) +
    #annotate("text", x = ymd("2008-12-15"), y = Inf, label = site_names[counter], hjust = 0, vjust = 1, size = t_size) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())

  return(gg)
}


# Harmonic is to fit Hour and DOY
fHarmonic <- function (theta, k = 4) {
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
