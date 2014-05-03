library(geoRglm)
library(maptools)
library(RColorBrewer)


base <- '~/Rubem_Ceratti/Outros/Weather_WC'
setwd(base)


br0 <- readShapePoly("Shapefiles/BRASIL.shp")
br1 <- readShapePoly("Shapefiles/BR_Contorno.shp")
stations <- read.csv('stations_v2.csv')
weather <- read.csv('weather.csv')
hosts <- read.csv('hosts.csv')

hosts <- merge(hosts, stations, by = 'Code')



varData <- function(variable, inf, sup) {
  a <- weather[, c('Station', variable)]
  a <- na.omit(a)
  vals <- unlist(a[variable])
  a <- a[vals > inf & vals < sup, ]
  form <- as.formula(paste(variable, '~ Station'))
  a <- aggregate(form, a, mean)

  b <- merge(stations, a, by.x = 'Code', by.y = 'Station')

  as.geodata(b, c('Lon', 'Lat'), variable) #, realisations = 'Code'
}



varKriging <- function(data, locs, pols) {
  kappa <- seq(.1, 2.5, .2)
  sigma2 <- 20
  phi <- 1
  ini.cov <- c(sigma2, phi)

  cv.fun <- function(k, data) {
    tryCatch({
    mod <- likfit(data, ini.cov.pars = ini.cov, fix.nugget = FALSE, kappa = k,
                  cov.model = 'matern')
    xvalid(data, model = mod)
    }, error = function(e) NA)
  }

  cv <- lapply(kappa, cv.fun, data = data)
  cv.meanError <- sapply(cv, function(x) {
    if(is.list(x))
      mean(x$error^2)
    else
      NULL
  })

  kappa.cv <- kappa[which.min(cv.meanError)]


  m1 <- likfit(data, ini.cov.pars = ini.cov, fix.nugget = FALSE, kappa = kappa.cv,
               cov.model = 'matern')

  krige.conv(data, locations = locs, borders = pols[[1]]@coords,
             krige = krige.control(obj.model = m1))
}



x <- seq(bbox(br1)['x', 1], bbox(br1)['x', 2], l = 80)
y <- seq(bbox(br1)['y', 1], bbox(br1)['y', 2], l = 80)
locs <- as.matrix(expand.grid(x = x, y = y))


temp <- list('AvgTempC', -10, 50)
humi <- list('AvgHumidity', 0, 100)
Vars <- list(temp, humi)

pols <- br1@polygons[[1]]@Polygons
krige.list <- vector('list', 2)

for(i in 1:2) {
  d <- Vars[[i]]
  tes <- varData(d[[1]], d[[2]], d[[3]])
  krige.list[[i]] <- varKriging(tes, locs, pols)
}

names(krige.list) <- c('Temperature', 'Humidity')
save(krige.list, file = 'Krige_Maps.RData')



plotKrig <- function(brShape0) {
  for(j in 1:27) {
    pols0 <- brShape0@polygons[[j]]@Polygons
    for(i in 1:length(pols0)) 
      lines(pols0[[i]])
  }
}


mycol <- colorRampPalette(brewer.pal(11, "Spectral"))(50) 
xl <- c(-69, -68)
yl <- c(-35, -25)

par(mfrow = c(1, 2))
image(krige.list$Temperature, col = rev(mycol), xlab = "", ylab = "",
      main = 'Temperature', axes = F)
legend.krige(x.leg = xl, y.leg = yl, krige.list$Temperature$pred, 
             col = rev(mycol), vert = T)
plotKrig(br0)
points(hosts$Lon, hosts$Lat, pch = 17, col = 3)

image(krige.list$Humidity, col = mycol, xlab = "", ylab = "",
      main = 'Humidity', axes = F)
legend.krige(x.leg = xl, y.leg = yl, krige.list$Humidity$pred, 
             col = mycol, vert = T)
plotKrig(br0)
points(hosts$Lon, hosts$Lat, pch = 17, col = 3)