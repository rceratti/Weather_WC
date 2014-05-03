setwd('C:/Users/Rubem/Dropbox/Weather_WC')

source("Functions.R")


# Stations
stationsData <- read.csv2('stations.csv', h = FALSE, stringsAsFactors = FALSE)
stationsData <- stationsData[order(stationsData$V1), ]
rownames(stationsData) <- NULL


hosts <- c('Belo Horizonte', 'Brasilia', 'Cuiaba', 'Curitiba', 'Fortaleza',
           'Manaus', 'Natal', 'Porto Alegre', 'Recife', 'Rio de Janeiro',
           'Salvador', 'Sao Paulo')


indHosts <- pmatch(toupper(hosts), stationsData$V1)
indHosts[c(4, 8, 10)] <- c(49, 118, 60)

cbind(hosts, stationsData$V1[indHosts])  # Looks ok!
hostsData <- data.frame(Hosts = hosts, Code = stationsData$V2[indHosts])
write.csv(hostsData, 'hosts.csv', row.names = FALSE)


stations <- stationsData$V2
indStations <- stations != ""
stations <- stations[indStations]


stationsData0 <- stationsData[indStations, ]
latMin <- as.numeric(substr(stationsData0$V5, 1, 2))/60
lat <- stationsData0$V4 + latMin
lat <- lat * ifelse(substr(stationsData0$V5, 3, 3) == 'N', 1, -1)
lonMin <- as.numeric(substr(stationsData0$V7, 1, 2))/60
lon <- -(stationsData0$V6 + lonMin)

stationsData0$Lat <- lat
stationsData0$Lon <- lon
stationsData0 <- stationsData0[, c('V2', 'Lat', 'Lon')]
names(stationsData0)[1] <- 'Code'

write.csv(stationsData0, 'stations_v2.csv', row.names = FALSE)



# Dates
dates <- sapply(2008:2013, function(x){
  begin <- paste0(x, '-06-12')
  end <- paste0(x, '-07-13')
  paste(seq.Date(as.Date(begin), as.Date(end), "1 day"))
})

dates <- as.vector(dates)



brData <- lapply(stations, weatherBR, dates = dates)


brData0 <- lapply(brData, function(x) {
  if(is.null(nrow(x)))
    return(NULL)
  else
    return(x)
})
brData0 <- do.call(rbind, brData0)

write.csv(brData0, 'weather.csv', row.names = FALSE)