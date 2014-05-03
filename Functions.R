library(weatherData)




gWD <- function(station, Date) {
  tryCatch(getWeatherData(station, Date, 
                          opt_temperature_only = F),
           error = function(e) NULL) 
}



summarize <- function(x) {
  if(is.character(x))
    x <- as.numeric(x[x != "N/A"])
  x <- x[x > -9999]
  c(mean(x, na.rm = T), min(x, na.rm = T), max(x, na.rm = T))
}



weatherBR <- function(station, dates) {
  x <- lapply(dates, function(Date, station){
    cat('Getting data (', Date, ' -- ', station, ')... ')
    dat <- gWD(station, Date)
    cat('Done.\n')
    
    if(is.null(dat))
      return(NULL)
    else {
      if(!'TemperatureC' %in% names(dat)) 
        dat$TemperatureC <- 5*(dat$TemperatureF-32)/9
      TempC <- summarize(dat$TemperatureC)
      Humidity <- summarize(dat$Humidity)
      dat0 <- data.frame(Date = Date, t(TempC), t(Humidity))
      labs <- c('Avg', 'Min', 'Max')
      names(dat0)[2:7] <- c(paste0(labs, 'TempC'), paste0(labs, 'Humidity'))
      return(dat0)
    }
  }, station = station)
  
  
  x <- do.call(rbind, x)
  x$Station <- station
  return(x)
}
