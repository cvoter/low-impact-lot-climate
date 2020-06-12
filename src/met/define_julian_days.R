#' define_julian_days.R
#' 
#' Generate vector of julian day numbers
#' 
#' The final julian day vector contains the julian day for every hour in a time
#' series with the given starting date (day, month, year) and total number of
#' years. This function assumes the starting hour is "1" and that the time
#' series represents a complete year or multiple complete years.
#' 
#' REQUIRES
#' lubridate (package)
#' 
#' INPUTS
#' nyears:      number of years in time series
#' start.year:  year of first record in time series
#' start.month: month of first record in time series, default: 10
#' start.day:   day of first record in time series, default: 1
#' 
#' OUTPUTS
#' julian.day:  vector with the julian day of every hour in time series

define_julian_days <- function(nyears, 
                               start.year, 
                               start.month = 10, 
                               start.day = 1){
  # Julian days by day
  library(lubridate)
  start.string <- sprintf("%4d-%02d-%02d",
                          start.year,
                          start.month,
                          start.day)
  start.date  <- as.Date(start.string)
  end.date    <- start.date - days(1) + years(nyears)
  date.vector <- seq.Date(from = start.date, 
                          to = end.date, 
                          by = "days")
  Jday.vector <- yday(date.vector)
  
  # Julian days by hour
  julian.day   <- NULL
  for (d in 1:length(Jday.vector)) {
    this.day   <- rep(Jday.vector[d], 24)
    julian.day <- c(julian.day, this.day)
  }
  
  return(julian.day)
}