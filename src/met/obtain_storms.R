#' obtain_storms.R
#' 
#' Get discrete storms from hourly precipitation data for multiple cities
#' 
#' Loops through a given number of cities to extract storm information from
#' hourly precipitation data. Assumes the number of cities corresponds with
#' the indicies of cities.
#' 
#' REQUIRES
#' define_storms (function)
#' 
#' INPUTS
#' hourly.precipitation.ET0: data frame with hourly info for each city
#'     --> hour:          time since start of timeseries (hour)
#'     --> city.index:    unique id for city
#'     --> precipitation: precipitation (mm/hr)
#'     --> ET0:           reference evapotranspiration (mm/hr)
#' city.locations: data frame with list of cities and location info 
#'     --> city.index:    unique id for city (integer)
#'     --> city.name:     city's name (factor)
#'     --> state:         city's state (factor)
#'     --> latitude.deg:  latitude of city in decimal degrees (numeric)
#'     --> longitude.deg: longitude of city in decimal degrees (numeric)
#'     --> elevation.m:   elevation of city in meters (integer)
#' nlocations: number of cities in input dataset
#' 
#' OUTPUTS
#' all.city.storms: a data frame with the following columns:
#'     --> city.index:       unique id for city
#'     --> storm.index:      storm id number for given precipitation time series
#'     --> antecedent.hours: dry hours since previous storm
#'     --> start.hour:       start hour of storm
#'     --> end.hour:         end hour of storm
#'     --> duration:         total hours in storm
#'     --> wet.hours:        total hours with precipitation > 0 in storm
#'     --> depth:            total depth of storm (mm)
#'     --> intensity.avg:    average intensity during wet hours of storm (mm/hr)
#'     --> intensity.max:    peak 1hr intensity (mm/hr) during storm

obtain_storms <- function(hourly.precipitation.ET0, 
                       city.locations,
                       nlocations) {
  all.city.storms   <- NULL
  for (i in 1:nlocations) {
    index           <- city.locations$city.index[i]
    hourly.met      <- subset(hourly.precipitation.ET0, city.index==index)
    storms          <- define_storms(hourly.met$precipitation)
    nstorms         <- length(storms$storm.index)
    city.index      <- rep(index, nstorms)
    city.storms     <- as.data.frame(cbind(city.index, storms))
    all.city.storms <- as.data.frame(rbind(all.city.storms, city.storms))
    
    rownames(all.city.storms) <- NULL
  }
  return(all.city.storms)
}