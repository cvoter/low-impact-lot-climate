#' obtain_hourly_met.R
#' 
#' Calculate hourly precipitation and reference ET (ET0) for multiple cities.
#' Assumes timeseries start at 00:00 UTC on the given start day, month, and year.
#' 
#' This function performs the following steps:
#' 1. Loops through all locations in input list
#' 2. Finds and reads in NLDAS met data (of any length) for that location
#' 3. Performs unit conversions and extracts required inputs for FAO ET0
#' 4. Calls the FAO Penman Montheith function to calculate ET0
#' 4. Stores hourly precipitation and ET0 for each city in a data frame
#' 
#' REQUIRES
#' calculate_ET0 (function)
#' convert_nldas_units (function)
#' define_julian_days (function)
#' 
#' INPUTS
#' city.locations: data frame w/list of cities and location info, cols include: 
#'     --> city.index:    unique id for city (integer)
#'     --> city.name:     city's name (factor)
#'     --> state:         city's state (factor)
#'     --> latitude.deg:  latitude of city in decimal degrees (numeric)
#'     --> longitude.deg: longitude of city in decimal degrees (numeric)
#'     --> elevation.m:   elevation of city in meters (integer)
#' met.dir:      directory with NLDAS2 met data for each location
#' met.filename: filename for NLDAS2 data, default: "nldas.1hr.clm.txt"
#' nyears:       number of years in time series
#' start.year:   year of first record in time series
#' start.month:  month of first record in time series, default: 10
#' start.day:    day of first record in time series, default: 1
#' 
#' OUTPUTS
#' hourly.precipitation.ET0: a data frame with the following columns:
#'     --> hour:          time since start of timeseries (hour)
#'     --> city.index:    unique id for city
#'     --> precipitation: precipitation (mm/hr)
#'     --> ET0:           reference evapotranspiration (mm/hr)

obtain_hourly_met <- function(city.locations, 
                              met.dir,
                              met.filename = "nldas.1hr.clm.txt",
                              nyears, 
                              start.year, 
                              start.month = 10, 
                              start.day = 1){
  # Initialize
  nlocations               <- length(city.locations$city.index)
  hourly.precipitation.ET0 <- NULL
  ET0.calc                 <- NULL
  for (loc in 1:nlocations) {
    # Load city & met data
    city.index         <- city.locations$city.index[loc]
    location.dir       <- sprintf("%s/loc%02d", met.dir, city.index)
    met.filepath       <- sprintf("%s/%s", location.dir, met.filename)
    met.data           <- read.table(met.filepath, header = FALSE)
    colnames(met.data) <- c("down.short.radiation", 
                            "down.long.radiation", 
                            "precipitation", 
                            "temperature.K", 
                            "wind.EW", 
                            "wind.NS", 
                            "pressure", 
                            "specific.humidity")
    
    # Convert units on met data
    met.data <- convert_nldas_units(met.data)
    
    # Extract location parameters
    elevation             <- city.locations$elevation.m[loc]
    latitude.degrees      <- city.locations$latitude.deg[loc]
    longitude.measurement <- -city.locations$longitude.deg[loc]
    
    # Get timing information (assumes start at 00:00 UTC)
    julian.day <- define_julian_days(nyears, start.year, start.month, start.day)
    nhours     <- length(julian.day)
    
    # Extract met parameters (may truncate met data, but must start with first
    # time step as written to met.data source file)
    met.pressure  <- met.data$pressure[1:nhours]
    wind.measured <- met.data$wind.measured[1:nhours]
    temperature.K <- met.data$temperature.K[1:nhours]
    Rs            <- met.data$down.short.radiation[1:nhours]
    humidity      <- met.data$specific.humidity[1:nhours]
    precipitation <- met.data$precipitation[1:nhours]
    
    # Get ET0
    ET0.calc <- calculate_ET0(elevation, 
                              latitude.degrees, 
                              longitude.timezone = 0,
                              longitude.measurement, 
                              julian.day, 
                              met.pressure, 
                              wind.measured, 
                              temperature.K, 
                              Rs, 
                              humidity, 
                              nhours)
    
    # Add hourly precip & ET0 for this city to overall data frame
    hours                    <- c(1:nhours)
    this.city                <- rep(city.index, nhours)
    this.city.data           <- as.data.frame(cbind(hours, 
                                                    this.city, 
                                                    precipitation, 
                                                    ET0.calc))
    hourly.precipitation.ET0 <- as.data.frame(rbind(hourly.precipitation.ET0,
                                                    this.city.data))
    
    # Reset for next city
    ET0.calc <- NULL
  }
  
  # Final data frame
  colnames(hourly.precipitation.ET0) <- c("hour",
                                          "city.index",
                                          "precipitation",
                                          "ET0")
  return(hourly.precipitation.ET0)
}