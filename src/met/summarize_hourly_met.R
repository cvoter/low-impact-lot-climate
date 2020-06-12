#' summarize_hourly_met.R
#' 
#' Calculates summary metrics based on a 1-year time series of hourly met
#' (precipitation and ET0).
#' 
#' REQUIRES
#' calculate_intermittency (function)
#' calculate_corr_PtoET0 (function)
#' 
#' INPUTS
#' hourly.precipitation.ET0: a data frame with the following columns:
#'     --> hour:          time since start of timeseries (hour)
#'     --> city.index:    unique id for city
#'     --> precipitation: precipitation (mm/hr)
#'     --> ET0:           reference evapotranspiration (mm/hr)
#' moving.sum.days: a vector with all moving windows to evaluate (days)
#' nlocations:      number of cities in input dataset
#' 
#' OUTPUTS
#' hourly.met.summary: a data frame with the following columns:
#'     --> city.index    : unique id for city
#'     --> precipitation : total annual precipitation (mm)
#'     --> ET0toP        : ratio of total annual ET0 to total annual 
#'                         precipitation
#'     --> logETP        : log10 of ET0toP
#'     --> wet.fraction  : fraction of hours with precipitation
#'     --> burstiness    : a measure of the interamount times between 
#'                         precipitation
#'     --> memory        : autocorrelation of interamount times between 
#'                         precipitation
#'     --> one or more columns named in the format "corr.PtoET0.%dday" 
#'         corresponding to the number moving.sum.days given as an input.

summarize_hourly_met <- function(hourly.precipitation.ET0, 
                                 moving.sum.days,
                                 nlocations){
  hourly.met.summary <- NULL
  for (i in 1:nlocations) {
    hourly.met    <- subset(hourly.precipitation.ET0, city.index==i)
    precipitation <- hourly.met$precipitation
    ET0           <- hourly.met$ET0

    # Metrics
    intermittency         <- calculate_intermittency(precipitation)
    annual.precipitation  <- sum(precipitation)
    ET0toP                <- sum(ET0)/annual.precipitation
    logETP                <- log10(ET0toP)
    wet.fraction          <- length(precipitation[which(precipitation > 0)])/
                             length(precipitation)
    correlations          <- calculate_corr_PtoET0(hourly.met, moving.sum.days)
    
    # Append to data frames
    city.metrics <- as.data.frame(c(i, 
                                    annual.precipitation, 
                                    ET0toP, 
                                    logETP, 
                                    wet.fraction, 
                                    intermittency$burstiness, 
                                    intermittency$memory,
                                    correlations))
    colnames(city.metrics) <- c("city.index", 
                                "precipitation", 
                                "ET0toP", 
                                "logETP", 
                                "wet.fraction", 
                                "burstiness",
                                "memory",
                                colnames(correlations))
    hourly.met.summary      <- as.data.frame(rbind(hourly.met.summary, 
                                                   city.metrics))
  }
  rownames(hourly.met.summary) <- NULL
  return(hourly.met.summary)
}