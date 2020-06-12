#' calculate_corr_PtoET0.R
#' 
#' Calculate the correlation between precipitation and reference
#' evapotranspiration (ET0) for a given window or list of window. Positive
#' correlation indicates water availability corresponds with energy availability
#' (e.g., summer thunderstorms). Negative correlation indicates water
#' availability and energy availability are inversely related (e.g., winter
#' rainy season).
#' 
#' This function calculates the moving sum of precipitation and ET0 for a given
#' window or list of windows (e.g., 7 day, 30 day), then calculates the Spearman
#' correlation between the two moving sums. It returns a data frame that
#' contains the city indices and all correlation values for the desired set of
#' moving windows.
#' 
#' REQUIRES
#' calculate_moving_sum (function)
#' 
#' INPUTS
#' hourly.precipitation.ET0: a data frame with the following columns:
#'     --> hour:          time since start of timeseries (hour)
#'     --> city.index:    unique id for city
#'     --> precipitation: precipitation (mm/hr)
#'     --> ET0:           reference evapotranspiration (mm/hr)
#' moving.sum.days: a vector with all moving windows to evaluate (days)
#' 
#' OUTPUTS
#' correlations.PtoET0: a data frame with the following columns:
#'     --> one or more columns named in the format "corr.PtoET0.%dday" 
#'         corresponding to the number moving.sum.days given as an input. 

calculate_corr_PtoET0 <- function(hourly.met, moving.sum.days){
  # Caclulate correlations for each window
  moving.sum.hours    <- 24*moving.sum.days
  correlations.PtoET0 <- NULL
  for (interval in moving.sum.hours) {
    moving.precipitation <- moving_sum(hourly.met$precipitation, interval)
    moving.ET0           <- moving_sum(hourly.met$ET0, interval)
    correlation          <- cor(x = moving.precipitation,
                                y = moving.ET0, 
                                method = "spearman")
    correlations.PtoET0  <- as.data.frame(cbind(correlations.PtoET0, 
                                                correlation))
  }
  
  # Convert moving.sum.days into columnames
  correlation.names      <- NULL
  for (i in 1:length(moving.sum.days)) {
    day                  <- moving.sum.days[i]
    correlation.names[i] <- sprintf("corr.PtoET0.%dday",day)
  }
  colnames(correlations.PtoET0) <- correlation.names
  
  return(correlations.PtoET0)
}