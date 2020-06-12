#' calculate_moving_sum.R
#' 
#' Calculate the moving sum of a timeseries for a given moving window.
#' 
#' 
#' INPUTS
#' time.series: vector of values to summarize
#' interval:    length of moving window to use, must have same units as the time 
#'              step of time.series
#' 
#' OUTPUTS
#' moving.series: a vector with length equal to the input time.series minus the
#'                interval representing the moving sum of values over the given
#'                interval.

moving_sum <- function(time.series, interval){
  length.moving.series <- length(time.series) - interval
  moving.series        <- rep(0, length.moving.series)
  for (i in 1:length.moving.series) {
    moving.series[i]   <- sum(time.series[i:(i+interval-1)])
  }
  return(moving.series)
}