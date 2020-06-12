#' calculate_intermittency.R
#' 
#' Calculates two measures of rainfall intermittency: burstiness and memory.
#' 
#' The burstiness [-1, 1] of precipitation is a measure of the interamount times
#' between precipitation. A steady precipitation pattern with equal interamount
#' times has a burstiness of 1. A poisson process has a burstiness of 0. A
#' standard lognormal distribution has a burstiness of 0.135. The longer and
#' fatter the right tail of the interamount time distribution, the higher the
#' burstiness value.
#' 
#' The memory [-1, 1] of precipitation is a measure of the autocorrelation of
#' interamount times. Positive memory means that short interamount times tend to
#' be followed by short ones and long ones by long ones. Negative memory means
#' that short interamount times tend to be followed by long ones. A Poisson
#' process has a memory of zero. Large values indicate "patchy" precipitation
#' and can also be a sign of daily or seasonal cycles, depending on the time
#' scale at which they are computed.
#' 
#' REFERENCES
#' Schleiss, M., & Smith, J. A. (2015). Two Simple Metrics for Quantifying
#' Rainfall Intermittency: The Burstiness and Memory of Interamount Times.
#' Journal of Hydrometeorology, 17(1), 421–436.
#' https://doi.org/10.1175/JHM-D-15-0078.1
#' 
#' INPUTS
#' precipitation:         vector with hourly precipitation (mm)
#' interamount.time.mean: fixed interamount time for comparison, default: 24hr
#' measurement.minimum:   minimum depth of rain (mm) per hour to include in
#'                        analysis
#' 
#' OUTPUTS
#' A list with two variables: "burstiness" and "memory"

calculate_intermittency <- function(precipitation, 
                                    interamount.time.mean = 24, 
                                    measurement.minimum = 0.1) {
  
  # MEAN INTERAMOUNT DEPTH AND TIME --------------------------------------------
  total.precipitation       <- sum(precipitation)
  nhours                    <- length(precipitation)
  interamount.depth.mean    <- interamount.time.mean*total.precipitation/nhours
  interamount.depth.minimum <- interamount.time.mean*measurement.minimum
  interamount.time.minimum  <- 10
  
  # WARNINGS -------------------------------------------------------------------
  # # Check reasonableness of mean interamount (mm)
  # if ( interamount.depth.mean < interamount.depth.minimum ) {
  #   warning (sprintf('calculated mean interamount, %0.2f, is less than minimum 
  #                    interamount, %0.2f',
  #                    interamount.depth.mean, interamount.depth.minimum))
  # }
  # # Check reasonableness of mean interamount time (hr)
  # if ( interamount.time.mean < interamount.time.minimum ) {
  #   warning (sprintf('defined mean interamount time, %d, is less than 
  #                    minimum interamount time, %d',
  #                    interamount.time.mean, interamount.time.minimum))
  # } else if ( interamount.time.mean > nhours/100 ) {
  #   warning (sprintf('defined mean interamount time, %d, is greater than 
  #                    maximum interamount time, %0.0f',interamount.time.mean,
  #                    nhours/100))
  # }
  
  # CALCULATE ACTUAL INTERAMOUNT TIMES -----------------------------------------
  last.match.time             <- 0
  running.total.precipitation <- 0
  interamount.time            <- 0
  j                           <- 1
  for (i in 1:length(precipitation)) {
    running.total.precipitation    <- running.total.precipitation + 
                                      precipitation[i]
    if (running.total.precipitation >= interamount.depth.mean ){
      number.matches                <- floor(running.total.precipitation/
                                               interamount.depth.mean)
      for (m in 1:number.matches) {
        j                           <- j + 1
        this.match.time             <- (i - 1) + m/number.matches
        interamount.time[j]         <- this.match.time - last.match.time
        last.match.time             <- this.match.time
        running.total.precipitation <- running.total.precipitation - 
                                       interamount.depth.mean
      }
    }
  }
  
  # CALCULATE BURSTINESS AND MEMORY --------------------------------------------
  interamount.time.std <- sd(interamount.time)
  burstiness           <- (interamount.time.std - interamount.time.mean)/
                          (interamount.time.std + interamount.time.mean)
  memory <- cor(x = interamount.time[1:(length(interamount.time)-1)], 
                y = interamount.time[2:length(interamount.time)], 
                method = "spearman")
  
  return(list(burstiness=burstiness, memory=memory))
}