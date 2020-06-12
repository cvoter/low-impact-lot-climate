#' define_storms.R
#' 
#' Separates hourly precipitation series into discrete storms
#' 
#' This function takes an hourly precipitation time series (mm/hr) and 
#' separates it into discrete storms based. Storms are defined as separated 
#' from the previous storm by some minimum number of hours (default: 6hrs) with
#' a minimum precipitation depth (default: 2.5mm).
#' 
#' INPUTS
#' precipitation:           vector with hourly precipitation (mm/hr)
#' interval.between.storms: minimum number of dry hours between storms
#' minimum.storm.depth:     minimum precip depth that counts as a storm
#' 
#' OUTPUTS
#' storms: a data frame with the following columns:
#'     --> storm.index:      storm id number for given precipitation time series
#'     --> antecedent.hours: dry hours since previous storm
#'     --> start.hour:       start hour of storm
#'     --> end.hour:         end hour of storm
#'     --> duration:         total hours in storm
#'     --> wet.hours:        total hours with precipitation > 0 in storm
#'     --> depth:            total depth of storm (mm)
#'     --> intensity.avg:    average intensity during wet hours of storm (mm/hr)
#'     --> intensity.max:    peak 1hr intensity (mm/hr) during storm

define_storms <- function(precipitation, 
                          interval.between.storms = 6,
                          minimum.storm.depth = 2.5) {
  storms              <- NULL
  storm.on            <- 0
  storm.intensity.max <- 0
  storm.index         <- 0
  
  for (i in 1:length(precipitation)) {
    if (i < interval.between.storms) {
      storm.window <- i - 1 # first few hours
    } else {
      storm.window <- interval.between.storms - 1 # all other hours
    }
    
    if (precipitation[i] > 0) {
      #RAIN!!
      # Storm is off, turn it on if minimum storm depth exceeded
      if (storm.on == 0) {
        if (sum(precipitation[(i-storm.window):i]) >= minimum.storm.depth) {
          # Turn on, make note of start
          storm.on          <- 1
          storm.index       <- storm.index + 1
          window.precip     <- precipitation[(i-storm.window):i]
          time.rain.started <- min(which(window.precip > 0))
          storm.start       <- i - interval.between.storms + time.rain.started
          
          # Account for rain during previous window hours
          wet.hours <- length(which(precipitation[(i-storm.window):(i-1)] > 0))
          storm.depth <- sum(precipitation[(i-storm.window):(i-1)])
          
          # Note antecedent dry weather hours before this storm
          if (storm.index == 1) {
            storm.antecedent.hours <- storm.start - 1 # first storm
          } else {
            storm.antecedent.hours <- storm.start - storm.end - 1 # others
          }
        }
      }
      
      # Storm is on, update storm parameters
      if (storm.on == 1) {
        wet.hours           <- wet.hours + 1
        storm.depth         <- storm.depth + precipitation[i]
        storm.intensity.max <- max(storm.intensity.max, precipitation[i])
      }
      
    } else { 
      # NO RAIN!!
      # Storm is on, turn it off if interval between storms exceeded
      if (storm.on == 1) {
        if (sum(precipitation[(i-storm.window):i]) < minimum.storm.depth) {
          # record final storm metrics
          storm.on            <- 0
          last.storm.precip   <- precipitation[(i-interval.between.storms):i]
          time.rain.ended     <- max(which(last.storm.precip > 0))
          storm.end           <- i-1 - interval.between.storms +time.rain.ended
          storm.duration      <- storm.end - storm.start + 1
          storm.intensity.avg <- round(storm.depth/wet.hours,1)
          this.storm          <- c(storm.index, 
                                   storm.antecedent.hours,
                                   storm.start, 
                                   storm.end, 
                                   storm.duration, 
                                   wet.hours, 
                                   storm.depth, 
                                   storm.intensity.avg, 
                                   storm.intensity.max)
          storms              <- as.data.frame(rbind(storms,this.storm))
          storm.intensity.max <- 0
        }
      }
    } #end rain check
  } #end time series loop
  colnames(storms) <- c("storm.index", 
                       "antecedent.hours", 
                       "start.hour", 
                       "end.hour", 
                       "duration", 
                       "wet.hours", 
                       "depth", 
                       "intensity.avg", 
                       "intensity.max")
  return(storms)
}