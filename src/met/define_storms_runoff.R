#' define_storms_runoff
#'
#' Extract runoff from baseline and low impact models for each storm
#' 
#' For given set of storms, hourly runoff from baseline scenarios, and hourly
#' runoff from low impact scenarios, this function adds total storm runoff
#' to other storm metrics. Assumes each entry in runoff data corresponds to 
#' hours in storm data. All depths in mm.
#' 
#' INPUTS
#' storms: data frame with the following:
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
#' nstorms:           number of storms in input data frame
#' runoff.baseline:   vector with baseline hourly runoff (mm)
#' runoff.low.impact: vector with low impact hourly runoff (mm)
#' 
#' OUTPUTS
#' storms: same storms data frame, with the following columns added:
#'     --> runoff.baseline:   storm runoff (mm) from baseline model scenario
#'     --> runoff.low.impact: storm runoff (mm) from low impact model scenario
#'     --> runoff.delta:      difference in storm runoff (baseline - low impact)

define_storms_runoff <- function(storms,
                                 nstorms,
                                 runoff.baseline,
                                 runoff.low.impact){
  for (s in 1:nstorms) {
    # Storm start/end
    start.hour <- storms$start.hour[s]
    end.hour   <- storms$end.hour[s]
    
    # Baseline runoff
    after.storm               <- tail(runoff.baseline, -end.hour+1)
    end.runoff                <- min(which(after.storm == 0)) + end.hour - 1
    storm.runoff              <- runoff.baseline[start.hour:end.runoff]
    storms$runoff.baseline[s] <- sum(storm.runoff)
    
    # Low impact runoff
    after.storm                 <- tail(runoff.low.impact, -end.hour+1)
    end.runoff                  <- min(which(after.storm == 0)) + end.hour - 1
    storm.runoff                <- runoff.low.impact[start.hour:end.runoff]
    storms$runoff.low.impact[s] <- sum(storm.runoff)
    
    # Runoff difference
    storms$runoff.delta[s] <- storms$runoff.baseline[s] - 
                              storms$runoff.low.impact[s]
  }
  return(storms)
}
  