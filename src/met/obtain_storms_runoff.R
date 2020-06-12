#' obtain_storms_runoff.R
#' 
#' Add storm runoff info to storms precip info for multiple cities
#' 
#' Given an input storms data frame for multiple cities, this function 
#' determines total runoff from each storm in both the baseline and low impact 
#' model scenarios, as well as the difference between baseline and low impact 
#' runoff. This script assumes that hourly flux data for each model is saved 
#' underneath the input "results.dir" within a folder for the location 
#' (e.g., loc25/) and in the form loc25_baseline_hourly_balance.Rda or 
#' loc25_low_impact_hourly_balance.Rda. This function loops through all cities 
#' but calls on pair_storms_runoff to actually match up storm and runoff info.
#' 
#' REQUIRES
#' define_storms_runoff (function)
#' 
#' INPUTS
#' all.city.storms: data frame with the following columns:
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
#' nlocations:  number of cities in input dataset
#' results.dir: path to hourly outputs, default: 'results/model_outputs'
#' 
#' OUTPUTS
#' all.city.storms.runoff: all.city.storms with additional columns:
#'     --> runoff.baseline:   storm runoff (mm) from baseline model scenario
#'     --> runoff.low.impact: storm runoff (mm) from low impact model scenario
#'     --> runoff.delta:      difference in storm runoff (baseline - low impact)

obtain_storms_runoff <- function(all.city.storms,
                                nlocations,
                                results.dir = 'results/model_outputs'){
  
  all.city.storms.runoff <- NULL
  for (i in 1:nlocations){
    # Set location
    loc      <- sprintf('loc%02d',i)
    
    # Load storm data
    storms   <- subset(all.city.storms, city.index==i)
    nstorms  <- length(storms$storm.index)
    
    # Load baseline runoff
    filename <- sprintf("%s/%s_baseline/%s_baseline_hourly_balance.Rda",
                        results.dir, 
                        loc, 
                        loc)
    load(filename)
    runoff.baseline  <- hourly.balance$surface.runoff
    
    # Load low-impact runoff
    filename <- sprintf("%s/%s_low_impact/%s_low_impact_hourly_balance.Rda",
                        results.dir, 
                        loc, 
                        loc)
    load(filename)
    runoff.low.impact <- hourly.balance$surface.runoff
    
    # Pair runoff with storms
    storms.runoff <- pair_storms_runoff(storms,
                                        nstorms,
                                        runoff.baseline,
                                        runoff.low.impact)
    
    # Bind to data frame with all cities
    all.city.storms.runoff <- as.data.frame(rbind(all.city.storms.runoff,
                                                  storms.runoff))
  }
  return(all.city.storms.runoff)
}