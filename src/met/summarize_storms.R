#' summarize_storms.R
#' 
#' Calculates summary metrics based on a 1-year time series of individual
#' storms.
#' 
#' REQUIRES
#' dplyr (package)
#' 
#' INPUTS
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
#' nlocations: number of cities in input dataset
#' 
#' OUTPUTS
#' storm.summary: a data frame with the following columns:
#'     --> city.index         : unique id for city
#'     --> nstorms            : number of unique storms during the year
#'     --> mean.antecedent    : mean antecedent dry period between storms (hrs)
#'     --> mean.duration      : mean duration of storms (hrs)
#'     --> mean.depth         : mean depth of storms (mm)
#'     --> mean.intensity.avg : mean intensity of all storms (mm/hr)
#'     --> mean.intensity.max : mean peak intensity of all storms (mm/hr)
#'     --> total.diff.max.avg.intensity : cumulative difference between each 
#'                                        storm's peak intensity and average 
#'                                        intensity
#'     --> mean.diff.max.avg.intensity  : mean difference between each storm's 
#'                                        peak intensity and average intensity

summarize_storms <- function(all.city.storms, nlocations){
  library(dplyr)
  storm.summary  <- NULL
  for (i in 1:nlocations) {
    # Storm data
    storms <- subset(all.city.storms, city.index==i)
    
    # Calculate storm metrics
    nstorms                      <- length(storms$storm.index)
    storm.means                  <- storms %>% 
                                    summarize_at(c("antecedent.hours", 
                                                   "duration", 
                                                   "depth", 
                                                   "intensity.avg", 
                                                   "intensity.max"),
                                                 mean)
    diff.max.avg.intensity       <- storms$intensity.max - storms$intensity.avg
    total.diff.max.avg.intensity <- sum(diff.max.avg.intensity)
    mean.diff.max.avg.intensity  <- mean(diff.max.avg.intensity)
    
    # Append to data frames
    city.metrics           <- as.data.frame(c(i, 
                                              nstorms, 
                                              storm.means, 
                                              total.diff.max.avg.intensity,
                                              mean.diff.max.avg.intensity))
    colnames(city.metrics) <- c("city.index", 
                                "nstorms", 
                                "mean.antecedent",
                                "mean.duration", 
                                "mean.depth", 
                                "mean.intensity.avg", 
                                "mean.intensity.max",
                                "total.diff.max.avg.intensity", 
                                "mean.diff.max.avg.intensity")
    storm.summary          <- as.data.frame(rbind(storm.summary, city.metrics))
  }
  return(storm.summary)
}