#' summarize_thresholds.R
#' 
#' Summarizes 1 year of hourly precipitation and storms for each city based on a
#' variety of depth/intensity and percentile thresholds.
#' 
#' Given the input vector for threshold depths (mm) this function calculates the
#' following (X = input depth, mm):
#'     --> percent of total rainfall falling as hourly rainfall over X mm/hr
#'     --> percent of total storm depth falling in storms with over X mm depth
#'     --> percent of total storm depth falling in storms with average intensity 
#'         over X mm/hr
#'     --> percent of total wet hours with hourly rainfall over X mm/hr
#'     --> percent of storms with over X mm depth
#'     --> percent of storms with over X mm/hr average intensity
#' 
#' Given the input vector for threshold percentiles (e.g., 50%, 90%), this
#' function also calculates the following (X = input percentile):
#'     --> Hourly rainfall depth representing the X % hourly rainfall depth 
#'         (X % of hourly rainfall is this depth or smaller)
#'     --> Storm depth representing the X % total storm depth (X % of all storm 
#'         depth falls in storms of this depth or smaller)
#' 
#' REQUIRES
#' calculate_percent_match (function)
#' 
#' INPUTS
#' hourly.precipitation.ET0: a data frame with the following columns:
#'     --> hour:             time since start of timeseries (hour)
#'     --> city.index:       unique id for city
#'     --> precipitation:    precipitation (mm/hr)
#'     --> ET0:              reference evapotranspiration (mm/hr)
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
#' threshold.depths: a vector with all depths to use as thresholds (mm)
#' threshold.names: corresponding strings to use when naming depth threshold
#'                  columns.
#' threshold.percents: a vector with all percentiles to use as thresholds (%)
#' nlocations: number of locations in hourly.precipitation.ET0 and 
#'             all.city.storms datasets.
#' 
#' OUTPUTS
#' thresholds: a data frame with the following columns:
#'     --> city.index: unique id of city.
#'     --> pcnt.rain.depth.over.%s: percent of total precipitation with hourly 
#'                                  intensity greater than %s.
#'     --> pcnt.stormdepth.depth.over.%s: percent of total storm depth with 
#'                                        falling in storms with depth greater 
#'                                        than %s.
#'     --> pcnt.stormdepth.intensity.over.%s: percent of total storm depth with 
#'                                            falling in storms with average 
#'                                            intensity greater than %s.
#'     --> pcnt.wethours.over.%s: percent of hours with precipitation with 
#'                                hourly precipitation greater than %s.
#'     --> pcnt.nstorms.depth.over.%s: percent of storms with total depth 
#'                                     greater than %s.
#'     --> pcnt.nstorms.intensity.over.%s: percent of storms with average 
#'                                         intensity greater than %s.
#'     --> pcnt.%02d.rain.depth:  %02d percentile for hourly precipitation depth
#'     --> pcnt.%02d.storm.depth: %02d percentile for total storm depth

summarize_thresholds <- function(hourly.precipitation.ET0, 
                                 all.city.storms, 
                                 threshold.depths,
                                 threshold.names,
                                 threshold.percents, 
                                 nlocations){
  thresholds <- NULL
  for (i in 1:nlocations) {
    # Subset hourly precip & storms by city and rank by depth
    hourly.met    <- hourly.precipitation.ET0 %>%
                     filter(city.index == i) %>%
                     arrange(precipitation) %>%
                     mutate(cum.precip = cumsum(precipitation),
                            cum.percent = 100*cum.precip/sum(precipitation))
    storms        <- all.city.storms %>%
                     filter(city.index == i) %>%
                     arrange(depth) %>%
                     mutate(cum.depth = cumsum(depth),
                            cum.percent = 100*cum.depth/sum(depth))
    precipitation <- hourly.met$precipitation
   
    # Total depth
    total.precip       <- sum(precipitation)
    total.storm.depths <- sum(storms$depth)
    
    # Total wet time
    total.wet.hours    <- length(precipitation[precipitation > 0])
    total.nstorms      <- length(storms$storm.index)
    
    # THRESHOLDS: DEPTH/INTENSITY ----------------------------------------------
    city.thresholds     <- i
    threshold.colnames <- "city.index"
    for (j in 1:length(threshold.depths)) {
      threshold       <- threshold.depths[j]
      name            <- threshold.names[j]
      
      # Subset by threshold
      rain.subset            <- precipitation[precipitation > threshold]
      storm.depth.subset     <- storms %>%
                                filter(.data$depth > threshold)
      storm.intensity.subset <- storms %>%
                                filter(.data$intensity.avg > threshold)
      
      # Calculate percent above threshold (relative to depth and wet time)
      rain.depth      <- calculate_percent_match(rain.subset, 
                                                 total.precip, 
                                                 total.wet.hours)
      storm.depth     <- calculate_percent_match(storm.depth.subset$depth,
                                                 total.storm.depths,
                                                 total.nstorms)
      storm.intensity <- calculate_percent_match(storm.intensity.subset$depth,
                                                 total.storm.depths,
                                                 total.nstorms)
      
      # Append to data frame
      city.thresholds    <- c(city.thresholds, 
                              rain.depth$depth, 
                              storm.depth$depth,
                              storm.intensity$depth,
                              rain.depth$time,
                              storm.depth$time,
                              storm.intensity$time)
      threshold.colnames <- c(threshold.colnames, 
                              sprintf("pcnt.rain.depth.over.%s", name),
                              sprintf("pcnt.stormdepth.depth.over.%s", name),
                              sprintf("pcnt.stormdepth.intensity.over.%s", name),
                              sprintf("pcnt.wethours.over.%s", name),
                              sprintf("pcnt.nstorms.depth.over.%s", name),
                              sprintf("pcnt.nstorms.intensity.over.%s", name))
    }
    # THRESHOLDS: PERCENTILES --------------------------------------------------
    for (j in 1:length(threshold.percents)) {
      percent            <- threshold.percents[j]
      # `approx` warns that there are several non-unique "x" values and it's
      # collapsing them; I know this and want this behavior.
      rain.pcnt          <- suppressWarnings(approx(hourly.met$cum.percent,
                                                    hourly.met$precipitation,
                                                    xout = percent)$y)
      storm.pcnt         <- suppressWarnings(approx(storms$cum.percent,
                                                    storms$depth,
                                                    xout = percent)$y)
      city.thresholds    <- c(city.thresholds, rain.pcnt, storm.pcnt)
      threshold.colnames <- c(threshold.colnames, 
                              sprintf("pcnt.%02.0f.rain.depth", percent),
                              sprintf("pcnt.%02.0f.storm.depth", percent))
    }
    thresholds           <- as.data.frame(rbind(thresholds, city.thresholds))
  }
  
  # Adjust names and return 
  colnames(thresholds)  <- threshold.colnames
  rownames(thresholds)  <- NULL
  return(thresholds)
}