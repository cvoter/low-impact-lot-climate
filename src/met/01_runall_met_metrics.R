# 01_runall_met_metrics.R
#
# Calcluates all meteorological parameters to test as predictors for modeled
# hydrologic outputs. Flags trigger whether to calculate for only modeled
# year vs. for 30 yr time series.
#
# 1. SETUP ENVIRONMENT. Loads libraries, sources functions, loads data.
# 2. DEFINE PARAMETERS. Set some manually, set others based on time series,
#                       and define static parameters.
# 3. OBTAIN HOURLY ET0 AND STORM DATA. Depending on manual parameters, either 
#                                      calculates hourly ET0 and defines storms 
#                                      or uses existing .Rda files.
# 4. CALCULATE ALL DESCRIPTORS. Calculate pre-defined met metrics for each year
#                               in the timeseries (1 year for model runs, 30
#                               years for long-term climate prediction).
# 5. SAVE MET SUMMARY. Final data frame has nearly 100 columns, including:
#     --> city.index    : unique id for city
#     --> precipitation : total annual precipitation (mm)
#     --> ET0toP        : ratio of total annual ET0 to total annual 
#                         precipitation
#     --> logETP        : log10 of ET0toP
#     --> wet.fraction  : fraction of hours with precipitation
#     --> burstiness    : a measure of the interamount times between 
#                         precipitation
#     --> memory        : autocorrelation of interamount times between 
#                         precipitation
#     --> one or more columns named in the format "corr.PtoET0.%dday" 
#         corresponding to the number moving.sum.days given as an input.
#     --> nstorms            : number of unique storms during the year
#     --> mean.antecedent    : mean antecedent dry period between storms (hrs)
#     --> mean.duration      : mean duration of storms (hrs)
#     --> mean.depth         : mean depth of storms (mm)
#     --> mean.intensity.avg : mean intensity of all storms (mm/hr)
#     --> mean.intensity.max : mean peak intensity of all storms (mm/hr)
#     --> total.diff.max.avg.intensity : cumulative difference between each 
#                                        storm's peak intensity and average 
#                                        intensity
#     --> mean.diff.max.avg.intensity  : mean difference between each storm's 
#                                        peak intensity and average intensity
#     --> pcnt.rain.depth.over.%s: percent of total precipitation with hourly 
#                                  intensity greater than %s.
#     --> pcnt.stormdepth.depth.over.%s: percent of total storm depth with 
#                                        falling in storms with depth greater 
#                                        than %s.
#     --> pcnt.stormdepth.intensity.over.%s: percent of total storm depth with 
#                                            falling in storms with average 
#                                            intensity greater than %s.
#     --> pcnt.wethours.over.%s: percent of hours with precipitation with 
#                                hourly precipitation greater than %s.
#     --> pcnt.nstorms.depth.over.%s: percent of storms with total depth 
#                                     greater than %s.
#     --> pcnt.nstorms.intensity.over.%s: percent of storms with average 
#                                         intensity greater than %s.
#     --> pcnt.%02d.rain.depth:  %02d percentile for hourly precipitation depth
#     --> pcnt.%02d.storm.depth: %02d percentile for total storm depth

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries
library(dplyr) # used in several functions

# Source functions
function_files <- c(list.files("src/met", pattern = "calculate*"),
                    list.files("src/met", pattern = "convert*"),
                    list.files("src/met", pattern = "define*"),
                    list.files("src/met", pattern = "obtain*"),
                    list.files("src/met", pattern = "summarize*"))
for (file in function_files) {
  source(sprintf("src/met/%s", file))
}

# Load data
location.file  <- "data/locations/locations_with_elev.csv"
city.locations <- read.csv(location.file)
nlocations     <- length(city.locations$city.index)

# 2. DEFINE PARAMETERS --------------------------------------------------------
# Manually set these parameters
calculate.ET0    <- TRUE # TRUE = calculates ET0, FALSE = loads existing file
calculate.storms <- TRUE # TRUE = calculates storms, FALSE = loads file
model.year.only  <- FALSE  # TRUE = WY2014 only, FALSE = WY1980-WY2010 (30 yr)

# Parameters dependent on time series (model year only or 30 yr series)
if (model.year.only) {
  nyears                 <- 1
  start.year             <- 2013
  met.dir                <- 'data/weather'
  ET0.output.filename    <- "results/met/hourly.precipitation.ET0.Rda"
  storms.output.filename <- "results/met/all.city.storms.Rda"
  output.filename        <- "results/met/met.summary.Rda"
} else {
  nyears                 <- 30
  start.year             <- 1980
  met.dir                <- 'data/climate'
  ET0.output.filename    <- "results/met/hourly.precipitation.ET0.long.term.Rda"
  storms.output.filename <- "results/met/all.city.storms.long.term.Rda"
  output.filename        <- "results/met/met.summary.long.term.Rda"
}

# Other, more static paramters
start.month        <- 10
start.day          <- 1
met.filename       <- "nldas.1hr.clm.txt"
moving.sum.days    <- c(3, 5, 7, 30)
threshold.depths   <- c(4.5, 5, 5.5, 6, 6.35, 
                        7, 10, 12.7, 25.4)
threshold.names    <- c('4.5mm','5mm','5.5mm','6mm','0.25in',
                        '7mm','10mm','0.5in','1in')
threshold.percents <- c(50, 60, (100*2/3), 70, 75, 80, 85, 90, 95)

# 3. OBTAIN HOURLY ET0 AND STORM DATA ------------------------------------------
# ET0
if (calculate.ET0) {
  hourly.precipitation.ET0 <- obtain_hourly_met(city.locations, 
                                                met.dir, 
                                                met.filename, 
                                                nyears, 
                                                start.year, 
                                                start.month, 
                                                start.day)
  
  save(hourly.precipitation.ET0, file = ET0.output.filename)
} else {
  load(ET0.output.filename)
}

# Storms
if (calculate.storms) {
  all.city.storms <- obtain_storms(hourly.precipitation.ET0, 
                                   city.locations,
                                   nlocations)
  save(all.city.storms, file = storms.output.filename)
} else {
  load(storms.output.filename)
}

# 4. CALCULATE ALL DESCRIPTORS ------------------------------------------------
met.summary.long.term <- NULL
year.start.hour       <- 1
for (y in 1:nyears) {
  # Extract input dates, precip, ET0, and storms for year y
  day.vector             <- define_julian_days(nyears = 1, 
                                               start.year + y - 1, 
                                               start.month, 
                                               start.day)
  year.end.hour          <- length(day.vector) + year.start.hour - 1
  year.precipitation.ET0 <- hourly.precipitation.ET0 %>%
                            filter(hour >= year.start.hour,
                                   hour <= year.end.hour)
  year.all.storms        <- all.city.storms %>%
                            filter(start.hour >= year.start.hour,
                                   start.hour <= year.end.hour)
  
  # Calculate metrics for year y
  hourly.met.summary <- summarize_hourly_met(year.precipitation.ET0,
                                             moving.sum.days,
                                             nlocations)
  storm.summary      <- summarize_storms(year.all.storms, nlocations)
  threshold.summary  <- summarize_thresholds(year.precipitation.ET0, 
                                             year.all.storms, 
                                             threshold.depths,
                                             threshold.names,
                                             threshold.percents, 
                                             nlocations)
  
  # Merge all metrics
  met.summary    <- merge(hourly.met.summary, storm.summary, by = "city.index")
  met.summary    <- merge(met.summary, threshold.summary, by = "city.index")
  
  # If long term, bind year y summary to larger data frame
  if (!model.year.only) {
    year                  <- as.data.frame(matrix(rep(y,nlocations)))
    colnames(year)        <- "year"
    met.summary           <- cbind(year, met.summary)
    met.summary.long.term <- as.data.frame(rbind(met.summary.long.term, 
                                                 met.summary))
  }
  year.start.hour       <- year.end.hour + 1
}

# 5. SAVE MET SUMMARY ---------------------------------------------------------
if (model.year.only) {
  save(met.summary, file = output.filename)
} else {
  save(met.summary.long.term, file = output.filename)
}