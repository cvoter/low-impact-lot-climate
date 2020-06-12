# 02_process_hourly_fluxes.R

# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, defines directories, sources functions
# 2. LOAD AND RESAVE FLUXES. Loads hourly fluxes as csvs, calulates water
#    balance errors, and resaves to .Rda files
# 3. PROCESS FLUXES BY LOT. For each lot (aka runname), calculates cumulative
#    runoff, deep drainage, ET, and subsurface storage as A) depth, B) fraction
#    of precipitation, C) fraction of root zone fluxes (using dreep drainge),
#    and D) fraction of root zone fluxes (using subsurface storage).
# 4. PROCESS FLUXES (CHANGES) BY LOCATION. For each location, calculate change
#    in fluxes due to low-impact practices as A) depth, B) fraction of
#    precipitation, and C) fraction of root zone fluxes (using deep drainage).
# 5. SAVE PROCESSED DATA FRAMES. By lot and by location.
# 6. PLOT WATER BALANCE ERRORS. Visualize cumulative relative and depth errors
#   for CLM only, ParFlow only, and entire model.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load Libraries 
library(ggplot2) # For plotting

# Define project directories
project.dir <- 'J:/git_research/dissertation/ch02_low_impact_lot_climate'
scripts.dir <- sprintf('%s/src/model_outputs', project.dir)
results.dir <- sprintf('%s/results/model_outputs', project.dir)

# Source functions
source(sprintf("%s/functions_hourly_fluxes.R", scripts.dir))

# 2. LOAD AND RESAVE FLUXES ---------------------------------------------------
# Define model run parameters
nhrs = 8760
nlocations = 51
hour <- c(1:nhrs)

# Initalize indicies and summary data frames
cum.errors <- NULL
fluxes.lot <- NULL
i <- 0

# Retrieve and re-save hourly flux data for each model run
for ( location in 1:nlocations) {
  for (lot in c("baseline", "low_impact")) {
    # Initalize indicies and data frames within loop
    hourly.balance <- NULL
    i <- i + 1
    
    # Runname based on location and lot type
    runname <- sprintf("loc%02d_%s", location, lot)
    
    # Define filenames for csv input and Rda output
    input.filepath <- sprintf("%s/%s/%s_hourly_balance.csv", 
                              results.dir, runname, runname)
    output.filepath <- sprintf('%s/%s/%s_hourly_balance.Rda', 
                               results.dir, runname, runname)
    
    # Load data from csv, replace colnames with R versions
    hourly.balance <- read.csv(input.filepath)
    colnames(hourly.balance) = c("precipitation", 
                                 "delta.storage.surface",
                                 "delta.storage.canopy",
                                 "delta.storage.snow",
                                 "delta.storage.subsurface",
                                 "evaptranssum",
                                 "evaporation", 
                                 "surface.runoff",
                                 "transpiration", 
                                 "deep.drainage",
                                 "recharge")
    
    # Calculate this run's errors and add to this run's output dataframe
    errors <- calculate_balance_errors(hourly.balance)
    hourly.balance <- as.data.frame(cbind(hour,hourly.balance, errors))
    
    # Add this run's final cumulative errors to summary error dataframe
    this.cum.errs <- subset(hourly.balance, hour==nhrs,
                            select = c("cum.err.depth.CLM", "cum.err.rel.CLM",
                                       "cum.err.depth.PF", "cum.err.rel.PF",
                                       "cum.err.depth", "cum.err.rel"))
    cum.errors <- as.data.frame(rbind(cum.errors, this.cum.errs))
    
    # Add this run's cumulative P, RO, DD, and ET to summary flux dataframe
    fluxes.lot$city.index[i] <- location
    fluxes.lot$lot.type[i] <- lot
    fluxes.lot$precip.depth[i] <- sum(hourly.balance$precipitation)
    fluxes.lot$runoff.depth[i] <- sum(hourly.balance$surface.runoff)
    fluxes.lot$ET.depth[i] <- sum(hourly.balance$evaporation) + 
                              sum(hourly.balance$transpiration)
    fluxes.lot$drainage.depth[i] <- sum(hourly.balance$deep.drainage)
    fluxes.lot$subsurface.depth[i] <- fluxes.lot$precip.depth[i] - 
                                      fluxes.lot$runoff.depth[i] - 
                                      fluxes.lot$ET.depth[i]
    
    # Save data frame to Rda file
    save(hourly.balance, file = output.filepath)
  }
}

# 3. PROCESS FLUXES BY LOT ----------------------------------------------------

# Convert fluxes as DEPTH from list to dataframe
fluxes.lot <- as.data.frame(fluxes.lot)

# Calculate fluxes as FRACTION PRECIP
fluxes.lot$runoff.precip <- fluxes.lot$runoff.depth/
                            fluxes.lot$precip.depth
fluxes.lot$ET.precip <- fluxes.lot$ET.depth/
                        fluxes.lot$precip.depth
fluxes.lot$drainage.precip <- fluxes.lot$drainage.depth/
                              fluxes.lot$precip.depth
fluxes.lot$subsurface.precip <- fluxes.lot$subsurface.depth/
                                fluxes.lot$precip.depth

# Fluxes as FRACTION RZ (DRAINAGE)
RZ.drainage <- fluxes.lot$runoff.depth + fluxes.lot$ET.depth +
               fluxes.lot$drainage.depth
fluxes.lot$runoff.RZ <- fluxes.lot$runoff.depth/
                        RZ.drainage
fluxes.lot$ET.RZ <- fluxes.lot$ET.depth/
                    RZ.drainage
fluxes.lot$drainage.RZ <- fluxes.lot$drainage.depth/
                          RZ.drainage

# Fluxes as FRACTION RZ (SUBSURFACE)
RZ.subsurface <- fluxes.lot$runoff.depth + fluxes.lot$ET.depth +
                 fluxes.lot$subsurface.depth
fluxes.lot$runoff.RZ2 <- fluxes.lot$runoff.depth/
                         RZ.subsurface
fluxes.lot$ET.RZ2 <- fluxes.lot$ET.depth/
                     RZ.subsurface
fluxes.lot$drainage.RZ2 <- fluxes.lot$subsurface.depth/
                           RZ.subsurface


# 4. PROCESS FLUXES (CHANGES) BY LOCATION -------------------------------------

# Calculate flux differences as DEPTH
fluxes.diff = NULL
for ( i in 1:nlocations ) {
  this.precip = subset(fluxes.lot, 
                       lot.type=='low_impact' & city.index==i, 
                       select=precip.depth)
  this.baseline = subset(fluxes.lot, 
                         lot.type=='baseline' & city.index==i, 
                         select=c(runoff.depth, ET.depth, 
                                  drainage.depth, subsurface.depth))
  this.low.impact = subset(fluxes.lot, 
                           lot.type=='low_impact' & city.index==i, 
                           select=c(runoff.depth, ET.depth, 
                                    drainage.depth, subsurface.depth))
  this.difference = this.low.impact - this.baseline
  this.city = as.data.frame(c(i, this.precip, this.difference))
  colnames(this.city) = c("city.index", "precip.depth", "runoff.depth",
                          "ET.depth", "drainage.depth", "subsurface.depth")
  fluxes.diff = as.data.frame(rbind(fluxes.diff, this.city))
}

# Make runoff reduction positive (for later analysis)
fluxes.diff$runoff.depth = -fluxes.diff$runoff.depth

# Calculate flux differences as FRACTION PRECIP
fluxes.diff$runoff.precip <- fluxes.diff$runoff.depth/
                             fluxes.diff$precip.depth
fluxes.diff$ET.precip <- fluxes.diff$ET.depth/
                         fluxes.diff$precip.depth
fluxes.diff$drainage.precip <- fluxes.diff$drainage.depth/
                               fluxes.diff$precip.depth
fluxes.diff$subsurface.precip <- fluxes.diff$subsurface.depth/
                                 fluxes.diff$precip.depth

# 5. SAVE PROCESSED DATA FRAMES -----------------------------------------------
save(fluxes.lot, fluxes.diff, 
     file = sprintf('%s/fluxes_summaries.Rda', results.dir))


# 6. PLOT WATER BALANCE ERRORS ------------------------------------------------
plot.err.CLM = plot_water_balance_errors(cum.err.depth.CLM, 
                                         cum.err.rel.CLM,
                                         "CLM")
plot.err.PF = plot_water_balance_errors(cum.err.depth.PF, 
                                        cum.err.rel.PF,
                                        "PF")
plot.err = plot_water_balance_errors(cum.err.depth, 
                                     cum.err.rel,
                                     "Overall")