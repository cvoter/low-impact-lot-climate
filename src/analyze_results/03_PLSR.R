# 03_PLSR.R
# 
# Develop PLSR models relating climate metrics with ParFlow modeled results of
# the reduction in runoff due to LID practices and the partitioning angle.
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, sources functions, loads data.
# 2. CALCULATE PLSR. Sets up input for calculate_plsr.R to develop PLSR models.
# 3. SUMMARIZE PLSR RESULTS. Summarize the goodness-of-fit and predictions from 
#    PLSR models.
# 4. SAVE. Save PLSR models and summary results.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(dplyr) # for %>%
library(pls)   # for plsr

# Source functions
source("src/analyze_results/calculate_plsr.R")
source("src/analyze_results/calculate_fit.R")
source("src/analyze_results/VIP.R")
source("src/analyze_results/classify_results.R")
source("src/analyze_results/summarise_results.R")

# Load data
load("results/in.out.loc.Rda")
load("results/in.out.loc.scaled.Rda")
load("results/predict.scaled.Rda")
load("results/met/met.summary.Rda")

city.locations <- read.csv("data/locations/locations_with_elev.csv")
nlocations     <- length(city.locations$city.index)

# Get one big data frame
in.out.loc.scaled$year <- 0
predict.scaled         <- bind_rows(predict.scaled, in.out.loc.scaled)

# 2. CALCULATE PLSR ------------------------------------------------------------
# Set parameters
df          <- predict.scaled
flux.names  <- c("runoff.precip", "vector.angle")
plot.names  <- c("Reduction in Runoff", "Partitioning Angle")
var.options <- colnames(met.summary)[-c(1,3)] # add 3 to exclude ETotoP too
seed        <- 1    # seed for random number generator
n.val       <- 6   # number of cities to hold for external validation
break.upper <- list(runoff.precip = 0.25,
                    vector.angle = 2*pi/9)
break.lower <- list(runoff.precip = 0.15,
                    vector.angle = pi/9)

# Initalize summary variables
plsr.results   <- as.list(NULL)
plsr.model     <- as.list(NULL)
plsr.fit       <- as.data.frame(NULL)
plsr.VIP       <- as.list(NULL)
plsr.summaries <- as.list(NULL)

# 3. SUMMARIZE PLSR RESULTS ----------------------------------------------------
# Loop through PLSR calculations, summarize results
for (flux.name in flux.names) {
  flux.mean <- mean(in.out.loc[,flux.name])
  flux.sd   <- sd(in.out.loc[,flux.name])
  
  PLS.out   <- calculate_pls(df, flux.name, var.options, seed, n.val, 
                             flux.mean, flux.sd)
  plsr.model[[flux.name]]  <- PLS.out$PLS.model
  PLS.out$fit.metrics      <- PLS.out$fit.metrics %>%
                              mutate(flux = plot.names[which(flux.names == flux.name)])
  plsr.fit                 <- rbind(plsr.fit, PLS.out$fit.metrics)
  plsr.VIP[[flux.name]]    <- PLS.out$PLS.VIP
  vars.keep                <- colnames(PLS.out$PLS.VIP)
  drivers                  <- met.summary[,c("city.index", vars.keep)]
  plsr.results[[flux.name]]     <- merge(PLS.out$results, drivers, by = "city.index")
  plsr.results[[flux.name]]$bin <- classify_results(plsr.results[[flux.name]]$plsr, 
                                               break.upper[[flux.name]],
                                               break.lower[[flux.name]])
  plsr.summaries[[flux.name]]   <- summarise_results(plsr.results[[flux.name]], 
                                                city.locations)
}

# 4. SAVE ----------------------------------------------------------------------
save(plsr.model, file = "results/plsr.model.Rda")
save(plsr.fit, file = "results/plsr.fit.Rda")
save(plsr.results, file = "results/plsr.results.Rda")
save(plsr.VIP, file = "results/plsr.VIP.Rda")
save(plsr.summaries, file = "results/plsr.summaries.Rda")
