# 01_merge_weather_and_fluxes.R
#
# Merges model inputs with model results. Scales all parameters for later use in
# stats models.
#
# 1. SETUP ENVIRONMENT. Load libraries, data
# 2. MERGE RESULTS. Untransform values by modelrun (lot) and city (loc)
# 3. SCALE RESULTS. Center by mean, scale by standard deviation.
# 4. SAVE.
#
# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries
library(dplyr)

# Load data
load("results/met/met.summary.Rda")  # met.summary
load("results/met/met.summary.long.term.Rda")  # met.summary.long.term
load('results/model_outputs/fluxes_summaries.Rda')  # fluxes.diff, fluxes.lot
city.locations <- read.csv("data/locations/locations_with_elev.csv")

# 2. MERGE RESULTS -------------------------------------------------------------
# Untransformed values by modelrun (lot) and city (loc)
in.out.lot <- merge(fluxes.lot, met.summary, by = "city.index")
in.out.loc <- merge(fluxes.diff, met.summary, by = "city.index")

# 3. SCALE RESULTS -------------------------------------------------------------
# Mean-centered, SD-scaled parameters and fluxes
in.out.loc.scaled            <- as.data.frame(scale(in.out.loc))
in.out.loc.scaled$city.index <- in.out.loc$city.index

# Scale long-term met parameters
means <- colMeans(met.summary)[-c(1)]
sds   <- apply(met.summary, 2, sd)[-c(1)]
if (all(colnames(met.summary[-c(1)]) == colnames(met.summary.long.term[-c(1,2)]))) {
  predict.scaled <- sweep(met.summary.long.term[-c(1,2)], 2, means, FUN = "-")
  predict.scaled <- sweep(predict.scaled, 2, sds, FUN = "/")
}
predict.scaled$year       <- met.summary.long.term$year
predict.scaled$city.index <- met.summary.long.term$city.index 

# 4. SAVE ----------------------------------------------------------------------
# Add in city name and state
city.locations    <- city.locations %>%
                     select(city.index, city.name, state)
in.out.loc        <- merge(city.locations, in.out.loc, by = "city.index")
in.out.lot        <- merge(city.locations, in.out.lot, by = "city.index")
in.out.loc.scaled <- merge(city.locations, in.out.loc.scaled, by = "city.index")
predict.scaled    <- merge(city.locations, predict.scaled, by = "city.index")

# Save
save(in.out.loc, file = "results/in.out.loc.Rda")
save(in.out.lot, file = "results/in.out.lot.Rda")
save(in.out.loc.scaled, file = "results/in.out.loc.scaled.Rda")
save(predict.scaled, file = "results/predict.scaled.Rda")