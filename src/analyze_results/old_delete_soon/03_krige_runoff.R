# 03_krige_runoff.R
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, defines directories, sources functions,
#    loads data
# 2. KRIGE RUNOFF. Define x & y parameters for predicting runoff. Calculate
#    kriged grid and plot.
# 3. SAVE. Kriged data frames with scaling factors for parameters, and plots.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(ggplot2) # for plotting
library(ggrepel) # for city/state labels
library(dplyr) # for %>%
library(sp) # for spatial coordinates data frame
library(gstat) # for kriging
library(extrafont) # for font text on plots

# Source functions
source("src/analyze_results/functions_kriging.R")
source("src/analyze_results/functions_kriging_plotting.R")
source("src/analyze_results/functions_predictions.R")

# Load data
load("results/in.out.loc.scaled.Rda")
city.locations <- read.csv("data/locations/locations_with_elev.csv")
nlocations     <- length(in.out.loc$city.index)

# 3. KRIGE RUNOFF -------------------------------------------------------------
# Define parameters
x.param            <- "logETP"
x.title            <- "PET:P"
logx               <- TRUE
y.param            <- "pcnt.90.rain.depth"
y.title            <- expression(paste(P[90],' Rainfall Intensity (mm/hr)'))
z.param            <- "runoff.precip"
z.upper.transition <- 0.25
z.lower.transition <- 0.15
z.upper.limit      <- 0.30
z.lower.limit      <- 0.10
z.interval         <- 0.05
v.model.type       <- "Sph"
xy.res             <- 400
text.size          <- 24
legend.size        <- 24
#text.size         <- 10
#legend.size       <- 10

# Extract means and sds for un-scaling data labels later.
col.nums <- extract_col_nums(parameters.fluxes.no.index, 
                             x.param, y.param, z.param)
means <- extract_stats(parameters.fluxes.stats$means, col.nums)
sds <- extract_stats(parameters.fluxes.stats$sds, col.nums)

# Krige returns un-binned spatial data frame ($sp) and binned data frame ($df)
kriged.runoff <- krige_sp_and_binned_df(parameters.fluxes.scaled,
                                        x.param, y.param, z.param, 
                                        sds, means,
                                        z.upper.transition, z.lower.transition,
                                        v.model.type, xy.res)
krige.model.runoff <- convert_sp_df(kriged.runoff$sp)

# Test skill
skill.subset <- subset(parameters.fluxes.no.index, select = c(x.param, y.param, z.param))
colnames(skill.subset) = c("x","y","z.obs")

# un-binned matches
krige.model.runoff$z <- krige.model.runoff$z*sds$z + means$z
krige.model.runoff.match <- find_closest_kriged_z(skill.subset, means, sds, krige.model.runoff)

# binned matches
skill.subset$z.obs = bin_results(skill.subset$z, 1, 0, z.upper.transition, 
                                 z.lower.transition)
skill.subset <- find_closest_kriged_z(skill.subset, means, sds, kriged.runoff$df)
skill = matrix(nrow = 3, ncol = 3)
for (pred in 1:3){
  for (obs in 1:3) {
    skill[pred,obs] = nrow(subset(skill.subset, z.obs == obs & z == pred))
  }
}

# Plotting returns 1) kriging only ($kriged), 2) points only ($points), and 3)
# binned contours with points ($contours)
plots.runoff <- get_plots(kriged.runoff, parameters.fluxes.scaled, 
                          x.param, y.param, z.param, means, sds, 
                          z.upper.transition, z.lower.transition,
                          z.lower.limit, z.upper.limit, z.interval,
                          colorbar.name = "", colorbar.palette = "Blues",
                          colorbar.labels.on = 0, keep.city.labels = NULL,
                          #colorbar.labels.on = 1, keep.city.labels = NULL,
                          x.title, y.title, is.angle = 0, text.size, legend.size,
                          logx, small.panel = 0) #small.panel = 1)

# 3. SAVE ---------------------------------------------------------------------
# save(kriged.runoff, means, sds, x.param, y.param, z.param,
#      krige.model.runoff.match, skill.subset,
#      file = sprintf("%s/kriged.runoff.Rda", results.dir))

# for (image.type in c("svg","png")) {
#   ggsave(sprintf("plot_runoff_points_only.%s",image.type),
#          plot = plots.runoff$points,
#          device = image.type,
#          path = sprintf("%s/figures",results.dir),
#          scale = 1,
#          width = 7, height = 5.5, units = "in",
#          #width = 8.7, height = 6, units = "cm",
#          dpi = 300)
#   ggsave(sprintf("plot_runoff.%s",image.type),
#          plot = plots.runoff$contours,
#          device = image.type,
#          path = sprintf("%s/figures",results.dir),
#          scale = 1,
#          width = 7, height = 5.5, units = "in",
#          #width = 8.7, height = 6, units = "cm",
#          dpi = 300)
# }

# plots.runoff <- get_plots(kriged.runoff, parameters.fluxes.scaled, 
#                           x.param, y.param, z.param, means, sds, 
#                           z.upper.transition, z.lower.transition,
#                           z.lower.limit, z.upper.limit, z.interval,
#                           colorbar.name = "", colorbar.palette = "Blues",
#                           colorbar.labels.on = 1, keep.city.labels = city.locations$city.name, 
                          # x.title, y.title, is.angle = 0, text.size, legend.size,
                          # logx, small.panel = 0)
# for (image.type in c("svg","png")) {
#   ggsave(sprintf("plot_runoff_labels.%s",image.type),
#          plot = plots.runoff$contours,
#          device = image.type,
#          path = sprintf("%s/figures",results.dir),
#          scale = 1,
#          width = 7, height = 5.5, units = "in",
#          #width = 18, height = 12, units = "cm",
#          dpi = 300)
# }