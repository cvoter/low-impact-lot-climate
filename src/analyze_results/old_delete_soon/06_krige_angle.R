# 05_krige_angle.R
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, defines directories, sources functions,
#    loads data
# 2. KRIGE ANGLE. Define x & y parameters for predicting angle. Calculate
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

# Define project directories
project.dir <- 'J:/git_research/dissertation/ch02_low_impact_lot_climate'
scripts.dir <- sprintf('%s/src/analyze_results', project.dir)
results.dir <- sprintf('%s/results', project.dir)
data.dir <- sprintf('%s/data', project.dir)

# Source functions
source(sprintf("%s/functions_kriging.R", scripts.dir))
source(sprintf("%s/functions_kriging_plotting.R", scripts.dir))
source(sprintf("%s/functions_predictions.R", scripts.dir))

# Load data
load(sprintf("%s/scaled.with.stats.Rda", results.dir))
location.file <- sprintf("%s/locations/locations_with_elev.csv", data.dir)
city.locations <- read.csv(location.file)
nlocations = length(city.locations$city.index)

# 2. KRIGE ANGLE --------------------------------------------------------------
# Define parameters
x.param = "logETP"
x.title = "PET:P"
logx = TRUE
y.param = "corr.PtoET0.7day"
y.title = "7-day cor. PET v. P"
z.param = "vector.angle"
z.upper.transition <- 2*pi/9
z.lower.transition <- pi/9
z.upper.limit <- pi/3
z.lower.limit <- 0
z.interval <- pi/9
v.model.type = "Exp"
xy.res = 400
text.size = 24
legend.size = 24
#text.size = 10
#legend.size = 10

# Extract means and sds for un-scaling data labels later.
# This z.param (vector.angle) does not have a scaled value, so z.param = NULL,
# means$z = 0, and sds$z = 1
col.nums <- extract_col_nums(parameters.fluxes.no.index, 
                             x.param, y.param, z.param = NULL)
means <- extract_stats(parameters.fluxes.stats$means, col.nums, z.val = 0)
sds <- extract_stats(parameters.fluxes.stats$sds, col.nums, z.val = 1)

# Krige returns un-binned spatial data frame ($sp) and binned data frame ($df)
kriged.angle <- krige_sp_and_binned_df(parameters.fluxes.scaled,
                                       x.param, y.param, z.param, 
                                       sds, means,
                                       z.upper.transition, z.lower.transition, 
                                       v.model.type, xy.res)

krige.model.angle <- convert_sp_df(kriged.angle$sp)

# Test skill
skill.subset <- subset(parameters.fluxes.scaled, select = c(x.param, y.param, z.param))
colnames(skill.subset) = c("x","y","z.obs")

# un-binned matches
krige.model.angle$z <- krige.model.angle$z*sds$z + means$z
krige.model.angle.match <- find_closest_kriged_z(skill.subset, means, sds, krige.model.angle)

# binned matches
skill.subset$z.obs = bin_results(skill.subset$z, sds$z, means$z, z.upper.transition, 
                                 z.lower.transition)
skill.subset <- find_closest_kriged_z(skill.subset, means, sds, kriged.angle$df)
skill = matrix(nrow = 3, ncol = 3)
for (pred in 1:3){
  for (obs in 1:3) {
    skill[pred,obs] = nrow(subset(skill.subset, z.obs == obs & z == pred))
  }
}

# Plotting returns 1) kriging only ($kriged), 2) points only ($points), and 3)
# binned contours with points ($contours)
plots.angle <- get_plots(kriged.angle, parameters.fluxes.scaled, 
                         x.param, y.param, z.param, means, sds, 
                         z.upper.transition, z.lower.transition,
                         z.lower.limit, z.upper.limit, z.interval,
                         colorbar.name = "", colorbar.palette = "YlGnBu",
                         colorbar.labels.on = 0, keep.city.labels = NULL,
                         #colorbar.labels.on = 1, keep.city.labels = NULL, 
                         x.title, y.title, is.angle = 1, text.size, legend.size,
                         logx, small.panel = 0) #small.panel = 1)

# 3. SAVE ---------------------------------------------------------------------
# save(kriged.angle, means, sds, x.param, y.param, z.param,
#      krige.model.angle.match, skill.subset,
#      file = sprintf("%s/kriged.angle.Rda", results.dir))
# for (image.type in c("svg","png")) {
#   ggsave(sprintf("plot_angle_points_only.%s",image.type),
#          plot = plots.angle$points,
#          device = image.type,
#          path = sprintf("%s/figures",results.dir),
#          scale = 1,
#          width = 7, height = 5.5, units = "in",
#          #width = 8.7, height = 6, units = "cm",
#          dpi = 300)
#   ggsave(sprintf("plot_angle.%s",image.type),
#          plot = plots.angle$contours,
#          device = image.type,
#          path = sprintf("%s/figures",results.dir),
#          scale = 1,
#          width = 7, height = 5.5, units = "in",
#          #width = 8.7, height = 6, units = "cm",
#          dpi = 300)
# }
# 
# plots.angle <- get_plots(kriged.angle, parameters.fluxes.scaled, 
#                          x.param, y.param, z.param, means, sds, 
#                          z.upper.transition, z.lower.transition,
#                          z.lower.limit, z.upper.limit, z.interval,
#                          colorbar.name = "", colorbar.palette = "YlGnBu",
#                          colorbar.labels.on = 1, keep.city.labels = city.locations$city.name, 
#                          x.title, y.title, is.angle = 1, text.size, legend.size,
#                          logx, small.panel = 0)
# for (image.type in c("svg","png")) {
#   ggsave(sprintf("plot_angle_labels.%s",image.type),
#          plot = plots.angle$contours,
#          device = image.type,
#          path = sprintf("%s/figures",results.dir),
#          scale = 1,
#          width = 7, height = 5.5, units = "in",
#          #width = 18, height = 12, units = "cm",
#          dpi = 300)
# }