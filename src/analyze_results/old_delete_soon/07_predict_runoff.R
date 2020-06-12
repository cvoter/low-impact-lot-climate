# 07_predict_runoff.R
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, defines directories, sources functions,
#    loads data
# 2. MAKE PREDICTIONS. Use save kriged values to find closest match for met
#    parameters, then count number of years a city falls into each bin.
# 3. PLOT. Generate map of US with pie charts indicating frequency in each
#    category. Save as png and svg.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(ggplot2) # for plotting
library(scatterpie) #for pies on map

# Define project directories
project.dir <- 'J:/git_research/dissertation/ch02_low_impact_lot_climate'
scripts.dir <- sprintf('%s/src/analyze_results', project.dir)
results.dir <- sprintf('%s/results', project.dir)
data.dir <- sprintf('%s/data', project.dir)

# Source functions
source(sprintf("%s/functions_predictions.R", scripts.dir))

# Load data
location.file <- sprintf("%s/locations/locations_with_elev.csv", data.dir)
city.locations <- read.csv(location.file)
nlocations = length(city.locations$city.index)
load(sprintf("%s/met/met.summary.long.term.Rda", results.dir))
load(sprintf("%s/kriged.runoff.Rda", results.dir))

# 2. MAKE PREDICTIONS ---------------------------------------------------------
long.term.subset <- subset(met.summary.long.term, 
                           select = c("city.index", "year", x.param, y.param))
colnames(long.term.subset) = c("city.index","year","x","y")

long.term.subset <- find_closest_kriged_z(long.term.subset, means, sds, 
                                          kriged.runoff$df)

summary.runoff.predictions <- summarize_long_term_z(long.term.subset, 
                                                    nlocations)

# 3. PLOT ---------------------------------------------------------------------

plot.runoff.predictions <- plot_predictions_on_us_map(summary.runoff.predictions, 
                                                      color.palette = "Blues", 
                                                      text.size = 10,
                                                      is.angle = 0)
for (image.type in c("svg","png")) {
  ggsave(sprintf("map_runoff_predictions.%s",image.type), 
         plot = plot.runoff.predictions, 
         device = image.type, 
         path = sprintf("%s/figures",results.dir),
         scale = 1, 
         width = 10.8, height = 5.4, units = "cm",
         dpi = 300)
}
