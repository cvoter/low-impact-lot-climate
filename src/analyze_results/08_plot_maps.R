# 08_plot_maps.R
#
# Creates maps of the US with pie charts over each modeled city indicating
# PLSR-predicted performance of LID practices from WY1981-WY2010.
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, loads data, sets constant parameters,
#    defines function for creating scatterpie plots.
# 2. PLOT. Creates scatterpie plots for reduction in runoff and partitioning 
#    angle.
# 3. SAVE. Saves plots

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(ggplot2) # for plotting
library(extrafont)
library(scatterpie)
library(maps)

# Load Data
load("results/plsr.summaries.Rda")
city.locations <- read.csv("data/locations/locations_with_elev.csv")

# Set Parameters
text.size    <- 10

# Define plotting function
plot_scatterpie <- function(bin.predicts, color.values, label.names, text.size){
  us       <- map_data('state')
  plot.obj <- ggplot(us, aes(long, lat)) +
              geom_map(map=us, aes(map_id=region), 
                       fill="grey60", color="grey") +
              geom_scatterpie(data = bin.predicts,
                              aes(longitude.deg, latitude.deg, r = 0.9),
                              cols = c("z.1","z.2","z.3"),
                              alpha = 0.7) +
              scale_fill_manual( breaks = c("z.1","z.2","z.3"),
                                 labels = label.names,
                                 values = rev(color.values)) +
              labs(title = "", fill = NULL) + 
              coord_equal() +
              # coord_fixed(1) +
              theme_void() +
              theme(text=element_text(size = text.size,  
                                      family = "Segoe UI Semilight"),
                    legend.margin = margin(0,0,0,0),
                    legend.box.margin = margin(-10,-45,-10,-45),
                    legend.key.size = unit(0.7,"line"),
                    legend.position = "bottom")
  return(plot.obj)
}

# 2. PLOT ----------------------------------------------------------------------
# Runoff
df.input <- merge(city.locations, plsr.summaries$runoff.precip)
color.values <- RColorBrewer::brewer.pal(3, "Blues")
label.names  <- c("> 25%", "15% - 25%", "< 15%")
plot.runoff.map <- plot_scatterpie(df.input, color.values, label.names, text.size)

# Partitioning
df.input <- merge(city.locations, plsr.summaries$vector.angle)
color.values <- RColorBrewer::brewer.pal(3, "YlGnBu")
label.names  <- c("Mostly Drainage", "Mix", "Mostly ET")
plot.partition.map <- plot_scatterpie(df.input, color.values, label.names, text.size)

# 3. SAVE ----------------------------------------------------------------------
for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_runoff_map.%s",image.type),
         plot = plot.runoff.map,
         device = image.type,
         path = "results/figures",
         scale = 1,
         width = 18, height = 8, units = "cm",
         dpi = 300)
  ggsave(sprintf("plot_partition_map.%s",image.type),
         plot = plot.partition.map,
         device = image.type,
         path = "results/figures",
         scale = 1,
         width = 18, height = 8, units = "cm",
         dpi = 300)
}
