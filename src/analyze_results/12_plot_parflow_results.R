# 12_plot_parflow_results.R
#
# Creates maps of the US with cities colored by ParFlow results.
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, loads data, sets constant parameters.
# 2. REDUCTION IN RUNOFF. Creates map of parflow results by city for reduction 
#    in runoff.
# 3. PARTITIONING ANGLE. Creates map of parflow results by city for partitioning
#    angle.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(ggplot2) # for plotting
library(extrafont)
library(scatterpie)
library(maps)

# Load Data
load("results/in.out.loc.Rda")
city.locations <- read.csv("data/locations/locations_with_elev.csv")

# Adjust for colorbar
in.out.loc$vector.angle[in.out.loc$vector.angle < 0] <- 0
in.out.loc$vector.angle[in.out.loc$vector.angle > pi/3] <- pi/3
df <- merge(city.locations, in.out.loc)

# Set Parameters
text.size    <- 10

# 2. REDUCTION IN RUNOFF -------------------------------------------------------
us       <- map_data('state')
plot.obj <- ggplot(us, aes(long, lat)) +
              geom_map(map=us, aes(map_id=region), 
                       fill="grey60", color="grey") +
              geom_point(data = df,
                         aes(x = longitude.deg, 
                             y = latitude.deg,
                             fill = runoff.precip),
                         color = "black",
                         size = 3,
                         shape = 21) +
              scale_fill_distiller(palette = "Blues",
                                   direction = 1,
                                   limits = c(0.05, 0.35),
                                   breaks = seq(0.10, 0.30, 0.05),
                                   labels = sprintf("%.0f%%", 
                                                    100*seq(0.10, 0.30, 0.05)),
                                   name = "") +
              labs(title = "", fill = NULL) + 
              coord_fixed(1.3) +
              theme_void() +
              theme(text=element_text(size = text.size,  
                                      family = "Segoe UI Semilight"),
                    legend.position = "none")
for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_parflow_runoff_map.%s",image.type),
         plot = plot.obj,
         device = image.type,
         path = "results/figures",
         scale = 1,
         width = 6.5, height = 4, units = "in",
         dpi = 300)
}

# 3. PARTITIONING ANGLE --------------------------------------------------------
us       <- map_data('state')
plot.obj <- ggplot(us, aes(long, lat)) +
            geom_map(map=us, aes(map_id=region), 
                     fill="grey60", color="grey") +
            geom_point(data = df,
                       aes(x = longitude.deg, 
                           y = latitude.deg,
                           fill = vector.angle),
                       color = "black",
                       size = 3,
                       shape = 21) +
            scale_fill_distiller(name = "",
                                 palette = "YlGnBu",
                                 direction = 1,
                                 breaks = seq(0, pi/3, pi/9),
                                 limits = c(0, pi/3),
                                 labels = c(expression(""<=0), 
                                            expression(pi/9), 
                                            expression(2*pi/9), 
                                            expression("">=pi/3))) +
            labs(title = "", fill = NULL) + 
            coord_fixed(1.3) +
            theme_void() +
            theme(text=element_text(size = text.size,  
                                    family = "Segoe UI Semilight"),
                  legend.position = "none")
for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_parflow_angle_map.%s",image.type),
         plot = plot.obj,
         device = image.type,
         path = "results/figures",
         scale = 1,
         width = 6.5, height = 4, units = "in",
         dpi = 300)
}