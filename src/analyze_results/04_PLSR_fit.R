# 04_PLSR_fit.R
#
# Visualize goodness-of-fit of PLSR models in several ways.
#
# This script performs the following steps:
# 1. SETUP. Loads libraries, loads data, sets constant parameters.
# 2. PLSR FIT TABLE. Creates a table with goodness-of-fit results for each PLSR
#    model.
# 3. PLSR FIT PLOT. Plots goodness-of-fit results for each PLSR model.
# 4. PLSR COMPARISON RUNOFF. Creates a map of the U.S. highlighting difference
#    between PLSR predicted reduction in runoff and ParFlow reduction in runoff 
#    for each city.
# 5. PLSR COMPARISON RUNOFF. Creates a map of the U.S. highlighting difference
#    between PLSR predicted partitioning angle and ParFlow reduction in runoff 
#    for each city.

# 1. SETUP ---------------------------------------------------------------------
# Load libraries 
library(ggplot2) # for plotting
library(ggrepel) # for city/state labels
library(dplyr) # for %>%
library(extrafont) # for font text on plots
library(cowplot) # for plot_grid
library(reshape2)

# Load data
load("results/plsr.fit.Rda")
load("results/plsr.results.Rda")
city.locations <- read.csv("data/locations/locations_with_elev.csv")

# Parameters
text.size <- 24

# 2. PLSR FIT TABLE ------------------------------------------------------------
plsr.fit.table       <- dcast(plsr.fit, 
                              flux+group~fit.metric, 
                              value.var = "fit.value")
plsr.fit.table[,3:6] <- round(plsr.fit.table[,3:6], 2)


# 3. PLSR FIT PLOT -------------------------------------------------------------
plot.fit        <- ggplot() +
                   geom_bar(data = plsr.fit,
                            aes(x = flux,
                                y = fit.value,
                                fill = group),
                            position = "dodge",
                            stat = "identity",
                            color = "black") +
                   facet_wrap(~fit.metric, scales = "free_y") +
                   labs(x = "", y = "Value")  +
                   scale_fill_brewer(name = "",
                                     palette = "PuBuGn",
                                     breaks = c("cal", "val"),
                                     labels = c("Cross-Validation", 
                                                "External Validation")) +
                   theme_bw() +
                   theme(text = element_text(family = "Segoe UI Semilight", 
                                             size = text.size),
                         legend.position = "top")

# 4. PLSR COMPARISON RUNOFF ----------------------------------------------------
df.runoff <- subset(plsr.results$runoff.precip, group != "predict")
df.runoff <- merge(df.runoff, city.locations, by = "city.index")

plot.runoff <- ggplot(df.runoff,
                      aes(x = parflow, 
                          y = plsr)) +
               geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
               geom_point(shape = 16,
                          size = 2.5) +
               labs(x = "ParFlow", y = "PLSR", title = "Reduction in Runoff") +
               # geom_text_repel(aes(label = sprintf("%s, %s", city.name, state)),
               #                 color = "darkred",
               #                 box.padding = 0.4,
               #                 point.padding = 0.4,
               #                 segment.color = "darkred",
               #                 size=5,
               #                 family="Segoe UI Semibold") +
               theme_bw() +
               theme(text = element_text(family = "Segoe UI Semilight", 
                                         size = text.size),
                     legend.position = "top")

us       <- map_data('state')
plot.obj <- ggplot(us, aes(long, lat)) +
            geom_map(map=us, aes(map_id=region), 
                     fill="grey60", color="grey") +
            geom_point(data = df.runoff,
                       aes(x = longitude.deg, 
                           y = latitude.deg,
                           fill = 100*(plsr-parflow),
                           shape = group),
                       size = 6) +
            scale_fill_gradient2() +
            scale_shape_manual(name = "",
                               breaks = c("cal", "val"),
                               labels = c("Int. Validation", "Ext. Validation"),
                               values = c(21, 24)) +
            labs(title = "Runoff: PLSR - ParFlow", fill = "") + 
            # coord_equal() +
            coord_fixed(1.2) +
            theme_void() +
            theme(text=element_text(size = text.size,  
                                    family = "Segoe UI Semilight"),
                  legend.position = "bottom")

# 4. PLSR COMPARISON ANGLE -----------------------------------------------------
df.angle <- subset(plsr.results$vector.angle, group != "predict")
df.angle <- merge(df.angle, city.locations, by = "city.index")

plot.angle <- ggplot(df.angle, aes(x = parflow, y = plsr)) +
              geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
              geom_point(shape = 16, size = 2.5) +
              labs(x = "ParFlow", y = "PLSR", title = "Partitioning Angle") +
              # geom_text_repel(aes(label = sprintf("%s, %s", city.name, state)),
              #                 color = "darkred",
              #                 box.padding = 0.4,
              #                 point.padding = 0.4,
              #                 segment.color = "darkred",
              #                 size=5,
              #                 family="Segoe UI Semibold") +
              theme_bw() +
              theme(text = element_text(family = "Segoe UI Semilight", 
                                        size = text.size),
                    legend.position = "top")

us       <- map_data('state')
plot.obj <- ggplot(us, aes(long, lat)) +
            geom_map(map=us, aes(map_id=region), 
                     fill="grey60", color="grey") +
            geom_point(data = df.angle,
                       aes(x = longitude.deg, 
                           y = latitude.deg,
                           fill = (plsr-parflow),
                           shape = group),
                       size = 6) +
            scale_fill_gradient2() +
            scale_shape_manual(name = "",
                               breaks = c("cal", "val"),
                               labels = c("Int. Validation", "Ext. Validation"),
                               values = c(21, 24)) +
            labs(title = "Partitioning Angle: PLSR - ParFlow", fill = "") + 
            # coord_equal() +
            coord_fixed(1.2) +
            theme_void() +
            theme(text=element_text(size = text.size,  
                                    family = "Segoe UI Semilight"),
                  legend.position = "bottom")