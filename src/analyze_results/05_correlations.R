#05_correlations.R
#
# Creates a correlation matrix of a subset of the 89 climate metrics. Saves a
# csv with the full correlation matrix.
#
# This script performs the following steps:
# 1. SETUP. Loads libraries, loads data.
# 2. CORRELATIONS. Generates correlations matrix of all 89 climate metrics as
#    well as a smaller subset. Plots and saves plot of smaller subset. Saves 
#    full matrix as a csv.

# 1. SETUP ---------------------------------------------------------------------
load("results/met/met.summary.long.term.Rda")
text.size <- 10
library(reshape2)
library(ggplot2)
library(dplyr)
library(extrafont)

# 2. CORRELATIONS --------------------------------------------------------------
# Short Correlations
selected_cols <- c("precipitation",	
                   "logETP", 
                   "wet.fraction",	
                   "corr.PtoET0.30day",
                   "burstiness",	
                   "memory", 
                   "nstorms",	
                   "mean.antecedent",	
                   "mean.duration",	
                   "mean.depth",	
                   "mean.intensity.avg",	
                   "mean.intensity.max",	
                   "total.diff.max.avg.intensity",
                   "mean.diff.max.avg.intensity",	
                   "pcnt.75.rain.depth",	
                   "pcnt.75.storm.depth",
                   "pcnt.stormdepth.depth.over.0.25in",	
                   "pcnt.stormdepth.depth.over.1in",
                   "pcnt.nstorms.depth.over.0.25in",
                   "pcnt.nstorms.depth.over.1in",	
                   "pcnt.wethours.over.0.25in",	
                   "pcnt.wethours.over.1in",	
                   "pcnt.rain.depth.over.0.25in",	
                   "pcnt.rain.depth.over.1in",	
                   "pcnt.stormdepth.intensity.over.0.25in",
                   "pcnt.stormdepth.intensity.over.1in",	
                   "pcnt.nstorms.intensity.over.0.25in",	
                   "pcnt.nstorms.intensity.over.1in")
friendly_names <- c("Precipitation",	
                    "PET:P", 
                    "% of time raining",	
                    "30-day corr. P vs. PET",
                    "Burstiness",	
                    "Memory", 
                    "Number of storms",	
                    "Mean interstorm duration",	
                    "Mean storm duration",	
                    "Mean storm depth",	
                    "Mean storm intensity",	
                    "Mean storm peak intensity",	
                    "Cum. diff. peak vs. mean intensity",
                    "Mean diff. peak vs. mean intensity",	
                    "75th percentile precip intensity",	
                    "75th percentile storm depth",
                    "% storm depth > 0.25in",	
                    "% storm depth > 1in",
                    "% storms w/depth > 0.25in",
                    "% storms w/depth > 1in",	
                    "% rainy hours > 0.25in/hr",	
                    "% rainy hours > 1in/hr",	
                    "% total precip > 0.25in/hr",	
                    "% total precip > 1in/hr",	
                    "% storm depth > 0.25in/hr",
                    "% storm depth > 1in/hr",	
                    "% storms w/intensity > 0.25in/hr",	
                    "% storms w/intensity > 1in/hr")

# Grab only desired variables and calculate correlations
small.corr.df         <- met.summary.long.term[,selected_cols]
small.corr            <- as.data.frame(cor(small.corr.df, method = "spearman"))
rownames(small.corr)  <- friendly_names
colnames(small.corr)  <- friendly_names

# Melt data to plot-able format
melted.corr           <- melt(as.matrix(small.corr))
colnames(melted.corr) <- c("x", "y", "corr")
melted.corr$corr      <- round(melted.corr$corr, digits = 2)

# Plot heat map of correlations
plot_obj <- ggplot(melted.corr) + 
            geom_tile(aes(x = x, y = y, fill = corr)) + 
            geom_text(aes(x = x, y = y,
                          label = sprintf("%.02f",corr)),
                      size = (3.5/14)*text.size,
                      family = "Segoe UI Semilight") +
            geom_rect(xmin = as.numeric(melted.corr$x[[1]]) - 0.5,
                      xmax = as.numeric(melted.corr$x[[8]]) + 0.5,
                      ymin = as.numeric(melted.corr$x[[nrow(small.corr)]]) + 0.5, 
                      ymax = as.numeric(melted.corr$x[[nrow(small.corr)-7]]) - 0.5,
                      fill = NA,
                      color = "black",
                      size = 1.2) +
            scale_y_discrete(limits = rev(levels(melted.corr$y))) +
            scale_x_discrete(position = "top") +
            scale_fill_distiller(name = sprintf("Spearmean Correlation"),
                                 palette = "RdBu",
                                 direction = 1,
                                 limits = c(-1,1)) +
            labs(x = "", y = "") +
            guides(fill = guide_colorbar(title.vjust = 0.8)) +
            theme_bw() + 
            theme(text = element_text(family = "Segoe UI Semilight", 
                                      size = text.size),
                  axis.text.x = element_text(angle = 90, hjust=0, vjust = 0.5),
                  legend.position = "bottom",
                  legend.margin = margin(0,0,0,0))

ggsave("results/figures/correlation_matrix.png",
       plot = plot_obj,
       width = 8,
       height = 8,
       units = "in")

# Full Correlations -----------------------------------------------------------
selected_cols <- c("precipitation",	
                   "logETP", 
                   "wet.fraction",	
                   "corr.PtoET0.3day",	
                   "corr.PtoET0.5day",	
                   "corr.PtoET0.7day",	
                   "corr.PtoET0.30day",
                   "burstiness",	
                   "memory", 
                   "nstorms",	
                   "mean.antecedent",	
                   "mean.duration",	
                   "mean.depth",	
                   "mean.intensity.avg",	
                   "mean.intensity.max",	
                   "total.diff.max.avg.intensity",
                   "mean.diff.max.avg.intensity",	
                   "pcnt.stormdepth.depth.over.4.5mm",	
                   "pcnt.stormdepth.depth.over.5mm",	
                   "pcnt.stormdepth.depth.over.5.5mm",	
                   "pcnt.stormdepth.depth.over.6mm",	
                   "pcnt.stormdepth.depth.over.0.25in",	
                   "pcnt.stormdepth.depth.over.7mm",	
                   "pcnt.stormdepth.depth.over.10mm",	
                   "pcnt.stormdepth.depth.over.0.5in",	
                   "pcnt.stormdepth.depth.over.1in",	
                   "pcnt.nstorms.depth.over.4.5mm",	
                   "pcnt.nstorms.depth.over.5mm",	
                   "pcnt.nstorms.depth.over.5.5mm",	
                   "pcnt.nstorms.depth.over.6mm",	
                   "pcnt.nstorms.depth.over.0.25in",	
                   "pcnt.nstorms.depth.over.7mm",	
                   "pcnt.nstorms.depth.over.10mm",
                   "pcnt.nstorms.depth.over.0.5in",	
                   "pcnt.nstorms.depth.over.1in",	
                   "pcnt.wethours.over.4.5mm",	
                   "pcnt.wethours.over.5mm",	
                   "pcnt.wethours.over.5.5mm",	
                   "pcnt.wethours.over.6mm",	
                   "pcnt.wethours.over.0.25in",
                   "pcnt.wethours.over.7mm",	
                   "pcnt.wethours.over.10mm",	
                   "pcnt.wethours.over.0.5in",	
                   "pcnt.wethours.over.1in",	
                   "pcnt.rain.depth.over.4.5mm",	
                   "pcnt.rain.depth.over.5mm",	
                   "pcnt.rain.depth.over.5.5mm",	
                   "pcnt.rain.depth.over.6mm",	
                   "pcnt.rain.depth.over.0.25in",	
                   "pcnt.rain.depth.over.7mm",	
                   "pcnt.rain.depth.over.10mm",	
                   "pcnt.rain.depth.over.0.5in",	
                   "pcnt.rain.depth.over.1in",	
                   "pcnt.stormdepth.intensity.over.4.5mm",	
                   "pcnt.stormdepth.intensity.over.5mm",	
                   "pcnt.stormdepth.intensity.over.5.5mm",	
                   "pcnt.stormdepth.intensity.over.6mm",	
                   "pcnt.stormdepth.intensity.over.0.25in",	
                   "pcnt.stormdepth.intensity.over.7mm",	
                   "pcnt.stormdepth.intensity.over.10mm",	
                   "pcnt.stormdepth.intensity.over.0.5in",
                   "pcnt.stormdepth.intensity.over.1in",	
                   "pcnt.nstorms.intensity.over.4.5mm",	
                   "pcnt.nstorms.intensity.over.5mm",	
                   "pcnt.nstorms.intensity.over.5.5mm",	
                   "pcnt.nstorms.intensity.over.6mm",	
                   "pcnt.nstorms.intensity.over.0.25in",	
                   "pcnt.nstorms.intensity.over.7mm",	
                   "pcnt.nstorms.intensity.over.10mm",	
                   "pcnt.nstorms.intensity.over.0.5in",	
                   "pcnt.nstorms.intensity.over.1in",	
                   "pcnt.50.rain.depth",	
                   "pcnt.60.rain.depth",	
                   "pcnt.67.rain.depth",	
                   "pcnt.70.rain.depth",	
                   "pcnt.75.rain.depth",	
                   "pcnt.80.rain.depth",	
                   "pcnt.85.rain.depth",	
                   "pcnt.90.rain.depth",	
                   "pcnt.95.rain.depth",	
                   "pcnt.50.storm.depth",	
                   "pcnt.60.storm.depth",	
                   "pcnt.67.storm.depth",	
                   "pcnt.70.storm.depth",	
                   "pcnt.75.storm.depth",	
                   "pcnt.80.storm.depth",
                   "pcnt.85.storm.depth",	
                   "pcnt.90.storm.depth",	
                   "pcnt.95.storm.depth")

corr.df      <- met.summary.long.term[,selected_cols]
correlations <- as.data.frame(cor(corr.df, method = "spearman"))
rownames(correlations) <- colnames(corr.df)
colnames(correlations) <- colnames(corr.df)
write.csv(correlations, "results/correlations.csv")
