#06_plot_runoff.R
#
# Creates a plot of PC1 vs. PC2 based on PLSR model for reduction in runoff due
# to LID practices with ParFlow model results shown as points.
#
# This script performs the following steps:
# 1. SETUP. Loads libraries, sources functions, loads data, sets 
#    constant parameters.
# 2. PREDICT. Generates range of input data, then generates predictions of
#    reduction in runoff using PLSR model. Performs transformations to scale and
#    unscale data. Adds labels for example cities.
# 3. PLOT. Creates runoff reduction plot.
# 4. SAVE. Saves plot.
# 5. VARIABLE AXES. Creates and saves plot indicating the direction of increase
#    realtive to x and y axes for all 4 variables included in PLSR model.

# 1. SETUP ---------------------------------------------------------------------
# Load libraries 
library(dplyr) # for %>%
library(extrafont) # for font text on plots
library(ggplot2)
library(ggrepel)
library(plotly)
library(pls)
library(stringr)

source("src/analyze_results/classify_results.R")

# Load data
load("results/plsr.model.Rda")
load("results/plsr.results.Rda")
load("results/met/met.summary.Rda")  # met.summary
load("results/predict.scaled.Rda")
load("results/in.out.loc.Rda")
load("results/in.out.loc.scaled.Rda")
city.locations <- read.csv("data/locations/locations_with_elev.csv")

# Parameters
text.size <- 10
ex_cities <- c("Baltimore", "Madison", "Oklahoma City", "Phoenix", "El Paso")

# 2. PREDICT -------------------------------------------------------------------
# Get range of variables
vars.options <- as.character(plsr.model$runoff.precip$terms[[3]])
vars.options <- unlist(str_split(vars.options, " \\+ "))
vars.options <- vars.options[vars.options != "+"]

ls <- list()
for (var in vars.options) {
  ls[[var]] <- seq(min(predict.scaled[,var]),
                  max(predict.scaled[,var]),
                  length.out = 10)
}
df <- expand.grid(ls)

# Calculate components
PCs   <- predict(plsr.model$runoff.precip, 
                 newdata = df,
                 type = "scores")
PCs <- as.data.frame(PCs)
colnames(PCs) <- c("PC1", "PC2")

# Get range of compontents
ls <- list()
ls$X1 <- seq(min(PCs$PC1),
              max(PCs$PC1),
              length.out = 200)
ls$X2 <- seq(min(PCs$PC2),
              max(PCs$PC2),
              length.out = 200)
df    <- expand.grid(ls)

# Calculate runoff
df$PC1 <- df$X1*plsr.model$runoff.precip$Yloadings[1,1]
df$PC2 <- df$X2*plsr.model$runoff.precip$Yloadings[1,2]
##HIJAICKING!!!
df     <- expand.grid(list(PC1 = seq(-2.5, 2, 0.05),
                        PC2 = seq(-1.5, 2, 0.05)))
####
df$Y   <- df$PC1+df$PC2+plsr.model$runoff.precip$Ymeans

# Unscale and bin runoff
flux.mean <- mean(in.out.loc$runoff.precip)
flux.sd   <- sd(in.out.loc$runoff.precip)
df$Y      <- df$Y*flux.sd + flux.mean
df$bin    <- classify_results(df$Y, break.upper = 0.25, break.lower = 0.15)

# Actual Model PCs
parflow <- predict(plsr.model$runoff.precip, 
                 newdata = in.out.loc.scaled,
                 type = "scores")
parflow <- as.data.frame(parflow)
colnames(parflow) <- c("X1", "X2")
parflow$PC1 <- parflow$X1*plsr.model$runoff.precip$Yloadings[1,1]
parflow$PC2 <- parflow$X2*plsr.model$runoff.precip$Yloadings[1,2]
parflow$Y   <- in.out.loc.scaled$runoff.precip*flux.sd + flux.mean 
parflow$bin <- classify_results(parflow$Y, break.upper = 0.25, break.lower = 0.15)
parflow$label <- sprintf("%s, %s", in.out.loc$city.name, in.out.loc$state)
parflow$label[!in.out.loc$city.name %in% ex_cities] <- ""


# Get variable loadings
var.loads <- plsr.model$runoff.precip$loadings[,1:2]
var.loads <- as.data.frame(var.loads)
colnames(var.loads) <- c("PC1", "PC2")
var.loads$var <- rownames(var.loads)
rownames(var.loads) <- NULL
var.zeros <- var.loads
var.zeros$PC1 <- 0
var.zeros$PC2 <- 0
var.loads <- rbind(var.loads, var.zeros)

# 3. PLOT ----------------------------------------------------------------------
plot_obj <- ggplot() +
            geom_tile(data = df,
                      aes(x = PC1, 
                          y = PC2, 
                          fill = as.factor(bin))) +
            geom_point(data = parflow,
                       aes(x = PC1, 
                           y = PC2,
                           color = Y),
                       shape = 16, 
                       size = 1.5) +
            geom_point(data = parflow,
                       aes(x = PC1, 
                           y = PC2),
                       color = "black",
                       shape = 1, 
                       size = 1.5) +
            geom_point(data = filter(parflow, label != ""),
                       aes(x = PC1, 
                           y = PC2,
                           color = Y),
                       shape = 16, 
                       size = 1.5) +
            geom_point(data = filter(parflow, label != ""),
                       aes(x = PC1, 
                           y = PC2),
                       color = "darkred",
                       shape = 1, 
                       size = 1.5) +
            geom_text_repel(data = parflow,
                            aes(x = PC1,
                                y = PC2,
                                label = label),
                            color = "darkred",
                            inherit.aes = FALSE,
                            box.padding = 0.4,
                            point.padding = 0.4,
                            segment.color = "darkred",
                            size=1.5,
                            family="Segoe UI Semibold") +
            scale_y_continuous(expand = c(0,0)) +
            scale_x_continuous(expand = c(0,0)) +
            scale_color_distiller(palette = "Blues",
                                  direction = 1,
                                  limits = c(0.05, 0.35),
                                  breaks = seq(0.10, 0.30, 0.05),
                                  labels = sprintf("%.0f%%", 
                                                   100*seq(0.10, 0.30, 0.05)),
                                  name = "") +
            scale_fill_brewer(name = "",
                              palette = "Blues",
                              direction = -1,
                              labels = c("> 25%", "15% - 25%", "< 15%")) +
            theme_bw() +
            theme(text = element_text(size=text.size, 
                                      family="Segoe UI Semilight"),
                  axis.title = element_text(size=text.size, 
                                            family="Segoe UI Semibold"),
                  legend.key.size = unit(0.5, "cm"),
                  legend.margin = margin(-10,0,-10,0),
                  legend.box.margin = margin(-20,-5,-10,-8),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank())

# 4. SAVE ----------------------------------------------------------------------
ggsave("plot_runoff.svg",
       plot = plot_obj,
       device = "svg",
       path = "results/figures",
       scale = 1,
       width = 8.7, height = 6, units = "cm",
       dpi = 300)
ggsave("plot_runoff.png",
       plot = plot_obj,
       device = "png",
       path = "results/figures",
       scale = 1,
       width = 8.7, height = 6, units = "cm",
       dpi = 300)

# ggsave("plot_runoff_labels.svg",
#        plot = plot_obj,
#        device = "svg",
#        path = "results/figures",
#        scale = 1,
#        width = 6.5, height = 4.5, units = "in",
#        dpi = 300)
# ggsave("plot_runoff_labels.png",
#        plot = plot_obj,
#        device = "png",
#        path = "results/figures",
#        scale = 1,
#        width = 6.5, height = 4.5, units = "in",
#        dpi = 300)

# 5. VARIABLE AXES -------------------------------------------------------------
plot_obj <- ggplot() +
            geom_blank(data = df, aes(x = PC1, y = PC2)) +
            geom_line(data = var.loads,
                      aes(x = PC1,
                          y = PC2,
                          group = var)) +
            scale_y_continuous(expand = c(0,0)) +
            scale_x_continuous(expand = c(0,0)) +
            theme_bw() +
            theme(text = element_text(size=text.size,
                                      family="Segoe UI Semilight"),
                  axis.title = element_text(size=text.size,
                                            family="Segoe UI Semibold"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
ggsave("plot_runoff_vars.svg",
       plot = plot_obj,
       device = "svg",
       path = "results/figures",
       scale = 1,
       width = 8.7, height = 6, units = "cm",
       dpi = 300)

ggsave("plot_runoff_vars.png",
       plot = plot_obj,
       device = "png",
       path = "results/figures",
       scale = 1,
       width = 8.7, height = 6, units = "cm",
       dpi = 300)
