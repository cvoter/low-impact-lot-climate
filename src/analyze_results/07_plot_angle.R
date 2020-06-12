# 07_plot_angle.R
#
# Creates a plot of precipitation vs. PET based on PLSR model with ParFlow model
# results shown as points.
#
# This script performs the following steps:
# 1. SETUP. Loads libraries, sources functions, loads data, sets 
#    constant parameters.
# 2. PREDICT. Generates range of precipitation and PET data, then generates
#    predictions of partitioning angle using PLSR model. Performs 
#    transformations to scale and unscale data. Adds labels for example cities.
# 3. PLOT. Creates partitioning angle plot.
# 4. SAVE. Saves plot.

# 1. SETUP ---------------------------------------------------------------------
# Load libraries 
library(dplyr) # for %>%
library(extrafont) # for font text on plots
library(ggplot2)
library(plotly)
library(pls)
library(stringr)

source("src/analyze_results/classify_results.R")

load("results/in.out.loc.Rda")
load("results/plsr.model.Rda")
load("results/met/met.summary.Rda") 
load("results/predict.scaled.Rda") 

text.size <- 10
ex_cities <- c("Baltimore", "Madison", "Oklahoma City", "Phoenix", "El Paso")

# 2. CREATE PLOTTING DATA ------------------------------------------------------
ls <- list()
for (var in c("logETP", "precipitation")) {
  ls[[var]] <- seq(min(predict.scaled[,var]),
                   max(predict.scaled[,var]),
                   length.out = 250)
}
df <- expand.grid(ls)

# Predict Y
Y.out <- predict(plsr.model$vector.angle, 
                 newdata = df)
Y.out <- as.data.frame(Y.out)
colnames(Y.out) <- c("Y")
df <- cbind(df, Y.out)

# Unscale and bin angle
PET.mean  <- mean(in.out.loc$logETP)
PET.sd    <- sd(in.out.loc$logETP)
df$logETP <- df$logETP*PET.sd + PET.mean

P.mean <- mean(in.out.loc$precipitation)
P.sd   <- sd(in.out.loc$precipitation)
df$precipitation <- df$precipitation*P.sd + P.mean

flux.mean <- mean(in.out.loc$vector.angle)
flux.sd   <- sd(in.out.loc$vector.angle)
df$Y      <- df$Y*flux.sd + flux.mean
df$bin    <- classify_results(df$Y, break.upper = 2*pi/9, break.lower = pi/9)

# Label example cities
in.out.loc$label <- sprintf("%s, %s", in.out.loc$city.name, in.out.loc$state)
in.out.loc$label[!in.out.loc$city.name %in% ex_cities] <- ""

# 3. PLOT ----------------------------------------------------------------------
plot_obj <- ggplot() +
            geom_tile(data = df,
                      aes(x = logETP, 
                          y = precipitation, 
                          fill = as.factor(bin))) +
            geom_point(data = in.out.loc,
                       aes(x = logETP, 
                           y = precipitation,
                           color = vector.angle),
                       shape = 16, 
                       size = 1.5) +
            geom_point(data = in.out.loc,
                       aes(x = logETP, 
                           y = precipitation),
                       color = "black",
                       shape = 1, 
                       size = 1.5) +
            geom_point(data = filter(in.out.loc, label != ""),
                       aes(x = logETP, 
                           y = precipitation),
                       color = "darkred",
                       shape = 1, 
                       size = 1.5) +
            # Label Cities - text
            geom_text_repel(data = in.out.loc,
                            aes(x = logETP,
                                y = precipitation,
                                label = label),
                            color = "darkred",
                            inherit.aes = FALSE,
                            box.padding = 0.4,
                            point.padding = 0.4,
                            segment.color = "darkred",
                            size=1.5,
                            family="Segoe UI Semibold") +
            labs(x = "PET:P", y = "Annual Precipitation (mm)") +
            scale_x_continuous(expand = c(0,0),
                               breaks = log10(c(1, 10)),
                               labels = c("1", "10"),
                               minor_breaks = log10(c(0.1,1,2,3,4,5,6,7,8,9,10,20))) +
            scale_y_continuous(expand = c(0,0)) +
            scale_color_distiller(palette = "YlGnBu",
                                  direction = 1,
                                  limits = c(-pi/9, 4*pi/9),
                                  breaks = c(0, pi/9, 2*pi/9, pi/3),
                                  labels = c("0", expression(pi/9), 
                                             expression(2*pi/9), expression(pi/3)),
                                  name = "") +
            scale_fill_brewer(name = "",
                              palette = "YlGnBu",
                              direction = -1,
                              labels = c("Mostly DD", "Mix", "Mostly ET")) +
            guides(color = guide_colorbar(order=1),
                   fill = guide_legend(order=2)) +
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
ggsave("plot_angle.svg",
       plot = plot_obj,
       device = "svg",
       path = "results/figures",
       scale = 1,
       width = 8.7, height = 6, units = "cm",
       dpi = 300)
ggsave("plot_angle.png",
       plot = plot_obj,
       device = "png",
       path = "results/figures",
       scale = 1,
       width = 8.7, height = 6, units = "cm",
       dpi = 300)

# ggsave("plot_angle_labels.svg",
#        plot = plot_obj,
#        device = "svg",
#        path = "results/figures",
#        scale = 1,
#        width = 6.5, height = 4.5, units = "in",
#        dpi = 300)
# ggsave("plot_angle_labels.png",
#        plot = plot_obj,
#        device = "png",
#        path = "results/figures",
#        scale = 1,
#        width = 6.5, height = 4.5, units = "in",
#        dpi = 300)
