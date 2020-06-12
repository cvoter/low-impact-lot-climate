# 11_Budyko.R
#
# Creates a plot of PET:P vs. AET:P for all ParFlow model results, colored by
# partitioning angle.
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, sources functions, loads data, sets 
#    constant parameters.
# 2. COLORS AND LABELS. Combines input data frames, defines fill colors and 
#    labels to use on plot.
# 3. PLOT. Creates Budyko plot.
# 4. SAVE. Saves Budyko plot.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(ggplot2) # for plotting
library(ggrepel) # for city/state labels
library(dplyr) # for %>%
library(extrafont) # for font text on plots

# Load data
load("results/in.out.lot.Rda")
load("results/in.out.loc.Rda")
city.locations <- read.csv("data/locations/locations_with_elev.csv")
nlocations     <- length(city.locations$city.index)

# Set parameters
text.size   <- 10
legend.size <- 10
label.size  <- 3

# 2. COLORS AND LABELS ---------------------------------------------------------
fill.color <- in.out.loc %>% select("city.index", "vector.angle")
Budyko.data <- in.out.lot %>% 
               select("city.index","lot.type","ET.precip", "ET0toP", "logETP")
Budyko.data <- merge(Budyko.data, fill.color, by = "city.index")
Budyko.data <- merge(Budyko.data, city.locations, by = "city.index")

#Set labels
Budyko.data$label = sprintf('%s, %s',Budyko.data$city.name, Budyko.data$state)
for (i in 1:nrow(Budyko.data)) {
  if (Budyko.data$city.name[i] %in% 
       c("Baltimore", "Madison", "Oklahoma City", "Phoenix", "El Paso") &
       Budyko.data$lot.type[i] == 'baseline') {
  } else
    Budyko.data$label[i] <- ""
}

# 3. PLOT ----------------------------------------------------------------------
# Get the points on there
plot.Budyko <- ggplot() +
               geom_point(data = Budyko.data, 
                          aes(x = ET0toP,
                              y = ET.precip,
                              color = vector.angle,
                              shape = lot.type),
                          size = 2)

# Connect them with a line
for (i in c(1:nlocations)) {
  plot.Budyko = plot.Budyko + 
                geom_line(data = subset(Budyko.data, 
                                        city.index == i),
                          aes(x = ET0toP,
                              y = ET.precip,
                              color = vector.angle))
}

# Add other stuff to the plot
plot.Budyko <- plot.Budyko +
               scale_shape_manual(labels = c("Baseline", "Low Impact"),
                     values = c(16,17))  +
               geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
               geom_segment(aes(x = 1, xend = 30, y = 1, yend = 1)) +
               scale_color_distiller(palette = "YlGnBu",
                                     direction = 1,
                                     breaks = seq(0, pi/3, pi/9),
                                     limits = c(-pi/9, 4*pi/9),
                                     labels = c("0", 
                                                expression(pi/9), 
                                                expression(2*pi/9), 
                                                expression(pi/3)),
                                     name = "Partitioning\nAngle") +
               scale_y_continuous(expand = c(0,0), 
                                   limits = c(0,1.3)) +
               scale_x_continuous(expand = c(0,0), 
                                  limits = c(0,30)) +
               coord_cartesian(clip = 'off') +
               # geom_text_repel(data = Budyko.data,
               #                 aes(x = ET0toP,
               #                     y = ET.precip,
               #                     label = label),
               #                 color = "darkred",
               #                 box.padding = 0.4,
               #                 point.padding = 0.4,
               #                 segment.color = "darkred",
               #                 size=label.size,
               #                 family="Segoe UI Semibold") +
              labs(x = "PET:P", 
                   y = "AET:P", 
                   shape="Lot", 
                   color="Partitioning\nAngle") + 
              guides(color = guide_colorbar(order = 0),
                     shape = guide_legend(order = 1)) +
              theme_bw() +
              theme(text = element_text(size=text.size, 
                                        family="Segoe UI Semilight"),
                    axis.title = element_text(size=text.size, 
                                              family="Segoe UI Semibold"),
                    legend.title = element_text(size=legend.size, 
                                                family="Segoe UI Semibold"))
plot.Budyko

# 4. SAVE ----------------------------------------------------------------------
for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_budyko.%s",image.type),
         plot = plot.Budyko,
         device = image.type,
         path = "results/figures",
         scale = 1,
         width = 17.8, height = 8, units = "cm",
         dpi = 300)
}
