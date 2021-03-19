# 11_Budyko.R
#
# Creates a plot of PET:P vs. ET:P and PET vs. DD:P for all ParFlow model
# results, colored by partitioning angle.
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, sources functions, loads data, sets 
#    constant parameters.
# 2. COLORS AND LABELS. Combines input data frames, defines fill colors and 
#    labels to use on plot.
# 3. BUDYKO CURVE. Create data frame with standard Budyko curve.
# 4. PLOT MAIN. Creates main figure with PET vs. ET and PET vs. DD plots.
# 5. PLOT INSET. Plots smaller insets with x-axis zoomed in.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(ggplot2) # for plotting
library(ggrepel) # for city/state labels
library(dplyr) # for %>%
library(extrafont) # for font text on plots
library(patchwork) # for combining plots

# Load data
load("results/in.out.lot.Rda")
load("results/in.out.loc.Rda")
city.locations <- read.csv("data/locations/locations_with_elev.csv")
nlocations     <- length(city.locations$city.index)

# Set parameters
text.size   <- 10
legend.size <- 10
label.size  <- 3

# Plot function
plot_Budyko <- function(df, yname, ylabel, Budyko.curve){
  colnames(df)[colnames(df) == yname] <- "ydata"
  plot_obj <- ggplot() +
              geom_point(data = df, 
                         aes(x = ET0toP,
                             y = ydata,
                             fill = vector.angle,
                             shape = lot.type),
                         color = "black",
                         size = 1.5)
  
  # Connect them with a line
  for (i in c(1:nlocations)) {
    plot_obj <- plot_obj + 
                geom_line(data = subset(df, city.index == i),
                          aes(x = ET0toP,
                              y = ydata,
                              color = vector.angle))
  }
  
  # Add Budyko curve & limits
  if (yname == "ET.precip") {
    plot_obj <- plot_obj +
                geom_line(data = Budyko.curve,
                          aes(x = PET/P, y = AET/P)) +
                geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
                geom_hline(yintercept = 1, linetype = "dashed")
  }
  
  # Add other stuff to the plot
  plot_obj <- plot_obj +
              scale_shape_manual(name = "Lot",
                                 labels = c("Baseline", "Low Impact"),
                                 values = c(21,24)) +
              scale_color_distiller(palette = "YlGnBu",
                                    direction = 1,
                                    breaks = seq(0, pi/3, pi/9),
                                    limits = c(0, pi/3),
                                    labels = c(expression(""<=0), 
                                               expression(pi/9), 
                                               expression(2*pi/9), 
                                               expression("">=pi/3)),
                                    name = "Partitioning\nAngle") +
              scale_fill_distiller(palette = "YlGnBu",
                                    direction = 1,
                                    breaks = seq(0, pi/3, pi/9),
                                    limits = c(0, pi/3),
                                    labels = c(expression(""<=0), 
                                               expression(pi/9), 
                                               expression(2*pi/9), 
                                               expression("">=pi/3)),
                                    name = "Partitioning\nAngle") +
              scale_y_continuous(expand = c(0,0), 
                                 limits = c(-0.03,1.3)) +
              scale_x_continuous(expand = c(0,0), 
                                 limits = c(0,30)) +
              geom_text_repel(data = df,
                              aes(x = ET0toP,
                                  y = ydata,
                                  label = label),
                              color = "darkred",
                              box.padding = 0.4,
                              point.padding = 0.4,
                              segment.color = "darkred",
                              size=label.size,
                              family="Segoe UI Semibold") +
              labs(x = "PET:P", y = ylabel) + 
              # guides(color = guide_colorbar(order = 0),
              #        shape = guide_legend(order = 1)) +
              theme_bw() +
              theme(text = element_text(size=text.size, 
                                        family="Segoe UI Semilight"),
                    axis.title = element_text(size=text.size, 
                                              family="Segoe UI Semibold"),
                    legend.title = element_text(size=legend.size, 
                                                family="Segoe UI Semibold"))
  return(plot_obj)
}

# 2. COLORS AND LABELS ---------------------------------------------------------
fill.color  <- in.out.loc %>% select("city.index", "vector.angle")
Budyko.data <- in.out.lot %>% 
               select("city.index","lot.type","ET.precip", "ET0toP", "logETP",
                      "runoff.precip", "drainage.precip")
Budyko.data <- merge(Budyko.data, fill.color, by = "city.index")
Budyko.data <- merge(Budyko.data, city.locations, by = "city.index")
Budyko.data$vector.angle[Budyko.data$vector.angle < 0] <- 0
Budyko.data$vector.angle[Budyko.data$vector.angle > pi/3] <- pi/3

#Set labels
Budyko.data$label = sprintf('%s, %s',Budyko.data$city.name, Budyko.data$state)
for (i in 1:nrow(Budyko.data)) {
  if (Budyko.data$city.name[i] %in%
       c("Baltimore", "Madison", "Oklahoma City", "Phoenix", "El Paso") &
       Budyko.data$lot.type[i] == 'baseline') {
  } else
    Budyko.data$label[i] <- ""
}
# Budyko.data$label[Budyko.data$lot.type == "baseline"] <- ""
# Budyko.data$label[Budyko.data$ET0toP <= 1.5] <- ""

# 3. BUDYKO CURVE --------------------------------------------------------------
n <- 1.8
Budyko.curve     <- data.frame(P = seq(0,3000,10),
                               PET = seq(0,3000,10))
Budyko.curve     <- expand.grid(Budyko.curve)
Budyko.curve$AET <- Budyko.curve$P*Budyko.curve$PET/
                    ((Budyko.curve$P^n + Budyko.curve$PET^n)^(1/n))

# 4. PLOT MAIN -----------------------------------------------------------------
pET     <- plot_Budyko(Budyko.data, "ET.precip", "ET:P", Budyko.curve) + 
           theme(axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.x = element_blank())
pDD     <- plot_Budyko(Budyko.data, "drainage.precip", "DD:P", Budyko.curve) 
pBudyko <- pET + pDD + plot_layout(ncol = 1, guides = "collect")

for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_budyko.%s",image.type),
         plot = pBudyko,
         device = image.type,
         path = "results/figures",
         scale = 1,
         width = 16, height = 11.4, units = "cm",
         # width = 9, height = 8, units = "in",
         dpi = 300)
}

# 5. PLOT INSET ----------------------------------------------------------------
pET     <- plot_Budyko(Budyko.data, "ET.precip", "ET:P", Budyko.curve) + 
           scale_y_continuous(limits = c(0.25, 1), expand = c(0,0)) + 
           scale_x_continuous(limits = c(0.8, 1.5), expand = c(0,0))
pDD     <- plot_Budyko(Budyko.data, "drainage.precip", "DD:P", Budyko.curve) + 
           scale_y_continuous(limits = c(0, 0.75), expand = c(0,0)) + 
           scale_x_continuous(limits = c(0.8, 1.5), expand = c(0,0))
pBudyko <- pET + pDD + plot_layout(ncol = 1, guides = "collect") & 
           theme(legend.position = "none")

for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_budyko_inset.%s",image.type),
         plot = pBudyko,
         device = image.type,
         path = "results/figures",
         scale = 1,
         width = 7, height = 6.4, units = "cm",
         # width = 5, height = 4, units = "in",
         dpi = 300)
}
