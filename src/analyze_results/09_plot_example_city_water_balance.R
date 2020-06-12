# 09_plot_example_city_water_balance.R
#
# Creates bar plot of change in runoff, deep drainage, and ET as a percent of
# precipitation for 5 example cities.
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, defines directories, sources functions,
#    loads data
# 2. EXTRACT EXAMPLE CITIES. Subsets larger results into just the example cities 
#    of interest. 
# 3. PLOT BALANCE. Bar plot of change in runoff, deep drainage, and ET as
#    percent of precipitation.
# 4. SAVE. Save plot.


# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries
library(ggplot2) # for plotting
library(patchwork) # for multi-plot plot
library(reshape2) # for melting
library(extrafont) # for font text on plots

# Load data
location.file  <- "data/locations/locations_with_elev.csv"
city.locations <- read.csv(location.file)
load("results/in.out.loc.Rda")

# 2. EXTRACT EXAMPLE CITIES ---------------------------------------------------
view.cities <- NULL
view.cities$city.name <- c("Baltimore", "Madison", "Oklahoma City", 
                          "Phoenix", "El Paso")
view.cities <- as.data.frame(view.cities)
for (i in 1:length(view.cities$city.name)) {
  which.city                <- which(city.locations$city.name == 
                                       as.character(view.cities$city.name[i]))
  view.cities$city.index[i] <- city.locations$city.index[which.city]
  city.state                <- city.locations$state[which.city]
  view.cities$city.title[i] <- sprintf("%s, %s", 
                                      view.cities$city.name[i], city.state)
}

example.cities <- subset(in.out.loc, 
                         select = c(city.index, runoff.precip, 
                                    drainage.precip, ET.precip),
                         city.index %in%  view.cities$city.index)
example.cities$city.index <- factor(example.cities$city.index, 
                                    levels=view.cities$city.index)
example.cities <- merge(example.cities, view.cities, by = "city.index")
colnames(example.cities) <- c("city.index","Runoff","Drainage",
                             "ET","city.name","city.title")

melted.cities <- melt(example.cities, 
                     id.vars = c("city.index","city.name", "city.title"))

# 3. PLOT BALANCES ------------------------------------------------------------
plot_city <- function(city.fluxes){
  plot <- ggplot(city.fluxes) + 
         geom_col(aes(x = variable, y = value),
                  fill = "grey40") +
         labs(x = "", y = "", title = "") +
         scale_y_continuous(limits = c(0,0.31),
                            expand = c(0,0),
                            labels = scales::percent) +
         theme_bw() +
         theme(text = element_text(size=12, family="Segoe UI Semilight"),
               panel.grid = element_line(color = "white"),
               panel.background = element_rect(fill = "transparent", colour = NA),  
               plot.background = element_rect(fill = "transparent", colour = NA))
  return(plot)
}

city1 <- subset(melted.cities, city.index == view.cities$city.index[1])
city2 <- subset(melted.cities, city.index == view.cities$city.index[2])
city3 <- subset(melted.cities, city.index == view.cities$city.index[3])
city4 <- subset(melted.cities, city.index == view.cities$city.index[4])
city5 <- subset(melted.cities, city.index == view.cities$city.index[5])

p1    <- plot_city(city1)
p2    <- plot_city(city2)
p3    <- plot_city(city3)
p4    <- plot_city(city4)
p5    <- plot_city(city5)

grid_plot <- p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 5) & 
  theme(text = element_text(size=12, family="Segoe UI Semilight"),
        panel.grid = element_line(color = "white"),
        panel.background = element_rect(fill = "transparent", colour = NA),  
        plot.background = element_rect(fill = "transparent", colour = NA))

# 4. SAVE ----------------------------------------------------------------------
for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_city_balances.%s",image.type), 
         plot = grid_plot, 
         device = image.type, 
         path = "results/figures",
         scale = 1, 
         width = 11.25, height = 2.3, units = "in",
         dpi = 300)
}
