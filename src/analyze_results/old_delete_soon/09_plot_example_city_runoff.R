# 09_plot_example_city_runoff.R
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, defines directories, sources functions,
#    loads data
# 2. DEFINE PARAMETERS 
# 3. GET OR LOAD HOURLY AND STORM DATA
# 4. CALCULATE ALL DESCRIPTORS
# 5. SAVE MET SUMMARY


# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries
library(ggplot2) # for plotting
library(cowplot) # for multi-plot plot
library(extrafont) # for font text on plots

# Define project directories
project.dir <- 'J:/git_research/dissertation/ch02_low_impact_lot_climate'
scripts.dir <- sprintf('%s/src/analyze_results', project.dir)
results.dir <- sprintf('%s/results', project.dir)
data.dir <- sprintf('%s/data', project.dir)

# Source functions
add_plot_details <- function(plot, total.runoff.reduction){
  plot = plot + 
         geom_point(aes(x = 100*runoff.delta/runoff.baseline, 
                        y = intensity.avg), 
                    shape = 16, size = 2, color = "black") +
         scale_y_continuous(limits = c(0,12), expand = c(0,0)) +
         scale_x_continuous(limits = c(20,100), expand = c(0,0)) +
         labs(x = "Reduction in Runoff from Storm (%)",
         y = "Average Storm \nIntensity (mm/hr)") +
         annotate(geom="text", x=0.5*(100-20)+20, y=11, 
                  label=sprintf("-%.0f%%",total.runoff.reduction), 
                  color="black", size = 3, family="Segoe UI Semibold") +
         theme_bw() +
         theme(plot.title = element_text(hjust = 0.5,
                                         family="Segoe UI Semibold"),
               text = element_text(size=10, 
                                   family="Segoe UI Semilight"),
               axis.title = element_text(size=8, 
                                         family="Segoe UI Semibold"))
  return(plot)
}

get_cum_precip <- function(hourly.precip) {
  hourly.precip = hourly.precip[order(hourly.precip$precipitation),]
  for (i in 1:length(hourly.precip$precipitation)) {
    hourly.precip$cum[i] = sum(hourly.precip$precipitation[1:i])
    hourly.precip$cum.percent[i] = 100*hourly.precip$cum[i]/
      sum(hourly.precip$precipitation)
  }
  return(hourly.precip)
}

get_90th_intensity <- function(hourly.precip) {
  interp = approx(hourly.precip$cum.percent,
                  hourly.precip$precipitation,
                  xout=90)
  intensity = interp$y
  return(intensity)
}

add_plot_details_percentile <- function(plot, intensity, 
                                        total.runoff.reduction) { 
  plot <- plot +
          scale_x_continuous(expand = c(0,0),
                             limits = c(0, 35)) +
          scale_y_continuous(expand = c(0,0)) +
          annotate(geom="text", x=(intensity+6), y=85, 
                   label=sprintf("%.1f mm/hr", intensity), 
                   color="black", size = 3, family="Segoe UI Semibold") +
          annotate(geom="text", x=28, y=25, 
                   label=sprintf("Runoff: -%.0f%%", total.runoff.reduction), 
                   color="black", size = 3, family="Segoe UI Semibold") +
          labs(x = "Precipitation Intensity (mm/hr)",
              y = "Cumulative Precipitation (%)") +
          theme_bw() +
          theme(text = element_text(size=10, family="Segoe UI Semilight"),
                axis.title = element_text(size=8, family="Segoe UI Semibold"))
  return(plot)
}

# Load data
location.file <- sprintf("%s/locations/locations_with_elev.csv", data.dir)
city.locations <- read.csv(location.file)
load(sprintf("%s/all.city.storms.runoff.Rda", results.dir))
load(sprintf("%s/all.inputs.outputs.Rda", results.dir))
load(sprintf("%s/met/hourly.precipitation.ET0.Rda", results.dir))

# 2. SUBSET RESULTS BY EXAMPLE CITIES -----------------------------------------
view.cities = NULL
view.cities$city.name = c("Baltimore", "Madison", "Phoenix")
view.cities = as.data.frame(view.cities)
for (i in 1:length(view.cities$city.name)) {
  which.city = which(city.locations$city.name == 
                       as.character(view.cities$city.name[i]))
  view.cities$city.index[i] = city.locations$city.index[which.city]
  city.state = city.locations$state[which.city]
  view.cities$city.title[i] = sprintf("%s, %s", 
                                      view.cities$city.name[i], city.state)
}

# City 1 Results
city1.precip = subset(hourly.precipitation.ET0, 
                      city.index == view.cities$city.index[1])
city1.precip = get_cum_precip(city1.precip)
city1.90th.intensity = get_90th_intensity(city1.precip)
city1.vline = NULL
city1.vline$x = c(city1.90th.intensity, city1.90th.intensity)
city1.vline$y = c(0,90)
city1.hline = NULL
city1.hline$x = c(0, city1.90th.intensity)
city1.hline$y = c(90,90)
# 
# city1.storms = subset(all.city.storms.runoff, 
#                       city.index == view.cities$city.index[1])
# city1.mean.storm = mean(city1.storms$intensity.avg)
# city1.median.runoff = median(100*city1.storms$runoff.delta/
#                                city1.storms$runoff.baseline, na.rm = TRUE)
city1.fluxes = subset(all.by.location,
                      city.index == view.cities$city.index[1])
city1.total.runoff = 100*city1.fluxes$runoff.precip

# City 2 Results
city2.precip = subset(hourly.precipitation.ET0, 
                      city.index == view.cities$city.index[2])
city2.precip = get_cum_precip(city2.precip)
city2.90th.intensity = get_90th_intensity(city2.precip)
city2.vline = NULL
city2.vline$x = c(city2.90th.intensity, city2.90th.intensity)
city2.vline$y = c(0,90)
city2.hline = NULL
city2.hline$x = c(0, city2.90th.intensity)
city2.hline$y = c(90,90)
# 
# city2.storms = subset(all.city.storms.runoff, 
#                       city.index == view.cities$city.index[2])
city2.fluxes = subset(all.by.location,
                      city.index == view.cities$city.index[2])
# city2.mean.storm = mean(city2.storms$intensity.avg)
# city2.median.runoff = median(100*city2.storms$runoff.delta/
                               # city2.storms$runoff.baseline,na.rm = TRUE)
city2.total.runoff = 100*city2.fluxes$runoff.precip

# City 3 Results
city3.precip = subset(hourly.precipitation.ET0, 
                      city.index == view.cities$city.index[3])
city3.precip = get_cum_precip(city3.precip)
city3.90th.intensity = get_90th_intensity(city3.precip)
city3.vline = NULL
city3.vline$x = c(city3.90th.intensity, city3.90th.intensity)
city3.vline$y = c(0,90)
city3.hline = NULL
city3.hline$x = c(0, city3.90th.intensity)
city3.hline$y = c(90,90)
# 
# city3.storms = subset(all.city.storms.runoff, 
#                       city.index == view.cities$city.index[3])
# city3.mean.storm = mean(city3.storms$intensity.avg)
# city3.median.runoff = median(100*city3.storms$runoff.delta/
#                                city3.storms$runoff.baseline)
 city3.fluxes = subset(all.by.location, 
                       city.index == view.cities$city.index[3], na.rm = TRUE)
 city3.total.runoff = 100*city3.fluxes$runoff.precip

# 3. PLOT RUNOFF --------------------------------------------------------------
plot.city1 <- ggplot()  +
              geom_line(data = city1.precip,
                        aes(x = precipitation,
                            y = cum.percent)) +
              geom_line(data = as.data.frame(city1.hline),
                        aes(x = x, y = y),
                        linetype = 2) +
              geom_line(data = as.data.frame(city1.vline),
                        aes(x = x, y = y),
                        linetype = 2) +
              geom_point(aes(x = city1.90th.intensity,
                             y = 90),
                         color = "red", shape = 8)
plot.city1 <- add_plot_details_percentile(plot.city1, city1.90th.intensity, 
                                          city1.total.runoff)

plot.city2 <- ggplot()+
              geom_line(data = city2.precip,
                        aes(x = precipitation,
                            y = cum.percent)) +
              geom_line(data = as.data.frame(city2.hline),
                        aes(x = x, y = y),
                        linetype = 2) +
              geom_line(data = as.data.frame(city2.vline),
                        aes(x = x, y = y),
                        linetype = 2) +
              geom_point(aes(x = city2.90th.intensity,
                             y = 90),
                         color = "red", shape = 8)
plot.city2 <- add_plot_details_percentile(plot.city2, city2.90th.intensity,
                                          city2.total.runoff)

plot.city3 <- ggplot() +
              geom_line(data = city3.precip,
                        aes(x = precipitation,
                            y = cum.percent)) +
              geom_line(data = as.data.frame(city3.hline),
                        aes(x = x, y = y),
                        linetype = 2) +
             geom_line(data = as.data.frame(city3.vline),
                       aes(x = x, y = y),
                       linetype = 2) +
              geom_point(aes(x = city3.90th.intensity,
                             y = 90),
                         color = "red", shape = 8)
plot.city3 <- add_plot_details_percentile(plot.city3, city3.90th.intensity,
                                          city3.total.runoff)

runoff.grid <- ggdraw(plot_grid(plot.city1, plot.city2, plot.city3, ncol=3))

for (image.type in c("svg","png")) {
  ggplot2::ggsave(sprintf("plot_city_runoff.%s",image.type), 
         plot = runoff.grid, 
         device = image.type, 
         path = sprintf("%s/figures",results.dir),
         scale = 1, 
         width = 8, height = 1.8, units = "in",
         dpi = 300)
}
