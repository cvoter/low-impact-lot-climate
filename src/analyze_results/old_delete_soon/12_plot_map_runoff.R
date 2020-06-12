# 07_predict_runoff.R
#
# This script performs the following steps:
# 1. SETUP ENVIRONMENT. Loads libraries, defines directories, sources functions,
#    loads data
# 2. MAKE PREDICTIONS. Use save kriged values to find closest match for met
#    parameters, then count number of years a city falls into each bin.
# 3. PLOT. Generate map of US with pie charts indicating frequency in each
#    category. Save as png and svg.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(ggplot2) # for plotting
library(scatterpie) #for pies on map

# Define project directories
project.dir <- 'J:/git_research/dissertation/ch02_low_impact_lot_climate'
scripts.dir <- sprintf('%s/src/analyze_results', project.dir)
results.dir <- sprintf('%s/results', project.dir)
data.dir <- sprintf('%s/data', project.dir)

# Source functions
runoff_bin_labels <- function(z.upper.transition, z.lower.transition,
                              lower.lim, upper.lim, interval, labels.on) {
  color.breaks <- 100*seq(lower.lim, upper.lim, interval)
  nbreaks <- length(color.breaks)
  
  if (labels.on == 1) {
    fill.label1 <- sprintf("> %d%%",100*z.upper.transition)
    fill.label2 <- sprintf("%d%%-%d%%", 100*z.lower.transition,
                           100*z.upper.transition)
    fill.label3 <- sprintf("< %d%%",100*z.lower.transition)
    fill.labels <- c(fill.label1, fill.label2, fill.label3)
    
    color.labels <- NULL
    for (i in 1:nbreaks) {
      color.labels[i] <- sprintf("%.0f%%",color.breaks[i])
    }
    
  } else {
    fill.labels = c("","","")
    color.labels = rep("",nbreaks)
  }
  return(list(fill=fill.labels, color=color.labels))
}

angle_bin_labels <- function(labels.on) {
  if (labels.on == 1) {
    fill.labels = c("Mostly DD", "Mix", "Mostly ET")
    color.labels = c("0", expression(pi/9), 
                     expression(2*pi/9), expression(pi/3))
  } else {
    fill.labels = c("","","")
    color.labels = c("","","","")
  }
  return(list(fill=fill.labels, color=color.labels))
}

set_colorbar_info <- function(lower, upper, interval, mean, sd, name,
                              palette, min, max) {
  break.values = seq(lower, upper, interval)
  breaks = NULL
  for (value in break.values) {
    mark = (value - mean)/sd
    breaks = c(breaks, mark)
  }
  mid = (lower + upper)/2
  largest.magnitude = max(mid-min,max-mid) + 0.00001*sd
  limits = c((mid-largest.magnitude-mean)/sd, (mid+largest.magnitude-mean)/sd)
  return(list(breaks=breaks, limits=limits, name=name, palette=palette))
}

# Load data
location.file <- sprintf("%s/locations/locations_with_elev.csv", data.dir)
city.locations <- read.csv(location.file)
nlocations = length(city.locations$city.index)
load(sprintf("%s/all.inputs.outputs.Rda", results.dir))

# 2. MAKE PREDICTIONS ---------------------------------------------------------
subset.to.plot <- subset(all.by.location, select = c(city.index, runoff.precip, 
                                                     vector.angle))
subset.to.plot <- merge(city.locations, subset.to.plot, by = "city.index")


# RUNOFF ----------------------------------------------------------------------
z.upper.transition <- 0.25
z.lower.transition <- 0.15
z.upper.limit <- 0.30
z.lower.limit <- 0.10
z.interval <- 0.05

bin.labels <- runoff_bin_labels(z.upper.transition, z.lower.transition,
                                z.lower.limit, z.upper.limit, z.interval, 
                                labels.on = 1)
colorbar.info <- set_colorbar_info(z.lower.limit, z.upper.limit,
                                   z.interval, 0, 1, "", "Blues",
                                   min(subset.to.plot$runoff.precip), 
                                   max(subset.to.plot$runoff.precip))
us <- map_data('state')
plot.map.runoff <- ggplot(us, aes(long, lat)) +
                   geom_map(map=us, aes(map_id=region), 
                            fill="grey60", color="grey") +
                   coord_fixed(1.3) +
                   geom_point(data = subset.to.plot,
                               aes(x = longitude.deg, 
                                   y = latitude.deg, 
                                   fill = runoff.precip),
                               color="black",pch=21, size = 3) + 
                   scale_fill_distiller(palette = colorbar.info$palette,
                                        direction = 1,
                                        limits = colorbar.info$limits,
                                        breaks = colorbar.info$breaks,
                                        labels = bin.labels$color,
                                        name = colorbar.info$name) +
                   theme_void() +
                   theme(text=element_text(size = 10,  
                                           family = "Segoe UI Semilight"),
                   legend.margin = margin(0,0,0,0),
                   legend.box.margin = margin(-30,-45,-30,-45))

for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_runoff_map.%s",image.type),
         plot = plot.map.runoff,
         device = image.type,
         path = sprintf("%s/figures",results.dir),
         scale = 1,
         width = 18, height = 8, units = "cm",
         dpi = 300)
}

# ANGLE ----------------------------------------------------------------------
z.upper.transition <- 2*pi/9
z.lower.transition <- pi/9
z.upper.limit <- pi/3
z.lower.limit <- 0
z.interval <- pi/9

bin.labels <- angle_bin_labels(labels.on = 1)
colorbar.info <- set_colorbar_info(z.lower.limit, z.upper.limit,
                                   z.interval, 0, 1, "", "YlGnBu",
                                   min(subset.to.plot$vector.angle), 
                                   max(subset.to.plot$vector.angle))
us <- map_data('state')
plot.map.angle <- ggplot(us, aes(long, lat)) +
  geom_map(map=us, aes(map_id=region), 
           fill="grey60", color="grey") +
  coord_fixed(1.3) +
  geom_point(data = subset.to.plot,
             aes(x = longitude.deg, 
                 y = latitude.deg, 
                 fill = vector.angle),
             color="black",pch=21, size = 3) + 
  scale_fill_distiller(palette = colorbar.info$palette,
                       direction = 1,
                       limits = colorbar.info$limits,
                       breaks = colorbar.info$breaks,
                       labels = bin.labels$color,
                       name = colorbar.info$name) +
  theme_void() +
  theme(text=element_text(size = 10,  
                          family = "Segoe UI Semilight"),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(-30,-45,-30,-45))


for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_angle_map.%s",image.type),
         plot = plot.map.angle,
         device = image.type,
         path = sprintf("%s/figures",results.dir),
         scale = 1,
         width = 18, height = 8, units = "cm",
         dpi = 300)
}