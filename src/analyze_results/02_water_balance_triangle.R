# 02_water_balance_triangle.R
#
# Plots water balance results (runoff, deep drainage, evapotranspiration) on a
# triangle plot, then calculates angle, classifies type of results (mostly DD,
# mix, or mostly ET) and saves this information to the inputs & outputs data
# frames.
#
# Balance types:
#   1 = above upper.transition (mostly DD)
#   2 = mix
#   3 = below lower.transition (mostly ET)
#
# 1. SETUP ENVIRONMENT. Defines directories, loads data
# 2. MAKE TRIANGLE. Specify fluxes used for water balance triangle and plot 
#                   triangle.
# 3. CALCULATE VECTORS. Convert ternary coordinate to cartesian coordinates and
#                       calculate vector between each baseline/low-impact pair.
# 4. SAVE. Both water balance triangle and vector info.

# 1. SETUP ENVIRONMENT --------------------------------------------------------
# Load libraries 
library(ggplot2)   # for plotting
library(ggtern)    # for ternary plot
library(extrafont) # for font text on plots
library(dplyr)     # for subsetting

# Source functions
source("src/analyze_results/plot_triangle.R")

# Load data
load("results/in.out.lot.Rda")
load("results/in.out.loc.Rda")
load("results/in.out.loc.scaled.Rda")
nlocations <- length(in.out.loc$city.index)

text.size <- 10

# 2. MAKE TRIANGLE ------------------------------------------------------------
plot.info           <- in.out.lot %>%
                       select(city.index, lot.type, runoff.RZ, drainage.RZ, 
                              ET.RZ, logETP)
colnames(plot.info) <- c("city.index", "lot.type", "runoff", "drainage", 
                         "ET", "logETP")
plot.triangle        <- plot_triange(plot.info, nlocations, text.size)

# 3. CALCULATE VECTORS --------------------------------------------------------
# Define x, y, z for ternary coordinates, convert to cartesian
tmp      <- plyr::rename(plot.info, 
                         c("runoff"="x","drainage"="y","ET"="z"))
crd.tern <- coord_tern()
crd.cart <- tlr2xy(tmp, crd.tern)

# Calculate magnitude & angle (rad) of vector btwn baseline & low-impact results
triangle.stats <- NULL
for ( i in 1:nlocations) {
  # Extract x & y coordinates
  baseline   <- crd.cart %>%
                filter(lot.type == 'baseline' & city.index == i) %>%
                select(x, y)
  low.impact <- crd.cart %>%
                filter(lot.type == 'low_impact' & city.index == i) %>%
                select(x, y)
  
  # Vector between two points
  ydiff        <- low.impact$y - baseline$y
  xdiff        <- low.impact$x - baseline$x
  magnitude    <- sqrt(ydiff^2 + xdiff^2)
  angle        <- atan(ydiff/xdiff)
  if (angle >= 2*pi/9) {
    balance.type <- 1 # mostly (2/3) DD
  } else if (angle < 2*pi/9 & angle > pi/9) {
    balance.type <- 2 # mix
  } else {
    balance.type <- 3 # mostly (2/3) ET
  }
  
  # Add this city vector magnitudes and angles to overall data frame
  city.vector     <- c(i, magnitude, angle, balance.type)
  triangle.stats  <- as.data.frame(rbind(triangle.stats, city.vector))
}
colnames(triangle.stats)     <- c("city.index", 
                                  "vector.magnitude", 
                                  "vector.angle",
                                  "balance.type")
rownames(triangle.stats)     <- NULL
triangle.scaled              <- as.data.frame(scale(triangle.stats))
triangle.scaled$city.index   <- triangle.stats$city.index
triangle.scaled$balance.type <- triangle.stats$balance.type

# 4. SAVE VECTOR INFO ---------------------------------------------------------
in.out.loc        <- merge(in.out.loc, 
                           triangle.stats,
                           by = "city.index")
in.out.loc.scaled <- merge(in.out.loc.scaled, 
                           triangle.scaled, 
                           by = "city.index")

save(in.out.loc, file = "results/in.out.loc.Rda")
save(in.out.loc.scaled, file = "results/in.out.loc.scaled.Rda")

for (image.type in c("svg","png")) {
  ggsave(sprintf("plot_triangle.%s",image.type), 
         plot = plot.triangle, 
         device = image.type, 
         path = "results/figures",
         scale = 1, 
         width = 10.5, height = 7.4, units = "cm",
         dpi = 300)
}