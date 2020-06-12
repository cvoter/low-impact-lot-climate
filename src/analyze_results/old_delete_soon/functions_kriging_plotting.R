# functions_kriging_plotting.R


# subset_for_plotting ---------------------------------------------------------
subset_for_plotting <- function(df, x.param, y.param, z.param){
  plot.param = subset(df, select = c(x.param, y.param, z.param,
                                     "city.name", "state"))
  colnames(plot.param) = c("x","y","z","city.name","state")
  for (i in 1:length(plot.param$city.name)) {
    plot.param$label[i] = sprintf('%s, %s',
                                  plot.param$city.name[i],
                                  plot.param$state[i])
  }
  return(plot.param)
}

# get_mins ---------------------------------------------------------
get_mins <- function(plot.param, means, sds) {
  x.min <- min(plot.param$x)*sds$x+means$x
  y.min <- min(plot.param$y)*sds$y+means$y
  z.min <- min(plot.param$z)*sds$z+means$z
  return(list(x=x.min, y=y.min, z=z.min))
}

# get_maxs ---------------------------------------------------------
get_maxs <- function(plot.param, means, sds) {
  x.max <- max(plot.param$x)*sds$x+means$x
  y.max <- max(plot.param$y)*sds$y+means$y
  z.max <- max(plot.param$z)*sds$z+means$z
  return(list(x=x.max, y=y.max, z=z.max))
}

# set_colorbar_info -----------------------------------------------------
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

# limit_city_labels ----------------------------------------------------------------
# Limit the city labels, if desired.
limit_city_labels <- function(plot.param, keep.labels){
  for (i in 1:length(plot.param$city.name)) {
    if (plot.param$city.name[i] %in% keep.labels) {
    } else {
      plot.param$label[i] = ""
    }
  }
  return(plot.param)
}

# runoff_bin_labels -----------------------------------------------------------------
# get color and fill labels
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

# angle_bin_labels ------------------------------------------------------------
# get color and fill labels
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

# plot_kriged -----------------------------------------------------------------
# Plot kriged results
plot_kriged <- function(kriged.sp, x.param, y.param, means, sds,
                        colorbar.info){
  require(dplyr)
  plot.kriged <- kriged.sp %>% as.data.frame %>%
    ggplot(aes(x = x*sds$x + means$x, y= y*sds$y + means$y)) + 
    geom_tile(aes(fill=(var1.pred))) + 
    scale_fill_distiller(palette = colorbar.info$palette,
                         direction = 1) + 
    labs(x = x.param, y = y.param) + 
    theme_bw()
  
  return(plot.kriged)
}

# plot_points -----------------------------------------------------------------
# Plot points only (dot.size = 2.5 for publications)
plot_points <- function(plot.param, means, sds, mins, maxs,
                        bin.labels, colorbar.info, x.title, y.title,
                        text.size = 12, legend.size = 10, logx, dot.size = 3.5) {
  plot.points <- ggplot() +
    # City points - colored by bin
    geom_point(data = plot.param,
               #shape = 16, size = 1.5,
               shape = 16, size = dot.size,
               aes(x = x*sds$x + means$x, 
                   y = y*sds$y + means$y, 
                   color = z)) +
    # City points - outlined in black
    geom_point(data = plot.param,
               #shape = 1, size = 1.5,
               shape = 1, size = dot.size,
               color = "black",
               aes(x = x*sds$x + means$x, 
                   y = y*sds$y + means$y)) +
    # City points - color scale
    scale_color_distiller(palette = colorbar.info$palette,
                          direction = 1,
                          limits = colorbar.info$limits,
                          breaks = colorbar.info$breaks,
                          labels = bin.labels$color,
                          name = colorbar.info$name) +
    # Labeled Cities - outline
    geom_point(data = subset(plot.param, label != ""),
               #shape = 1, size = 2,
               shape = 1, size = dot.size+0.5,
               color= "darkred",
               aes(x = x*sds$x + means$x, 
                   y = y*sds$y + means$y)) +
    # Label Cities - text
    geom_text_repel(data = plot.param,
                    aes(x = x*sds$x + means$x, 
                        y = y*sds$y + means$y,
                        label = label),
                    color = "darkred",
                    inherit.aes = FALSE,
                    box.padding = 0.4,
                    point.padding = 0.4,
                    segment.color = "darkred",
                    size=dot.size,
                    family="Segoe UI Semibold") +
    ylim(mins$y, maxs$y) +
    labs(x = x.title, y.title) +
    # Theme - change text fonts and sizes
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(clip = 'off') +
    theme_bw() +
    theme(text = element_text(size=text.size, family="Segoe UI Semilight"),
          axis.title = element_text(size=text.size, family="Segoe UI Semibold"),
          legend.title = element_text(size=legend.size, family="Segoe UI Semibold"),
          legend.margin = margin(-10,0,-10,0),
          legend.box.margin = margin(-20,-5,-10,-8))
  if (logx == TRUE) {
    plot.points <- plot.points +
                   scale_x_continuous(breaks = log10(c(0.8, 0.9,
                                                       1, 2, 3, 
                                                       4, 5, 6, 
                                                       7, 8, 9,
                                                       10, 20, 30)),
                                      labels = c("","","1",
                                                 "", "", "", "", "", 
                                                 "", "", "",
                                                 "10", "", ""),
                                      lim = c(mins$x, maxs$x),
                                      expand = c(0,0))
  } else {
    plot.points <- plot.points + xlim(mins$x, maxs$x)
  }
  
  return(plot.points)
}

# plot_contours_points -----------------------------------------------------------------
# Plot points and contours (dot size = 2.5 for publications)
plot_contours_points <- function(kriged.df, plot.param, means, sds, mins,
                                 maxs, bin.labels, colorbar.info,
                                 x.title = expression(ET[0]:P), y.title, 
                                 text.size = 12, legend.size = 10, logx,
                                 small.panel, dot.size = 3.5) {
  plot.contours.points <- ggplot() +
    # Kriged Contours
    geom_tile(data = kriged.df,
              aes(x = x*sds$x + means$x, 
                  y = y*sds$y + means$y,
                  fill = as.factor(z))) +
    # City points - colored by bin
    geom_point(data = plot.param,
               #shape = 16, size = 1.5,
               shape = 16, size = dot.size,
               aes(x = x*sds$x + means$x, 
                   y = y*sds$y + means$y, 
                   color = z)) +
    # City points - outlined in black
    geom_point(data = plot.param,
               #shape = 1, size = 1.5,
               shape = 1, size = dot.size,
               color = "black",
               aes(x = x*sds$x + means$x, 
                   y = y*sds$y + means$y)) +
    # City points - color scale
    scale_color_distiller(palette = colorbar.info$palette,
                          direction = 1,
                          limits = colorbar.info$limits,
                          breaks = colorbar.info$breaks,
                          labels = bin.labels$color,
                          name = colorbar.info$name) +
    # Kriged Contours - color scale
    scale_fill_brewer(palette = colorbar.info$palette, 
                      direction = -1,
                      labels = bin.labels$fill,
                      name = colorbar.info$name) +
    # Labeled Cities - outline
    geom_point(data = subset(plot.param, label != ""),
               #shape = 1, size = 2,
               shape = 1, size = dot.size+0.5,
               color= "darkred",
               aes(x = x*sds$x + means$x, 
                   y = y*sds$y + means$y)) +
    # Label Cities - text
    geom_text_repel(data = plot.param,
                    aes(x = x*sds$x + means$x, 
                        y = y*sds$y + means$y,
                        label = label),
                    color = "darkred",
                    inherit.aes = FALSE,
                    box.padding = 0.4,
                    point.padding = 0.4,
                    segment.color = "darkred",
                    size=dot.size,
                    family="Segoe UI Semibold") +
    # Legend - flip color order
    guides(fill = guide_legend(reverse = FALSE)) +
    labs(x = x.title, y = y.title) +
    # Theme - change text fonts and sizes
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(clip = 'off') +
    theme_bw() +
    theme(text = element_text(size=text.size, family="Segoe UI Semilight"),
          axis.title = element_text(size=text.size, family="Segoe UI Semibold"),
          legend.title = element_text(size=legend.size, family="Segoe UI Semibold"),
          legend.margin = margin(-10,0,-10,0),
          legend.box.margin = margin(-20,-5,-10,-8))
  if (small.panel == 1) {
    plot.contours.points <- plot.contours.points +
                            theme(legend.key.size = unit(0.5, "cm"))
  }
  
  if (logx == TRUE) {
    plot.contours.points <- plot.contours.points +
                            # X-scale for log(ET0:P) - label as log10
                            scale_x_continuous(breaks = log10(c(0.8, 0.9,
                                                                      1, 2, 3, 
                                                                      4, 5, 6, 
                                                                      7, 8, 9,
                                                                      10, 20, 30)),
                                               labels = c("","","1",
                                                          "", "", "", "", "", 
                                                          "", "", "",
                                                          "10", "", ""),
                                               lim = c(mins$x, maxs$x),
                                               expand = c(0,0))
  } else {
    plot.contours.points <- plot.contours.points + xlim(mins$x, maxs$x)
  }
  return(plot.contours.points)
}

# get_plots -------------------------------------------------------------------
get_plots <- function(kriged, parameters.fluxes.scaled, 
                      x.param, y.param, z.param,
                      means, sds, z.upper.transition, z.lower.transition,
                      z.lower.limit, z.upper.limit, z.interval,
                      colorbar.name = "", colorbar.palette,
                      colorbar.labels.on = 1, keep.city.labels = NULL,
                      x.title, y.title, is.angle = 0, text.size = 12, 
                      legend.size = 10, logx = TRUE, small.panel) {
  
  plot.param <- subset_for_plotting(parameters.fluxes.scaled, 
                                    x.param, y.param, z.param)
  mins <- get_mins(plot.param, means, sds)
  maxs <- get_maxs(plot.param, means, sds)
  colorbar.info <- set_colorbar_info(z.lower.limit, z.upper.limit,
                                     z.interval, means$z, sds$z,
                                     colorbar.name, colorbar.palette,
                                     mins$z, maxs$z)
  if (is.angle == 1){
    bin.labels <- angle_bin_labels(colorbar.labels.on)
  } else {
    bin.labels <- runoff_bin_labels(z.upper.transition, z.lower.transition,
                                    z.lower.limit, z.upper.limit, z.interval, 
                                    colorbar.labels.on)
  }
  plot.param <- limit_city_labels(plot.param, keep.city.labels)
  
  # PLOT
  plot.kriged <- plot_kriged(kriged$sp, x.param, y.param, means, sds, 
                             colorbar.info)
  plot.points <- plot_points(plot.param, means, sds, mins, maxs,
                             bin.labels, colorbar.info, x.title, y.title,
                             text.size, legend.size, logx)
  plot.contours <- plot_contours_points(kriged$df, plot.param, means, sds, 
                                        mins, maxs, bin.labels, 
                                        colorbar.info, x.title, y.title,
                                        text.size, legend.size, logx,
                                        small.panel)
  return(list(kriged = plot.kriged, points = plot.points, 
              contours = plot.contours))
}