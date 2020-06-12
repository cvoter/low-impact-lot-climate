# functions_predictions.R

# find_closest_kriged_z -------------------------------------------------------
find_closest_kriged_z <- function(df, means, sds, kriged) {
  df$z = rep(0,nrow(df))
  for (i in 1:nrow(df)) {
    obs.x <- (df$x[i]-means$x)/sds$x
    obs.y <- (df$y[i]-means$y)/sds$y
    closest.x <- kriged$x[which.min(abs(kriged$x - obs.x))]
    closest.y <- kriged$y[which.min(abs(kriged$y - obs.y))]
    df$z[i] <- as.numeric(subset(kriged, x == closest.x & y == closest.y,
                                 select = "z"))
  }
  return(df)
}

# summarize_long_term_z -------------------------------------------------------
summarize_long_term_z <- function(df, nlocations){
  summary.z.predictions = NULL
  for (i in 1:nlocations) {
    city.subset <- subset(df, city.index == i)
    z.1 <- sum(city.subset$z == 1, na.rm = TRUE)
    z.2 <- sum(city.subset$z == 2, na.rm = TRUE)
    z.3 <- sum(city.subset$z == 3, na.rm = TRUE)
    city.summary = c(i, z.1, z.2, z.3)
    summary.z.predictions <- as.data.frame(rbind(summary.z.predictions, 
                                                 city.summary))
  }
  colnames(summary.z.predictions) <- c("city.index", "z.1","z.2",
                                       "z.3")
  rownames(summary.z.predictions) <- NULL
  
  summary.z.predictions <- merge(city.locations, summary.z.predictions, 
                                 by = "city.index")
  return(summary.z.predictions)
}

# plot_predictions_on_us_map --------------------------------------------------
plot_predictions_on_us_map <- function(summary.z.predictions, color.palette,
                                       text.size, is.angle){
  color.values = RColorBrewer::brewer.pal(3, color.palette)
  if (is.angle == 0) {
    label.names = c("> 25%", "15%-25%", "< 15%")
  } else {
    label.names = c("Mostly DD", "Mix", "Mostly ET")
  }

  us <- map_data('state')
  plot.z.prediction <- ggplot(us, aes(long, lat)) +
                       geom_map(map=us, aes(map_id=region), 
                                fill="grey60", color="grey") +
                       coord_fixed(1.3) +
                       geom_scatterpie(data = summary.z.predictions,
                                       aes(longitude.deg, latitude.deg, r = 0.9),
                                       cols = c("z.1","z.2","z.3"),
                                       alpha = 0.7) +
                       scale_fill_manual( breaks = c("z.1","z.2","z.3"),
                                          labels = label.names,
                                          values = rev(color.values)) +
                       labs(title = "", fill = NULL) + 
                     # coord_equal() +
                       coord_fixed(1.3) +
                       theme_void() +
                       theme(text=element_text(size = text.size,  
                                               family = "Segoe UI Semilight"),
                             legend.margin = margin(0,0,0,0),
                             legend.box.margin = margin(-10,-45,-10,-45),
                             legend.key.size = unit(0.7,"line"))
  return(plot.z.prediction)
}