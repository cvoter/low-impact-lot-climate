# functions_triangle.R

# plot_triangle ---------------------------------------------------------------
plot_triange <- function (balances.to.plot) {
  triangle.plot <- ggtern(data = balances.to.plot,
                          aes(x = runoff,
                              y = drainage,
                              z = ET)) +
                   geom_mask() +
                   geom_point(data = balances.to.plot, 
                              aes(color = log10(ET0toP),
                                  shape = lot.type),
                              size = 2) +
                   scale_shape_manual(labels = c("Baseline", "Low Impact"),
                                      values = c(16,17)) +
                   labs(shape="Lot") +
                   Tarrowlab("Deep Drainage") + 
                   Larrowlab("Runoff") + 
                   Rarrowlab("Evapotranspiration") +
                   theme_bw() +
                   theme_hidetitles() +
                   theme_showgrid() +
                   theme_showarrows()
  return(triangle.plot)
}

# add_lines -------------------------------------------------------------------
add_lines <- function(triangle.plot, balances.to.plot, nlocations){
  for (i in c(1:nlocations)) {
    triangle.plot = triangle.plot + 
                    geom_line(data = subset(balances.to.plot, city.index == i),
                              aes(color = log10(ET0toP)))
  }
  triangle.plot  = triangle.plot  +
                   scale_color_distiller(palette = "RdYlBu",
                                         direction = -1,
                                         limits = c(-0.5, 1.5),
                                         breaks = c(-1, 0, 1),
                                         labels = c("0.1","1.0","10"),
                                         name = "PET:P") + 
                   guides(color = guide_colorbar(order = 0),
                          shape = guide_legend(order = 1)) #Lot 1st, ET0 2nd
  return(triangle.plot)
}

# classify_angle -------------------------------------------------------------
classify_angle <- function(angle, upper.transition, lower.transition) {
  if (angle >= upper.transition) {
    balance.type <- 1 # mostly (2/3) DD
  } else if (angle < upper.transition & angle > lower.transition) {
    balance.type <- 2 # mix
  } else {
    balance.type <- 3 # mostly (2/3) ET
  }
  return(balance.type)
}