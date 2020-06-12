#' plot_triangle.R
#' 
#' Creates initial triangle diagram illustrating balance among runoff, deep
#' drainage and ET for the baseline and low impact lots in each city. Then adds
#' lines to existing triangle plot connecting baseline and low impact balances
#' for each city. Finally, updates font, text size, etc. for ggplot figure
#' theme.
#' 
#' REQUIRES
#' ggtern (library)
#' ggplot2 (library)
#' extrafont (library)
#' 
#' INPUTS
#' df: a data frame with the following columns:
#'    --> city.index: unique id for each city
#'    --> lot.type  : type of modelrun, "baseline" or "low_impact" (factor)
#'    --> runoff    : fraction of runoff relative to root zone balance
#'    --> drainage  : fraction of deep drainage relative to root zone balance
#'    --> ET        : fraction of evapotranspiration relative to root zone 
#'                    balance
#'    --> logETP    : log10 of the ratio of reference ET (ET0) to precipitation
#' nlocations: number of cities in dataset
#' 
#' OUTPUTS
#' plot.obj: ggplot object with formatted triangle

plot_triange <- function (df, nlocations, text.size = 10) {
  library(ggtern)
  library(ggplot2)
  library(extrafont)
  
  # Basic triangle -------------------------------------------------------------
  plot.obj <- ggtern(data = df,
                     aes(x = runoff,
                         y = drainage,
                         z = ET)) +
                   geom_mask() +
                   geom_point(data = df, 
                              aes(color = logETP,
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
  
  # Add lines ------------------------------------------------------------------
  for (i in 1:nlocations) {
    plot.obj <- plot.obj + 
      geom_line(data = subset(df, city.index == i),
                aes(color = logETP))
  }
  plot.obj   <- plot.obj  +
                scale_color_distiller(palette = "RdYlBu",
                                      direction = -1,
                                      limits = c(-0.5, 1.5),
                                      breaks = c(-1, 0, 1),
                                      labels = c("0.1","1.0","10"),
                                      name = "PET:P") + 
                guides(color = guide_colorbar(order = 0),
                       shape = guide_legend(order = 1)) #Lot 1st, ET0 2nd
  
  # Update theme ---------------------------------------------------------------
  plot.obj <- plot.obj  +
              theme(text = element_text(size = text.size, 
                                        family = "Segoe UI Semilight"),
                    legend.title = element_text(size = text.size, 
                                                family = "Segoe UI Semibold"),
                    legend.margin = margin(0,0,0,0),
                    legend.box.margin = margin(0,-30,0,-30),
                    legend.spacing.y = unit(0.4, 'cm'),
                    legend.key.size = unit(0.4, "cm"))
  
  return(plot.obj)
}