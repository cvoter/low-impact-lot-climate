# functions_kriging.R

# extract_col_nums ---------------------------------------------------------
extract_col_nums <- function(parameters.fluxes.no.index, 
                             x.param, y.param, z.param = NULL){
  x.col = which(colnames(parameters.fluxes.no.index)==x.param)
  y.col = which(colnames(parameters.fluxes.no.index)==y.param)
  if (is.null(z.param)) {
    z.col = NULL
  } else {
    z.col = which(colnames(parameters.fluxes.no.index)==z.param)
  }
  return(list(x=x.col, y=y.col, z=z.col))
}

# extract_stats ---------------------------------------------------------
extract_stats <- function(stats.matrix, col.nums, z.val = NULL){
  x.val = stats.matrix[1,col.nums$x]
  y.val = stats.matrix[1,col.nums$y]
  if (is.null(col.nums$z)) {
    z.val = z.val
  } else {
    z.val = stats.matrix[1,col.nums$z]
  }
  return(list(x=x.val, y=y.val, z=z.val))
}

# subset_kriging_spatial_xyz --------------------------------------------------
# Subset and make into spatial points data frame for kriging
subset_kriging_xyz <- function(df, x.param, y.param, z.param){
  require(sp)
  k.subset = subset(df, select = c(x.param,y.param,z.param))
  colnames(k.subset) = c("x","y","z")
  coordinates(k.subset) = ~x + y
  return(k.subset)
}

# convert_sp_df ---------------------------------------------------------------
# Convert spatial variable to data frame
convert_sp_df <- function(sp.df) {
  df <- as.data.frame(sp.df)
  df$var1.var = NULL
  colnames(df) = c("x","y","z")
  return(df)
}

# create_estimate_grid --------------------------------------------------------
# Create grid for estimates as a spatial points data frame
create_estimate_grid <- function(xy.res, k.subset) {
  require(sp)
  plot.grid = NULL
  xo=seq(bbox(k.subset)[1,1], bbox(k.subset)[1,2], length=xy.res)
  yo=seq(bbox(k.subset)[2,1], bbox(k.subset)[2,2], length=xy.res)
  count = 1
  for (i in 1:xy.res) {
    for (j in 1:xy.res) {
      plot.grid$x[count] = xo[i]
      plot.grid$y[count] = yo[j]
      count = count + 1
    }
  }
  plot.grid = as.data.frame(plot.grid)
  coordinates(plot.grid) = ~x + y
  return(plot.grid)
}

# bin_results -----------------------------------------------------------------
bin_results <- function(z.vector, z.sd, z.mean, upper.transition, 
                        lower.transition) {
  for (i in 1:length(z.vector)) {
    if (z.vector[i]*z.sd+z.mean >= upper.transition) {
      z.vector[i] <- 1 # highest values
    } else if (z.vector[i]*z.sd+z.mean < upper.transition &
               z.vector[i]*z.sd+z.mean > lower.transition) {
      z.vector[i] <- 2 # middle values
    } else {
      z.vector[i] <- 3 # lowest values
    }
  }
  return(z.vector)
}

# krige_results ---------------------------------------------------------------
krige_sp_and_binned_df <- function(parameters.fluxes.scaled,
                                   x.param, y.param, z.param, sds, means,
                                   z.upper.transition, z.lower.transition,
                                   v.model.type, xy.res){
  require(gstat)
  k.subset <- subset_kriging_xyz(parameters.fluxes.scaled, 
                                 x.param, y.param, z.param)
  plot.grid <- create_estimate_grid(xy.res, k.subset)
  k.fcn <- z ~ x + y # z depends on two predictors
  k.vgm <- variogram(k.fcn, k.subset) # calculates sample variogram 
  k.fit <- fit.variogram(k.vgm, vgm(v.model.type), fit.kappa = TRUE) # fit model
  kriged <- krige(k.fcn, k.subset, plot.grid, model=k.fit)
  plot(k.vgm, k.fit)
  kriged.df <- convert_sp_df(kriged)
  kriged.df$z <- bin_results(kriged.df$z, sds$z, means$z, 
                             z.upper.transition, z.lower.transition)
  return(list(df = kriged.df, sp = kriged))
}


