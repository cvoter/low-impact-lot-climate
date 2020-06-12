#' convert_nldas_units.R
#' 
#' Convert units on raw NLDAS inputs
#' 
#' The default units on NLDAS2 inputs is not the same as are required in the
#' FAO Penman Monteith equations. This function perfoms the needed unit 
#' conversions.
#' 
#' INPUTS
#' met.data: data frame generated from raw NLDAS2 data, columns include:
#'     --> down.short.radiation: incoming shorwave radiation (W/m^2)
#'     --> down.long.radiation:  incoming longwave radiation (W/m^2)
#'     --> precipitation:        precipiation (mm/s)
#'     --> temperature.K:        temperature (K)
#'     --> wind.EW:              wind in east-west direction (m/s)
#'     --> wind.NS:              wind in north-south direction (m/s)
#'     --> pressure:             pressure (Pa)
#'     --> specific.humidity:    specific humidity (kg/kg)
#' 
#' OUTPUTS
#' met.data: same as inputs, with unit conversions performed. Specifically:
#'     --> down.short.radiation: (W/m^2) to (MJ/m^2*hr)
#'     --> precipitation:        (mm/s) to (mm/hr)
#'     --> wind.measured:        vectors (EW and NS) to magnitude (m/s)
#'     --> pressure:             (Pa) to (kPa)

convert_nldas_units <- function(met.data){
  met.data$down.short.radiation <- met.data$down.short.radiation*(60*60/1e6)
  met.data$precipitation        <- round(met.data$precipitation*3600, 1)
  met.data$wind.measured        <- sqrt(met.data$wind.EW^2 + met.data$wind.NS^2)
  met.data$pressure             <- met.data$pressure/1000
  return(met.data)
}