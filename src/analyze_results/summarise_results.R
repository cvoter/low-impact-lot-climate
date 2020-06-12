#' summarise_results.R
#' 
#' Summarises how many results fall into each of 3 bins.
#' 
#' REQUIRES
#' 
#' INPUTS
#' results: a data frame with the following columns: 
#'    --> city.index: unique id of city
#'    --> group: predict, cal (for calibration) or val (for validation)
#'    --> bin: the bin number for the result of interest (1, 2, or 3)
#' city.locations: a data frame with the following columns:
#'    --> city.index: unique id of city
#'    --> city.name: name of the city
#'    --> state: name of the state
#'    --> latitude.deg: latitude of the city (degrees)
#'    --> longitude.deg: longitude of the city (degrees)
#'    --> elevation.m: elevation of the city (mamsl)
#' 
#' OUTPUTS
#' summary: a data frame with the following columns:
#'    --> city.index: unique id of city
#'    --> city.name: name of the city
#'    --> state: name of the state
#'    --> latitude.deg: latitude of the city (degrees)
#'    --> longitude.deg: longitude of the city (degrees)
#'    --> elevation.m: elevation of the city (mamsl)
#'    --> z.1: number of results falling into bin #1 (lower than lowest break)
#'    --> z.2: number of results falling into bin #2 (between breaks)
#'    --> z.3: number of results falling into bin #3 (higher than upper break)

summarise_results <- function (results, city.locations) {
  summary <- NULL
  for (i in unique(results$city.index)) {
    this.city    <- results %>%
                    filter(city.index == i,
                           group == "predict")
    z.1          <- sum(this.city$bin == 1, na.rm = TRUE)
    z.2          <- sum(this.city$bin == 2, na.rm = TRUE)
    z.3          <- sum(this.city$bin == 3, na.rm = TRUE)
    city.summary <- c(i, z.1, z.2, z.3)
    summary      <- as.data.frame(rbind(summary, city.summary))
  }
  colnames(summary)      <- c("city.index", "z.1","z.2", "z.3")
  rownames(summary)      <- NULL 
  summary                <- merge(city.locations, summary, by = "city.index")
  return(summary)
}
