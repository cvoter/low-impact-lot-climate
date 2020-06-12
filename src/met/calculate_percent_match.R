#' calculate_percent_match.R
#' 
#' Given a vector representing a subset of data, as well as the total sum and
#' total length of the full dataset, calculates the percent of these totals
#' represented by the subset.
#' 
#' INPUTS
#' subset:      a vector of values representing a subset of a full dataset
#' total.depth: the sum of values in the full dataset
#' total.time:  the number of values in the full dataset
#' 
#' OUTPUTS
#' a list with the following:
#'     --> depth: the percent of the total value represented by the subset
#'     --> time:  the percent of the total number of data points represented by 
#'                the subset 

calculate_percent_match <- function(subset, total.depth, total.time){
  if (length(subset) == 0){
    # No values in subset
    depth.over.threshold <- 0
    time.over.threshold  <- 0
  } else {
    # At least one value in subset
    depth.over.threshold <- 100*sum(subset)/total.depth
    time.over.threshold  <- 100*length(subset)/total.time
  }
  return(list(depth = depth.over.threshold, 
              time = time.over.threshold))
}