#' classify_results.R
#' 
#' Classifies results into 3 bins based on given upper and lower breaks.
#' 
#' REQUIRES
#' 
#' INPUTS
#' estimate: a vector with all results to classify
#' 
#' OUTPUTS
#' bin: a vector with the bin number of all results. 
#'  --> Bin #1 is greater than the upper break
#'  --> Bin #2 is between the upper and lower breaks
#'  --> Bin #3 is below the lower break

classify_results <- function (result, break.upper, break.lower) {
  bin <- NA
  for (i in 1:length(result)){
    if (result[i] >= break.upper) {
      bin[i] <- 1
    } else if (result[i] >= break.lower) {
      bin[i] <- 2
    } else {
      bin[i] <- 3
    }
  }
  return(bin)
}
