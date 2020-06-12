#' Calculate goodness of fit
#' 
#' Calculates the goodness of fit of a simulated value with the observed value
#' using the specified method (root mean squared error, normalized root mean
#' squared error, R2, or Nash-Sutcliffe Efficiency).
#' 
#' @param sim a vector with the simulated values
#' @param obs a vector with the observed values
#' @param method either "RMSE" (root mean squared error), "NRMSE" (normalized 
#'               root mean squared error), "R2", or "NSE" (Nash-Sutcliffe 
#'               Efficiency).
#' 
#' @return fit, the goodness of fit value
#' 
#' @export

calculate_fit <- function(sim, obs, method){
  # Quality control
  if (length(sim) != length(obs)) stop("vectors not the same size")
  
  # Fit calculations
  if (method == "RMSE"){
    fit <- mean((obs-sim)^2)^0.5
  } else if (method == "NRMSE"){
    fit <- (mean((obs-sim)^2)^0.5)/abs(mean(obs))
  } else if (method == "R2"){
    fit <- summary(lm(sim~obs))$r.squared
  } else if (method == "NSE") {
    fit <- 1-sum((obs-sim)^2)/sum((obs-mean(obs))^2)
  } else if (method == "PBIAS") {
    fit <- 100*sum(obs-sim)/sum(obs)
  } else if (method == "RSR") {
    fit <- mean((obs-sim)^2)^0.5/sd(obs)
  }
  
  return(fit)
}