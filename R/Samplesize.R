#' Sample size calculation
#'
#' Convert degrees Fahrenheit temperatures to degrees Celsius
#' @param standard The standard deviation
#' @param pw The power
#' @param alpha The alpha
#' @param mde The minimum delta effect
#' @return The sample size
#' @examples 
#' sample_size = calculate_sample_size(500,0.05,0.9,200)

calculate_sample_size = function(standard,alph,pw,mde){
  z1 = qnorm(1-alph/2)
  z2= qnorm(pw)
  n = 2*(standard^2) * (z1+z2)^2/(mde^2) 
  print(n) 
}

