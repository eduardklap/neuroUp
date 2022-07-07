#' Add together two numbers
#'
#' @param x A data vector
#' @return The ratio of \code{mean(x)} and \code{sd(x)}
#' @examples
#' theta(rnorm(50,0,1))


theta <- function(x){
  mean(x)/sd(x)
  }
