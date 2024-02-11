#' helper function to sample the data and estimate the difference
#'
#' @param data Dataframe with the data to be analyzed
#' @param vars_of_interest Vector containing the names of the variables to be
#'   compared on their means
#' @param sample_size The range of sample size to be used
#' @return list with estimate, variance, stdev, sterror, lower, upper values for
#'   unstandardized differences as well as Cohen's d for given sample_size
#' @noRd

sample_diff <- function(data, vars_of_interest, sample_size){
  # create a copy of data with shuffled rows
  data <- data[sample(nrow(data)),]
  # select the first rows with the size of the test sample size
  datasub<-data[1:(max(sample_size)),]
  # calculate the mean difference between the variables of interest
  estimate <- mean(datasub[[vars_of_interest[1]]]) - mean(datasub[[vars_of_interest[2]]])
  # calculate the variance of the difference
  variance <- stats::var(datasub[[vars_of_interest[1]]]) + stats::var(datasub[[vars_of_interest[2]]]) -
    (2 * (stats::cor(datasub[[vars_of_interest[1]]], datasub[[vars_of_interest[2]]], 
            use="pairwise.complete.obs")
        * stats::sd(datasub[[vars_of_interest[1]]] * stats::sd(datasub[[vars_of_interest[2]]]))))
  stdev <- sqrt(variance)
  sterror <- stdev/sqrt(sample_size)
  lower <- estimate - 1.96*sterror
  upper <- estimate + 1.96*sterror
  # bootstrap Cohen's D for each dataset
  # first define theta function
  theta <- function(x) {
    mean(x) / stats::sd(x)
  }
  # bootstrap * 1000
  bcd <-  bootstrap::bootstrap(datasub[[vars_of_interest[1]]] -
                                 datasub[[vars_of_interest[2]]], 1000, theta)
  # calculate average Cohen's D
  cohens_d <- mean(bcd$thetastar)
  # lower bound for Cohen's D
  d_lower <- cohens_d - 1.96*((stats::sd(bcd$thetastar)))
  # upper bound for Cohen's D
  d_upper <- cohens_d + 1.96*((stats::sd(bcd$thetastar)))
  
  return(list(estimate, variance, stdev, sterror, lower, upper, cohens_d, d_lower, d_upper))
}
