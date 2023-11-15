#' helper function to sample the data and estimate the difference
#'
#' @param data Dataframe with the data to be analyzed
#' @param vars_of_interest Vector containing the names of the variables to be compared on their means
#' @param sample_size The range of sample size to be used
#' @return list with correlations and credible intervals for given sample_size

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
  # Estimate of Cohen's D for each dataset
  cohens_d <- estimate/stdev
  # Variance of Cohen's D for each dataset see logic above: 1/(SD of diff)^2*VAR. Becomes 1, because standardized
  d_variance <- (1/(stdev)^2)*variance
  # SE of Cohens's D for each dataset
  d_sterror <- sqrt(d_variance)/sqrt(sample_size)
  # Lower bound for Cohen's D
  d_lower <- cohens_d - 1.96*d_sterror
  # Upper bound for Cohen's D
  d_upper <- cohens_d + 1.96*d_sterror
  return(list(estimate, variance, stdev, sterror, lower, upper, cohens_d, d_variance, d_sterror, d_lower, d_upper))
}
