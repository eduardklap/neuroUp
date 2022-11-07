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
  variance <- var(datasub[[vars_of_interest[1]]]) + var(datasub[[vars_of_interest[2]]]) -
    (2 * (cor(datasub[[vars_of_interest[1]]], datasub[[vars_of_interest[2]]], 
            use="pairwise.complete.obs")
        * sd(datasub[[vars_of_interest[1]]] * sd(datasub[[vars_of_interest[2]]]))))
  stdev <- sqrt(variance)
  sterror <- stdev/sqrt(sample_size)
  lower <- estimate - 1.96*sterror
  upper <- estimate + 1.96*sterror
  return(list(estimate, variance, stdev, sterror, lower, upper))
}
