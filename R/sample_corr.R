#' helper function to sample the data and estimate the correlation
#'
#' @param data Dataframe with the data to be analyzed
#' @param vars_of_interest Vector containing the names of the variables to be compared on their means
#' @param sample_size The range of sample size to be used
#' @return list with correlations and credible intervals for given sample_size

sample_corr <- function(data, vars_of_interest, sample_size){
  # create a copy of data with shuffled rows
  data <- data[sample(nrow(data)),]
  # select the first rows with the size of the test sample size
  datasub<-data[1:(max(sample_size)),]
  # estimate the correlation between the variables of interest and the .95 credible interval of the correlation
  cor <- cor(datasub[[vars_of_interest[1]]],datasub[[vars_of_interest[2]]])
  cred <- CIr(r = cor, n = (max(sample_size)), level = .95)
  return(list(cor, cred))
}
