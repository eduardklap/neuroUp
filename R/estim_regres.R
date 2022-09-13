#' Determine point estimate, SD and SE, 95\% Credibility Intervals, and interval width, for both
#' differences in raw means and Cohen's D's for multiple sample sizes
#'
#' @param data Dataframe with the data to be analyzed
#' @param vars_of_interest Vector containing the names of the variables to be compared on their means
#' @param k The number of permutations to be used for each sample size
#' @param row_select The range of sample size to be used
#' @param name The title of the dataset or variables to be displayed with the figure
#' @return And array containing the difference in raw means with associated SD, SE, 95\% CI, and width of
#' the 95\% CI, the Cohen's D value of the difference with associated SD, SE, 95\% CI, and width of
#' the 95\% CI, ans the sample size on which each row of results is based.

estim_regres <- function(data, vars_of_interest, k, row_select, name){
  
  # ====================================================================
  # create storage
  # note analyses start at 5, therefore the number of rows
  # in store is max of row_select divided by 5 minus 1
  # ====================================================================
  set.seed(1234)
  store <- matrix(0, nrow = ceiling((max(row_select)/5-1)), ncol = 8)
  cor <- matrix(0, nrow = k, ncol = 1)
  secor <- matrix(0, nrow = k, ncol = 1)
  cred <- matrix(0, nrow = k, ncol = 2)
  storecount <- 0
  
  # ====================================================
  # estimate correlation and credible interval and store
  # for N in the interval 5 to 271 and for each sample
  # size for 1000 permutations of the original data set
  # ====================================================
  
  model <- 'vars_of_interest[1] ~ 1 + vars_of_interest[2]'
  
  for (i in seq(1, (max(row_select)-5), by = 5) ){
    
    storecount <- storecount + 1  
    
    for (j in 1:k){
      
      data <- data[sample(nrow(data)),]
      datasub<-data[1:(i+5),]
      
      # OPTION 2: correlation, that is, no normal approximation  
      
      cor[j] <- cor(datasub[[vars_of_interest[1]]],datasub[[vars_of_interest[2]]])
      cred[j,] <- CIr(r = cor[j], n = (i+4), level = .95)  
      
    } 
    
    store[storecount,1] <- i+5
    store[storecount,2] <- mean(cred[,1])
    store[storecount,3] <- mean(cred[,2])
    store[storecount,4] <- mean(cor)
    store[storecount,5] <- quantile(cred[,1], c(.025))
    store[storecount,6] <- quantile(cred[,1], c(.975))
    store[storecount,7] <- quantile(cred[,2], c(.025))
    store[storecount,8] <- quantile(cred[,2], c(.975))
  }
  
  # name columns and put store in tibble
  colnames(store) <- c("N", "low", "high", "est", "l1", "l2", "u1", "u2")
  CorrData <- as_tibble(store)
  
  #plot the results
  Fig2c <- ggplot(CorrData, aes(x=N)) +  
    theme_classic(base_size=14)+ 
    geom_line(aes(y=low), col="#E69F00") +  
    geom_line(aes(y=high), col="#E69F00") + 
    geom_line(aes(y=l1),lty=2, col="#009E73")+  
    geom_line(aes(y=l2),lty=2, col="#009E73")+  
    geom_line(aes(y=u1),lty=2, col="#009E73")+  
    geom_line(aes(y=u2),lty=2, col="#009E73") +  
    geom_line(aes(y=est), col="#E69F00", lwd=1.5) +
    labs(y = "correlation", title = name) +
    geom_hline(yintercept=0, linetype="dashed") + 
    scale_x_continuous(breaks = seq(5, length(row_select), by = 20))
}  
