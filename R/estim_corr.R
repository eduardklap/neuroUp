#' Determine point estimate, SD and SE, 95\% Credibility Intervals, and interval width, for both
#' differences in raw means and Cohen's D's for multiple sample sizes
#'
#' @param data Dataframe with the data to be analyzed
#' @param vars_of_interest Vector containing the names of the variables to be compared on their means
#' @param k The number of permutations to be used for each sample size
#' @param sample_size The range of sample size to be used
#' @param name The title of the dataset or variables to be displayed with the figure
#' @return And array containing the difference in raw means with associated SD, SE, 95\% CI, and width of
#' the 95\% CI, the Cohen's D value of the difference with associated SD, SE, 95\% CI, and width of
#' the 95\% CI, ans the sample size on which each row of results is based.

estim_corr <- function(data, vars_of_interest, k, sample_size, name){
  # create a tibble to store the output in
  output <- tibble()
  output_total <- tibble()
  # loop X number of times over the different sample sizes 
  for (j in 1:k) {
    for (i in sample_size){
      #return output as a vector
      output_vector <- unlist(sample_corr(data, vars_of_interest,i))
      # store vector output in the table
      output[1,1] <- i
      output[1,2] <- output_vector[1]
      output[1,3] <- output_vector[2]
      output[1,4] <- output_vector[3]
      output[1,5] <- j
      # add output to output_total tibble
      output_total <- rbind(output_total, output)
    }
    # 
    # return(output_total)
    # output_total
  }
  colnames(output_total) <- c("N", "correlation", "cred_low", "cred_high", "permutation")
  # divide the total dataset by 5 to select sample sizes
  filt_sel <- round((sample_size[length(sample_size)] - sample_size[1])/5)
  # select the 5 different sample sizes for visualization
  output_selection <- output_total %>%
    filter(  N == N[1] |
               N ==  (N[1] + filt_sel) |
               N ==  (N[1] + 2*filt_sel) | 
               N == (N[1] + 3*filt_sel) | 
               N == N[length(N)]  )    
  # calculate overall intervals per selected sample size
  overall_output <- output_total %>%
    group_by(N) %>%
    summarise(
      correlation = mean(correlation, na.rm = TRUE),
      cred_low = mean(cred_low, na.rm = TRUE),
      cred_high = mean(cred_high, na.rm = TRUE),
      permutation = 999) %>%
    ungroup()
  # select the 5 different sample sizes for visualization
  overall_selection <- overall_output %>%
    filter(  N == N[1] |
               N ==  (N[1] + filt_sel) |
               N ==  (N[1] + 2*filt_sel) | 
               N == (N[1] + 3*filt_sel) | 
               N == N[length(N)]  )
  
  # combine 10 datasets per sample size with overall per sample size
  total_selection <- rbind(output_selection, overall_selection)
  
  lvl_plot <- levels(factor(total_selection$permutation))
  lvl_plot[lvl_plot == "999"] <- "Overall"
  total_selection$permutation <- factor(total_selection$permutation, labels=lvl_plot)
  total_selection$N <- as.factor(total_selection$N)
  #return(total_selection)
  
  figure_corr <- ggplot(data=total_selection, aes(x = N, y = correlation, 
                                                  colour = permutation, linetype = permutation) ) +
    theme_classic(base_size = 14) +
    geom_point(position=position_dodge(.8),aes(x = N, y = correlation, colour = permutation,
                                               size = permutation)) +
    scale_size_manual(values = c(2,2,2,2,2,2,2,2,2,2,4)) +
    scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1,1,6)) +
    geom_errorbar(aes(ymin = cred_low, ymax=cred_high), width=.1, position = position_dodge(.8)) +
    scale_color_manual(values = c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73",
                                  "#009E73","#009E73","#009E73","#009E73", "#E69F00") ) +
    labs(title = name) +
    geom_hline(yintercept=0, linetype="dashed")
  
  #return(list(total_selection, figure_corr))
}
