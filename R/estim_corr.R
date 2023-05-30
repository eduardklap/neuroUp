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
  output <- tibble::tibble()
  output_total <- tibble::tibble()
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
  }
  colnames(output_total) <- c("N", "correlation", "lower", "upper", "permutation")
  # calculate overall intervals per sample size
  overall_output <- output_total %>%
    dplyr::mutate(
      nozero = (lower > 0 & upper > 0) | (lower < 0 & upper < 0)) %>%
    dplyr::group_by(N) %>%
    dplyr::summarise(
      correlation = mean(correlation, na.rm = TRUE),
      lower = mean(lower, na.rm = TRUE),
      upper = mean(upper, na.rm = TRUE),
      nozero = mean(nozero, na.rm = TRUE),
      permutation = 999) %>%
    dplyr::ungroup()
  # function to divide the total dataset by 5 and to filter the sample sizes
  filt_sample <- function(sample_size, output_total) {
    filt_sel <- round((sample_size[length(sample_size)] - sample_size[1])/5)
    dplyr::filter(output_total,  N == N[1] |
                    N ==  (N[1] + filt_sel) |
                    N ==  (N[1] + 2 * filt_sel) |
                    N == (N[1] + 3 * filt_sel) |
                    N == N[length(N)]  )
  }
  # select 10 random permutations for the 5 different sample sizes for every permutation for visualization 
  # (only when k >50 random, otherwise select the first 10 permutations)
  output_selection <- filt_sample(sample_size, output_total) 
  output_selection <- if(k > 10) {
    dplyr::filter(output_selection, 
                  permutation %in% sample(unique(permutation), 
                                          size = 10, 
                                          replace = FALSE))
  } else
  {
    dplyr::filter(output_selection, 
                  permutation %in% 1:10)
  }
  # select the 5 different sample sizes of the overall interval for visualization
  overall_selection <- filt_sample(sample_size, overall_output)
  # combine 10 datasets per sample size with overall per sample size
  total_selection <- dplyr::bind_rows(output_selection, overall_selection)
  
  # turn permutations and N into factors for visualisation
  lvl_plot <- levels(factor(total_selection$permutation))
  lvl_plot[lvl_plot == "999"] <- "Overall"
  total_selection$permutation <- factor(total_selection$permutation, labels=lvl_plot)
  total_selection$N <- as.factor(total_selection$N)
  overall_selection$N <- as.factor(overall_selection$N)
  
  # plot figure for the correlations
  figure_corr <- ggplot2::ggplot(data = total_selection, 
                                 aes(x = N,
                                     y = correlation,
                                     colour = permutation,
                                     linetype = permutation) ) +
    theme_classic(base_size = 12) +
    geom_point(position=position_dodge(.8),
               aes(x = N,
                   y = correlation,
                   colour = permutation,
                   size = permutation)) +
    scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4)) +
    scale_linetype_manual(values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 6)) +
    geom_errorbar(aes(ymin = lower, ymax=upper),
                  width=.1,
                  position = position_dodge(.8)) +
    scale_color_manual(values = c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73",
                                  "#009E73","#009E73","#009E73","#009E73", "#E69F00") ) +
    labs(title = name) +
    geom_hline(yintercept=0, linetype="dashed")
  
  # plot proportion of non-zero values for selected samples
  figure_nozero <- ggplot2::ggplot(data = overall_selection,
                                   aes(x = N,
                                       y = nozero) ) +
    theme_classic(base_size = 12)  +
    geom_col(color = "#000000", 
             fill = "#E69F00",
             width = 0.6) +
    ylim(0,1) +
    labs(title = name, 
         y = "Proportion not containing zero")
  
  return(list(total_selection, figure_corr, figure_nozero))
}
