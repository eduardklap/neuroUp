#' Estimate correlations
#'
#' `estim_corr` determines point estimate, SD and SE, 95% Credibility
#' Intervals, and interval width, for Pearson correlations for multiple sample
#' sizes
#'
#' @param data Dataframe with the data to be analyzed
#' @param vars_of_interest Vector containing the names of the variables to be
#'   correlated
#' @param k The number of permutations to be used for each sample size
#' @param sample_size The range of sample size to be used (min:max)
#' @param name The title of the dataset or variables to be displayed with the
#'   figure
#' @returns
#' * `tbl_selection` returns a [tibble::tibble()] containing estimates of the Pearson’s correlation between
#' two correlated variables for five different sample sizes (starting with the
#' minimum sample size, then 1/5th parts of the total dataset).
#' * `fig_corr` returns a scatterplot where for the five different sample sizes, 10 out of the total number
#' of HDCIs computed are displayed (in green). The average estimate with
#' credible interval summarizing the total number of HDCIs for each sample size
#' are plotted in orange
#' * `fig_corr_nozero` returns a barplot where for each of the five sample sizes the proportion of permutations
#' not containing zero is displayed
#' * `tbl_total` returns a [tibble::tibble()] containing estimates of the Pearson’s correlation between
#' two correlated variables for all sample sizes, including the lower and upper
#' range and the permuation number.
#' @examples
#' data_gambling <- gambling
#' estim_corr(data_gambling, 
#'   c("lnacc_self_winvsloss", "age"), 10, 
#'   20:221, "Gambling NAcc correlation with age")
#' @importFrom rlang .data
#' @export

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
      nozero = (.data$lower > 0 & .data$upper > 0) | (.data$lower < 0 & .data$upper < 0)) %>%
    dplyr::group_by(.data$N) %>%
    dplyr::summarise(
      correlation = mean(.data$correlation, na.rm = TRUE),
      lower = mean(.data$lower, na.rm = TRUE),
      upper = mean(.data$upper, na.rm = TRUE),
      nozero = mean(.data$nozero, na.rm = TRUE),
      permutation = 999) %>%
    dplyr::ungroup()
  # function to divide the total dataset by 5 and to filter the sample sizes
  filt_sample <- function(sample_size, output_total) {
    filt_sel <- round((sample_size[length(sample_size)] - sample_size[1])/5)
    dplyr::filter(output_total,  .data$N == .data$N[1] |
                    .data$N ==  (.data$N[1] + filt_sel) |
                    .data$N ==  (.data$N[1] + 2 * filt_sel) |
                    .data$N == (.data$N[1] + 3 * filt_sel) |
                    .data$N == .data$N[length(.data$N)]  )
  }
  # select 10 random permutations for the 5 different sample sizes for every permutation for visualization 
  # (only when k >50 random, otherwise select the first 10 permutations)
  output_selection <- filt_sample(sample_size, output_total) 
  output_selection <- if(k > 10) {
    dplyr::filter(output_selection, 
                  .data$permutation %in% sample(unique(.data$permutation), 
                                          size = 10, 
                                          replace = FALSE))
  } else
  {
    dplyr::filter(output_selection, 
                  .data$permutation %in% 1:10)
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
                                 ggplot2::aes(x = .data$N,
                                              y = .data$correlation,
                                              colour = .data$permutation,
                                              linetype = .data$permutation) ) +
    ggplot2::theme_classic() +
    ggplot2::geom_point(position = ggplot2::position_dodge(.8),
                        ggplot2::aes(x = .data$N,
                                     y = .data$correlation,
                                     colour = .data$permutation,
                                     size = .data$permutation)) +
    ggplot2::scale_size_manual(values = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4)) +
    ggplot2::scale_linetype_manual(values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 6)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                  width=.1,
                  position = ggplot2::position_dodge(.8)) +
    ggplot2::scale_color_manual(values = c("#009E73","#009E73","#009E73","#009E73","#009E73","#009E73",
                                  "#009E73","#009E73","#009E73","#009E73", "#E69F00") ) +
    ggplot2::labs(title = name) +
    ggplot2::geom_hline(yintercept=0, linetype="dashed")
  
  # plot proportion of non-zero values for selected samples
  figure_nozero <- ggplot2::ggplot(data = overall_selection,
                                   ggplot2::aes(x = .data$N,
                                                y = .data$nozero) ) +
    ggplot2::theme_classic()  +
    ggplot2::geom_col(color = "#000000",
                      fill = "#E69F00",
                      width = 0.6) +
    ggplot2::ylim(0,1) +
    ggplot2::labs(title = name, 
         y = "Proportion not containing zero")
  
  return(list(tbl_selection = total_selection,
              fig_corr = figure_corr, 
              fig_corr_nozero = figure_nozero,
              tbl_total = output_total))
}
