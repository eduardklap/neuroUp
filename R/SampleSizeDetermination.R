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


SampleSizeDetermination <- function(data, vars_of_interest, k, row_select, name){ #nmin

  # vars_of_interest <- c("MFG_LEARNING", "MFG_APPLICATION")
  #row_select <- c(nmin:nrow(data))

  # Create K different permutations of the data
  #Results <- array(NA, dim=c(length(c(nmin:nrow(data))), 15, k))
  Results <- array(NA, dim=c(length(row_select), 15, k))

  for(j in 2:k){
    df1 <- data
    assign(paste("df", j, sep = ""), data[sample(nrow(data)),])
  }

  # Now for the actual simulations

  for(i in 1:length(row_select)){

    # The final row that needs to be in the data used in each replication
    # Select the rows to use from each of the 4 datasets in the replication

    for(l in 1:k){
      assign(paste("d", l, sep = ""), eval(parse(text = paste("df", l, sep = "")))[1:row_select[i],])

      # Calculate Mean Differences
      eval(parse(text = paste("d", l, "$", vars_of_interest[1],sep = "")))
      Results[i,1,l] <- mean(eval(parse(text = paste("d", l, "$", vars_of_interest[1],sep = ""))),na.rm=TRUE) - mean(eval(parse(text = paste("d", l, "$", vars_of_interest[2],sep = ""))),na.rm=TRUE)

      # Variance of the difference
      Results[i,2,l] <- var(eval(parse(text = paste("d", l, "$", vars_of_interest[1],sep = ""))),na.rm=TRUE) + var(eval(parse(text = paste("d", l, "$", vars_of_interest[2],sep = ""))),na.rm=TRUE) -
        (2*(cor(eval(parse(text = paste("d", l, "$", vars_of_interest[1],sep = ""))),eval(parse(text = paste("d", l, "$", vars_of_interest[2],sep = ""))),use="pairwise.complete.obs")
            *sd(eval(parse(text = paste("d", l, "$", vars_of_interest[1],sep = ""))),na.rm=TRUE)*sd(eval(parse(text = paste("d", l, "$", vars_of_interest[2],sep = ""))),na.rm=TRUE)))


      # SD and SE of the difference
      Results[i,3,l] <- sqrt(Results[i,2,l])
      Results[i,4,l] <- Results[i,3,1]/sqrt(nrow(eval(parse(text = paste("d", l, sep = "")))))

      # Lower and Upper Bound of 95% Interval
      Results[i,5,l] <- Results[i,1,l] - 1.96*Results[i,4,l]
      Results[i,6,l] <- Results[i,1,l] + 1.96*Results[i,4,l]

      # Width of the Interval and whether the interval contains the value 0.
      Results[i,7,l] <- Results[i,6,l] - Results[i,5,l]
      Results[i,8,l] <- Results[i,5,l] >= 0 | Results[i,6,l] <= 0

      # Estimate of Cohen's D for each dataset
      Results[i,9,l] <- Results[i,1,l]/Results[i,3,l]

      # Variance of Cohen's D for each dataset see logic above: 1/(SD of diff)^2*VAR. Becomes 1, because standardized
      Results[i,10,l] <- (1/(Results[i,3,l])^2)*Results[i,2,l]

      # SE of Cohens's D for each dataset
      Results[i,11,l] <- Results[i,10,l]/sqrt(nrow(eval(parse(text = paste("d", l, sep = "")))))

      #Upper Bound For Cohen's D
      Results[i,12,l] <- Results[i,9,l] + 1.96*Results[i,11,l]

      # Lower Bound For Cohen's D
      Results[i,13,l] <- Results[i,9,l] - 1.96*Results[i,11,l]

      # Also store sample size estimates are based on
      Results[i,14,l] <- row_select[i]

      # Also store the dataset used. Easier for follow up analyses
      Results[i,15,l] <- l
    }

    LongData <- Results[,,1]

    for(p in 2:k){
      LongData <- rbind(LongData, Results[,,p])
    }
    # set meaningful columnnames
    colnames(LongData) <- c("Estimate", "Var", "SD", "SE", "LB", "UB", "Width", "ZiI", "CohensD", "SD_CD", "SE_CD", "UB_CD","LB_CD", "N", "dataset")

    #row_select

    LongData <- as_tibble(LongData)

    filt_sel <- round((row_select[length(row_select)] - row_select[1])/5)

    LongSelect1 <- LongData %>%
      filter(N == row_select[1] | N == (row_select[1] + filt_sel) | N == (row_select[1] + 2*filt_sel) |N == (row_select[1] + 3*filt_sel) | N == row_select[length(row_select)])

    # calculate overall intervals per selected sample size
    OverallIntervals <- LongData %>%
      group_by(N) %>%
      summarise(
        Estimate = mean(Estimate, na.rm = TRUE),
        Var = mean(Var, na.rm = TRUE),
        SD = mean(SD, na.rm = TRUE),
        SE = mean(SE, na.rm = TRUE),
        LB = mean(LB, na.rm = TRUE),
        UB = mean(UB, na.rm = TRUE),
        Width = mean(Width, na.rm = TRUE),
        CohensD = mean(CohensD, na.rm = TRUE),
        SD_CD = mean(SD_CD, na.rm = TRUE),
        SE_CD = mean(SE_CD, na.rm = TRUE),
        UB_CD = mean(UB_CD, na.rm = TRUE),
        LB_CD = mean(LB_CD, na.rm = TRUE),
        ZiI = mean(ZiI, na.rm = TRUE),
        dataset = 999) %>%
      ungroup()
    # select the 5 different sample sizes for visualization
    LongSelect2 <- OverallIntervals %>%
      filter(N == row_select[1] | N == (row_select[1] + filt_sel) | N == (row_select[1] + 2*filt_sel) |N == (row_select[1] + 3*filt_sel) | N == row_select[length(row_select)])

    # put columns in correct order
    LongSelect2 <- LongSelect2 %>%
      dplyr::select(Estimate, Var,SD, SE, LB, UB, Width, CohensD, SD_CD, SE_CD, UB_CD, LB_CD,
                    ZiI, N, dataset)
    # combine 10 datasets per sample size with overall per sample size
    LongSelectTotal <- rbind(LongSelect1, LongSelect2)

    lvl_plot <- levels(factor(LongSelectTotal$dataset))
    lvl_plot[lvl_plot == "999"] <- "Overall"

    LongSelectTotal$dataset <- factor(LongSelectTotal$dataset, labels=lvl_plot)
    LongSelectTotal$N <- as.factor(LongSelectTotal$N)

    # The errorbars overlapped, so use position_dodge to move them horizontally
    pd <- position_dodge(.8) # move them .05 to the left and right

    #### new plot Eduard to visualize different samples + overall ####
    Fig1a <- ggplot(data=LongSelectTotal, aes(x=N, y=Estimate, colour=dataset,linetype=dataset)) +
      theme_classic(base_size=14)+
      geom_point(position=pd,aes(x=N, y=Estimate, colour=dataset, size=dataset)) +
      scale_size_manual(values = c(2,2,2,2,2,2,2,2,2,2,4))+
      geom_line()+
      scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1,1,6))+
      geom_errorbar(aes(ymin=LB, ymax=UB), width=.1,position=pd)+
      scale_color_manual(values=c("#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#56B4E9","#CC79A7"))+
      labs(title = name) +
      geom_hline(yintercept=0, linetype="dashed")



    }
  return(list(LongData, Fig1a))
}
