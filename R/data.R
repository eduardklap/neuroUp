#' Feedback task fMRI region of interest data
#'
#' A dataset containing the parameter estimates of
#'  the atlas-based middle frontal gyrus (Harvard-Oxford cortical atlas; 
#'  thresholded at 50%; center-of-mass coordinates x = -4, y = 22, z = 43),
#'  ), with one value for the mean activation during learning and one value 
#'  for the mean activation during application for all participants.
#'
#' @format A data frame with 271 rows and 4 variables:
#' \describe{
#'   \item{participant_id}{unique id for every participant}
#'   \item{age}{age in years (8.01-25.95)}
#'   \item{mfg_learning}{parameter estimates for the middle frontal gyrus during
#'   the learning phase (-2.54-4.83)}
#'   \item{mfg_application}{parameter estimates for the middle frontal gyrus during
#'   the application phase (-6.46-3.09)}
#' }
#' @source {Peters, S., & Crone, E. A. (2017). Increased striatal activity in adolescence 
#'  benefits learning. Nature Communications, 8(1), 1983.}
#'  \doi{10.1038/s41467-017-02174-z}
"feedback"

#' Gambling task fMRI region of interest data
#'
#' A dataset containing the parameter estimates of
#'  the anatomical mask of the left nucleus accumbens
#'  (Harvard-Oxford subcortical atlas; thresholded at 40%; 28 voxels included),
#'  with one value for the mean activation during winning and one value for the 
#'  mean activation during losing for all participants.
#'
#' @format A data frame with 221 rows and 5 variables:
#' \describe{
#'   \item{participant_id}{unique id for every participant}
#'   \item{age}{age in years (11.94-28.46)}
#'   \item{lnacc_self_win}{parameter estimates for the left nucleas accumbens during
#'   winning (-2.78-3.41)}
#'   \item{lnacc_self_loss}{parameter estimates for the left nucleas accumbens during
#'   losing (-3.84-3.28)}
#'   \item{lnacc_self_winvsloss}{parameter estimates for the left nucleas accumbens for
#'   the contrast winning versus losing (-2.60-4.47)}
#' }
#' @source {Schreuders, E., Braams, B. R., Blankenstein, N. E., Peper, J. S., Güroğlu, B., & 
#'  Crone, E. A. (2018). Contributions of reward sensitivity to ventral striatum activity across 
#'  adolescence and early adulthood. Child development, 89(3), 797-810.}
#'  \doi{10.1111/cdev.13056}
"gambling"

#' Self-evaluations task fMRI region of interest data
#'
#' A dataset containing the parameter estimates of the
#'  left medial prefrontal cortex (x = −6, y = 50, z = 4),
#'  with one value for the mean activation during self-evaluation 
#'  and one value for the mean activation during the control condition for all participants.
#'
#' @format A data frame with 149 rows and 4 variables:
#' \describe{
#'   \item{participant_id}{unique id for every participant}
#'   \item{age}{age in years (11.00-20.92)}
#'   \item{mpfc_self}{parameter estimates for the left medial prefrontal cortex during
#'   self-evaluation (-2.82-4.97)}
#'   \item{mpfc_control}{parameter estimates for the lmedial prefrontal cortex during
#'   the control condition (-7.17-3.50)}
#' }
#' @source {van der Cruijsen, R., Blankenstein, N. E., Spaans, J. P., Peters, S., & Crone, E. A. 
#'  (2023). Longitudinal self-concept development in adolescence. Social Cognitive and Affective 
#'  Neuroscience, 18(1), nsac062.}
#'  \doi{10.1093/scan/nsac062}
"self_eval"

#' Vicarious Charity task fMRI region of interest data
#'
#' A dataset containing the parameter estimates from
#'  the anatomical mask of the left nucleus accumbens (Harvard-Oxford subcortical atlas; 
#'  thresholded at 40%; center-of-mass coordinates x = −10, y = 12, z = −7; 28 voxels included),
#'  with one value for the mean activation during gaining for self and one value for the mean 
#'  activation during no-gain for self and charity for all participants.
#'
#' @format A data frame with 156 rows and 4 variables:
#' \describe{
#'   \item{participant_id}{unique id for every participant}
#'   \item{age}{age in years (11.00-21.17)}
#'   \item{nacc_selfgain}{parameter estimates for the left nucleus accumbens during
#'   gaining for self (-5.66-3.05)}
#'   \item{nacc_bothnogain}{parameter estimates for the left nucleus accumbens during
#'   no-gain for self and charity (-6.44-2.97)}
#' }
#' @source {Spaans, J., Peters, S., Becht, A., van der Cruijsen, R., van de Groep, S., 
#'  & Crone, E. A. (2023). Longitudinal neural and behavioral trajectories of charity 
#'  contributions across adolescence. Journal of Research on Adolescence, 33(2), 480–495.}
#'  \doi{10.1111/jora.12820}
"vicar_char"