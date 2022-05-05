#' Example data (EAP)
#'
#' Example data for true score imputation under item response theory using
#' expected a posteriori (EAP) scoring. Under the data-generating model,
#' data were simulated on \code{x}, \code{y}, and \code{m} consistent with
#' the multiple regression model \code{y=1+0.4*x+0.6*m+e, e~N(0,1)}.
#' \code{x} and \code{m} were simulated according to a standard normal
#' distribution. After data generation, \code{x} and \code{y} were used
#' to simulate item response data on the 10-item Perceived Stress Scale
#' (Cohen, Karmarck, & Mermelstein, 1983) using the NIH Toolbox emotion
#' study norming parameters (Kupst et al., 2015). Then, the simulated item
#' response data were scored using the same item parameters, yielding T-scores
#' and standard errors for \code{x} and \code{y}.
#'
#' @format A data frame with five variables:
#' \describe{
#'   \item{Fx}{T score for predictor \code{x}}
#'   \item{Fy}{T score for outcome \code{y}}
#'   \item{SE.Fx}{T-score standard error for \code{x}}
#'   \item{SE.Fy}{T-score standard error for \code{y}}
#'   \item{m}{Observed predictor \code{m}}
#' }
#'
#' @details
#'
#' See below for data-generating code. Only code for generating the true
#' \code{x}, \code{y}, and \code{m} are provided because the NIH Toolbox
#' item parameters are proprietary.
#'
#' See \code{vignette('TSI')} or \code{example(TSI)} for example usage.
#'
#' \preformatted{
#' set.seed(1)
#' x=rnorm(n,0,1)
#' m=rnorm(n,0,1)
#' y=1+0.4*x+0.6*m+rnorm(n,0,1)
#' }
#'
#' @references
#' Cohen, S., Kamarck, T., & Mermelstein, R. (1983). A global measure of perceived stress,
#' Journal of Health and Social Behavior, vol. 24.
#' Kupst, M. J., Butt, Z., Stoney, C. M., Griffith, J. W., Salsman, J. M., Folkman, S., &
#' Cella, D. (2015). Assessment of stress and self-efficacy for the NIH Toolbox for Neurological
#' and Behavioral Function. Anxiety, Stress, & Coping, 28(5), 531-544.
#'
"data_eap"
