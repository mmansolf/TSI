#' Example data (CTT)
#'
#' Example data for true score imputation under classical test theory (CTT).
#' Under the data-generating model, two groups (y=0 and y=1) were simulated
#' using normal distributions with mean and variance equal to 1 for y=0
#' and equal to 1.5 for y=1. Normal measurement error was then added to each
#' group corresponding to reliability (ratio of true to observed score
#' variance) of .6.
#'
#' @format A data frame with two variables:
#' \describe{
#'   \item{w}{An observed scoregenerated under CTT}
#'   \item{y}{A dummy variable indicating group membership}
#' }
#' @details
#'
#' See below for data-generating code.
#'
#' See \code{vignette('TSI')} or \code{example(TSI)} for example usage.
#'
#' \preformatted{
#' set.seed(0)
#' n=1000 #sample size
#' ratio=0.6 #ratio of true score variance to error variance; squared reliability coefficient
#' d=0.5 #mean difference
#' nimpute=10 #number of "posterior" draws of true score from modified conditional distribution of w
#' meanX=1 #mean of true score
#' meanW=1 #mean of observed score
#' varX=1 #variance of true score
#' varW=1 #variance of observed score
#'
#' x1=rnorm(n,meanX,sqrt(varX))
#' e1=rnorm(n,0,sqrt(varX*(1-ratio)/ratio))
#' w1=x1+e1
#' x2=rnorm(n,meanX+d*sqrt(varX),sqrt(varX))
#' e2=rnorm(n,0,sqrt(varX*(1-ratio)/ratio))
#' w2=x2+e2
#' #into the data frame
#' w=c(w1,w2)
#' y=c(rep(0,n),rep(1,n))
#' data_ctt=data.frame(w,y)
#' }
#'
"data_ctt"
