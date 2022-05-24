#' Custom mice imputation function for true score imputation
#'
#' This custom imputation function is used with the \code{mice}
#' package by setting method='truescore' for each variable imputed using
#' true score imputation, which will call this custom imputation function
#' \code{mice.impute.truescore}. Although possible, this function is not
#' meant to be run on its own; see documentation for other \code{mice}
#' imputation files, e.g., \code{\link[mice]{mice.impute.pmm}}, for details
#' on this usage. Example usage through the \code{mice} package is provided in
#' Examples below.
#'
#' @inherit mice::mice.impute.pmm return params
#'
#' @param calibration A list of calibration information used for true score
#' imputation. See below for details.
#'
#' @section Passing Calibration Information to \code{mice}:
#'
#' The \code{calibration} parameter is passed to the \code{mice} function
#' using the \code{blots} input. For each imputed true score, provide the
#' \code{calibration} information as a named list. The following elements
#' are required, in any order:
#'
#' \describe{
#'   \item{\code{os_name}}{Name of the variable in the data set containing the
#'   observed scores used for true score imputation}
#'   \item{\code{score_type}}{Type of score provided. Current options are
#'   \code{'CTT'}, corresponding to the classical test theory model of
#'   reliability; \code{'EAP'}, corresponding to expected a posteriori
#'   scoring in item response theory; and \code{'ML'}, corresponding to
#'   maximum likelihood scoring in item response theory. Each
#'   \code{score_type} requires specific other elements to be provided
#'   in \code{calibration} data; see below for these conditional elements.}
#'   \item{\code{mean}}{The mean of the score metric from calibration.
#'   For example, T scores are calibrated to a mean of 50, so if T scores
#'   are used, \code{mean} should be set to \code{50}.}
#'   \item{\code{var_ts}}{The variance of the score metric from calibration.
#'   For example, T scores are calibrated to a standard deviation of 10, so
#'   if T scores are used, \code{var_ts} should be set to \code{100}.}
#' }
#'
#' In addition, each \code{score_type} requires specific other elements
#' to be provided in \code{calibration} data:
#'
#' \describe{
#'   \item{\code{se_name}}{Required if \code{score_type == 'EAP'} or
#'   \code{score_type == 'ML'}. Name of the variable in the data set
#'   containing the standard error estimates of the observed scores
#'   provided in \code{os_name}.}
#'   \item{\code{reliability}}{Required if \code{score_type == 'CTT'}.
#'   Reliability estimate denoting the ratio of true score to
#'   observed score variance, as estimated from calibration.}
#' }
#'
#' @section Specifying the Predictor Matrix:
#' Based on (unpublished) simulation results, it seems the best way to specify
#' the predictor matrix for use in \code{mice} is for true scores to be
#' predicted from all observed variables but \emph{not} to predict other
#' missing data from the imputed true scores. This is the default behavior
#' when the \code{TSI} function is used, and we recommend, unless further
#' research identifies otherwise, that the same be done when using this
#' function to interact with \code{mice} directly.
#'
#' @examples
#' ##############
#' # CTT SCORES #
#' #add empty (NA) variables to data set to store true scores
#' data_ctt_2=data_ctt
#' data_ctt_2$TRUE_w=NA
#'
#' #true score imputation
#' set.seed(0)
#' mice.data=mice(data_ctt_2,m=5,
#'   blocks=list('TRUE_w'),
#'   method='truescore',
#'   calibration=list(os_name='w',
#'                    score_type='CTT',
#'                    reliability=0.6,
#'                    mean_ts=0,
#'                    var_ts=1),
#'   predictorMatrix=matrix(c(1,1,0),1,3,byrow=T),
#'   printFlag=F,
#'   remove.constant=F)
#' mice.data
#'
#' #analyze with imputed true scores
#' pool(with(mice.data,lm(TRUE_w~y)))
#'
#' #compare standard deviations of observed and imputed true scores
#' mice.data=complete(mice.data,'all')
#' sds=sapply(mice.data,function(d)apply(d,2,sd))
#' apply(sds,1,mean)
#'
#' ##############
#' # EAP SCORES #
#' #add empty (NA) variables to data set to store true scores
#' data_eap_2=data_eap
#' data_eap_2$Tx=NA
#' data_eap_2$Ty=NA
#'
#' #true score imputation
#' set.seed(0)
#' mice.data=mice(data_eap_2,m=5,maxit=5,
#'   method=c('pmm','pmm','pmm','pmm','pmm',
#'            'truescore','truescore'),
#'   blocks=list(Fx="Fx",Fy="Fy",SE.Fx="SE.Fx",SE.Fy="SE.Fy",m="m",
#'               Tx='Tx',Ty='Ty'),
#'   blots=list(Tx=list(calibration=list(os_name='Fx',
#'                                       se_name='SE.Fx',
#'                                       score_type='EAP',
#'                                       mean=50,
#'                                       var_ts=100,
#'                                       separated=T)),
#'              Ty=list(calibration=list(os_name='Fy',
#'                                       se_name="SE.Fy",
#'                                       score_type='EAP',
#'                                       mean=50,
#'                                       var_ts=100,
#'                                       separated=T))),
#'   predictorMatrix=matrix(c(0,1,1,1,1,0,0,
#'                            1,0,1,1,1,0,0,
#'                            1,1,0,1,1,0,0,
#'                            1,1,1,0,1,0,0,
#'                            1,1,1,1,0,0,0,
#'                            1,1,1,1,1,0,0,
#'                            1,1,1,1,1,0,0),7,7,byrow=T),
#'   printFlag=F,
#'   remove.constant=F)
#' mice.data
#'
#' #multiple regression with imputed true scores
#' pool(with(mice.data,lm(Ty~Tx+m)))
#' @export
mice.impute.truescore = function(y, ry, x, wy = NULL, calibration = NULL, ...) { # nolint: line_length_linter,object_name_linter.
  #######################################################################
  #INPUT VALIDATION: REJECT IF CALIBRATION DATA ARE NOT WELL-STRUCTURED #
  #######################################################################
  if (is.null(calibration$separated))
    calibration$separated = T

  #################### DATA PREPARATION
  # identify observed scores and standard errors
  score_add = calibration$mean
  score_mult = sqrt(calibration$var_ts)

  w = (x[, colnames(x) == calibration$os_name] - score_add) / score_mult
  se_w = x[, colnames(x) == calibration$se_name] / score_mult
  xdata = x[, which(!colnames(x) %in% c(calibration$os_name,
                                        calibration$se_name))]
  if (!is.data.frame(xdata)) {
    xdata = data.frame(xdata)
    names(xdata) = colnames(x)[
      which(!colnames(x) %in% c(calibration$os_name,
                                calibration$se_name))]
  }
  xdata = as.matrix(xdata)
  # filter only to cases with data on w
  which_2impute = which(!is.na(w) & apply(!is.na(xdata), 1, all))
  w = w[which_2impute]
  se_w = se_w[which_2impute]
  xdata = xdata[which_2impute, ]
  x = x[which_2impute, ]
  if (!is.matrix(xdata)) {
    xdata = data.frame(xdata)
    names(xdata) = colnames(x)[
      which(!colnames(x) %in% c(calibration$os_name,
                                calibration$se_name))]
  }
  xdata = as.matrix(xdata)
  nsample = length(which_2impute)

  ######################################################
  # DEFINE OBSERVED SCORE VARIANCE BASED ON SCORE TYPE #
  if (calibration$score_type == "CTT") {
    # mean is easy; same as OS mean
    calibration$mean_os = mean(w, na.rm = T)
    calibration$mean_ts = calibration$mean_os
    # get observed score variance from data
    calibration$var_os = rep(var(w, na.rm = T), length(w))
    # get error variance from calibration information only
    var_e = (1 - calibration$reliability) / calibration$reliability
    # compute true score variance for our data
    calibration$var_ts = calibration$var_os - var_e
    # compute reliability for our data
    calibration$reliability = calibration$var_ts / calibration$var_os
  } else if (calibration$score_type == "EAP") {
    # Lord, 1986, but given for normal prior.
    i_eap2mle = 1 / (se_w^2) - 1

    # hmmm...
    if (!calibration$separated) {
      w = w + w / mean(i_eap2mle)
    } else w = w + w / i_eap2mle

    calibration$mean_os = mean(w)
    calibration$mean_ts = calibration$mean_os
    calibration$var_os = var(w, na.rm = T)
    # MLE-style standard errors?
    if (!calibration$separated) {
      calibration$var_ts = calibration$var_os - mean(1 / (i_eap2mle))
    } else calibration$var_ts = calibration$var_os - 1 / (i_eap2mle)
    calibration$reliability = calibration$var_ts / calibration$var_os

    if (any(calibration$var_ts < 0))
      calibration$var_ts = ifelse(calibration$var_ts < 0,
                                  0.01, calibration$var_ts)
    if (any(calibration$var_os < 0))
      calibration$var_os = ifelse(calibration$var_os < 0,
                                  0.01, calibration$var_os)
    if (any(calibration$reliability < 0))
      calibration$reliability = ifelse(calibration$reliability < 0,
                                       0.01, calibration$reliability)

    if (any(calibration$var_ts < 0) |
        any(calibration$var_os < 0) |
        any(calibration$reliability < 0))
      stop("help")
  } else if (calibration$score_type == "ML") {
    calibration$var_os = var(w, na.rm = T)
    calibration$mean_os = mean(w, na.rm = T)
    calibration$mean_ts = calibration$mean_os
    # Mislevy, Beaton, Kaplan & Sheehan, 1992; page 138
    if (!calibration$separated) {
      calibration$var_ts = calibration$var_os - mean(se_w^2)
    } else calibration$var_ts = calibration$var_os - se_w^2

    # re-calculate reliability
    calibration$reliability = calibration$var_ts / calibration$var_os

    # set calibration pars to 'reasonable' minimum value
    # if they dip below zero
    if (any(calibration$var_ts < 0))
      calibration$var_ts = ifelse(calibration$var_ts < 0,
                                  0.01, calibration$var_ts)
    if (any(calibration$var_os < 0))
      calibration$var_os = ifelse(calibration$var_os < 0,
                                  0.01, calibration$var_os)
    if (any(calibration$reliability < 0))
      calibration$reliability = ifelse(calibration$reliability < 0,
                                       0.01, calibration$reliability)

    if (any(calibration$var_ts < 0) |
        any(calibration$var_os < 0) |
        any(calibration$reliability < 0))
      stop("help")
  }
  # make it a vector
  if (length(calibration$var_os) == 1)
    calibration$var_os = rep(calibration$var_os, nrow(xdata))
  #################
  # DO IMPUTATION #
  #maindata: analysis data, with score in front
  #calibdata: calibration information
  #NCALIB: sample size in calibration data
  #nsample: sample size in estimation data
  #M: number of imputations
  #N: number of second stage draws
  #K: number of parameters of interest
  #S: number of covariates

  imputed = miec(maindata = cbind(w, xdata),
                 calibdata = calibration,
                 ncalib = Inf, nsample = nsample,
                 m = 1, n = 1, k = ncol(xdata), s = 0)
  # add NA's back in
  out = rep(NA, length(y))
  out[which_2impute] = imputed * score_mult + score_add
  out
}
