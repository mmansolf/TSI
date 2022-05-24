#' True score imputation
#'
#' Conduct true score imputation on variables with psychometric error,
#' optionally in concert with multiple imputation for missing data. This
#' function calls the \code{mice} function in the package of
#' the same name, using the custom imputation function
#' \code{\link[TSI]{mice.impute.truescore}} for imputation of mismeasured
#' variables. Direct calls to \code{mice} can get complicated (see
#' documentation of \code{\link[TSI]{mice.impute.truescore}} for examples),
#' so this function was created as a convenicene function to more easily
#' generate those function calls.
#'
#' @param data Data frame on which to conduct imputation. By default, columns
#' with missing values which are numeric will be imputed with the \code{pmm}
#' method from \code{mice}, columns with names in \code{os_names} will be
#' imputed using true score imputation, and non-numeric columns will be
#' ignored.
#' @param os_names Character vector of names of variables in \code{data} on
#' which to use true score imputation.
#' @param score_types Character vector specifying psychometric model(s) used
#' for true score imputation. Currently available options are \code{'CTT'}
#' for classical test theory, \code{'EAP'} for item response theory with
#' expected a posteriori scoring, and \code{'ML'} for item response theory
#' with maximum likelihood scoring (not recommended). The selected  model
#' should match how scores were generated, which requires some understanding
#' of the scoring process; for instance, HealthMeasures instruments, which
#' include PROMIS, NIH Toolbox, and NeuroQOL measures, use EAP scoring to
#' generate T scores and therefore \code{score_types='EAP'} would be
#' appropriate when using these T scores.
#' @param se_names Required for \code{score_types='EAP'} or
#' \code{score_types='ML'}. Character vector of names of variables in data set
#' containing standard errors for score variables specified by \code{os_names}.
#' One variable must be specified for each variable in \code{os_names}. Not
#' required for \code{score_types='CTT'}.
#' @param reliability Required for \code{score_types='CTT'}. Numeric vector of
#' reliability estimates, one for each observed score variable in
#' \code{os_names} referring to the reliability of the corresponding variable
#' named in \code{os_names}.
#' @param metrics Character vector of metrics of true scores for
#' imputation. Available values are \code{'z'} for z scores (mean 0,
#' variance 1), \code{'T'} for T scores (mean 50, variance 100), and
#' \code{'standard'} for standard (IQ metric) scores (mean 100, variance 225).
#' Either \code{metrics} or both \code{mean} and \code{var_ts} below must be
#' specified for each variable, with each element referring to the
#' corresponding variable named in \code{os_names}.
#' @param mean Numeric vector of means of true scores for imputation. Must be
#' specified if \code{metrics} is not specified.
#' @param var_ts Numeric vector of variances of true scores for imputation.
#' Must be specified if \code{metrics} is not specified.
#' @param separated Logical vector indicating whether, for variables imputed
#' with \code{score_types='EAP'} or \code{score_types='ML'}, true score
#' imputation uses an average standard error (\code{separated=F}), which runs
#' faster but doesn't account for differential measurement error of the
#' observed scores for each respondent, or whether separate standard errors
#' are used for each value of each observed score (\code{separated=T}), which
#' runs slower but accounts for differential measurement error.
#' @param ts_names Optional vector of names of true score variables which
#' will be created. Each element of \code{ts_names} denotes the name of the
#' variable which will be created by \code{TSI} based on observed scores from
#' the corresponding element of \code{os_names}. The default value of
#' \code{NULL} results in the prefix \code{true_} being prepended to each
#' element of \code{os_names} when generating the imputed true scores.
#' @param mice_args Named list of additional arguments passed to \code{mice}
#'
#' @examples
#' ##############
#' # CTT SCORES #
#' mice.data=TSI(data_ctt,
#'               os_names='w',
#'               score_types='CTT',
#'               reliability=0.6,
#'               mean=0,
#'               var_ts=1,
#'               mice_args=list(m=5,printFlag=F))
#' mice.data
#'
#' #analyze with imputed true scores
#' pool(with(mice.data,lm(true_w~y)))
#'
#' #compare standard deviations of observed and imputed true scores
#' mice.data=complete(mice.data,'all')
#' sds=sapply(mice.data,function(d)apply(d,2,sd))
#' apply(sds,1,mean)
#'
#' ##############
#' # EAP SCORES #
#' set.seed(0)
#' mice.data=TSI(data_eap,
#'               os_names=c('Fx','Fy'),
#'               se_names=c('SE.Fx','SE.Fy'),
#'               metrics='T',
#'               score_types='EAP',
#'               separated=T,
#'               ts_names=c('Tx','Ty'),
#'               mice_args=c(m=5,maxit=5,printFlag=F))
#' mice.data
#'
#' #multiple regression with imputed true scores
#' pool(with(mice.data,lm(Ty~Tx+m)))
#' @export
TSI=function( #nolint
  data, os_names, score_types,
  se_names = NULL, metrics = NULL, mean = NULL, var_ts = NULL, reliability = NULL,
  separated = rep(T, length(os_names)), ts_names = paste0("true_", os_names),
  mice_args) {

  ###############
  # BLOT CHECKS #
  #need at least one os_name
  p_to_impute = length(os_names)
  if (p_to_impute == 0) stop("
Provide the name of at least one observed score to impute as os_names.")
  args_to_test = setNames(
    list(list(se_names), list(metrics), list(score_types), list(mean),
         list(var_ts), list(separated)),
    c("se_names", "metrics", "score_types", "mean", "var_ts", "separated")
  )

  #test types
  types_to_test = c("character", "character", "character", "numeric",
                    "numeric", "logical")
  for (i in seq_len(length(args_to_test))) {
    if (!class(args_to_test[[i]][[1]]) %in% c("NULL", types_to_test[i]))
    stop(paste0("
Expected ", names(args_to_test)[i], " to be of type ",
                types_to_test[i], " or NULL; was ",
                class(args_to_test[[i]][[1]]), "."
    ))
  }

  #test lengths
  for (i in seq_len(length(args_to_test))) {
    if (!length(args_to_test[[i]][[1]]) %in% c(0, 1, p_to_impute))
    stop(paste0("
The number of elements in ",
                names(args_to_test)[i],
                " should be 1 or match the number of elements in os_names."
    ))
  }

  #stretch lengths if necessary
  if (length(metrics) == 1)metrics = rep(metrics, p_to_impute)
  if (length(score_types) == 1)score_types = rep(score_types, p_to_impute)
  if (length(mean) == 1)mean = rep(mean, p_to_impute)
  if (length(var_ts) == 1)var_ts = rep(var_ts, p_to_impute)
  if (length(separated) == 1)separated = rep(separated, p_to_impute)

  #test length of ts_names
  if (!length(ts_names) == length(os_names))
    stop("Must provide as many true score names (ts_names) as observed score names (os_names),  or leave ts_names blank and the prefix 'true_' will be appended to os_names to name the resulting true scores")

  #test permissible score_types and metrics
  if (!all(score_types %in% c("CTT", "EAP", "ML")))
    stop("Please specify score_type from the available types for true score imputation ('CTT', 'EAP', or 'ML')")

  #test permissible score_types and metrics
  if (!all(metrics %in% c(NULL, NA, "z", "T", "standard")))
    stop("Please specify metric from the available metrics for true score imputation ('z', 'T', 'standard')")

  #warn if ML is used
  if (any(score_types %in% c("ML")))
    stop("Warning: Maximum likelihood (ML) scoring is not recommended due to poor simulation performance. Proceed with caution.")

  for (i in seq_len(length(score_types))) {
    #test metrics: either have a metric, or have both mean and var_ts
    is_null_or_na = function(x) if (!is.null(x)) is.na(x) else is.null(x)

    is_null_metric = is_null_or_na(metrics[i])
    is_null_mean = is_null_or_na(mean[i])
    is_null_var_ts = is_null_or_na(var_ts[i])

    if ((is_null_metric & (is_null_mean | is_null_var_ts)) |
       (!is_null_metric & (!is_null_mean | !is_null_var_ts))) stop(paste0("Problem with variable ", i, ": Either assign a metric to each true score variable (e.g., 'T' for T scores) or assign BOTH a mean and var_ts for that variable."))
    #test that EAP and ML scoring have se_names
    if (score_types[i] %in% c("EAP", "ML")) {
      is_null_sename = is.null(se_names[i]) | is.na(se_names[i])
      if (is_null_sename) stop(paste0("Problem with variable ", i, ": Each observed score (os_name) based on EAP or ML scoring must include a corresponding standard error (se_names)."))
    } else if (score_types[i] == "CTT") {
      #test that CTT have reliability
      is_null_reliability = is.null(reliability[i]) | is.na(reliability[i])
      if (is_null_reliability) stop(paste0("Problem with variable ", i, ": Each observed score (os_name) based on CTT scoring must include a corresponding estimate of reliability."))
    }
  }

  ###############
  # DATA CHECKS #
  #test that all of os_names and se_names are in data
  missing_variables = c(os_names, se_names)[which(!c(os_names, se_names) %in% names(data))]
  if (length(missing_variables) > 0)
    stop(paste0("The following os_names and/or se_names were not found in the data: ",
                paste(missing_variables, collapse = ", "), "."))

  #test that data is data.frame
  if (!is.data.frame(data)) stop("Please provide data as data.frame")

  non_numeric_variables = names(data)[which(!sapply(data, class) %in% c("integer", "numeric"))]
  if (length(non_numeric_variables) > 0) {
    #test that all observed scores and standard errors are numeric
    if (any(c(os_names, se_names) %in% non_numeric_variables))
      stop(paste0("The following observed score and/or standard error variables are not numeric: ",
                  paste(intersect(non_numeric_variables, c(os_names, se_names)), collapse = ", "),
                  ". Please convert them to numeric prior to true score imputation."))
    #print warning that non-numeric variables will be ignored
    warning(paste0("The following variables are not numeric and will be ignored during imputation: ",
                   paste(non_numeric_variables, collapse = ", "),
                   ". This implementation of true score imputation does not allow non-numeric variables in the imputation model."))
  }

  #####################
  # ALL DONE; PREPARE #
  print("Preliminary checks passed for true score imputation! Building call to mice...")

  #blocks
  blocks = setNames(names(data), names(data))
  #method
  method = rep("pmm", ncol(data))
  #predictor_matrix
  predictor_matrix = matrix(1, ncol(data), ncol(data)) - diag(ncol(data))
  #drop non_numeric_variables
  if (length(non_numeric_variables) > 0) {
    predictor_matrix[which(names(data) %in% non_numeric_variables), ] = 0
    predictor_matrix[, which(names(data) %in% non_numeric_variables)] = 0
    method[which(names(data) %in% non_numeric_variables)] = ""
  }
  #blots
  blots = list()
  for (i in seq_len(length(ts_names))) {
    #initialize and name blot
    blots[[ts_names[i]]] = list()
    blots[[ts_names[i]]]$os_name = os_names[i]
    if (score_types[i] %in% c("EAP", "ML")) {
      blots[[ts_names[i]]]$se_name = se_names[i]
    } else if (score_types[i] == "CTT") {
      blots[[ts_names[i]]]$reliability = reliability[i]
    }
    blots[[ts_names[i]]]$score_type = score_types[i]
    blots[[ts_names[i]]]$separated = separated[i]
    if (!is_null_or_na(metrics[i])) {
      if (metrics[i] == "z") {
        mean[i] = 0
        var_ts[i] = 1
      } else if (metrics[i] == "T") {
        mean[i] = 50
        var_ts[i] = 100
      } else if (metrics[i] == "standard") {
        mean[i] = 100
        var_ts[i] = 225
      }
    }
    blots[[ts_names[i]]]$mean = mean[i]
    blots[[ts_names[i]]]$var_ts = var_ts[i]
  }
  #put into calibration list
  blots = lapply(blots, function(x)list(calibration = x))

  ############################
  # ADD TS VARIABLES TO DATA #
  for (n in ts_names) {
    data[[n]] = NA
    blocks[[n]] = n
    method = c(method, "truescore")
    predictor_matrix = rbind(predictor_matrix, 1) #add rows of 1s; predict TS's from everything
    if (length(non_numeric_variables) > 0) {
      predictor_matrix[, which(names(data) %in% non_numeric_variables)] = 0 #except non-numeric variables
    }
  }
  for (n in ts_names) {
    predictor_matrix = cbind(predictor_matrix, 0) #but don't use them to predict anything
  }

  #######
  # RUN #
  do.call(mice, c(list(data = data,
                      method = method,
                      blocks = blocks,
                      blots = blots,
                      predictorMatrix = predictor_matrix,
                      remove.constant = F),
                 mice_args))
}
