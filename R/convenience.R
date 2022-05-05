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
#' method from \code{mice}, columns with names in \code{OSNAMES} will be
#' imputed using true score imputation, and non-numeric columns will be
#' ignored.
#' @param OSNAMES Character vector of names of variables in \code{data} on
#' which to use true score imputation.
#' @param scoreTypes Character vector specifying psychometric model(s) used
#' for true score imputation. Currently available options are \code{'CTT'}
#' for classical test theory, \code{'EAP'} for item response theory with
#' expected a posteriori scoring, and \code{'ML'} for item response theory
#' with maximum likelihood scoring (not recommended). The selected  model
#' should match how scores were generated, which requires some understanding
#' of the scoring process; for instance, HealthMeasures instruments, which
#' include PROMIS, NIH Toolbox, and NeuroQOL measures, use EAP scoring to
#' generate T scores and therefore \code{scoreTypes='EAP'} would be
#' appropriate when using these T scores.
#' @param SENAMES Required for \code{scoreTypes='EAP'} or
#' \code{scoreTypes='ML'}. Character vector of names of variables in data set
#' containing standard errors for score variables specified by \code{OSNAMES}.
#' One variable must be specified for each variable in \code{OSNAMES}. Not
#' required for \code{scoreTypes='CTT'}.
#' @param reliability Required for \code{scoreTypes='CTT'}. Numeric vector of
#' reliability estimates, one for each observed score variable in
#' \code{OSNAMES} referring to the reliability of the corresponding variable
#' named in \code{OSNAMES}.
#' @param metrics Character vector of metrics of true scores for
#' imputation. Available values are \code{'z'} for z scores (mean 0,
#' variance 1), \code{'T'} for T scores (mean 50, variance 100), and
#' \code{'standard'} for standard (IQ metric) scores (mean 100, variance 225).
#' Either \code{metrics} or both \code{mean} and \code{var_ts} below must be
#' specified for each variable, with each element referring to the
#' corresponding variable named in \code{OSNAMES}.
#' @param mean Numeric vector of means of true scores for imputation. Must be
#' specified if \code{metrics} is not specified.
#' @param var_ts Numeric vector of variances of true scores for imputation.
#' Must be specified if \code{metrics} is not specified.
#' @param separated Logical vector indicating whether, for variables imputed
#' with \code{scoreTypes='EAP'} or \code{scoreTypes='ML'}, true score
#' imputation uses an average standard error (\code{separated=F}), which runs
#' faster but doesn't account for differential measurement error of the
#' observed scores for each respondent, or whether separate standard errors
#' are used for each value of each observed score (\code{separated=T}), which
#' runs slower but accounts for differential measurement error.
#' @param TSNAMES Optional vector of names of true score variables which
#' will be created. Each element of \code{TSNAMES} denotes the name of the
#' variable which will be created by \code{TSI} based on observed scores from
#' the corresponding element of \code{OSNAMES}. The default value of
#' \code{NULL} results in the prefix \code{TRUE_} being prepended to each
#' element of \code{OSNAMES} when generating the imputed true scores.
#' @param mice_args Named list of additional arguments passed to \code{mice}
#'
#' @examples
#' ##############
#' # CTT SCORES #
#' mice.data=TSI(data_ctt,
#'               OSNAMES='w',
#'               scoreTypes='CTT',
#'               reliability=0.6,
#'               mean=0,
#'               var_ts=1,
#'               mice_args=list(m=10,printFlag=F))
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
#' set.seed(0)
#' mice.data=TSI(data_eap,
#'               OSNAMES=c('Fx','Fy'),
#'               SENAMES=c('SE.Fx','SE.Fy'),
#'               metrics='T',
#'               scoreTypes='EAP',
#'               separated=T,
#'               TSNAMES=c('Tx','Ty'),
#'               mice_args=c(m=10,maxit=10,printFlag=F))
#' mice.data
#'
#' #multiple regression with imputed true scores
#' pool(with(mice.data,lm(Ty~Tx+m)))
#' @export
TSI=function(data,OSNAMES,scoreTypes,
             SENAMES=NULL,metrics=NULL,mean=NULL,var_ts=NULL,reliability=NULL,
             separated=rep(T,length(OSNAMES)),TSNAMES=paste0("TRUE_",OSNAMES),
             mice_args){
  # data=data.frame(m=1:4,Fx=1,Fy=2,SE.Fx=3,SE.Fy=4)
  # OSNAMES=c('Fx','Fy')
  # SENAMES=c('SE.Fx','SE.Fy')
  # metrics=c('T','T')
  # scoreTypes=c('EAP','EAP')
  # mean=NULL
  # var_ts=NULL
  # reliability=NULL
  # separated=c(T,F)
  # TSNAMES=paste0('TRUE_',OSNAMES)

  ###############
  # BLOT CHECKS #
  #need at least one OSNAME
  p_to_impute=length(OSNAMES)
  if(p_to_impute==0) stop('
Must provide the name of at least one observed score variable to impute as OSNAMES.')
  args_to_test=setNames(
    list(list(SENAMES),list(metrics),list(scoreTypes),list(mean),list(var_ts),list(separated)),
    c('SENAMES','metrics','scoreTypes','mean','var_ts','separated')
  )

  #test types
  types_to_test=c('character','character','character','numeric','numeric','logical')
  for(i in 1:length(args_to_test)){
    if(!class(args_to_test[[i]][[1]])%in%c('NULL',types_to_test[i])) stop(paste0('
Expected ',names(args_to_test)[i],' to be of type ',types_to_test[i],' or NULL; was ',class(args_to_test[[i]][[1]]),'.'
    ))
  }

  #test lengths
  for(i in 1:length(args_to_test)){
    if(!length(args_to_test[[i]][[1]])%in%c(0,1,p_to_impute)) stop(paste0('
The number of elements in ',names(args_to_test)[i],' should be 1 or match the number of elements in OSNAMES.'
    ))
  }

  #stretch lengths if necessary
  if(length(metrics)==1)metrics=rep(metrics,p_to_impute)
  if(length(scoreTypes)==1)scoreTypes=rep(scoreTypes,p_to_impute)
  if(length(mean)==1)mean=rep(mean,p_to_impute)
  if(length(var_ts)==1)var_ts=rep(var_ts,p_to_impute)
  if(length(separated)==1)separated=rep(separated,p_to_impute)

  #test length of TSNAMES
  if(!length(TSNAMES)==length(OSNAMES)) stop('Must provide as many true score names (TSNAMES) as observed score names (OSNAMES), or leave TSNAMES blank and the prefix "TRUE_" will be appended to OSNAMES to name the resulting true scores')

  #test permissible scoreTypes and metrics
  if(!all(scoreTypes%in%c('CTT','EAP','ML')))
    stop("Please specify scoreType from the available types for true score imputation ('CTT', 'EAP', or 'ML')")

  #test permissible scoreTypes and metrics
  if(!all(metrics%in%c(NULL,NA,'z','T','standard')))
    stop("Please specify metric from the available metrics for true score imputation ('z', 'T', 'standard')")

  #warn if ML is used
  if(any(scoreTypes%in%c('ML')))
    stop("Warning: Maximum likelihood (ML) scoring is not recommended due to poor simulation performance. Proceed with caution.")

  for(i in 1:length(scoreTypes)){
    #test metrics: either have a metric, or have both mean and var_ts
    is_null_or_na=function(x) if(!is.null(x)) is.na(x) else is.null(x)

    is_null_metric=is_null_or_na(metrics[i])
    is_null_mean=is_null_or_na(mean[i])
    is_null_var_ts=is_null_or_na(var_ts[i])

    if((is_null_metric & (is_null_mean | is_null_var_ts)) |
       (!is_null_metric & (!is_null_mean | !is_null_var_ts))) stop(paste0("Problem with variable ",i,": Either assign a metric to each true score variable (e.g., 'T' for T scores) or assign BOTH a mean and var_ts for that variable."))
    #test that EAP and ML scoring have SENAMES
    if(scoreTypes[i]%in%c('EAP','ML')){
      is_null_SENAME=is.null(SENAMES[i]) | is.na(SENAMES[i])
      if(is_null_SENAME) stop(paste0("Problem with variable ",i,": Each observed score (OSNAME) based on EAP or ML scoring must include a corresponding standard error (SENAMES)."))
    } else if(scoreTypes[i]=='CTT'){
      #test that CTT have reliability
      is_null_reliability=is.null(reliability[i]) | is.na(reliability[i])
      if(is_null_reliability) stop(paste0("Problem with variable ",i,": Each observed score (OSNAME) based on CTT scoring must include a corresponding estimate of reliability."))
    }
  }

  ###############
  # DATA CHECKS #
  #test that all of OSNAMES and SENAMES are in data
  missing_variables=c(OSNAMES,SENAMES)[which(!c(OSNAMES,SENAMES)%in%names(data))]
  if(length(missing_variables)>0)
    stop(paste0("The following OSNAMES and/or SENAMES were not found in the data: ",
                paste(missing_variables,collapse=", "),'.'))

  #test that data is data.frame
  if(!is.data.frame(data)) stop('Please provide data as data.frame')

  non_numeric_variables=names(data)[which(!sapply(data,class)%in%c('integer','numeric'))]
  if(length(non_numeric_variables)>0){
    #test that all observed scores and standard errors are numeric
    if(any(c(OSNAMES,SENAMES)%in%non_numeric_variables))
      stop(paste0("The following observed score and/or standard error variables are not numeric: ",
                  paste(intersect(non_numeric_variables,c(OSNAMES,SENAMES)),collapse=", "),
                  '. Please convert them to numeric prior to true score imputation.'))
    #print warning that non-numeric variables will be ignored
    warning(paste0("The following variables are not numeric and will be ignored during imputation: ",
                   paste(non_numeric_variables,collapse=", "),
                   '. This implementation of true score imputation does not allow non-numeric variables in the imputation model.'))
  }

  #####################
  # ALL DONE; PREPARE #
  print('Preliminary checks passed for true score imputation! Building call to mice...')

  #blocks
  blocks=setNames(names(data),names(data))
  #method
  method=rep('pmm',ncol(data))
  #predictorMatrix
  predictorMatrix=matrix(1,ncol(data),ncol(data))-diag(ncol(data))
  #drop non_numeric_variables
  if(length(non_numeric_variables)>0){
    #blocks=blocks[-which(names(data)%in%non_numeric_variables)]
    predictorMatrix[which(names(data)%in%non_numeric_variables),]=0
    predictorMatrix[,which(names(data)%in%non_numeric_variables)]=0
    method[which(names(data)%in%non_numeric_variables)]=''
  }
  #blots
  blots=list()
  for(i in 1:length(TSNAMES)){
    #initialize and name blot
    blots[[TSNAMES[i]]]=list()
    blots[[TSNAMES[i]]]$OSNAME=OSNAMES[i]
    if(scoreTypes[i]%in%c("EAP","ML")){
      blots[[TSNAMES[i]]]$SENAME=SENAMES[i]
    } else if(scoreTypes[i]=='CTT'){
      blots[[TSNAMES[i]]]$reliability=reliability[i]
    }
    blots[[TSNAMES[i]]]$scoreType=scoreTypes[i]
    blots[[TSNAMES[i]]]$separated=separated[i]
    if(!is_null_or_na(metrics[i])){
      if(metrics[i]=='z'){
        mean[i]=0
        var_ts[i]=1
      } else if(metrics[i]=='T'){
        mean[i]=50
        var_ts[i]=100
      } else if(metrics[i]=='standard'){
        mean[i]=100
        var_ts[i]=225
      }
    }
    blots[[TSNAMES[i]]]$mean=mean[i]
    blots[[TSNAMES[i]]]$var_ts=var_ts[i]
  }
  #put into calibration list
  blots=lapply(blots,function(x)list(calibration=x))

  ############################
  # ADD TS VARIABLES TO DATA #
  for(n in TSNAMES){
    data[[n]]=NA
    blocks[[n]]=n
    method=c(method,'truescore')
    predictorMatrix=rbind(predictorMatrix,1) #add rows of 1s; predict TS's from everything
    if(length(non_numeric_variables)>0){
      predictorMatrix[,which(names(data)%in%non_numeric_variables)]=0 #except non-numeric variables
    }
  }
  for(n in TSNAMES){
    predictorMatrix=cbind(predictorMatrix,0) #but don't use them to predict anything
  }

  #######
  # RUN #
  do.call(mice,c(list(data=data,
                      method=method,
                      blocks=blocks,
                      blots=blots,
                      predictorMatrix=predictorMatrix,
                      remove.constant=F),
                 mice_args))
}
