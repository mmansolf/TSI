#' True Score Imputation with mice
#'
#' This custom imputation function is used with the \code{mice}
#' package by setting method='truescore' for each variable imputed using
#' true score imputation, which will call this custom imputation function
#' \code{mice.impute.truescore}. Although possible, this function is not
#' meant to be run on its own; see documentation for other \code{mice}
#' imputation files, e.g., \code{\link[mice]{mice.impute.pmm}}, for details
#' on this usage. Example usage through the mice package is provided in
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
#'   \item{\code{OSNAME}}{Name of the variable in the data set containing the
#'   observed scores used for true score imputation}
#'   \item{\code{scoreType}}{Type of score provided. Current options are
#'   \code{'CTT'}, corresponding to the classical test theory model of
#'   reliability; \code{'EAP'}, corresponding to expected a posteriori
#'   scoring in item response theory; and \code{'ML'}, corresponding to
#'   maximum likelihood scoring in item response theory. Each
#'   \code{scoreType} requires specific other elements to be provided
#'   in \code{calibration} data; see below for these conditional elements.}
#'   \item{\code{mean}}{The mean of the score metric from calibration. For example,
#'   T scores are calibrated to a mean of 50, so if T scores are used,
#'   \code{mean} should be set to \code{50}.}
#'   \item{\code{varTS}}{The variance of the score metric from calibration. For
#'   example, T scores are calibrated to a standard deviation of 10, so
#'   if T scores are used, \code{varTS} should be set to \code{100}.}
#' }
#'
#' In addition, each \code{scoreType} requires specific other elements
#' to be provided in \code{calibration} data:
#'
#' \describe{
#'   \item{\code{SENAME}}{Required if \code{scoreType == 'EAP'} or
#'   \code{scoreType == 'ML'}. Name of the variable in the data set
#'   containing the standard error estimates of the observed scores
#'   provided in \code{OSNAME}.}
#'   \item{\code{reliability}}{Required if \code{scoreType == 'CTT'}.
#'   Reliability estimate denoting the ratio of true score to
#'   observed score variance, as estimated from calibration.}
#' }
#'
#' @section Specifying the Predictor Matrix:
#' Coming soon!
#'
#' @examples
#' Coming soon!
mice.impute.truescore=function(y,ry,x,wy=NULL,
                               calibration=NULL,...){
  # print(head(cbind(y,x)))
  # print(apply(x,2,mean))
  ########################################################################
  # INPUT VALIDATION: REJECT IF CALIBRATION DATA ARE NOT WELL-STRUCTURED #
  if(is.null(calibration$separated))calibration$separated=T

  ####################
  # DATA PREPARATION #
  #identify observed scores and standard errors
  scoreAdd=calibration$mean
  scoreMult=sqrt(calibration$varTS)
  w=(x[,colnames(x)==calibration$OSNAME]-scoreAdd)/scoreMult
  se.w=x[,colnames(x)==calibration$SENAME]/scoreMult
  xdata=x[,which(!colnames(x)%in%c(calibration$OSNAME,calibration$SENAME))]
  if(!is.data.frame(xdata)){
    xdata=data.frame(xdata)
    names(xdata)=colnames(x)[which(!colnames(x)%in%c(calibration$OSNAME,calibration$SENAME))]
  }
  xdata=as.matrix(xdata)
  #filter only to cases with data on w
  which.2impute=which(!is.na(w) & apply(!is.na(xdata),1,all))
  w=w[which.2impute]
  se.w=se.w[which.2impute]
  xdata=xdata[which.2impute,]
  x=x[which.2impute,]
  if(!is.matrix(xdata)){
    xdata=data.frame(xdata)
    names(xdata)=colnames(x)[which(!colnames(x)%in%c(calibration$OSNAME,calibration$SENAME))]
  }
  xdata=as.matrix(xdata)
  NSAMPLE=length(which.2impute)

  ######################################################
  # DEFINE OBSERVED SCORE VARIANCE BASED ON SCORE TYPE #
  if(calibration$scoreType=='CTT'){
    #mean is easy; same as OS mean
    calibration$meanOS=mean(w,na.rm=T)
    calibration$meanTS=calibration$meanOS
    #get observed score variance from data
    calibration$varOS=rep(var(w,na.rm=T),length(w))
    #get error variance from calibration information only
    varE=(1-calibration$reliability)/calibration$reliability
    #compute true score variance for our data
    calibration$varTS=calibration$varOS-varE
    #compute reliability for our data
    calibration$reliability=calibration$varTS/calibration$varOS
  } else if(calibration$scoreType=='EAP'){
    #Lord, 1986, but given for normal prior.
    I.EAP=1/(se.w^2)
    I.EAP2MLE=1/(se.w^2)-1
    # I.EAP=1/(x[,colnames(x)==calibration$SENAME]^2)

    #hmmm...
    # print(paste0('means ',calibration$OSNAME))
    # print(mean(w))
    if(!calibration$separated){
      w=w+w/mean(I.EAP2MLE)
    } else w=w+w/I.EAP2MLE
    # print(mean(w))
    calibration$meanOS=mean(w)
    calibration$meanTS=calibration$meanOS
    calibration$varOS=var(w,na.rm=T)
    #MLE-style standard errors?
    # original.varTS=calibration$varTS
    if(!calibration$separated){
      calibration$varTS=calibration$varOS-mean(1/(I.EAP2MLE))
    } else calibration$varTS=calibration$varOS-1/(I.EAP2MLE)
    # if(!calibration$separated){
    #   calibration$varTS=1-mean(1/I.EAP2MLE)
    # } else calibration$varTS=1-1/I.EAP2MLE
    calibration$reliability=calibration$varTS/calibration$varOS


    # if(any(calibration$varTS<0) |
    #    any(calibration$varOS<0) |
    #    any(calibration$reliability<0)) print('hi')

    if(any(calibration$varTS<0)) calibration$varTS=ifelse(
      calibration$varTS<0,
      0.01,calibration$varTS)
    if(any(calibration$varOS<0)) calibration$varOS=ifelse(
      calibration$varOS<0,
      0.01,calibration$varOS)
    if(any(calibration$reliability<0)) calibration$reliability=ifelse(
      calibration$reliability<0,
      0.01,calibration$reliability)

    if(any(calibration$varTS<0) |
       any(calibration$varOS<0) |
       any(calibration$reliability<0)) stop('help')
  } else if(calibration$scoreType=='ML'){
    #var(os)=var(ts)+SE^2
    calibration$varOS=var(w,na.rm=T)
    calibration$meanOS=mean(w,na.rm=T)
    calibration$meanTS=calibration$meanOS
    #Mislevy, Beaton, Kaplan & Sheehan, 1992; page 138
    # print(calibration[c('meanOS','meanTS','varOS','varTS','reliability')]%>%map(summary))
    # original.varTS=calibration$varTS
    if(!calibration$separated){
      calibration$varTS=calibration$varOS-mean(se.w^2)
    } else calibration$varTS=calibration$varOS-se.w^2

    #re-calculate reliability
    calibration$reliability=calibration$varTS/calibration$varOS
    # calibration[c('varOS','varTS','meanOS','meanTS','reliability')]%>%walk(~summary(.)%>%print)

    #set calibration pars to "reasonable" minimum value if they dip below zero
    if(any(calibration$varTS<0)) calibration$varTS=ifelse(
      calibration$varTS<0,
      0.01,calibration$varTS)
    if(any(calibration$varOS<0)) calibration$varOS=ifelse(
      calibration$varOS<0,
      0.01,calibration$varOS)
    if(any(calibration$reliability<0)) calibration$reliability=ifelse(
      calibration$reliability<0,
      0.01,calibration$reliability)

    if(any(calibration$varTS<0) |
       any(calibration$varOS<0) |
       any(calibration$reliability<0)) stop('help')
  }
  #make it a vector
  if(length(calibration$varOS)==1)calibration$varOS=rep(calibration$varOS,nrow(xdata))
  # print(summary(w))
  # for(i in 1:ncol(xdata))print(summary(xdata[,i]))
  #################
  # DO IMPUTATION #
  imputed=MIEC(maindata=cbind(w,xdata),
               calibdata=calibration,
               NCALIB=Inf, #sample size in calibration data?
               NSAMPLE=NSAMPLE, #sample size in estimation data?
               M=1, #number of imputations
               N=1, #number of second stage draws?
               K=ncol(xdata), #number of parameters of interest?
               S=0, #number of covariates?
               CTT=T) #Mansolf's parameters from CTT
  #add NA's back in
  out=rep(NA,length(y))
  # print(summary(imputed))
  out[which.2impute]=imputed*scoreMult+scoreAdd
  out
}
