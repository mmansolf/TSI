#main function \\
MIEC <- function(maindata,calibdata,NCALIB,NSAMPLE,M,N,K,S,CTT) {
  MIimputedXbasedoncalib=c()
  multipleImputedX=c()
  twostageMIimputedX=c()
  P=K+S
  count = 0
  while (count < M) {
    #Step-1: draw parameters by regressing X on W based on
    #measurement error model
    memParam=PrepareMemParam(calibdata,maindata[,1],NCALIB)
    #Step-2: draw parameters by regressing (Y, Z) on W based on main
    #interested "disease" model
    dmParam=generateRandomMultivarRegreParam(maindata,NSAMPLE,P)
    #Step-4: creating sweeping matrix on W by using parameters
    #obtained from step 1-3 and filling estimated covariance
    #parameter between U and X given W
    sweepMatrixonW=createMatrixonW(memParam, dmParam, P)
    #Step-5: calculate parameters of the imputation model for X
    #given (W,Y,Z) by the sweep operator
    impmodelParam = lapply(sweepMatrixonW,sweep)
    #Step-6: generate random draw for unknown X from its posterior
    #distribution, given W and Y
    varIndicator=length(impmodelParam[[1]])
    if (impmodelParam[[1]][varIndicator] < 0){
      impmodelParam[[1]][varIndicator] = 0
    }
    for (n in 1:N){
      secondStageDrawX = generateMissingvalue(impmodelParam,
                                              maindata)
      twostageMIimputedX=cbind(twostageMIimputedX,
                               secondStageDrawX)
    }
    count=count+1
  }
  #Output data with Y, Z, and multiply imputed X,
  #where maindata[,P+1] = U(Y,Z)
  twoStageMIimputeddata=twostageMIimputedX
  # print(mean(twoStageMIimputeddata))
  return(twoStageMIimputeddata)
}

# MICE FUNCTION #
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
