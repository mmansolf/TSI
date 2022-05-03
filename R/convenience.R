data=data.frame(m=1:4,Fx=1,Fy=2,SE.Fx=3,SE.Fy=4)
OSNAMES=c('Fx','Fy')
SENAMES=c('SE.Fx','SE.Fy')
metrics=c('T','T')
scoreTypes=c('EAP','EAP')
mean=NULL
var_ts=NULL
reliability=NULL
separated=c(T,F)
TSNAMES=paste0('TRUE_',OSNAMES)

###############
# BLOT CHECKS #
#need at least one OSNAME
p_to_impute=length(OSNAMES)
if(p_to_impute==0) stop('
Must provide the name of at least one observed score variable to impute as OSNAMES.')
args_to_test=setNames(
  list(SENAMES,metrics,scoreTypes,mean,var_ts,separated),
  c('SENAMES','metrics','scoreTypes','mean','var_ts','separated')
)

#test types
types_to_test=c('character','character','character','numeric','numeric','logical')
for(le in 1:length(args_to_test)){
  if(!class(args_to_test[[i]])%in%c('NULL',types_to_test[i])) stop(paste0('
Expected ',names(args_to_test)[i],' to be of type ',types_to_test[i],' or NULL; was ',class(args_to_test[[i]]),'.'
  ))
}

#test lengths
for(le in 1:length(args_to_test)){
  if(!length(args_to_test[[i]])%in%c(0,1,p_to_impute)) stop(paste0('
The number of elements in ',names(args_to_test)[i],' should be 1 or match the number of elements in OSNAMES.'
  ))
}

#test length of TSNAMES
if(!length(TSNAMES)==length(OSNAMES)) stop('Must provide as many true score names (TSNAMES) as observed score names (OSNAMES), or leave TSNAMES blank and the prefix "TRUE_" will be appended to OSNAMES to name the resulting true scores')

#test permissible scoreTypes and metrics
if(!all(scoreTypes%in%c('CTT','EAP','ML')))
  stop("Please specify scoreType from the available types for true score imputation ('CTT', 'EAP', or 'ML')")

#test permissible scoreTypes and metrics
if(!all(metrics%in%c(NULL,NA,'z','T','Standard')))
  stop("Please specify metric from the available metrics for true score imputation ('z', 'T', 'Standard')")

#warn if ML is used
if(any(scoreTypes%in%c('ML')))
  stop("Warning: Maximum likelihood (ML) scoring is not recommended due to poor simulation performance. Proceed with caution.")

for(i in 1:length(scoreTypes)){
  #test metrics: either have a metric, or have both mean and var_ts
  is_null_metric=is.null(metrics[i]) | is.na(metrics[i])
  is_null_mean=is.null(mean[i]) | is.na(mean[i])
  is_null_var_ts=is.null(var_ts[i]) | is.na(var_ts[i])
  if((is_null_metric & (is_null_mean | is_null_var_ts)) |
     (!is_null_metric & (!is_null_mean | !is_null_var_ts))) stop(paste0("Problem with variable ",i,": Either assign a metric to each true score variable (e.g., 'T' for T scores) or assign BOTH a mean and var_ts for that variable."))
  #test that EAP and ML scoring have SENAMES
  if(scoreType[i]%in%c('EAP','ML')){
    is_null_SENAME=is.null(SENAMES[i]) | is.na(SENAMES[i])
    if(is_null_SENAME) stop(paste0("Problem with variable ",i,": Each observed score (OSNAME) based on EAP or ML scoring must include a corresponding standard error (SENAMES)."))
  } else if(scoreType[i]=='CTT'){
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

#test that all columns of data are numeric
non_numeric_variables=names(data)[which(!sapply(data,class)%in%c('integer','numeric'))]
if(length(non_numeric_variables)>0)
  stop(paste0("The following variables are not numeric: ",
              paste(missing_variables,collapse=", "),
              '. Please convert them to numeric prior to true score imputation.'))

#####################
# ALL DONE; PREPARE #
print('Preliminary checks passed for true score imputation! Building call to mice...')

#blocks
blocks=setNames(names(data),names(data))
#method
method=rep('pmm',ncol(data))
#predictorMatrix
predictorMatrix=matrix(1,ncol(data),ncol(data))-diag(ncol(data))
#blots
blots=list()
for(i in 1:length(TSNAMES)){
  #initialize and name blot
  blots[[TSNAMES[i]]]=list()
  blots[[TSNAMES[i]]]$OSNAME=TSNAMES[i]
  if(scoreTypes[i]%in%c("EAP","ML")){
    blots[[TSNAMES[i]]]$SENAME=SENAMES[i]
  }
  blots[[TSNAMES[i]]]$scoreType=scoreTypes[i]
  blots[[TSNAMES[i]]]$separated=separated[i]
  if(!is.null(metrics[i]) & !is.na(metrics[i])){
    if(metrics[i]=='z'){
      mean[i]=0
      var_ts[i]=1
    } else if(metrics[i]=='T'){
      mean[i]=50
      var_ts[i]=100
    } else if(metrics[i]=='Standard'){
      mean[i]=100
      var_ts[i]=225
    }
    blots[[TSNAMES[i]]]$mean=mean[i]
    blots[[TSNAMES[i]]]$var_ts=var_ts[i]
  }
}

############################
# ADD TS VARIABLES TO DATA #
for(n in TSNAMES){
  data[[n]]=NA
  blocks[[n]]=n
  method=c(method,'truescore')
  predictorMatrix=cbind(predictorMatrix,0)
}

#######
# RUN #
mice.data=mice(data,
               method=method,
               blocks=blocks,
               blots=blots,
               predictorMatrix=predictorMatrix,
               remove.constant=F,
               ...)
