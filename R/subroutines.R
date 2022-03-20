#MANSOLF: Calculate using CTT instead
PrepareMemParam <- function(calibdata,wdata,n) {
  # ratio=calibdata[[1]]
  # beta0=rep(0,length(calibdata))
  # beta1=ratio
  # sigmasq=ratio-ratio^2
  # param=data.frame(beta0, beta1, sigmasq)
  # return(param)
  ratio=c(calibdata$reliability)
  varOS=c(calibdata$varOS)
  varTS=c(calibdata$varTS)
  meanOS=c(calibdata$meanOS)
  meanTS=c(calibdata$meanTS)
  # beta1=ratio/sqrt(varOS)*sqrt(varTS/ratio) #DONE
  beta1=sqrt(ratio*varTS/varOS) #DONE
  # sigmasq=(ratio-ratio^2)*varTS/ratio #DONE
  sigmasq=(1-ratio)*varTS #DONE
  beta0=meanTS-meanOS*beta1 #DONE

  ###################################
  # DEBUG: varOS for current sample #
  #Draw sigmaxsq from inverse chi-square distribution
  #with df = (n-2)
  # rss = sum((wdata-meanOS)^2)
  #variance reduction factor
  #varRF=varOS/varTS
  #varOS.obs = var(wdata)*varRF
  # varOS.obs=varOS/mean(varOS)*var(wdata)
  #Draw mux from normal distribution
  tmp = rnorm(1)
  meanOS.obs = meanOS + sqrt(mean(varOS)/length(wdata))*tmp
  varOS.obs = varOS*(length(wdata)-1)/rchisq(1,length(wdata)-1)

  param=data.frame(beta0=beta0,
                   beta1=beta1,
                   sigmasq=sigmasq,
                   mean=meanOS.obs,
                   varOS=varOS.obs)
  return(param)
}
#Draw parameters from their predictive distribution based on the main study data. \\
generateRandomMultivarRegreParam <- function(maindata,n,p) {
  # print(head(maindata))
  # if('Tx'%in%colnames(maindata))
  #   print(coef(lm(cbind(m,Tx)~w,data=maindata%>%as.data.frame))) else
  #     print(coef(lm(cbind(m,Ty)~w,data=maindata%>%as.data.frame)))
  maindata=as.matrix(na.omit(as.data.frame(maindata)))
  n=nrow(maindata)
  ndraw=1
  w=maindata[,1]
  wmat=mat.or.vec(n,2)
  wmat[,1]=1
  wmat[,2]=w
  umat=maindata[,2:(p+1)]
  ww=solve(t(wmat)%*%wmat)
  coeffhat=ww%*%(t(wmat)%*%umat)
  residual=umat-wmat%*%coeffhat
  rss=t(residual)%*%residual
  a=t(chol(ww))
  invrss=solve(rss) #calculate inverse covariance matrix
  #Draw covariance of U given W from inverse
  #wishart distribution with df = (n-(k+p)+1)
  #where k is dimension of wmat and p is dimension of umat
  df=n-(p+2)+1
  #DEBUG
  covmatrix=riwish(df, rss)
  # covmatrix=rss
  # print(rss%>%round(3))
  # print(covmatrix%>%round(3))
  covmatrix=as.matrix(covmatrix)
  betavar= kronecker(covmatrix, ww)
  vec.coeffhat=as.vector(coeffhat)
  beta = mvrnorm(ndraw, vec.coeffhat, betavar)
  #regression coefficients of U on W
  multivarRegrecoeff=matrix(beta,2,p)
  #residual covariance matrix of U given W
  multivarResidcov=covmatrix
  param=rbind(multivarRegrecoeff,multivarResidcov)

  #lm edition: predict w from everything else
  #reformat maindata and fit it
  # maindata=as.data.frame(maindata)%>%setNames(paste0('v',0:(ncol(maindata)-1)))
  # #predict everything from w
  # lmfit=lm(as.formula(paste0("cbind(",
  #                            paste(names(maindata)[-1],collapse=","),
  #                            ')~v0')),data=maindata)
  # print('regression params')
  # print(param%>%round(4))
  return(param)
}

#create the initinal mean and covariance matrix \\
createMatrixonW <- function(memParam, dmParam, P){
  #memParam is 3 vectors of length length(y)
  m = P+2+1
  matrixonW=matrix(NA, nrow = m, ncol = m)
  matrixonW[1:(m-1),3:(m-1)]=dmParam
  #make list of matrices on W
  moWlist=list()
  #unique values of measurement error model
  ub=unique(memParam$beta1)
  #assign values
  for(b in ub){
    #get a row
    bi=which(memParam$beta1==b)
    #make the matrix
    moW.i=matrixonW
    moW.i[1,1] = (1 + memParam$mean[bi[1]]^2/memParam$varOS[bi[1]])
    moW.i[1,2] = memParam$mean[bi[1]]/memParam$varOS[bi[1]]
    moW.i[2,2] = - 1/memParam$varOS[bi[1]]
    moW.i[1:2,m] = unlist(memParam[bi[1],1:2])
    moW.i[m,m] = memParam[bi[1],3]
    moW.i[3:(m-1),m] = dmParam[2,] * memParam[bi[1],3] / memParam[bi[1],2]
    #assign it
    moWlist=c(moWlist,list(moW.i))
    names(moWlist)[length(moWlist)]=paste(bi,collapse=",")
  }
  return(moWlist)
}
#complete initinal mean and covariance matrix \\
completeGmatrix <- function(G){
  size = nrow(G)
  for (i in 1:size){
    for (j in 1:size){
      if ( is.na(G[i,j]) ) {
        if ( is.na(G[j,i]) == FALSE ) {
          G[i,j] = G[j,i]
        } else {
          print(sprintf("ERROR: both elements at [%d,%d]
and [%d,%d] are null", i, j, j, i));
        }
      }
    }
  }
  if (isSymmetric(G)) {
    return(G)
  } else {
    return(NULL)
  }
}
#check the symmetry of the matrix created by the sweep operator
isSymmetric <- function(G){
  #   size = nrow(G)
  #   for (i in 1:size){
  #     for (j in 1:size){
  #       if ( abs(G[i, j] - G[j, i]) > 1e-10) {
  #         print(sprintf("ERROR: elements not matched at [%d,%d]
  # and [%d,%d]", i, j, j, i));
  #         return(FALSE);
  #       }
  #     }
  #   }
  return(TRUE)
}
#perform the sweep operator \\
sweep <- function(matrixonW){
  size = nrow(matrixonW)
  curH = completeGmatrix(matrixonW)
  # print('unsqpet matrix')
  # print(curH)
  newH= matrix(NA, nrow=size, ncol=size)
  for (l in 3:(size-1)){
    for (i in 1:size){
      for (j in 1:size){
        if (i==l && j==l) {
          newH[i,j] = -1/curH[l,l]
        } else if (i==l || j==l) {
          if(j==l) newH[i,j] = curH[i,j]/curH[j,j] else newH[i,j] = curH[i,j]/curH[i,i]
        } else {
          newH[i,j] = curH[i,j] - curH[i,l]*curH[l,j]/curH[l,l]
        }
      }
    }
    curH = newH;
    newH= matrix(NA, nrow=size, ncol=size)
  }
  # print('swetp matrix')
  # print(curH)
  param=curH[,size]
  return(param)
}
#Create imputed values for unobserved covariate X
generateMissingvalue <- function(param, maindata){
  nsample=nrow(maindata)
  nparam=length(param[[1]])
  #test whether the estimate of the residual variance is negative,
  #and display a warning message
  if (param[[1]][nparam] < 0) {
    print(sprintf("%s", "Warning: The estimate of the
residual variance of mismeasured covariate given
the observed data is negative."))
  }
  #param is a list; loop over its elements
  imputedX=rep(NA,nsample)
  for(iset in 1:length(param)){
    #indices
    rows.iset=as.numeric(unlist(strsplit(names(param)[iset],",",fixed=T)))
    mux = param[[iset]][1]
    for (i in 2:(nparam-1)){
      mux = mux + param[[iset]][i]*maindata[rows.iset,i-1]
    }
    #Generate X from its posterior distribution with mean mux
    #and variance sigmasqxx_wzy
    if(param[[iset]][nparam]<0) param[[iset]][nparam]=0.001 #eps!
    # print(mux)
    imputedX[rows.iset] = mux + sqrt(param[[iset]][nparam])*rnorm(length(rows.iset), mean=0, sd=1)
    # if(any(is.na(imputedX[rows.iset])))stop(paste0(
    #   paste(round(param[[iset]],5),collapse=", "),"\n",
    #   paste(round(maindata[rows.iset,],5),collapse=", ")))
    # print('hi')
  }
  return(imputedX)
}

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
