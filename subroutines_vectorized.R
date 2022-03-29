#MANSOLF: Calculate using CTT instead
PrepareMemParam <- function(calibdata, wdata, n) {
  # print('hi')
  #bring in calibration parameters
  ratio=c(calibdata$reliability)
  varOS=c(calibdata$varOS)
  varTS=c(calibdata$varTS)
  meanOS=c(calibdata$meanOS)
  meanTS=c(calibdata$meanTS)
  #calculate slope
  beta1=sqrt(ratio*varTS/varOS)
  #calculate residual variance
  sigmasq=(1-ratio)*varTS
  #calculate intercept
  beta0=meanTS-meanOS*beta1

  #Draw mux from normal distribution
  tmp = rnorm(1)
  meanOS.obs = meanOS + sqrt(mean(varOS)/length(wdata))*tmp
  #Draw sigmaxsq from inverse chi-square distribution
  #with df = (n-1) <- originally 2?
  varOS.obs = varOS*(length(wdata)-1)/rchisq(1,length(wdata)-1)

  #define parameter structure to return
  param=data.frame(beta0=beta0,
                   beta1=beta1,
                   sigmasq=sigmasq,
                   mean=meanOS.obs,
                   varOS=varOS.obs)

  #shrink it
  param.shrunk=unique(param)
  param=as.matrix(param)
  for(i in 1:nrow(param.shrunk)){
    rownames(param.shrunk)[i]=
      paste(
        which(
          rowSums(param!=matrix(param.shrunk[i,],nrow(param),5,byrow=T))==0),
        collapse=",")
  }

  #return it; we're done
  return(param.shrunk)
}
#Draw parameters from their predictive distribution based on the main study data. \\
generateRandomMultivarRegreParam <- function(maindata,n,p) {
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
  #Draw covariance of U given W from inverse
  #wishart distribution with df = (n-(k+p)+1)
  #where k is dimension of wmat and p is dimension of umat
  df=n-(p+2)+1
  covmatrix=riwish(df, rss)
  covmatrix=as.matrix(covmatrix)
  betavar= kronecker(covmatrix, ww)
  vec.coeffhat=as.vector(coeffhat)
  beta = mvrnorm(ndraw, vec.coeffhat, betavar)
  #regression coefficients of U on W
  multivarRegrecoeff=matrix(beta,2,p)
  #residual covariance matrix of U given W
  multivarResidcov=covmatrix
  #get return params
  param=rbind(multivarRegrecoeff,multivarResidcov)
  #return 'em
  return(param)
}

#3-D array edition
createMatrixonW <- function(memParam, dmParam, P){
  #memParam is 3 vectors of length length(y)
  m = P+2+1
  matrixonW=matrix(NA, nrow = m, ncol = m)
  matrixonW[1:(m-1),3:(m-1)]=dmParam
  #pre-allocate 3-D array
  moW=array(matrixonW,dim=c(dim(matrixonW),nrow(memParam)))
  moW[1,1,] = (1 + memParam$mean^2/memParam$varOS)
  moW[1,2,] = memParam$mean/memParam$varOS
  moW[2,2,] = - 1/memParam$varOS
  for(i in 1:2)moW[i,m,] = unlist(memParam[,i])
  moW[m,m,] = memParam[,3]
  for(i in 3:(m-1))moW[i,m,] = dmParam[2,i-2] * memParam[,3] / memParam[,2]
  dimnames(moW)=list(NULL,NULL,rownames(memParam))
  return(moW)
}

#complete initinal mean and covariance matrix \\
completeGmatrix <- function(G){
  size = dim(G)[1]
  for (i in 1:size){
    for (j in 1:size){
      if ( any(is.na(G[i,j,]) )) {
        if ( any(is.na(G[j,i,]) == FALSE )) {
          G[i,j,] = G[j,i,]
        } else {
          print(sprintf("ERROR: both elements at [%d,%d]
and [%d,%d] are null", i, j, j, i));
        }
      }
    }
  }
  G=(G+aperm(G,c(2,1,3)))/2
}

#perform the sweep operator \\
sweep <- function(matrixonW){
  size = dim(matrixonW)[1]
  curH = completeGmatrix(matrixonW)
  newH= NA*matrixonW
  for (l in 3:(size-1)){
    for (i in 1:size){
      for (j in 1:size){
        if (i==l && j==l) {
          newH[i,j,] = -1/curH[l,l,]
        } else if (i==l || j==l) {
          if(j==l) newH[i,j,] = curH[i,j,]/curH[j,j,] else newH[i,j,] = curH[i,j,]/curH[i,i,]
        } else {
          newH[i,j,] = curH[i,j,] - curH[i,l,]*curH[l,j,]/curH[l,l,]
        }
      }
    }
    curH = newH;
    newH= NA*matrixonW
  }
  param=t(curH[,size,])
  rownames(param)=dimnames(matrixonW)[[3]]
  return(param)
}

#Create imputed values for unobserved covariate X
generateMissingvalue <- function(param, maindata){
  nsample=nrow(maindata)
  nparam=ncol(param)
  rowsets=rownames(param)
  rownames(param)=NULL
  #test whether the estimate of the residual variance is negative,
  #and display a warning message
  if (any(param[,nparam] < 0)) {
    print(sprintf("%s", "Warning: The estimate of the
residual variance of mismeasured covariate given
the observed data is negative."))
    param[param[,nparam]<0,nparam]=0
  }
  #fill in matrix to collapse
  matrixToCollapse=matrix(NA,nsample,nparam)
  for(iset in 1:nrow(param)){
    rows.iset=as.numeric(unlist(strsplit(rowsets[iset],",",fixed=T)))
    matrixToCollapse[rows.iset,]=matrix(param[iset,1:(nparam)],
                                        length(rows.iset),nparam,byrow=T)
  }
  #matrix operations
  matrixToCollapse[,2:(nparam-1)]=matrixToCollapse[,2:(nparam-1)]*maindata
  imputedX=rowSums(matrixToCollapse[,-nparam])+sqrt(matrixToCollapse[,nparam])*rnorm(nsample, mean=0, sd=1)
  #return it
  return(imputedX)
}

#main function \\
MIEC <- function(maindata,calibdata,NCALIB,NSAMPLE,M,N,K,S,CTT) {
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
    impmodelParam = sweep(sweepMatrixonW)
    #Step-6: generate random draw for unknown X from its posterior
    #distribution, given W and Y
    # varIndicator=length(impmodelParam[[1]])
    # if (impmodelParam[[1]][varIndicator] < 0){
    #   impmodelParam[[1]][varIndicator] = 0
    # }
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
  return(twoStageMIimputeddata)
}
