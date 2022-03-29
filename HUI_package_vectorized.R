rm(list=ls())
library(tidyverse)
library(readr)
library(haven)
bigpct=proc.time()

nsim=1
set.seed(3)
##################
# PRE-PROCESSING #
##################
m=10
maxit=10

###############################
# READ AND LIGHTLY CLEAN DATA #
#data file
data=read_sav("C:/Users/Max/Documents/PROMIS Scoring Code/HUI/RecenterCompletes.sav")
data
#filter to test fullsim without taking an eternity
data=data[1:250,]
#codebook
library(openxlsx)
codebook=openxlsx::read.xlsx("C:/Users/Max/Documents/PROMIS Scoring Code/HUI/5000_Re-Center_PROMIS_DP_CODEBOOK.XLSX",
                             skipEmptyRows=F,colNames=F,startRow=5)%>%as_tibble
print(codebook,n=10)

####################
# WRANGLE CODEBOOK #
#split codebook by variable
splitcodebook=codebook
splitcodebook$var=NA
var=1
for(i in 1:nrow(splitcodebook)){
  splitcodebook$var[i]=var
  if(is.na(splitcodebook$X1[i])) var=var+1
}
splitcodebook=splitcodebook%>%filter(!is.na(X1))%>%group_split(var)
#codebook as tibble
splitcodetbl=tibble(
  name=splitcodebook%>%map_chr(~.[[1]][1]),
  label=splitcodebook%>%map_chr(~.[[2]][1]),
  vlabs=splitcodebook%>%map(function(x){
    x=x[-1,]
    x$X5=ifelse(!is.na(x$X6),x$X6,x$X5)
    x%>%dplyr::select(-X1,-X2,-X3,-X6,-var)%>%
      rename(ValueLabel=X5,
             SelChk.Value=X4)
  })
)%>%unnest(cols=vlabs)
splitcodetbl%>%print(n=25)
#examine a few variables
data%>%count(Qsocio03,Qsocio04,Qsocio05_1)
#so the Checked are turned into separate variables?
splitcodetbl=splitcodetbl%>%
  mutate(SelChk=strsplit(SelChk.Value,":",fixed=T)%>%
           map_chr(function(x)if(length(x)>=1) x[[1]] else NA),
         Value=strsplit(SelChk.Value,":",fixed=T)%>%
           map_chr(function(x)if(length(x)>=2) x[[2]] else NA))
splitcodetbl%>%
  count(SelChk.Value,SelChk,Value)%>%print(n=Inf) #ok
splitcodetbl=splitcodetbl%>%dplyr::select(-SelChk.Value)%>%
  mutate(Value=as.numeric(Value))

#what's the deal with checked?
splitcodetbl%>%filter(SelChk=="Checked")%>%print(n=Inf)
#take a few out
splitcodetbl=splitcodetbl%>%
  filter(name!='Qsocio05' | !Value%in%c(8,16,32))
#try again
data%>%dplyr::select(all_of(paste0(
  splitcodetbl%>%filter(SelChk=="Checked")%>%pull(name),
  "_",
  splitcodetbl%>%filter(SelChk=="Checked")%>%pull(Value))))%>%names #it worked! let's reshape these variables so we can get rid of "Checked"
Checkedtbl=splitcodetbl%>%filter(SelChk=="Checked")

#mutate data
data=data%>%
  rename(QPAININ11=QPAININ11r1,
         QPFB21r1=QPFB21)

##################################
# CLEAN AND PREPARE DEMOGRAPHICS #
splitcodetbl%>%print(n=200)
#age
data%>%count(Qsocio02)%>%print(n=Inf)
#set 5655 to NA
data=data%>%
  mutate(Qsocio02=ifelse(Qsocio02==5655,NA,Qsocio02))
data%>%count(Qsocio02)%>%print(n=Inf)

#female gender; 1=female
data%>%count(Qsocio03)
data=data%>%
  mutate(Qsocio03=Qsocio03-1)
data%>%count(Qsocio03)

#number of overnight hospital stays (not days) in past 12 months; sick days in past month
data%>%count(Qsocio19)%>%print(n=Inf)
data%>%count(Qsocio20)%>%print(n=Inf)
data%>%dplyr::select(Qsocio19,Qsocio20)%>%cor #not correlated!

#let's just use these four then
data=data%>%
  rename(Age=Qsocio02,
         Female=Qsocio03,
         HospitalStays=Qsocio19,
         SickDays=Qsocio20)%>%
  filter(!is.na(Age))

######################
# READ ROBERT'S FILE #
robert=readRDS("C:/Users/Max/Documents/PROMIS Scoring Code/HUI/Item_Parameters.rds")
#can I just merge these to the data?
for(r in robert){
  rnames=paste0("Q",rownames(r))
  rstubs=gsub('[[:digit:]]+', '', rnames)%>%unique
  # if(length(rstubs)>1)stop('What?')
  dmatches=NULL
  for(st in rstubs){
    dmatches=c(dmatches,
               names(data)[substr(names(data),1,nchar(st))==st])
  }
  # print(dmatches%in%rnames)
  print(dmatches[!dmatches%in%rnames])

}
names(robert)

########################
# FINISH ROBERT'S FILE #
#read global CUE sheets - from my own files!
ipar.global.mh=read.csv("C:/Users/Max/Documents/PROMIS Scoring Code/WorkingFiles/ipar/ipar_PROMIS v1.2 - Global Health_Mental Health.csv",
                        header=F)
ipar.global.ph=read.csv("C:/Users/Max/Documents/PROMIS Scoring Code/WorkingFiles/ipar/ipar_PROMIS v1.2 - Global Health_Physical Health.csv",
                        header=F)
rownames(ipar.global.mh)=c('Global02','Global04','Global05','Global10')
rownames(ipar.global.ph)=c('Global03','Global06','Global07','Global08')
colnames(ipar.global.mh)=c('a','cb1','cb2','cb3','cb4','NCAT')
colnames(ipar.global.ph)=c('a','cb1','cb2','cb3','cb4','NCAT')
robert$GlobalMH=ipar.global.mh
robert$GlobalPH=ipar.global.ph

#read sleep-related impairment CUE sheet
ipar.sleepimpairment=read.csv("C:/Users/Max/Documents/PROMIS Scoring Code/WorkingFiles/ipar/ipar_HUI_ONLY_PROMIS v1.0 - Sleep Related Impairment 6_18_2018-5.csv",
                              header=F)
rownames(ipar.sleepimpairment)=c("Sleep6","Sleep7","Sleep10","Sleep25","Sleep27","Sleep29","Sleep30","Sleep33")
colnames(ipar.sleepimpairment)=c('a','cb1','cb2','cb3','cb4','NCAT')
ipar.sleepimpairment$a=as.numeric(ipar.sleepimpairment$a)
ipar.sleepimpairment[1,1]=2.2395
robert$Sleep_Related_Impairment=ipar.sleepimpairment

#check again
for(r in robert){
  rnames=paste0("Q",rownames(r))
  rstubs=gsub('[[:digit:]]+', '', rnames)%>%unique
  # if(length(rstubs)>1)stop('What?')
  dmatches=NULL
  for(st in rstubs){
    dmatches=c(dmatches,
               names(data)[substr(names(data),1,nchar(st))==st])
  }
  # print(dmatches%in%rnames)
  print(dmatches[!dmatches%in%rnames])
} #messy, but looks right

#################
# FILTER ROBERT # <- sounds uncomfortable
robert=robert%>%map(function(x){
  x=x%>%dplyr::select(c(a,starts_with('cb'),NCAT))%>%as.matrix
  #get data to score
  data.toscore=data%>%dplyr::select(any_of(paste0("Q",rownames(x))))
  #filter rows of x
  x[substr(names(data.toscore),2,nchar(names(data.toscore))),]
})
str(robert)

#######################
# EXAMINE ITEM-TOTALS #
library(psych)
#rescore global07
table(data$QGlobal07)
table(data$QPFB21r1)
#recode for scoring
data=data%>%
  mutate(QGlobal07=car::recode(QGlobal07,
                               "0=5;1=4;2=4;3=4;4=3;5=3;6=3;7=2;8=2;9=2;10=1"),
         QPFB21r1=car::recode(QPFB21r1,
                              "1=1;2=1;3=2;4=3;5=4"))
table(data$QGlobal07)
table(data$QPFB21r1)
#check keys
robert%>%map(function(x){
  alpha(data%>%dplyr::select(any_of(paste0("Q",rownames(x)))))%>%pluck('item.stats')
}) #some problems!
data%>%count(QGlobal10)
data%>%count(QGlobal10)
data=data%>%
  mutate(QGlobal10=6-QGlobal10,
         # QGlobal07=6-QGlobal07,
         QGlobal08=6-QGlobal08,
         QSleep109=6-QSleep109,
         QSleep115=6-QSleep115,
         QSleep116=6-QSleep116)
robert%>%map(function(x){
  alpha(data%>%dplyr::select(any_of(paste0("Q",rownames(x)))),
        check.keys=T)%>%pluck('item.stats')
}) #some problems!

##############
# SIMULATION #
#sets working back to front, front to back
# vars2use=c((1:10)%>%as.list%>%map(~c(1:.,rep(NA,10-.))),
#            (1:10)%>%as.list%>%map(~c(rep(NA,10-.),(10-.+1):10)))[1:19]
#all sets of 1, all sets of 2
# vars2use=c(combn(1:10,1)%>%apply(2,list)%>%map(~.[[1]]),
#            combn(1:10,2)%>%apply(2,list)%>%map(~.[[1]]),
#            combn(1:10,3)%>%apply(2,list)%>%map(~.[[1]]))
#just all 10
vars2use=list(1:10)

pItems=c(1)
slopeScale=c(1)
items=1:10


###############
# SCORE ITEMS #
library(devtools)
# load_all('C:/Users/Max/Dropbox/RPackages/TSI')
source("C:/Users/Max/Dropbox/RPackages/subroutines_vectorized.R")
source("C:/Users/Max/Dropbox/RPackages/TSI/R/core.R")
library(MCMCpack)
library(MASS)
source("C:/Users/Max/Documents/PROMIS Scoring Code/WorkingFiles/ThetaSEeap.R")
for(i in 1:length(robert)){
  r=robert[[i]]
  data.toscore=data%>%dplyr::select(any_of(paste0("Q",rownames(r))))
  #add missingness
  if(pItems!=1){
    data.toscore[,1:floor((1-pItems)*ncol(data.toscore))]=NA
  }
  #multiply slopes by scalingFactor
  r[,1]=r[,1]*slopeScale
  #score data
  data.scored=thetaSE.eap(r%>%as.data.frame,data.toscore%>%as.matrix, maxCat = max(r[,'NCAT']))
  #transform to T-score metric
  data.scored[,1]=data.scored[,1]*10+50
  data.scored[,2]=data.scored[,2]*10
  #rename and concatenate to data
  data=bind_cols(data,
                 data.scored%>%as.data.frame%>%set_names(paste0(names(robert)[i],c("_T",'_SE'))))
}
#mean SE's for examination
data%>%dplyr::select(ends_with("_SE"))%>%map_dbl(mean,na.rm=T)
#reliability
1-(data%>%dplyr::select(ends_with("_SE"))%>%map_dbl(mean,na.rm=T))^2/100

#distributions
data%>%dplyr::select(ends_with("_T"))%>%map(summary)

#############
# RUN STUFF #
demographics=c('Age','Female','HospitalStays','SickDays')
useData=data%>%dplyr::select(all_of(demographics),
                             all_of(paste0(names(robert)[items],"_T")),
                             all_of(paste0(names(robert)[items],"_SE")))%>%
  mutate(across(c(Female),~.*10))

useData[,paste0(names(robert)[items],"_TRUE")]=NA
#prepare mice objects
mice.method=rep('truescore',length(robert[items]))
# mice.blocks=pmap(list(as.list(paste0(names(robert),"_T")),
#                       as.list(paste0(names(robert),"_SE")),
#                       as.list(paste0(names(robert),"_TRUE"))),c)%>%set_names(names(robert))
mice.blocks=pmap(list(as.list(paste0(names(robert)[items],"_TRUE"))),c)%>%set_names(names(robert)[items])
# mice.blocks.uniform=pmap(list(as.list(paste0(names(robert),"_T")),
#                               as.list(paste0(names(robert),"_TRUE"))),c)%>%set_names(names(robert))
#response-pattern-level reliability
mice.blots=names(robert)[items]%>%as.list%>%map(function(x){
  list(calibration=list(OSNAME=paste0(x,'_T'),
                        SENAME=paste0(x,'_SE'),
                        scoreType='EAP',
                        mean=50,
                        varTS=100))
})%>%set_names(names(robert)[items])

mice.predMx=cbind(matrix(1,length(robert[items]),4),
                  diag(length(robert[items])),
                  diag(length(robert[items])),
                  # matrix(0,length(robert[items]),length(robert[items]))
                  matrix(1,length(robert[items]),length(robert[items]))-diag(length(robert[items]))
)

out.sep=list()
pct=proc.time()
for(separated in c(T,F)){
  library(mice)
  pct=proc.time()
  mice.data=mice(useData,m=m,maxit=maxit,
                 method=mice.method,
                 blocks=mice.blocks,
                 blots=mice.blots%>%map(~map(.,function(x)c(x,list(separated=T)))),
                 predictorMatrix=mice.predMx,
                 printFlag=T)
  rtUseData=proc.time()-pct
  rtUseData
  plot(mice.data)

  ##############
  # SIMULATION #
  library(mnormt)
  library(abind)
  #means and covariances from mice data
  means.trueCorData=mice.data%>%complete('all')%>%
    map(~dplyr::select(.,-c(ends_with("_T"),ends_with("_SE")))%>%colMeans)%>%bind_rows%>%colMeans
  vcov.trueCorData=mice.data%>%complete('all')%>%
    map(~dplyr::select(.,-c(ends_with("_T"),ends_with("_SE")))%>%cov)%>%
    abind(along=3)%>%apply(c(1,2),mean)
  trueCorData=rmnorm(nrow(data),mean=means.trueCorData,
                     varcov=vcov.trueCorData)%>%
    as_tibble%>%set_names(c('Age','Female','HospitalStays','SickDays',names(robert)[items]))
  simHUI=list()
  for(j in 1:nsim){
    library(mirt)
    #rescale t-scores to theta metric; just divide by 10
    trueCorData2=trueCorData
    for(i in 1:length(robert[items])){
      r=(robert[items])[[i]]
      a=r[,'a']
      d=r%>%as.data.frame%>%dplyr::select(starts_with('cb'))%>%as.matrix
      d=-d*matrix(a,nrow(d),ncol(d),byrow=F)
      #simulate data
      sdata=simdata(a,d,Theta=(trueCorData[[i+4]]-50)/10,itemtype='graded')+1
      #add missingness
      if(pItems!=1){
        data.toscore[,1:floor((1-pItems)*ncol(data.toscore))]=NA
      }
      #multiply slopes by scalingFactor
      r[,1]=r[,1]*slopeScale
      #score data
      sscored=thetaSE.eap(r%>%as.data.frame,sdata,maxCat=max(r[,'NCAT']))
      trueCorData2[[paste0(names(robert[items])[i],'_T')]]=sscored[,1]*10+50
      trueCorData2[[paste0(names(robert[items])[i],'_SE')]]=sscored[,2]*10
    }

    #mice
    trueCorData2=trueCorData2%>%dplyr::select(all_of(c('Age','Female','HospitalStays','SickDays',
                                                       paste0(names(robert[items]),"_T"),
                                                       paste0(names(robert[items]),"_SE"))))
    trueCorData2[,paste0(names(robert[items]),"_TRUE")]=NA
    mice.predMx2=cbind(matrix(1,length(robert[items]),4),
                       diag(length(robert[items])),
                       diag(length(robert[items])),
                       matrix(0,length(robert[items]),length(robert[items]))
                       # matrix(1,length(robert[items]),length(robert[items]))-diag(length(robert[items]))
    )

    # pct=proc.time()
    factory <- function(fun)
      function(...) {
        warn <- err <- NULL
        res <- withCallingHandlers(
          tryCatch(fun(...), error=function(e) {
            err <<- conditionMessage(e)
            NULL
          }), warning=function(w) {
            warn <<- append(warn, conditionMessage(w))
            invokeRestart("muffleWarning")
          })
        list(res, warn=warn, err=err)
      }
    fact.mice=factory(mice)
    smice.data=fact.mice(trueCorData2,m=m,maxit=maxit,
                         method=mice.method,
                         blocks=mice.blocks,
                         blots=mice.blots%>%map(~map(.,function(x)c(x,list(separated=separated)))),
                         predictorMatrix=mice.predMx,
                         # visitSequence='revmonotone',
                         printFlag=F)
    smice.data2=fact.mice(trueCorData2,m=m,maxit=maxit,
                          method=mice.method,
                          blocks=mice.blocks,
                          blots=mice.blots%>%map(~map(.,function(x)c(x,list(separated=separated)))),
                          predictorMatrix=mice.predMx2,
                          # visitSequence='revmonotone',
                          printFlag=F)
    simHUI[[j]]=list(smice.data=smice.data,smice.data2=smice.data2)
  }
  rtHUIsim=proc.time()-pct
  rtHUIsim

  #just store stuff i guess
  argh1=simHUI%>%map(~.$smice.data)%>%pluck(1)%>%pluck(1)%>%list
  argh2=simHUI%>%map(~.$smice.data2)%>%pluck(1)%>%pluck(1)%>%list
  out.sep[[as.character(separated)]]=tibble(
    mice.data=list(mice.data),
    simRawData=list(trueCorData),
    simHUI=argh1,
    simHUI2=argh2,
    rtUseData=rtUseData[3],
    rtHUIsim=rtHUIsim[3])
}
out.sep
rtOverall=proc.time()-pct
rtOverall
library(mice)

#save for later
save.image('C:/Users/Max/Documents/sim_workspaces/ECHO OIF/hui_simulation.RData')
load('C:/Users/Max/Documents/sim_workspaces/ECHO OIF/hui_simulation.RData')

# allOut=bind_rows(out%>%map(~.$`FALSE`))
allOut=out.sep$`FALSE`
allOut
object.size(allOut)

##################
# ANALYZE OUTPUT #<- more thorough
library(abind)
getImpVarStat=function(x,f,ew){
  x%>%complete('all')%>%
    map(~dplyr::select(.,any_of(paste0(names(robert),ew)))%>%map_dbl(f))%>%
    bind_rows%>%colMeans
}
getImpSetStat=function(x,f,ew){
  x%>%complete('all')%>%
    map(~dplyr::select(.,Age,Female,HospitalStays,SickDays,
                       any_of(paste0(names(robert),ew)))%>%f)%>%
    abind(along=3)%>%apply(c(1,2),mean)
}
pct=proc.time()
allOut2=allOut%>%
  mutate(tMeans.dat=map(mice.data,getImpVarStat,mean,'_T'),
         trueMeans.dat=map(mice.data,getImpVarStat,mean,'_TRUE'),
         tSDs.dat=map(mice.data,getImpVarStat,sd,'_T'),
         trueSDs.dat=map(mice.data,getImpVarStat,sd,'_TRUE'),

         tCors.dat=map(mice.data,getImpSetStat,cor,'_T'),
         trueCors.dat=map(mice.data,getImpSetStat,cor,'_TRUE'),
         tCovs.dat=map(mice.data,getImpSetStat,cov,'_T'),
         trueCovs.dat=map(mice.data,getImpSetStat,cov,'_TRUE'),

         simMeans.sim=map(simRawData,~dplyr::select(.,any_of(names(robert)))%>%map_dbl(mean)),
         simSDs.sim=map(simRawData,~dplyr::select(.,any_of(names(robert)))%>%map_dbl(sd)),
         simCors.sim=map(simRawData,~dplyr::select(.,any_of(names(robert)))%>%cor),
         simCovs.sim=map(simRawData,~dplyr::select(.,any_of(names(robert)))%>%cov),

         # tMeans.sim=map(simHUI,function(x)map(x,getImpVarStat,mean,"_T")%>%bind_rows%>%colMeans),
         # trueMeans.sim=map(simHUI,function(x)map(x,getImpVarStat,mean,"_TRUE")%>%bind_rows%>%colMeans),
         # tSDs.sim=map(simHUI,function(x)map(x,getImpVarStat,sd,"_T")%>%bind_rows%>%colMeans),
         # trueSDs.sim=map(simHUI,function(x)map(x,getImpVarStat,sd,"_TRUE")%>%bind_rows%>%colMeans),
         #
         # tCors.sim=map(simHUI,function(x)map(x,getImpSetStat,cor,"_T")%>%abind(along=3)%>%apply(c(1,2),mean)),
         # trueCors.sim=map(simHUI,function(x)map(x,getImpSetStat,cor,"_TRUE")%>%abind(along=3)%>%apply(c(1,2),mean)),
         # tCovs.sim=map(simHUI,function(x)map(x,getImpSetStat,cov,"_T")%>%abind(along=3)%>%apply(c(1,2),mean)),
         # trueCovs.sim=map(simHUI,function(x)map(x,getImpSetStat,cov,"_TRUE")%>%abind(along=3)%>%apply(c(1,2),mean))
         tMeans.sim=map(simHUI,getImpVarStat,mean,'_T'),
         trueMeans.sim=map(simHUI,getImpVarStat,mean,'_TRUE'),
         tSDs.sim=map(simHUI,getImpVarStat,sd,'_T'),
         trueSDs.sim=map(simHUI,getImpVarStat,sd,'_TRUE'),

         tCors.sim=map(simHUI,getImpSetStat,cor,'_T'),
         trueCors.sim=map(simHUI,getImpSetStat,cor,'_TRUE'),
         tCovs.sim=map(simHUI,getImpSetStat,cov,'_T'),
         trueCovs.sim=map(simHUI,getImpSetStat,cov,'_TRUE')
  )
allOut2%>%print(width=Inf)

proc.time()-bigpct

#############################
# FOR PRO CORE PRESENTATION #
allOut2pc=allOut2%>%
  mutate(items=list(1:10))%>%
  dplyr::select(items,contains('Mean'),contains('SD'))%>%
  unnest(cols=everything())%>%
  mutate(items=names(robert))
allOut2pc

cor.plot(allOut2$tCors.dat[[1]],
         cex.axis=2/3,xlas=2)
cor.plot(allOut2$trueCors.dat[[1]],
         cex.axis=2/3,xlas=2)
mx2show=allOut2$tCors.dat[[1]]*lower.tri(diag(14),diag=F)+
  allOut2$trueCors.dat[[1]]*upper.tri(diag(14),diag=T)
rownames(mx2show)[5:14]=substr(rownames(mx2show)[5:14],1,nchar(rownames(mx2show)[5:14])-2)
colnames(mx2show)=rownames(mx2show)
cor.plot(mx2show,
         cex.axis=2/3,xlas=2)
mx2show=allOut2$tCors.dat[[1]]-allOut2$trueCors.dat[[1]]
rownames(mx2show)[5:14]=substr(rownames(mx2show)[5:14],1,nchar(rownames(mx2show)[5:14])-2)
colnames(mx2show)=rownames(mx2show)
cor.plot(mx2show,
         upper=F,
         cex.axis=2/3,xlas=2,zlim=c(-0.25,0.25))
fdsafds
#################
# FOR PAPER (?) #
#0. Do sample statistics differ between T-score and true-score analyses:
tCor=useData%>%dplyr::select(Age,Female,HospitalStays,SickDays,ends_with("_T"))%>%
  cor%>%round(3)
library(abind)
trueCor=mice.data%>%complete('all')%>%
  map(~dplyr::select(.,Age,Female,HospitalStays,SickDays,ends_with("_TRUE"))%>%cor)%>%
  abind(along=3)%>%apply(c(1,2),mean)
tCor-trueCor
library(psych)
cor.plot(tCor)
cor.plot(trueCor)
cor.plot(tCor-trueCor,diag=F,upper=F)

#covariances
tCov=useData%>%
  mutate(across(ends_with("_T"),~(.-50)/10))%>%
  dplyr::select(Age,Female,HospitalStays,SickDays,ends_with("_T"))%>%cov%>%round(3)
library(abind)
trueCov=mice.data%>%complete('all')%>%map(~dplyr::select(.,Age,Female,HospitalStays,SickDays,ends_with("_TRUE"))%>%
                                            mutate(across(ends_with("_TRUE"),~(.-50)/10))%>%cov)%>%abind(along=3)%>%apply(c(1,2),mean)
library(psych)
cor.plot(tCov,zlim=range(c(tCov,trueCov)))
cor.plot(trueCov,zlim=range(c(tCov,trueCov)))
cor.plot(trueCov-tCov,diag=F,upper=F)

#means and sd's
tMean=useData%>%dplyr::select(ends_with("_T"))%>%map_dbl(mean)
library(abind)
trueMean=mice.data%>%complete('all')%>%map(~dplyr::select(.,ends_with("_TRUE"))%>%map_dbl(mean))%>%bind_cols%>%rowMeans
trueMean
tMean
plot(tMean,trueMean)
abline(a=0,b=1)
tSD=useData%>%dplyr::select(ends_with("_T"))%>%map_dbl(sd)
library(abind)
trueSD=mice.data%>%complete('all')%>%map(~dplyr::select(.,ends_with("_TRUE"))%>%map_dbl(sd))%>%bind_cols%>%rowMeans
plot(tSD,trueSD)
abline(a=0,b=1)

#tabulations by group
mice.data%>%complete('all')%>%map(~mutate(.,Age=floor(Age/10),
                                          SickDays=floor(SickDays/5)*5,
                                          HospitalStays=floor(HospitalStays/5)*5)%>%
                                    group_by(Age,Female,HospitalStays,SickDays)%>%
                                    summarize(across(ends_with("_TRUE"),mean)))%>%
  bind_rows%>%
  group_by(Age,Female,HospitalStays,SickDays)%>%
  summarize(across(ends_with("_TRUE"),mean))
useData%>%mutate(Age=floor(Age/10),
                 SickDays=floor(SickDays/5)*5,
                 HospitalStays=floor(HospitalStays/5)*5)%>%
  group_by(Age,Female,HospitalStays,SickDays)%>%
  summarize(across(ends_with("_T"),mean))

# #1. Differences t-true in real data, using raw data's true scores as baseline
reshapeMx=function(x)setNames(do.call(c, as.data.frame(x)),
                              paste(rep(colnames(x), each=nrow(x)), rep(rownames(x),times=nrow(x)), sep="."))
# #get "true" values
# baseMeans=allOut2$trueMeans.dat[
#   allOut2$pItems==1 & allOut2$slopeScale==1 & (allOut2$trueMeans.dat%>%map_dbl(length))==10
# ][[1]]
# baseSDs=allOut2$trueSDs.dat[
#   allOut2$pItems==1 & allOut2$slopeScale==1 & (allOut2$trueMeans.dat%>%map_dbl(length))==10
# ][[1]]
#get them as vectors
allOut3=allOut2%>%
  mutate(tMeans.bias=map2(tMeans.sim,simMeans.sim,~.x-.y),
         trueMeans.bias=map2(trueMeans.sim,simMeans.sim,~.x-.y),
         tSDs.bias=map2(tSDs.sim,simSDs.sim,~.x-.y),
         trueSDs.bias=map2(trueSDs.sim,simSDs.sim,~.x-.y),
         tCors.bias=map2(tCors.sim,simCors.sim,~.x-.y)%>%map(~.[lower.tri(.)]),
         trueCors.bias=map2(trueCors.sim,simCors.sim,~.x-.y)%>%map(~.[lower.tri(.)]),
         tCovs.bias=map2(tCovs.sim,simCovs.sim,~.x-.y)%>%map(~.[lower.tri(.)]),
         trueCovs.bias=map2(trueCovs.sim,simCovs.sim,~.x-.y)%>%map(~.[lower.tri(.)]),
         mxItems=map(items,~matrix(paste0(rep(.,times=length(.)),rep(.,each=length(.))),
                                   nrow=length(na.omit(.)),ncol=length(na.omit(.)),byrow=T))%>%
           map(~.[lower.tri(.)])
  )
allOut3%>%print(width=Inf)


# allOut2=allOut2%>%
#   mutate(tTrueDifMeans=map2(tMeans.dat,trueMeans.dat,~.x-.y),
#          tTrueDifSDs=map2(tSDs.dat,trueSDs.dat,~.x-.y),
#          tTrueDifCors=map2(tCors.dat,trueCors.dat,~.x-.y)%>%map(reshapeMx),
#          tTrueDifCovs=map2(tCovs.dat,trueCovs.dat,~.x-.y)%>%map(reshapeMx))
#convert to separate tibbles, unnest, and plot
allOut4v=allOut3%>%
  dplyr::select(all_of(colnames(dmx)),ends_with('bias'))%>%
  mutate(items=map(items,~names(robert)[na.omit(.)]),
         nitems=items%>%map_dbl(length))%>%
  unnest(cols=c(contains('Means'),contains('SDs'),items))
allOut4m=allOut3%>%
  dplyr::select(all_of(colnames(dmx)),ends_with('bias'),mxItems,-items)%>%
  mutate(nitems=mxItems%>%map(~length(.))%>%map_dbl(~(1+sqrt(1+8*.))/2))%>%
  unnest(cols=c(contains('Cors'),contains('Covs'),mxItems))
allOut4v%>%
  group_by(items,nitems)%>%
  summarize(tMeans.bias=mean(abs(tMeans.bias)),
            trueMeans.bias=mean(abs(trueMeans.bias)))%>%
  ggplot(aes(x=tMeans.bias,y=trueMeans.bias,
             size=nitems^2,col=items))+
  geom_point()+geom_abline(slope=1,intercept=0)
allOut4v%>%
  group_by(items,nitems)%>%
  summarize(tSDs.bias=mean(abs(tSDs.bias)),
            trueSDs.bias=mean(abs(trueSDs.bias)))%>%
  ggplot(aes(x=tSDs.bias,y=trueSDs.bias,
             size=nitems^2,col=items))+
  geom_point()+geom_abline(slope=1,intercept=0)
pCor=allOut4m%>%
  group_by(mxItems,nitems)%>%
  summarize(tCors.bias=mean(abs(tCors.bias)),
            trueCors.bias=mean(abs(trueCors.bias)))%>%
  ggplot(aes(x=tCors.bias,y=trueCors.bias,
             size=nitems^2,col=mxItems))+
  geom_point()+geom_abline(slope=1,intercept=0)
r<-layer_scales(pCor)$x$range$range
s<-layer_scales(pCor)$y$range$range
t<-round(max(r,s),1)
pCor+coord_equal(xlim=c(0,t),ylim=c(0,t))
pCov=allOut4m%>%
  group_by(mxItems,nitems)%>%
  summarize(tCovs.bias=mean(abs(tCovs.bias)),
            trueCovs.bias=mean(abs(trueCovs.bias)))%>%
  ggplot(aes(x=tCovs.bias,y=trueCovs.bias,
             size=nitems^2,col=mxItems))+
  geom_point()+geom_abline(slope=1,intercept=0)
r<-layer_scales(pCov)$x$range$range
s<-layer_scales(pCov)$y$range$range
t<-round(max(r,s),1)
pCov+coord_equal(xlim=c(0,t),ylim=c(0,t))
fdsafsda

# group_by(across(all_of(colnames(dmx))))%>%summarize(tMeans.bias=mean(tMeans.bias),
#                                             trueMeans.bias=mean(trueMeans.bias))%>%
allOut4%>%pivot_longer(cols=c(tMeans.bias,trueMeans.bias))%>%
  ggplot(aes(x=items,y=value,fill=name,
             # col=interaction(pItems,slopeScale),
             group=interaction(items,name,pItems,slopeScale)))+
  geom_boxplot()+
  geom_hline(yintercept=0)
allOut4%>%pivot_longer(cols=c(tSDs.bias,trueSDs.bias))%>%
  ggplot(aes(x=items,y=value,fill=name,
             # col=interaction(pItems,slopeScale),
             group=interaction(items,name,pItems,slopeScale)))+
  geom_boxplot()+
  geom_hline(yintercept=0)
allOut3%>%
  dplyr::select(all_of(colnames(dmx)),tSDs.bias,trueSDs.bias)%>%
  mutate(items=map(items,~names(robert)[na.omit(.)]))%>%
  unnest(cols=c(contains('SDs'),items))%>%
  # group_by(across(all_of(colnames(dmx))))%>%summarize(tMeans.bias=mean(tMeans.bias),
  #                                             trueMeans.bias=mean(trueMeans.bias))%>%
  pivot_longer(cols=c(tSDs.bias,trueSDs.bias))%>%
  ggplot(aes(x=items,y=value,fill=name,
             # col=interaction(pItems,slopeScale),
             group=interaction(items,name,pItems,slopeScale)))+
  geom_boxplot()+
  geom_hline(yintercept=0)

fsdafsda
#old versions
allOut3%>%
  dplyr::select(all_of(colnames(dmx)),contains('Means'))%>%unnest(cols=contains('Means'))%>%
  pivot_longer(cols=-c(pItems,slopeScale))%>%
  mutate(scoreType=strsplit(name,".",fixed=T)%>%map_chr(~.[[1]]),
         source=strsplit(name,".",fixed=T)%>%map_chr(~.[[2]]),
         variable=strsplit(name,".",fixed=T)%>%map_chr(~.[[3]]))%>%dplyr::select(-name)%>%
  ggplot(aes(x=variable,y=value,
             col=interaction(pItems,slopeScale),
             group=interaction(pItems,slopeScale)))+geom_line()+
  facet_grid(source~scoreType)
allOutMinusBase%>%
  dplyr::select(all_of(colnames(dmx)),contains('SDs'))%>%unnest(cols=contains('SDs'))%>%
  pivot_longer(cols=-c(pItems,slopeScale))%>%
  mutate(scoreType=strsplit(name,".",fixed=T)%>%map_chr(~.[[1]]),
         source=strsplit(name,".",fixed=T)%>%map_chr(~.[[2]]),
         variable=strsplit(name,".",fixed=T)%>%map_chr(~.[[3]]))%>%dplyr::select(-name)%>%
  ggplot(aes(x=variable,y=value,
             col=interaction(pItems,slopeScale),
             group=interaction(pItems,slopeScale)))+geom_line()+
  facet_grid(source~scoreType)

rtPostProcess=proc.time()-pct
rtPostProcess

##################

fsdafsda
simtab
#add same stuff you did for main data set
simtab=simtab%>%
  mutate(tCor=smice.data%>%map(function(x)complete(x,'all')%>%
                                 map(~dplyr::select(.,ends_with("_T"))%>%cor)%>%
                                 abind(along=3)%>%apply(c(1,2),mean)),
         trueCor=smice.data%>%map(function(x)complete(x,'all')%>%
                                    map(~dplyr::select(.,ends_with("_TRUE"))%>%cor)%>%
                                    abind(along=3)%>%apply(c(1,2),mean)),
         tCov=smice.data%>%map(function(x)complete(x,'all')%>%
                                 map(~dplyr::select(.,ends_with("_T"))%>%cov)%>%
                                 abind(along=3)%>%apply(c(1,2),mean)),
         trueCov=smice.data%>%map(function(x)complete(x,'all')%>%
                                    map(~dplyr::select(.,ends_with("_TRUE"))%>%cov)%>%
                                    abind(along=3)%>%apply(c(1,2),mean)),
         tMean=smice.data%>%map(function(x)complete(x,'all')%>%
                                  map(~dplyr::select(.,ends_with("_T"))%>%map_dbl(mean))%>%
                                  bind_cols%>%rowMeans),
         trueMean=smice.data%>%map(function(x)complete(x,'all')%>%
                                     map(~dplyr::select(.,ends_with("_TRUE"))%>%map_dbl(mean))%>%
                                     bind_cols%>%rowMeans),
         tSD=smice.data%>%map(function(x)complete(x,'all')%>%
                                map(~dplyr::select(.,ends_with("_T"))%>%map_dbl(sd))%>%
                                bind_cols%>%rowMeans),
         trueSD=smice.data%>%map(function(x)complete(x,'all')%>%
                                   map(~dplyr::select(.,ends_with("_TRUE"))%>%map_dbl(sd))%>%
                                   bind_cols%>%rowMeans))
#which means are correct?
lvMeans=apply(trueCorData,2,mean)*10+50
lvSDs=apply(trueCorData,2,sd)*10
simtab%>%pull(trueMean)%>%map_dfc(~abs(.-lvMeans))%>%rowMeans%>%mean
simtab%>%pull(tMean)%>%map_dfc(~abs(.-lvMeans))%>%rowMeans%>%mean
simtab%>%pull(trueSD)%>%map_dfc(~abs(.-lvSDs))%>%rowMeans%>%mean
simtab%>%pull(tSD)%>%map_dfc(~abs(.-lvSDs))%>%rowMeans%>%mean
fdsafd

#tabulations by group
mice.data%>%complete('all')%>%map(~mutate(.,Age=floor(Age/10),
                                          SickDays=floor(SickDays/5)*5,
                                          HospitalStays=floor(HospitalStays/5)*5)%>%
                                    group_by(Age,Female,HospitalStays,SickDays)%>%
                                    summarize(across(ends_with("_TRUE"),mean)))%>%
  bind_rows%>%
  group_by(Age,Female,HospitalStays,SickDays)%>%
  summarize(across(ends_with("_TRUE"),mean))
useData%>%mutate(Age=floor(Age/10),
                 SickDays=floor(SickDays/5)*5,
                 HospitalStays=floor(HospitalStays/5)*5)%>%
  group_by(Age,Female,HospitalStays,SickDays)%>%
  summarize(across(ends_with("_T"),mean))


sfdafsd
#look at output
# simHUI[[1]][[1]]$smice.data%>%plot
# simHUI[[1]][[2]]$smice.data%>%plot
# simHUI[[1]][[3]]$smice.data%>%plot
# simHUI[[2]][[3]]$smice.data%>%plot
# simHUI[[3]][[3]]$smice.data%>%plot

#which # to look at?
# i=10
# rawcor=simHUI[[1]][[i]]$trueCorData[,1:(i+4)]%>%cor
# tcor=simHUI[[1]][[i]]$trueCorData%>%
#   dplyr::select(.,all_of(demographics),ends_with("_T"))%>%cor
# truecor=simHUI[[1]][[i]]$smice.data%>%complete('all')%>%
#   map(~cor(dplyr::select(.,all_of(demographics),ends_with("_TRUE"))))%>%abind(along=3)%>%apply(c(1,2),mean)
# cor.plot(rawcor-tcor)
# cor.plot(rawcor-truecor)

#number of items and other front matter
sets2run2=sets2run%>%
  mutate(across(all_of(paste0('var_',1:10)),~ifelse(!is.na(.),1,0)))%>% #recode items to T or F
  mutate(nitems=rowSums(across(all_of(paste0('var_',1:10))))) #number of items
sets2run2$nitems
#variances of imputed scores
sets2run2=sets2run2%>%
  mutate(imputed.variances=smice.data%>%map(
    ~complete(.,'all')%>%
      map(~dplyr::select(.,ends_with("_TRUE"))%>%map_dbl(~sd(.))))%>%
      map(~bind_rows(.)%>%apply(.,2,mean)))
sets2run2$imputed.variances
#stats of 'em
sets2run2=sets2run2%>%
  mutate(varmin=imputed.variances%>%map_dbl(min),
         varmean=imputed.variances%>%map_dbl(mean),
         varmedian=imputed.variances%>%map_dbl(median),
         varmax=imputed.variances%>%map_dbl(max))
#what matters?
aov(as.formula(paste0('varmin~nitems*(',
                      paste(paste0('var_',1:10),collapse="+"),")")),
    data=sets2run2)%>%summary
aov(as.formula(paste0('varmean~nitems*(',
                      paste(paste0('var_',1:10),collapse="+"),")")),
    data=sets2run2)%>%summary
aov(as.formula(paste0('varmedian~nitems*(',
                      paste(paste0('var_',1:10),collapse="+"),")")),
    data=sets2run2)%>%summary
aov(as.formula(paste0('varmax~nitems*(',
                      paste(paste0('var_',1:10),collapse="+"),")")),
    data=sets2run2)%>%summary

#save it out
save.image("C:/Users/Max/Documents/sim_workspaces/ECHO OIF/realdata_HUI_v6.RData")
