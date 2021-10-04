##set working directory
setwd("C:/Users/aughi/Desktop/Progetto Stat/Dataset_Southern_Italy/Dataset_No_Outlier")

##import DATA
library(readr)
DATA<- read.csv("FAS_000_50_NoOutlier.csv")
load("C:/Users/aughi/Desktop/Progetto Stat/Dataset_Southern_Italy/Dataset_No_Outlier/coeff_estimates_000.50HZ.RData")

#verify DATA structure
head(DATA)

#pre processing
#remove missing values of VS30
DATA<-DATA[-which(is.na(DATA$UsableVS30)),]

#define groups
EV.fact<-as.factor(DATA$ID)
ST.fact<-as.factor(DATA$Stat)

#set nonlinear coefficients
mh     <-coeff.estimates[[2]][1]
mref   <-coeff.estimates[[2]][2]
h      <-coeff.estimates[[2]][3]

#MODEL:independent  mixed effects due to station and event
# LogY =  const+ a1*ifelse((ML- mh)<=0,ML-mh,0)
#        + a2*ifelse((ML- mh)>0,ML-mh,0) 
#        +(c1*(ML-mref)+c2)*log10(sqrt(Dipo^2+h^2))
#        +c3* sqrt(Dipo^2+h^2)
#        +d1*log10(UsableVS30/800)
#        +randomeffectdependingonEV
#        +randomeffectdependingonST
## NOTE: we are supposing of an effect directly on the response value,non in one of the regressors
#        --> (1|EV.fact) : practically we account for a "casual intercept" depending on single event
#load package
library(lme4)
library(nlme)
attach(DATA)

#define DESIGN MATRIX
Z<-data.frame(X0=rep(1,dim(DATA)[1]),X1=ifelse((ML- mh)<=0,ML-mh,0),X2=ifelse((ML- mh)>0,ML-mh,0),
         X3=(ML-mref)*log10(sqrt(Dipo^2+h^2)),X4=log10(sqrt(Dipo^2+h^2)),X5=sqrt(Dipo^2+h^2),
         X6=log10(UsableVS30/800))
attach(Z)
LME.fit<-lmer(LogY~ X1+X2+X3+X4+X5+X6+(1|EV.fact)+(1|ST.fact), 
              data=cbind(Z,EV.fact,ST.fact,LogY))
summary(LME.fit)


# LME.fit1<-lme(LogY~ X1+X2+X3+X4+X5+X6, random=~1|EV.fact / ST.fact,
#              data=cbind(Z,EV.fact,ST.fact,LogY))
# summary(LME.fit1)
# aov.lme<-aov(LME.fit1)
# summary(aov.lme)
