#install.packages("dlnm")
library(dlnm)

head(drug,3)

#make the matrix of exposure histories
Qdrug<-as.matrix(drug[,rep(7:4,each=7)])
colnames(Qdrug)<-paste("lag",0:27,sep="")
head(Qdrug)

#define a cross-basis function: argvar deines the exposure-response function, and arglag deine the lag-response function
#"lin": linear function "ns": natural cubic spline

cbdrug<-crossbasis(Qdrug,lag=27,argvar=list("lin"),arglag=list(fun="ns",knots=c(9,18)))
summary(cbdrug)

#add the cross-basis matrix to the simple linear model
mdrug<-lm(out~cbdrug+sex,drug)

#predicting specific effect summaries to interpret the estimated exsposure-lag-response association
pdrug<-crosspred(cbdrug,mdrug,at=0:20*5)

#plots
plot(pdrug,zlab="Effect",xlab="Dose",ylab="Lag (days)")
plot(pdrug,var=60,ylab="Effect at dose 60",xlab="Lag (days)",ylim=c(-1,5))
plot(pdrug,lag=10,ylab="Effect at lag 10",xlab="Dose",ylim=c(-1,5))

#the outcome associated with an intake of a dose level of 10 five days earlier
pdrug$matfit["10","lag5"]

#overall cumulative effects associated with an exposure to 10
with(pdrug,cbind(allfit,alllow,allhigh)["10",])