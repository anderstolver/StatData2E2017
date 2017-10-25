### eksempel 9.8 ###
####################

data<-read.table(file="../data/bms_examp9_8.txt",header=T)
attach(data)
block<-factor(block)

### Overblik over fors??gsdesign ###
###################################

with(data,table(block,vty))

### blok som tilf??ldig effekt ###
#################################

library(nlme)
model1<-lme(yield~vty,random=~1|block,method="ML")
model2<-lme(yield~1,random=~1|block,method="ML")
anova(model2,model1)

slutmodel<-lme(yield~vty-1,random=~1|block,method="REML")
summary(slutmodel)
library(gmodels)
b.vs.a<-c(-1,1,rep(0,19))
c.vs.a<-c(0,-1,1,rep(0,18))
difs=rbind(b.vs.a,c.vs.a)
estimable(slutmodel,difs,conf.int=0.95)
summary(slutmodel)

### blok som systematisk effekt ###
###################################

mod1<-lm(yield~vty+block)
mod2a<-lm(yield~vty)
mod2b<-lm(yield~block)
anova(mod2a,mod1)
anova(mod2b,mod1)

summary(mod1)
adja<-c(1,rep(0,20),rep(1/21,20))
adjb<-c(1,1,rep(0,19),rep(1/21,20))
adjc<-c(1,rep(0,1),1,rep(0,18),rep(1/21,20))
adjd<-c(1,rep(0,2),1,rep(0,17),rep(1/21,20))
adje<-c(1,rep(0,3),1,rep(0,16),rep(1/21,20))
adjf<-c(1,rep(0,4),1,rep(0,15),rep(1/21,20))
adjg<-c(1,rep(0,5),1,rep(0,14),rep(1/21,20))
adjh<-c(1,rep(0,6),1,rep(0,13),rep(1/21,20))
adji<-c(1,rep(0,7),1,rep(0,12),rep(1/21,20))
adjj<-c(1,rep(0,8),1,rep(0,11),rep(1/21,20))
adjk<-c(1,rep(0,9),1,rep(0,10),rep(1/21,20))
adjl<-c(1,rep(0,10),1,rep(0,9),rep(1/21,20))
adjm<-c(1,rep(0,11),1,rep(0,8),rep(1/21,20))
adjn<-c(1,rep(0,12),1,rep(0,7),rep(1/21,20))
adjo<-c(1,rep(0,13),1,rep(0,6),rep(1/21,20))
adjp<-c(1,rep(0,14),1,rep(0,5),rep(1/21,20))
adjq<-c(1,rep(0,15),1,rep(0,4),rep(1/21,20))
adjr<-c(1,rep(0,16),1,rep(0,3),rep(1/21,20))
adjs<-c(1,rep(0,17),1,rep(0,2),rep(1/21,20))
adjt<-c(1,rep(0,18),1,rep(0,1),rep(1/21,20))
adju<-c(1,rep(0,19),1,rep(1/21,20))

adjmeans<-rbind(adja,adjb,adjc,adjd,adje,adjf,adjg,adjh,adji,adjj,adjk,adjl,adjm
                ,adjn,adjo,adjp,adjq,adjr,adjs,adjt,adju)

estimable(mod1,adjmeans)->fixest
fixest

