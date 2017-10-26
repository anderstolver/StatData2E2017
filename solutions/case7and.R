# Statistical Data Analysis 2, 2011   #
# R-program with comments as solution #
# to case 7.                          #
# Ib Michael Skovgaard                #
# Anders Tolver                       #
#######################################

#### (a)

peas=read.table(file="../data/bms_exercise9_4.txt",header=T)
peas
attach(peas)
N=factor(N)
P=factor(P)
K=factor(K)
block=factor(block)


## The data set contains 24 observations and 5 variables:
  ##          N (1,2)
  ##          P (1,2)
  ##          K (1,2) 
  ##          block (1,2, ..., 6)
  ##          yield

## The first three are factors to be modelled with systematic effects.
## We want to include interactions between these.
## Blocks enter the model with random effects (in principle).
## The NxPxK interaction is confounded with blocks (in three pairs of 
##  blocks). It may or may not be included in the model. We include it
##  here. Thus the model has the main effects of N,P,K and the three
##  2-factor interactions plus NxPxK.


#### (b)

library(nlme)
model1=lme(yield~N*P*K,random=~1|block,method="ML")
model1a=lme(yield~N*P*K,random=~1|block,method="REML")
summary(model1a)
model2=lme(yield~N*P+N*K+P*K,random=~1|block,method="ML")
model3a=lme(yield~N*P+N*K,random=~1|block,method="ML")
model3b=lme(yield~N*P+P*K,random=~1|block,method="ML")
model3c=lme(yield~N*K+P*K,random=~1|block,method="ML")
model4a=lme(yield~N*P+K,random=~1|block,method="ML")
model4b=lme(yield~N*K+P,random=~1|block,method="ML")
model4c=lme(yield~N+P*K,random=~1|block,method="ML")
model5a=lme(yield~N*P,random=~1|block,method="ML")
model5b=lme(yield~N*K,random=~1|block,method="ML")
model5c=lme(yield~P*K,random=~1|block,method="ML")
model5d=lme(yield~N+P+K,random=~1|block,method="ML")
model6a=lme(yield~N+P,random=~1|block,method="ML")
model6b=lme(yield~N+K,random=~1|block,method="ML")
model6c=lme(yield~P+K,random=~1|block,method="ML")
model7a=lme(yield~N,random=~1|block,method="ML")
model7b=lme(yield~K,random=~1|block,method="ML")

anova(model2,model1) # NxPxK non-sign.

anova(model3a,model2) # PxK non-sign.

anova(model4b,model3a) # NxP non-sign.

anova(model5b,model4b) # P non-sign.

anova(model6b,model5b) # NxK non-sign.

anova(model7a,model6b) # K sign.
anova(model7b,model6b) # N sign.

## model6b with no effect of P and no interaction but main effect of
## N and K is the final model.

## To obtain the estimates:

finalmodel = lme(yield ~ N+K, random= ~1 | block,method="REML")
summary(finalmodel)

library(gmodels)

N.effect= c(0,1,0) ## effect of N2 minus that of N1 (ref.level)
K.effect= c(0,0,1) ## effect of K2 minus that of K1 (ref.level)

effects = rbind(N.effect, K.effect)
estimable(finalmodel, effects, conf.int=0.95)

## N2 gives higher yield than N1, K2 gives lower yield than K1.


#### (c)

## tre-faktor vekselvirkningen NxKxP er konfunderet med blokkene.


#### (d):

model <- lm(yield~N*P*K)
summary(model)

modelblock = lm(yield ~block + N*P*K)
summary(modelblock)

## no estimate for confunded interaction  NxPxK when block is included as systematic effect

#### (e):

## It is optional whether the estimate for the interaction of (N, P) is estimated
## based on a model with only N x P included or (as it done below) 
## N x P x K is included in the fixed effect parts of the model.

## easy solution

summary(modelblock)  ## To see the order of the parameters.
confint(modelblock)

## Estimate for interaction (N,P): -3.77 [-10.76,3.22]

## more complicated - but also correct

modelny<-lm(yield~block+N+P+K+N*P+N*K+P*K)
summary(modelny) ## To see the order of the parameters.
NP= c(0,0,0,0,0,0,0,0,0,1,0,0) 
  ## The double-difference, where only one of the four terms is not zero.
estimable(modelny, rbind(NP), conf.int=0.95)

#### (f):

summary(modelblock)
NPK= c(0,0,0,0,0,0,0,0,0,0,0,0,1) 
  ## The triple-difference, where only one of the eight terms is not zero.
estimable(modelblock, rbind(NPK), conf.int=0.95)
  ## This is not estimable because of confounding with blocks.
  ## Because the N:P:K is NA R won't do it.

#### (g):

## refit model2 with method="REML"

model2ny=lme(yield~N*P+N*K+P*K,random=~1|block,method="REML")

summary(model2ny)  ## To see the order of the parameters.
NP= c(0,0,0,0,1,0,0) 
  ## The double-difference, where only one of the four terms is not zero.
estimable(model2ny, rbind(NP), conf.int=0.95)

## Note that it gives exactly the same result as in (e).


#### (h):

summary(model1)
NPK= c(0,0,0,0,0,0,0,1) 
  ## The triple-difference, where only one of the eight terms is not zero.
estimable(model1, rbind(NPK), conf.int=0.95)
## Now we get an estimate of the 3-factor interaction! (With a rather large
##   SE, though, because it uses the information between blocks.)


#### (i):
## If the blocks are modelled as fixed effects they explain all differences
## between the 6 blocks,included that part that might be due to the NxPxK 
## interaction, which hence cannot be estimated, because it is unseparable 
## from the block effects. If blocks are modelled as random, that part of 
## the differences between blocks that corresponds to the estimate of the
## NxPxK interaction is estmated with an SE that is based on the general
## (remaining) variation BETWEEN blocks. This is called recovery of 
## inter-block information. Note that the estimate corresponding to 
## NxPxK (see question f) is a difference between block totals, hence it
## is a comparison between blocks.


#### (j):
## An obvious way would be to confound NxPxK in blocks 1 and 2 (as is done),
## but then confound NxP in blocks 3 and 4, and NxK in blocks 5 and 6. 
## This is called partial confounding, and it makes all effects estimable
##   although with different precisions. One would usually keep the main 
##   effects unconfounded if a choice has to be made.


#### (k):
## According to above we might like to confound each of the effects 
## equally often. Seven effects: N, P, K, NxP, ... each in two blocks 
## would give an experiment with 14 blocks. This could of course be 
## doubled to 28 blocks and so on.


#### (l):

VarCorr(finalmodel) ## gives the variance estimates
  ## block variance estimate = 13.3
  ## residual varaince est   = 15.5
s2=  15.5
sB2= 13.3

### Formula (9.11) in BMS:

dfe= 4*14 - 14 - 8 + 1
tquantile= qt(0.975, df=dfe)
rblock= 4   ## number of units in each block
rtreat= 7   ## reps of each treatment
nu.treat= 8 ## number of treatments
lambda= 3

x= sqrt(2*(rblock*sB2 + s2)/(lambda*nu.treat*sB2 + rtreat*s2) )
LSD= tquantile*sqrt(s2)*x
LSD  ## the answer 



















