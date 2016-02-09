#BIOS Exercise 6

setwd('C:/Users/Natalie/Desktop/BIOS12/Exercise 6')

getwd()

#read in 'RMfor.csv'.

str(RMfor)

#make a scatterplot matrix of the 4 tree variables

pairs(~oak+birch+alder+lime, data=RMfor, main="Simple Scatterplot Matrix")

#don't seem independent

#now we try to predict usage of lime from the other usage variables,
#taking interactions into account
#to do this, need to compute new variables for the 2 way interaction terms between oak, birch, and alder and add them to dataframe
#then use rcorr in Hmisc library to compute Pearson correlations

RMfor$oxb<-RMfor$oak*RMfor$birch
RMfor$oxa<-RMfor$oak*RMfor$alder
RMfor$bxa<-RMfor$birch*RMfor$alder
library(Hmisc)
rcorr(as.matrix(RMfor[4:10]), type="pearson")

#strong negative correlation bw birch and lime, p=0.0001

#make a partial correlations matrix controlling for most sig variable
#using pcor.test in the ppcor library

library(ppcor)
p.values<-rep(NA,10)
dim(p.values)<-c(5,2)
lim_oak<-pcor.test(RMfor$lime, RMfor$oak, RMfor$birch, method="pearson")
p.values[1,1]<-"lim_oak"
p.values[1,2]<-lim_oak$p.value
lim_ald<-pcor.test(RMfor$lime, RMfor$alder, RMfor$birch, method="pearson")
p.values[2,1]<-"lim_ald"
p.values[2,2]<-lim_ald$p.value
lim_oxb<-pcor.test(RMfor$lime, RMfor$oxb, RMfor$birch, method="pearson")
p.values[3,1]<-"lim_oxb"
p.values[3,2]<-lim_oxb$p.value
lim_oxa<-pcor.test(RMfor$lime, RMfor$oxa, RMfor$birch, method="pearson")
p.values[4,1]<-"lim_oxa"
p.values[4,2]<-lim_oxa$p.value
lim_bxa<-pcor.test(RMfor$lime, RMfor$bxa, RMfor$birch, method="pearson")
p.values[5,1]<-"lim_bxa"
p.values[5,2]<-lim_bxa$p.value
p.values

#once birch is accounted for, oak has the biggest effect

#make a new partial correlation matrix
#that controls for birch and oak
#repeat until no more variables should be included at the 5% lvl

p.values2<-rep(NA, 8)
dim(p.values2)<-c(4,2)
lim_ald<-pcor.test(RMfor$lime, RMfor$alder, RMfor[4:5], method="pearson")
p.values2[1,1]<-"lim_ald"
p.values2[1,2]<-lim_ald$p.value
lim_oxb<-pcor.test(RMfor$lime, RMfor$oxb, RMfor[4:5], method="pearson")
p.values2[2,1]<-"lim_oxb"
p.values2[2,2]<-lim_oxb$p.value
lim_oxa<-pcor.test(RMfor$lime, RMfor$oxa, RMfor[4:5], method="pearson")
p.values2[3,1]<-"lim_oxa"
p.values2[3,2]<-lim_oxa$p.value
lim_bxa<-pcor.test(RMfor$lime, RMfor$bxa, RMfor[4:5], method="pearson") 
p.values2[4,1]<-"lim_bxa"
p.values2[4,2]<-lim_bxa$p.value
p.values2

#after birch and oak accounted for, alder has effect

p.values3<-rep(NA, 6)
dim(p.values3)<-c(3,2)
lim_oxb<-pcor.test(RMfor$lime, RMfor$oxb, RMfor[4:6], method="pearson")
p.values3[1,1]<-"lim_oxb"
p.values3[1,2]<-lim_oxb$p.value
lim_oxa<-pcor.test(RMfor$lime, RMfor$oxa, RMfor[4:6], method="pearson")
p.values3[2,1]<-"lim_oxa"
p.values3[2,2]<-lim_oxa$p.value
lim_bxa<-pcor.test(RMfor$lime, RMfor$bxa, RMfor[4:6], method="pearson")
p.values3[3,1]<-"lim_bxa"
p.values3[3,2]<-lim_bxa$p.value
p.values3

#make a stepwise regression, with lime dependent and all others independent
#use stepAIC in the MASS library
library(MASS)
fit<-lm(lime~birch, data=RMfor)
step<-stepAIC(fit, scope=list(upper=~oak+birch+alder+oxb+oxa+bxa, lower=~1), direction="forward")
step$anova
summary(step)

#final model contains birch, oak, and alder (same as above procedure)

# kappa thing ----
#can check for collinearity problems in the model using kappa

model<-model.matrix(~oak+birch+alder+oxb+oxa+bxa, data=RMfor)
kappa(model)

#high kappa values are not good
#an alternative is to use vif in the car library
library(car)
fit2<-lm(lime~oak+birch+alder+oxb+oxa+bxa, data=RMfor)
vif(fit2)

#variance inflation facotrs (vif) higher than 4 may indicate issues

#---Part B---

#read in 'PCA.csv'. 

str(PCA)

#exclude missing samples first

PCA.na<-na.omit(PCA)

#run a regression with age dependent and the nine body measures as independent
#use forward model selection (as above)

head(PCA)
library(MASS)
fitPCA<-lm(age~wing, data=PCA.na)
stepPCA<-stepAIC(fitPCA, scope= list(upper=~wing+tarsv+tail+wproj+fett+weigh+bill+bihe+biwi, lower=~1), direction="forward")
stepPCA$anova
summary(stepPCA)

#it selects 4 variables, wing, weight, bill height, and bill weight.
#only wing and bill height are significant because AIC maximizes the overall fit of the model, not the number of sig predictors

#now rerun same regression but use backward selection

fitPCA.bw<-lm(age~wing+tarsv+tail+wproj+fett+weigh+bill+bihe+biwi, data=PCA.na)
stepPCA.bw<-stepAIC(fitPCA.bw, scope=list(upper=~wing+tarsv+tail+wproj+fett+weigh+bill+bihe+biwi, lower=~1), direction="backward")
stepPCA.bw$anova
summary(stepPCA.bw)

#finds same variables as in forward selection

#make a third regression using forward selection
#but excluding the wing parameter.

head(PCA)
fitPCA2<-lm(age~tarsv, data=PCA.na)
stepPCA2<-stepAIC(fitPCA2, scope=list(upper=~tarsv+tail+wproj+fett+weigh+bill+bihe+biwi, lower=~1), direction="forward")
stepPCA2$anova
summary(stepPCA2)
library(Hmisc)
rcorr(PCA$wing, PCA$wproj, type="pearson")

#tarsv, wproj, weigh, bill height, and bill weight are the new parameters
#wproj is a measure of how pointy the wing is
#the correlation (0.29) means that if wing length is excluded, wing projection can be used instead and it has decent explanatory power

#---partC---

#read in the file 'PHEASANT.csv'

str(PHEASANT)

#plot weight against ageday

plot(PHEASANT$WEIGHT, PHEASANT$AGEDAY)

#use non-linear regressions to estimate parameters
#using command nls
#save the predicted values

pheasant<-PHEASANT
str(pheasant)

nls<-nls(pheasant$WEIGHT~a/(1+exp(-k*(pheasant$AGEDAY-i))), data=pheasant, start=list(a=1400, k=0.05, i=60), na.action=na.omit)
summary(nls)

pheasant$PRED<-predict(nls, pheasant)
summary(nls, correlation=TRUE)

#plot the original weight values and the predicted against the age
plot(pheasant$WEIGHT~pheasant$AGEDAY)
points(pheasant$PRED~pheasant$AGEDAY, col="blue")

#you can do the same analysis for the variable TARSH
#should follow same general equation but need diff start values
#make a scatteplot to see what start values may be reasonable

plot(pheasant$TARSH~pheasant$AGEDAY)
nls2<-nls(TARSH~a/(1+exp(-k*(AGEDAY-i))), data=pheasant, start=list(a=100, k=0.05, i=30), na.action=na.omit)
summary(nls2)

pheasant$PRED<-predict(nls2, pheasant)
summary(nls2, correlation=TRUE)

plot(pheasant$TARSH~pheasant$AGEDAY, xlab="age", ylab="tarsus height")
points(pheasant$PRED~pheasant$AGEDAY, col="blue")

