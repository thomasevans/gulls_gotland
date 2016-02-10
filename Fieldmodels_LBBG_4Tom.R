####Prep: read in file 'LBBGfield2.xlsx', all variables, no NA, seems to work better with lme4.####



# field<-LBBGfield2



field <- read.table("LBBGfield2.txt", header = TRUE,
                    sep = "\t", dec = ",")
View(field)
str(field)
head(field)

#for some reason crop has 10 lvls when it should only have 9
#so use crop2 instead

field$Transect<-as.factor(field$Transect)
field$crop3<-as.factor(field$crop3)
field$crop2<-as.factor(field$crop2)
field$veg.cover2<-as.factor(field$veg.cover2)
field$day<-as.factor(field$day)
field$season<-as.factor(field$season)
field$wind.dir4<-as.factor(field$wind.dir4)
# str(field$veg.height)
# field$veg.height <-as.numeric(as.character(field$veg.height))


str(field)


#convert ppt to mm from m 
#air temp seems to be in celsius already

ppt_mm<-field$ppt*1000
field<-cbind(field, ppt_mm)

str(field)
head(field)

#load package
library(lme4)

##########models, for real this time#####
mod.int<-glmer(LBBG~(1|Transect/Field), family=binomial, data=field)
mod.field1<-glmer(LBBG~crop3+veg.height+veg.cover2+
                    (1|Transect/Field), family=binomial, data=field)
mod.field2<-glmer(LBBG~veg.height+veg.cover2+
                    (1|Transect/Field), family=binomial, data=field)
mod.field3<-glmer(LBBG~crop3+veg.height+
                    (1|Transect/Field), family=binomial, data=field)
mod.field4<-glmer(LBBG~veg.height+(1|Transect/Field), family=binomial, data=field)
mod.field5<-glmer(LBBG~season+day+(1|Transect/Field), family=binomial, data=field)
mod.field6<-glmer(LBBG~season+(1|Transect/Field), family=binomial, data=field)
mod.field7<-glmer(LBBG~day+(1|Transect/Field), family=binomial, data=field)
mod.field8<-glmer(LBBG~veg.height+season+(1|Transect/Field), family=binomial, data=field)
mod.field9<-glmer(LBBG~veg.height+season+gulls+(1|Transect/Field), family=binomial, data=field)
mod.field10<-glmer(LBBG~veg.height+season+waders+(1|Transect/Field), family=binomial, data=field)
mod.field11<-glmer(LBBG~veg.height+season+earthworm+(1|Transect/Field), family=binomial, data=field)
mod.field12<-glmer(LBBG~crop3+veg.height+veg.cover2+season+day+
                     (1|Transect/Field), family=binomial, data=field)

library(mnormt)
library(sjPlot)
library(Rcpp)
library(arm)
library(MuMIn)

aicc.val <- AICc(mod.int, mod.field1, mod.field2, mod.field3, mod.field4,
                 mod.field5, mod.field6, mod.field7, 
                 mod.field8, mod.field9, mod.field10, mod.field11, mod.field12)
str(aicc.val)
aicc.val

aicc.val$dAICc <- aicc.val$AICc - aicc.val$AICc[9]
aicc.val[order(aicc.val$dAICc),]

rf<-r.squaredGLMM(mod.int)
rf1<-r.squaredGLMM(mod.field1)
rf2<-r.squaredGLMM(mod.field2)
rf3<-r.squaredGLMM(mod.field3)
rf4<-r.squaredGLMM(mod.field4)
rf5<-r.squaredGLMM(mod.field5)
rf6<-r.squaredGLMM(mod.field6)
rf7<-r.squaredGLMM(mod.field7)
rf8<-r.squaredGLMM(mod.field8)
rf9<-r.squaredGLMM(mod.field9)
rf10<-r.squaredGLMM(mod.field10)
rf11<-r.squaredGLMM(mod.field11)
rf12<-r.squaredGLMM(mod.field12)

rf
rf1
rf2
rf3
rf4
rf5
rf6
rf7
rf8
rf9
rf10
rf11
rf12

stdz.mod.field8<-standardize(mod.field8, standardize.y=FALSE)
summary(stdz.mod.field8)

stdz.mod.field12<-standardize(mod.field12, standardize.y=FALSE)
summary(stdz.mod.field12)

#good to know: crop1 is roots, crop2 is grass, crop3 is cereals, crop4 is ryegrass, crop5 is other (rapeseed and snowpeas)



# VIF + Kappa -------
summary(stdz.mod.field8)

# Get functions from 'mer-utils.R'
source("mer-utils.R")


# Kappa
mod.list <- list(mod.int, mod.field1, mod.field2, mod.field3, mod.field4,
              mod.field5, mod.field6, mod.field7, 
              mod.field8, mod.field9, mod.field10, mod.field11, mod.field12)
kappas <- unlist(lapply(mod.list, kappa.mer))


kappa.df <- cbind.data.frame((c("mod.int", "mod.field1", "mod.field2", "mod.field3", "mod.field4",
                                "mod.field5", "mod.field6", "mod.field7", 
                                "mod.field8", "mod.field9", "mod.field10", "mod.field11", "mod.field12")), kappas)
names(kappa.df) <- c("mod", "kappa")
# See what this looks like
view(kappa.df)


vif.mer(stdz.mod.field8)

vif.mer(stdz.mod.field12)

colldiag.mer(stdz.mod.field8)
colldiag.mer(stdz.mod.field12)

maxcorr.mer(stdz.mod.field12)
maxcorr.mer(stdz.mod.field8)


