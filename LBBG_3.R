#Double-checking earthworm association with LBBG

x<-as.matrix(c(277,16,20,3))
dim(x)<-c(2,2)
x
chisq.test(x)
fisher.test(x)

#p value is 0.1512 from Fishers. x2 is 1.0355, d.f. is 1.
#check the expected values
exp <- matrix(NA,2,2)
exp[1,1]<-(((sum(x[1,]))/sum(x))*((sum(x[,1]))/sum(x))*(sum(x)))
exp[1,2]<-(((sum(x[1,]))/sum(x))*((sum(x[,2]))/sum(x))*(sum(x)))
exp[2,1]<-(((sum(x[2,]))/sum(x))*((sum(x[,1]))/sum(x))*(sum(x)))
exp[2,2]<-(((sum(x[2,]))/sum(x))*((sum(x[,2]))/sum(x))*(sum(x)))
exp

#expected value for earthworm and LBBG presence is lower than 5 
#so Fishers is correct test, results are:

p-value = 0.1512
95 percent confidence interval:
  0.4462021 10.1958446
odds ratio 
2.585771 

#checking gulls and waders vs LBBG

g<-as.matrix(c(283,10,9,14))
dim(g)<-c(2,2)
g
chisq.test(g)
fisher.test(g)

exp.g <- matrix(NA,2,2)
exp.g[1,1]<-(((sum(g[1,]))/sum(g))*((sum(g[,1]))/sum(g))*(sum(g)))
exp.g[1,2]<-(((sum(g[1,]))/sum(g))*((sum(g[,2]))/sum(g))*(sum(g)))
exp.g[2,1]<-(((sum(g[2,]))/sum(g))*((sum(g[,1]))/sum(g))*(sum(g)))
exp.g[2,2]<-(((sum(g[2,]))/sum(g))*((sum(g[,2]))/sum(g))*(sum(g)))
exp.g

#for gulls exp<5 so Fishers, and this is sig (p<0.001)
p-value = 1.42e-12
95 percent confidence interval:
  13.64995 142.85456
sample estimates:
  odds ratio 
42.36057 

#waders
w<-as.matrix(c(244,49,12,11))
dim(w)<-c(2,2)
w
chisq.test(w)
fisher.test(w)

exp.w <- matrix(NA,2,2)
exp.w[1,1]<-(((sum(w[1,]))/sum(w))*((sum(w[,1]))/sum(w))*(sum(w)))
exp.w[1,2]<-(((sum(w[1,]))/sum(w))*((sum(w[,2]))/sum(w))*(sum(w)))
exp.w[2,1]<-(((sum(w[2,]))/sum(w))*((sum(w[,1]))/sum(w))*(sum(w)))
exp.w[2,2]<-(((sum(w[2,]))/sum(w))*((sum(w[,2]))/sum(w))*(sum(w)))
exp.w

#for waders, also exp<5 so Fishers and p<0.05 (!!)
p-value = 0.00102
95 percent confidence interval:
  1.706591 11.961495
sample estimates:
  odds ratio 
4.534289 

#crop type
c<-cbind(c(18,0),c(34,8),c(85,5),c(6,0),c(6,0),c(5,1),c(23,1),c(22,2),c(81,3))
c

chisq.test(c)
fisher.test(c)

c.exp<-matrix(NA,2,9)
c.exp[1,1]<-(((sum(c[1,]))/sum(c))*((sum(c[,1]))/sum(c))*(sum(c)))
c.exp[1,2]<-(((sum(c[1,]))/sum(c))*((sum(c[,2]))/sum(c))*(sum(c)))
c.exp[1,3]<-(((sum(c[1,]))/sum(c))*((sum(c[,3]))/sum(c))*(sum(c)))
c.exp[1,4]<-(((sum(c[1,]))/sum(c))*((sum(c[,4]))/sum(c))*(sum(c)))
c.exp[1,5]<-(((sum(c[1,]))/sum(c))*((sum(c[,5]))/sum(c))*(sum(c)))
c.exp[1,6]<-(((sum(c[1,]))/sum(c))*((sum(c[,6]))/sum(c))*(sum(c)))
c.exp[1,7]<-(((sum(c[1,]))/sum(c))*((sum(c[,7]))/sum(c))*(sum(c)))
c.exp[1,8]<-(((sum(c[1,]))/sum(c))*((sum(c[,8]))/sum(c))*(sum(c)))
c.exp[1,9]<-(((sum(c[1,]))/sum(c))*((sum(c[,9]))/sum(c))*(sum(c)))
c.exp[2,1]<-(((sum(c[2,]))/sum(c))*((sum(c[,1]))/sum(c))*(sum(c)))
c.exp[2,2]<-(((sum(c[2,]))/sum(c))*((sum(c[,2]))/sum(c))*(sum(c)))
c.exp[2,3]<-(((sum(c[2,]))/sum(c))*((sum(c[,3]))/sum(c))*(sum(c)))
c.exp[2,4]<-(((sum(c[2,]))/sum(c))*((sum(c[,4]))/sum(c))*(sum(c)))
c.exp[2,5]<-(((sum(c[2,]))/sum(c))*((sum(c[,5]))/sum(c))*(sum(c)))
c.exp[2,6]<-(((sum(c[2,]))/sum(c))*((sum(c[,6]))/sum(c))*(sum(c)))
c.exp[2,7]<-(((sum(c[2,]))/sum(c))*((sum(c[,7]))/sum(c))*(sum(c)))
c.exp[2,8]<-(((sum(c[2,]))/sum(c))*((sum(c[,8]))/sum(c))*(sum(c)))
c.exp[2,9]<-(((sum(c[2,]))/sum(c))*((sum(c[,9]))/sum(c))*(sum(c)))

c.exp

#we have some expected vales <5 so Fishers is correct test, p>0.05 (not sig)

data:  c
p-value = 0.09442
alternative hypothesis: two.sided

#veg cover
v<-cbind(c(28,8), c(15,3), c(22,2), c(215, 7))
v
chisq.test(v)
fisher.test(v)

v.exp<-matrix(NA,2,4)
v.exp[1,1]<-(((sum(v[1,]))/sum(v))*((sum(v[,1]))/sum(v))*(sum(v)))
v.exp[1,2]<-(((sum(v[1,]))/sum(v))*((sum(v[,2]))/sum(v))*(sum(v)))
v.exp[1,3]<-(((sum(v[1,]))/sum(v))*((sum(v[,3]))/sum(v))*(sum(v)))
v.exp[1,4]<-(((sum(v[1,]))/sum(v))*((sum(v[,4]))/sum(v))*(sum(v)))
v.exp[2,1]<-(((sum(v[2,]))/sum(v))*((sum(v[,1]))/sum(v))*(sum(v)))
v.exp[2,2]<-(((sum(v[2,]))/sum(v))*((sum(v[,2]))/sum(v))*(sum(v)))
v.exp[2,3]<-(((sum(v[2,]))/sum(v))*((sum(v[,3]))/sum(v))*(sum(v)))
v.exp[2,4]<-(((sum(v[2,]))/sum(v))*((sum(v[,4]))/sum(v))*(sum(v)))
v.exp

#again, some exp vlaues <5 so Fishers is correct test. p<0.001 (!!)
data:  v
p-value = 0.0002019
alternative hypothesis: two.sided

#crop type with fewer groups (crop type 3)
c3<-cbind(c(39,9), c(85,5), c(104,3), c(22,2), c(29,1))
c3

chisq.test(c3)
fisher.test(c3)

c3.exp<-matrix(NA,2,4)
c3.exp[1,1]<-(((sum(c3[1,]))/sum(c3))*((sum(c3[,1]))/sum(c3))*(sum(c3)))
c3.exp[1,2]<-(((sum(c3[1,]))/sum(c3))*((sum(c3[,2]))/sum(c3))*(sum(c3)))
c3.exp[1,3]<-(((sum(c3[1,]))/sum(c3))*((sum(c3[,3]))/sum(c3))*(sum(c3)))
c3.exp[1,4]<-(((sum(c3[1,]))/sum(c3))*((sum(c3[,4]))/sum(c3))*(sum(c3)))
c3.exp[2,1]<-(((sum(c3[2,]))/sum(c3))*((sum(c3[,1]))/sum(c3))*(sum(c3)))
c3.exp[2,2]<-(((sum(c3[2,]))/sum(c3))*((sum(c3[,2]))/sum(c3))*(sum(c3)))
c3.exp[2,3]<-(((sum(c3[2,]))/sum(c3))*((sum(c3[,3]))/sum(c3))*(sum(c3)))
c3.exp[2,4]<-(((sum(c3[2,]))/sum(c3))*((sum(c3[,4]))/sum(c3))*(sum(c3)))
c3.exp

#Fishers test again due to <5 exp values, p<0.05

data:  c3
p-value = 0.01074
alternative hypothesis: two.sided

#read in file 'LBBGfield.xlsx', it has all variables
#to be tested in the field portion of the manuscript, includes NA

field<-LBBGfield
field
View(field)
str(field)
head(field)

----
####Prep: read in file 'LBBGfield2.xlsx', all variables, no NA, seems to work better with lme4.####

field<-LBBGfield2
field
View(field)
str(field)
head(field)

#for some reason crop has 10 lvls when it should only have 10
#so use crop2 instead

field$Transect<-as.factor(field$Transect)
field$crop3<-as.factor(field$crop3)
field$veg.cover2<-as.factor(field$veg.cover2)
field$day<-as.factor(field$day)
field$season<-as.factor(field$season)
field$wind.dir4<-as.factor(field$wind.dir4)

str(field)

----

field2$Transect<-as.factor(field2$Transect)
field2$crop3<-as.factor(field2$crop3)
field2$crop2<-as.factor(field2$crop2)
field2$veg.cover2<-as.factor(field2$veg.cover2)
field2$day<-as.factor(field2$day)
field2$season<-as.factor(field2$season)
field2$wind.dir4<-as.factor(field2$wind.dir4)

#convert ppt to mm from m 
#air temp seems to be in celsius already

ppt_mm<-field$ppt*1000
field<-cbind(field, ppt_mm)

str(field)
head(field)

------
#Construct 8 models to test
#F: crop type, veg height, veg cover
#B:birds (gulls and waders)
#T: time of day and season
#A: abiotic variables (ppt, cloud, air temp, wind speed and direction)
#interactions of day and season with everything except crop type and height, cover (day)

#na.RM???? apparently default in lme4 is na.omit so should be fine as is?

library(lme4)

#model1: F+B+T+A
mod.1<-glmer(LBBG~crop2+veg.height+veg.cover
             +gulls+waders
             +season+day
             +ppt+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
             +veg.height*season+veg.cover*season
             +gulls*day+gulls*season+waders*day+waders*season
             +ppt*season+cloud*season+air.temp*season+wind.speed*season+wind.dir.NS*season+wind.dir.EW*season
             +ppt*day+cloud*day+air.temp*day+wind.speed*day+wind.dir.NS*day+wind.dir.EW*day
             +(1|transect/field), family=binomial, data=field)
summary(mod.1)

#model2:F+B+T
mod.2<-glmer(LBBG~crop2+veg.height+veg.cover
             +gulls+waders
             +season+day
             +veg.height*season+veg.cover*season
             +gulls*day+gulls*season+waders*day+waders*season
             +(1|transect/field), family=binomial, data=field)
summary(mod.2)

#model3:F+B+A <--no time, not sure how meaningful this is, time is important to look at!
mod.3<-glmer(LBBG~crop2+veg.height+veg.cover
             +gulls+waders
             +ppt+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
             +(1|transect/field), family=binomial, data=field)
summary(mod.3)

#model4: F+T+A
mod.4<-glmer(LBBG~crop2+veg.height+veg.cover
             +season+day
             +ppt+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
             +veg.height*season+veg.cover*season
             +ppt*season+cloud*season+air.temp*season+wind.speed*season+wind.dir.NS*season+wind.dir.EW*season
             +ppt*day+cloud*day+air.temp*day+wind.speed*day+wind.dir.NS*day+wind.dir.EW*day
             +(1|transect/field), family=binomial, data=field)
summary(mod.4)

#model5: F+B
mod.5<-glmer(LBBG~crop2+veg.height+veg.cover
             +gulls+waders
             +(1|transect/field), family=binomial, data=field)
summary(mod.5)

#model6: F+T
mod.6<-glmer(LBBG~crop2+veg.height+veg.cover
             +season+day
             +veg.height*season+veg.cover*season
             +(1|transect/field), family=binomial, data=field)
summary(mod.6)

#model7: F+A
mod.7<-glmer(LBBG~crop2+veg.height+veg.cover
             +ppt+cloud+air.temp+wind.speed+wind.dir.NS+wind.dir.EW
             +(1|transect/field), family=binomial, data=field)
summary(mod.7)

#model8: F
mod.8<-glmer(LBBG~crop2+veg.height+veg.cover
             +(1|transect/field), family=binomial, data=field)
summary(mod.8)

-----
#construct a priori models, exclude unnecessary interactions
  
#model1: F+B+T+A
mod.1<-glmer(LBBG~crop3+veg.height+veg.cover2
               +gulls+waders
               +season+day
               +ppt_mm+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
               +veg.height*season+veg.cover2*season
               +(1|Transect/Field), family=binomial, data=field)
summary(mod.1)

#model2:F+B+T
mod.2<-glmer(LBBG~crop3+veg.height+veg.cover2
             +gulls+waders
             +season+day
             +veg.height*season+veg.cover2*season
             +(1|Transect/Field), family=binomial, data=field)
summary(mod.2)

#model3:F+B+A <--no time, not sure how meaningful this is, time is important to look at!
mod.3<-glmer(LBBG~crop3+veg.height+veg.cover2
             +gulls+waders
             +ppt_mm+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
             +(1|Transect/Field), family=binomial, data=field)
summary(mod.3)

#model4: F+T+A
mod.4<-glmer(LBBG~crop3+veg.height+veg.cover2
             +season+day
             +ppt_mm+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
             +veg.height*season+veg.cover2*season
             +(1|Transect/Field), family=binomial, data=field)
summary(mod.4)

#model5: F+B
mod.5<-glmer(LBBG~crop3+veg.height+veg.cover2
             +gulls+waders
             +(1|Transect/Field), family=binomial, data=field)
summary(mod.5)

#model6: F+T
mod.6<-glmer(LBBG~crop3+veg.height+veg.cover2
             +season+day
             +veg.height*season+veg.cover2*season
             +(1|Transect/Field), family=binomial, data=field)
summary(mod.6)

#model7: F+A
mod.7<-glmer(LBBG~crop3+veg.height+veg.cover2
             +ppt_mm+cloud+air.temp+wind.speed+wind.dir.NS+wind.dir.EW
             +(1|Transect/Field), family=binomial, data=field)
summary(mod.7)

#model8: F
mod.8<-glmer(LBBG~crop3+veg.height+veg.cover2
             +(1|Transect/Field), family=binomial, data=field)
summary(mod.8)

#------Running the models----

#F:veg cover, veg height, crop type
#T: season, day
#B: gulls, waders
#A: ppt, cloud cover, wind speed, wind direction, air temp

#For biotic, interested in whether there is a correlation bw waders and gulls and LBBG field use

mod.wad<-glmer(LBBG ~ waders+(1|Transect/Field), family=binomial, data=field)
summary(mod.wad)

mod.int<-glmer(LBBG~(1|Transect/Field), family=binomial, data=field)
summary(mod.int)

anova(mod.int, mod.wad)

boxplot(waders~LBBG, xlab="LBBG presence/absence", ylab="Wader species count", 
        data=field)

mod.earth<-glmer(LBBG~earthworm+(1|Transect/Field), family=binomial, data=field)
summary(mod.earth)

veg.height + season
mod.height.season <- glmer(LBBG~veg.height + season+(1|Transect/Field), family=binomial, data=field)
mod.earth2<-glmer(LBBG~ veg.height + season + earthworm+(1|Transect/Field), family=binomial, data=field)
anova(mod.height.season, mod.earth2)


mod.height.season <- glmer(LBBG~veg.height + season+(1|Transect/Field), family=binomial, data=field)
mod.earth2<-glmer(LBBG~ veg.height + season + earthworm+(1|Transect/Field), family=binomial, data=field)
anova(mod.height.season, mod.earth2)


mod.int<-glmer(LBBG~(1|Transect/Field), family=binomial, data=field)
summary(mod.int)

anova(mod.int, mod.earth)

#there is some kind of positive (?) correlation with waders but it isn't significant

mod.gulls<-glmer(LBBG ~ gulls+ (1|Transect/Field), family=binomial, data=field)
summary(mod.gulls)
confint(mod.gulls)

mod.int<-glmer(LBBG~(1|Transect/Field), family=binomial, data=field)
summary(mod.int)

anova(mod.int, mod.gulls)

plot(gulls~LBBG, data=field)
boxplot(gulls~LBBG, xlab="LBBG presence/absence", ylab="Gull species count", 
        data=field)
boxplot(gulls~LBBG, xlab="LBBG presence/absence", ylab="Gull species count", 
        ylim=c(0,20), data=field)

#Now see whether crop type, veg cover, and/or veg height explain LBBG presence/absence best

plot(field$veg.height~field$season)
plot(field$veg.height~field$LBBG)
boxplot(veg.height~LBBG, xlab="LBBG", ylab="vegetation height (cm)",
        notch=TRUE, las=1, names=c("Absent", "Present"), data=field)

LBBG=field$LBBG
veg.height=field$veg.height
library(sciplot)
?bargraph.CI

bargraph.CI(LBBG, veg.height,ylim=c(0,50), 
            ylab="Vegetation height (cm)", xlab="LBBG absence/presence", 
            las=1, yaxs="r", data=field)


#or

library(ggplot2)
library(plyr)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

lbbgvegheight<-summarySE(field, measurevar="Veg.height", groupvars="LBBG")
lbbgvegheight


plot(veg.cover2~LBBG, data=field)
plot(crop3~LBBG, data=field)

plot(LBBG~crop3, data=field)
plot(LBBG~veg.cover2, data=field)

#plotting veg cover and crop type by LBBG presence/absence

str(field)
table(field$LBBG, field$veg.cover2)
vegcover<-matrix(c(28,15,22,215,8,3,2,7), ncol=4, byrow=TRUE)
vegcover
rownames(vegcover)<-c("Absence", "Presence")
colnames(vegcover)<-c("1", "2", "3", "4")
vegcover

barplot(vegcover, xlab="Vegetation cover class", ylab="Number of counts", 
        col=c("grey", "black"), legend=c("LBBG absence", "LBBG presence"), beside=TRUE)

#or

vegcover2<-matrix(c(8,3,2,7), ncol=4, byrow=TRUE)
vegcover2
colnames(vegcover2)<-c("1", "2", "3", "4")
vegcover2
barplot(vegcover2, xlab="Vegetation cover class", ylab="Number of LBBG presence counts", 
        ylim=c(0,10), col=c("grey"))

#or proportions
table(field$LBBG, field$veg.cover2)
8/36 
3/18
2/24
7/222
vegcover3<-matrix(c(22,17,8,3), ncol=4, byrow=TRUE)
vegcover3
colnames(vegcover3)<-c("1", "2", "3", "4")
vegcover3
barplot(vegcover3, xlab="Vegetation cover class", ylab="Proportion of LBBG presence counts (%)", 
        ylim=c(0,25), col=c("grey"))

table(field$LBBG, field$crop3)
crop<-matrix(c(39,85,105,22,29,9,5,3,2,1), ncol=5, byrow=TRUE)
crop
rownames(crop)<-c("Absence", "Presence")
colnames(crop)<-c("Roots", "Grass", "Cereal", "Ryegrass", "Other")
crop

barplot(crop, xlab="Crop type", ylab="Number of counts", ylim=c(0,120),
        col=c("grey", "black"), legend=c("LBBG absence", "LBBG presence"), beside=TRUE)

#or

crop2<-matrix(c(9,5,3,2,1), ncol=5, byrow=TRUE)
crop2
colnames(crop2)<-c("Roots", "Grass", "Cereal", "Ryegrass", "Other")
crop2

barplot(crop2, xlab="Crop type", ylab="Number of LBBG presence counts", 
        ylim=c(0,10), col=c("grey"))

#or by proportions
table(field$LBBG, field$crop3)
9/48
5/90
3/108
2/24
1/30
crop3<-matrix(c(19,6,3,8,3), ncol=5, byrow=TRUE)
crop3
colnames(crop3)<-c("Roots", "Grass", "Cereal", "Ryegrass", "Other")
crop3
barplot(crop3, xlab="Crop type", ylab="Proportion of LBBG presence counts (%)", 
        ylim=c(0,25), col=c("grey"))

##########models again
mod.field<-glmer(LBBG~crop3+veg.height+veg.cover2+
                 (1|Transect/Field), family=binomial, data=field)
summary(mod.field)

mod.field2<-glmer(LBBG~veg.height+veg.cover2+
                    (1|Transect/Field), family=binomial, data=field)
summary(mod.field2)

mod.field3<-glmer(LBBG~crop3+veg.height+
                    (1|Transect/Field), family=binomial, data=field)
summary(mod.field3)

mod.field4<-glmer(LBBG~veg.height+(1|Transect/Field), family=binomial, data=field)
summary(mod.field4)

mod.field5<-glmer(LBBG~veg.height+veg.cover2+veg.height*veg.cover2+
                    (1|Transect/Field), family=binomial, data=field)
summary(mod.field5)

anova(mod.field, mod.field2, mod.field3, mod.field4, mod.field5)

anova(mod.field, mod.field4)

anova(mod.field, mod.field5)

anova(mod.field4, mod.field5)

anova(mod.field3, mod.field5)

mod.int<-glmer(LBBG~(1|Transect/Field), family=binomial, data=field)
summary(mod.int)

anova(mod.field4, mod.int)

library("MuMIn")

r.squaredGLMM(mod.int)

r.squaredGLMM(mod.field4)
summary(mod.field4)

#veg height and cover are correlated, need to check whether parametric is ok and about having veg cover
#in classes as opposed to numeric. Can argue that only one should be used in a model (veg height)

#boxplot notch

boxplot(veg.height~veg.cover2, xlab="Vegetation cover class", ylab="Vegetation height (cm)"
        , data=field, notch=TRUE)

boxplot(veg.height~crop3, xlab="Crop type", ylab="Vegetation height (cm)",
        data=field, notch=TRUE)

#veg cover class 1 has a median sig diff from 2, 3, 4. classes 2 and 3 don't differ sig from each other. veg class 4 differs sig from 1, 2, 3. 
#this means that veg cover and height are not independent variables, they are positively correlated. However,
#the model with the lowest AIC value excludes vegetation cover and it is simpler so therefore vegetation height is interpreted as being the most explanatory variable.

# Veg.height vs cover and crop type correlation -----
str(field)
f <- (field$season == 2) & (field$day ==1) 

mid.veg.height <- field$veg.height[f]
mid.veg.cover <- as.numeric(field$veg.cover2[f])

cor.test(mid.veg.height,mid.veg.cover,
         method = "spearman")

boxplot(mid.veg.height~mid.veg.cover, xlab= "Vegetation cover class",
        ylab="Vegetation height (cm)", las = 1)

# veg.height vs crop type - middle period only ----
mid.crop.type <- field$crop3[f]

# One-way ANOVA
mod.crop.height <- aov(mid.veg.height ~ mid.crop.type)
summary(mod.crop.height)
plot(mod.crop.height)

aggregate(mid.veg.height ~ mid.crop.type, FUN = "mean")
aggregate(mid.veg.height ~ mid.crop.type, FUN = "sd")


mod.cov.height <- aov(mid.veg.height ~ mid.veg.cover)
boxplot((mid.veg.height ~ mid.veg.cover))
summary(mod.cov.height)
plot(mod.cov.height)

# Post-hoc test to determine which crop types differ in height
TukeyHSD(mod.crop.height)

boxplot(mid.veg.height ~ mid.crop.type, xlab="Crop type", ylab="Vegetation height (cm)"
        ,las=1, names=c("Roots", "Grass", "Cereals", "Ryegrass", "Other"))
        
?boxplot


#abiotic and time variables

mod.time<-glmer(LBBG~season+day+(1|Transect/Field), family=binomial, data=field)
summary(mod.time)

mod.time2<-glmer(LBBG~season+day+season*day+(1|Transect/Field), family=binomial, data=field)
summary(mod.time2)

mod.time3<-glmer(LBBG~season+(1|Transect/Field), family=binomial, data=field)
summary(mod.time3)

mod.time4<-glmer(LBBG~day+(1|Transect/Field), family=binomial, data=field)
summary(mod.time4)

anova(mod.time, mod.time2, mod.time3, mod.time4)

anova(mod.int, mod.time4)


#I think we can get a significance value for 'season' only (i.e. overall) by comparing the intercept only model:
 # mod.int <- LBBG ~ (1 | Transect/Field)
#To this model:
 # mod.sea <- LBBG ~ season + (1 | Transect/Field)
#Anova(mod.int, mod.sea)

mod.int<-glmer(LBBG~(1|Transect/Field), family=binomial, data=field)
summary(mod.int)

mod.time3<-glmer(LBBG~season+(1|Transect/Field), family=binomial, data=field)
summary(mod.time3)

anova(mod.int, mod.time3)

r.squaredGLMM(mod.time3)

#plotting this is a bit tricky
library(lattice)

str(field)
table(field$LBBG, field$season)
season2013<-matrix(c(84,97,99,16,3,1), ncol=3, byrow=TRUE)
season2013
rownames(season2013)<-c("Absence", "Presence")
colnames(season2013)<-c("Beginning", "Middle", "End")
season2013<-as.table(season2013)
season2013

barplot(season2013, xlab="Season", ylab="# of counts", ylim=c(0,100),
        col=c("grey", "black"), legend=c("LBBG absence", "LBBG presence"), beside=TRUE, names.arg=c("Beginning", "Middle", "End"))

#or

season2013.2<-matrix(c(16,3,1), ncol=3, byrow=TRUE)
season2013.2
colnames(season2013.2)<-c("Beginning", "Middle", "End")
season2013.2<-as.table(season2013.2)
season2013.2
barplot(season2013.2, xlab="Season", ylab="# of LBBG presence counts", ylim=c(0,20),
        col=c("grey"), las=1,
        names.arg=c("Beginning", "Middle", "End"))

#time of day plot
table(field$LBBG, field$day)
day2013<-matrix(c(11,9), ncol=2, byrow=TRUE)
day2013
colnames(day2013)<-c("Morning", "Evening")
day2013<-as.table(day2013)
barplot(day2013, xlab="Time of day", ylab="# of LBBG presence counts", ylim=c(0,15),
        col=c("grey"), las=1,
        names.arg=c("Morning", "Evening"))


plot(field$air.temp~field$season)
plot(field$ppt_mm~field$season)
plot(field$cloud~field$season)
plot(field$wind.speed~field$season)
plot(field$wind.dir~field$season)

#air temp, wind speed, wind dir, change with seasons?

# abiotic x season ----
# Tom: I'm not sure about this bit!
f2 <- field$day == 2
cor.test(as.numeric(field$season[f2]), field$air.temp[f2],
         method = "spearman")

boxplot(field$air.temp[f2]~as.numeric(field$season[f2]), xlab="Season", ylab="Air temperature (Celsius)",
        las=1, ylim=c(12,20),names=c("Beginning", "Middle", "End"))

cor.test(as.numeric(field$season[f2]), field$cloud[f2],
         method = "spearman")

boxplot(field$cloud[f2]~as.numeric(field$season[f2]), xlab="Season", ylab="Cloud cover (%)",
        las=1,ylim=c(0,50), names=c("Beginning", "Middle", "End"))

cor.test(as.numeric(field$season[f2]), field$wind.speed[f2],
         method="spearman")

boxplot(field$wind.speed[f2]~as.numeric(field$season[f2]), xlab="Season", ylab="Wind speed (m/s)",
        las=1, ylim=c(0,10), names=c("Beginning", "Middle", "End"))

cor.test(as.numeric(field$season[f2]), field$wind.dir.NS[f2],
         method="spearman")

boxplot(field$wind.dir.NS[f2]~as.numeric(field$season[f2]), xlab="Season", ylab="Wind direction North-South (radians)",
        las=1, ylim=c(-1,1), names=c("Beginning", "Middle", "End"))

cor.test(as.numeric(field$season[f2]), field$wind.dir.EW[f2],
         method="spearman")

boxplot(field$wind.dir.EW[f2]~as.numeric(field$season[f2]), xlab="Season", ylab="Wind direction East-West (radians)",
        las=1, ylim=c(-1,1), names=c("Beginning", "Middle", "End"))

cor.test(as.numeric(field$season[f2]), field$wind.NS[f2],
         method="spearman")

boxplot(field$wind.NS[f2]~as.numeric(field$season[f2]), xlab="Season", ylab="Wind vector North-South",
        las=1, ylim=c(-6,6), names=c("Beginning", "Middle", "End"))

cor.test(as.numeric(field$season[f2]), field$wind.EW[f2],
         method="spearman")

boxplot(field$wind.EW[f2]~as.numeric(field$season[f2]), xlab="Season", ylab="Wind vector East-West",
        las=1, names=c("Beginning", "Middle", "End"))

#models including season and other abiotic terms? seems confusing...

mod.time3<-glmer(LBBG~season+(1|Transect/Field), family=binomial, data=field)
summary(mod.time3)

mod.time.ab<-glmer(LBBG~season+air.temp+season*air.temp+(1|Transect/Field), family=binomial, data=field)
summary(mod.time.ab)

mod.time.ab2<-glmer(LBBG~season+cloud+season*cloud+(1|Transect/Field), family=binomial, data=field)
summary(mod.time.ab2)

mod.time.ab3<-glmer(LBBG~season+wind.speed+season*wind.speed+(1|Transect/Field), family=binomial, data=field)
summary(mod.time.ab3)

anova(mod.time3, mod.time.ab, mod.time.ab3)


#what happens when we put everything (field, time, associations) together?
mod.int<-glmer(LBBG~(1|Transect/Field), family=binomial, data=field)
summary(mod.int)
mod.field<-glmer(LBBG~crop3+veg.height+veg.cover2+
                   (1|Transect/Field), family=binomial, data=field)
summary(mod.field)
mod.field2<-glmer(LBBG~veg.height+veg.cover2+
                    (1|Transect/Field), family=binomial, data=field)
summary(mod.field2)
mod.field3<-glmer(LBBG~crop3+veg.height+
                    (1|Transect/Field), family=binomial, data=field)
summary(mod.field3)
mod.field4<-glmer(LBBG~veg.height+(1|Transect/Field), family=binomial, data=field)
summary(mod.field4)

mod.time<-glmer(LBBG~season+day+(1|Transect/Field), family=binomial, data=field)
summary(mod.time)
mod.time2<-glmer(LBBG~season+(1|Transect/Field), family=binomial, data=field)
summary(mod.time2)
mod.time3<-glmer(LBBG~day+(1|Transect/Field), family=binomial, data=field)
summary(mod.time3)

mod.field.time<-glmer(LBBG~veg.height+season+(1|Transect/Field), family=binomial, data=field)
summary(mod.field.time)

mod.gulls<-glmer(LBBG ~veg.height+season+ gulls+ (1|Transect/Field), family=binomial, data=field)
summary(mod.gulls)
mod.wad<-glmer(LBBG ~ veg.height+season+waders+(1|Transect/Field), family=binomial, data=field)
summary(mod.wad)
mod.earth<-glmer(LBBG~veg.height+season+earthworm+(1|Transect/Field), family=binomial, data=field)
summary(mod.earth)

library("MuMIn")
r2.mod.int <- r.squaredGLMM(mod.int)
r2.mod.field <- r.squaredGLMM(mod.field)
r2.mod.field2 <- r.squaredGLMM(mod.field2)
r2.mod.field3 <- r.squaredGLMM(mod.field3)
r2.mod.field4 <- r.squaredGLMM(mod.field4)
r2.mod.time <- r.squaredGLMM(mod.time)
r2.mod.time2 <- r.squaredGLMM(mod.time2)
r2.mod.time3 <- r.squaredGLMM(mod.time3)
r2.mod.field.time <- r.squaredGLMM(mod.field.time)
r2.mod.gulls <- r.squaredGLMM(mod.gulls)
r2.mod.wad <- r.squaredGLMM(mod.wad)
r2.mod.earth <- r.squaredGLMM(mod.earth)


r2.new.mod.field.time <- rsquared.glmm.new(mod.field.time)


# Testing for significance
anova(mod.field4,mod.int)
anova(mod.gulls,mod.field.time)
anova(mod.wad,mod.field.time)
anova(mod.earth,mod.field.time)


# veg.cover vs. LBBG
?aggregate
aggregate(field$LBBG~field$veg.cover2, FUN = "sum")


r2.mod.new.int <- rsquared.glmm.new(mod.int)
r2.mod.new.field <- rsquared.glmm.new(mod.field)
r2.mod.new.field2 <- rsquared.glmm.new(mod.field2)
r2.mod.new.field3 <- rsquared.glmm.new(mod.field3)
r2.mod.new.field4 <- rsquared.glmm.new(mod.field4)
r2.mod.new.time <- rsquared.glmm.new(mod.time)
r2.mod.new.time2 <- rsquared.glmm.new(mod.time2)
r2.mod.new.time3 <- rsquared.glmm.new(mod.time3)
r2.mod.new.field.time <- rsquared.glmm.new(mod.field.time)
r2.mod.new.gulls <- rsquared.glmm.new(mod.gulls)
r2.mod.new.wad <- rsquared.glmm.new(mod.wad)
r2.mod.new.earth <- rsquared.glmm.new(mod.earth)





aicc.val <- AICc(mod.int, mod.field, mod.field2, mod.field3, mod.field4,
     mod.time, mod.time2, mod.time3, 
     mod.field.time,
     mod.gulls, mod.wad, mod.earth)
aicc.val
str(aicc.val)

#to get deltaAICc values
aicc.val$AICc-AICc(mod.field.time)


boxplot(field$veg.height~field$season, xlab="Season", ylab="Vegetation height (cm)",
        las=1, notch=TRUE, names=c("Beginning", "Middle", "End"))
        
cor.test(as.numeric(field$season[f2]), field$veg.height[f2],
                 method = "spearman")

# Logistic plot thing ------
install.packages("popbio")
library(popbio)
logi.hist.plot(field$veg.height, field$LBBG, 
               boxp = FALSE, type = "hist", col = "gray",
               xlabel = "Vegetation height (cm)")
