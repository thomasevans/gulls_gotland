#####GPS trips by LBBG 2011-2013 analysis re-do 2016-02-01 for ME revision#########

#read in 'trips_details_2016_01_29_detailed.csv', filtered by Tom as above
#rename to 'trips' so easier to handle

trips<-trips_details_2016_01_29_detailed

#take a look at the structure to make sure it makes sense
str(trips)

####calculating time since sunrise####

x <- difftime(as.POSIXct(as.character(trips$start_time), tz = "UTC"),
              as.POSIXct(as.character(trips$sunrise_date_time), tz = "UTC"),
              units = "hours")
hours.since.sunrise <- as.numeric(x)

str(hours.since.sunrise)

# Calculate cos thing
time.since.sunrise.cos <-  (cos(pi*hours.since.sunrise/12))

str(time.since.sunrise.cos)
trips$sunrise_prox<-time.since.sunrise.cos

#fix factors!

trips$year<-as.factor(trips$year)
trips$ring_number<-as.factor(trips$ring_number)
trips$stage <- factor(trips$stage,c("Incubation" ,"Chick_1" ,   "Chick_2"))
# levels(trips$stage) <- c("Incubation" ,"Chick_1" ,   "Chick_2")
str(trips$stage)
str(trips)

######analyzing foraging trips by proportion of foraging time spent on gotland#######

#checking what p_for_got looks like, both smallest and largest values ; != means 'not equal to'#
f<-trips$p_for_got != 0
head(sort(trips$p_for_got[f]))

f<-trips$p_for_got != 1
head(sort(trips$p_for_got[f], decreasing=TRUE))

1-0.9971014

#based on this, could go with epsilon as 0.0029

#now make 0 values 0.0029 and 1 values 1-0.0029 to avoid analyses problems later (see Thiele et al.)

trips.na<-trips[is.na(trips$p_for_got),]
trips<-trips[!is.na(trips$p_for_got),]

got_eps<-trips$p_for_got
got_eps[got_eps==0]<-0.0029
got_eps[got_eps==1]<-1-0.0029
summary(got_eps)
trips$got_eps<-got_eps

str(trips)

#make sure ppt is re-scaled due to small values (double-check the weather variables in this file!!!)
trips$ppt<-trips$ecwf_ppt*100
trips$cloud<-trips$tcdc_eatm_mean
trips$windNS<-trips$uwnd_10m_mean
trips$windEW<-trips$vwnd_10m_mean
trips$temp<-trips$air_2m_mean_c 
trips$sex<-trips$sex_tentative

#do some stuff, install/load required packages
#run a simple model with got_eps and specifying the link function as 'logit'
#run some diagnostics to see whether it makes any sense

library(lme4)
library(MuMIn)
library(arm)
library(HLMdiag)

HLMresid(mod.test)
compare_eb_ls(mod.test)
ggplot_qqnorm(mod.test)
group_qqnorm(mod.test)


mod.test<-glmer(got_eps~stage+year+sunrise_prox+ (1|ring_number), family=binomial(link='logit'), data=trips)

summary(mod.test)

mod.test.2<-glmer(p_for_got~stage+year+sunrise_prox+(1|ring_number), family=binomial(link='logit'), data=trips)

summary(mod.test.2)

resids2<-resid(mod.test.2)
plot(mod.test)
plot(mod.test.2)
qqmath(mod.test.2)
par(mfrow=c(1,2))
qqmath(mod.test)
qqmath(mod.test.2)

#run some diagnostics
# 1. histogram of raw obs
hist(trips$got_eps)

# 2. cumulative residuals against linear predictor
resids<-resid(mod.test)
plot(trips$got_eps[!is.na(trips$got_eps)],resids)

plot(mod.test)
library(lattice)

qqmath(mod.test)

####for real, running models#######

mod.1<-glmer(got_eps~(1|ring_number), family=binomial(link='logit'), data=trips)
mod.2<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+windNS+windEW+sex+temp*stage+windNS*windEW+(1|ring_number),family=binomial(link='logit'), data=trips) 
mod.3<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+windNS+windEW+sex+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.4<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+windNS+windEW+sex+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.5<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+windNS+windEW+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.6<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.7<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.8<-glmer(got_eps~stage+cloud+temp+ppt+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.9<-glmer(got_eps~stage+cloud+temp+ppt+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.10<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+year+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.11<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+temp*stage+(1|ring_number),family=binomial(link='logit'), data=trips)


#compare the models with ANOVA and AICc
anova(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8, mod.9, mod.10, mod.11)
aic.val <- AICc(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8, mod.9, mod.10, mod.11)
str(aic.val)
aic.val

#select model with lowest AICc, then take the diff bw it and all the others
aic.val$dAICc <- aic.val$AICc - aic.val$AICc[7]
aic.val[order(aic.val$dAICc),]

#to get coefficients of 'best' model, check out summary!
summary(mod.7)

#get the R2 values

r1<-r.squaredGLMM(mod.1)
r2<-r.squaredGLMM(mod.2)
r3<-r.squaredGLMM(mod.3)
r4<-r.squaredGLMM(mod.4)
r5<-r.squaredGLMM(mod.5)
r6<-r.squaredGLMM(mod.6)
r7<-r.squaredGLMM(mod.7)
r8<-r.squaredGLMM(mod.8)
r9<-r.squaredGLMM(mod.9)
r10<-r.squaredGLMM(mod.10)
r11<-r.squaredGLMM(mod.11)


r1
r2
r3
r4
r5
r6
r7
r8
r9
r10
r11


####summary stats------

summary(((trips$duration_s)/60/60))
mean(trips$duration_s)/60/60
sd(trips$duration_s)/60/60
hist((trips$duration_s)/60/60)
median(trips$duration_s)/60/60

View(trips)

summary(trips$dist_max)
mean(trips$dist_max)
sd(trips$dist_max)
hist(trips$dist_max)



