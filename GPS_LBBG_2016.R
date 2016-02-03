#####GPS trips by LBBG 2011-2013 analysis re-do 2016-02-01 for ME revision#########

#read in 'trips_details_2016_01_29.csv', filtered by Tom as above
#rename to 'trips' so easier to handle

trips<-trips_details_2016_01_29

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

######analyzing foraging trips by proportion of foraging time spent on gotland#######

#checking what p_for_got looks like, both smallest and largest values ; != means 'not equal to'#
f<-trips$p_for_got != 0
head(sort(trips$p_for_got[f]))

f<-trips$p_for_got != 1
head(sort(trips$p_for_got[f], decreasing=TRUE))

1-0.9971014

#based on this, could go with epsilon as 0.0029

#now make 0 values 0.0029 and 1 values 1-0.0029 to avoid analyses problems later (see Thiele et al.)

got_eps<-trips$p_for_got
got_eps[got_eps==0]<-0.0029
got_eps[got_eps==1]<-1-0.0029

summary(got_eps)
trips.na<-trips[is.na(trips$p_for_got),]

trips$got_eps<-got_eps

#make sure ppt is re-scaled due to small values (double-check the weather variables in this file!!!)

trips$ppt_100<-trips$ppt*100

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





