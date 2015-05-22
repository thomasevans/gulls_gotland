#Import file 'trips.txt'. It has been filtered etc for more info see 'Foraging_decision_LBBG.R'
#first sort out the factors

str(trips)

trips$gotland_fac<-as.factor(trips$gotland_fac)
trips$month_fac<-as.factor(trips$month_fac)
trips$year_fac<-as.factor(trips$year_fac)

#####AM/PM thing####

If you have multiple date_times, use this code

# Make an example date-time object
x <- as.POSIXct(c("2014-05-02 12:45:00",
                  "2014-05-02 10:45:00",
                  "2014-05-02 19:45:00"), tz = "UTC")

# Extract the hour from the time
x.hour <- as.numeric(format(x, "%H"))

# Label as AM or PM
# Empty variable
am.pm <- NULL

# If before midday, label AM, else label PM
if(x < 12) am.pm <- "AM" else am.pm <- "PM"

# If you have a vector (list) of values, test this for all of these
am.pm.fun <- function(x){
  if(x < 12) am.pm <- "AM" else am.pm <- "PM"
  return(am.pm)
}

am.pm.thing <- sapply(x.hour, am.pm.fun)


####calculating time since sunrise####

# Time sinse sunrise
# (cos(??*Hours since sunrise/12))

# Make example vector of sunrise times
# Below are examples, not true sunrise times
sunrise_times <- as.POSIXct(c("2014-05-02 05:45:00",
                              "2014-05-06 05:38:00",
                              "2014-08-10 06:04:00"), tz = "UTC")

# Example trip departure times (NB sunrise time #1 above corresponds to trip departure time #1 below)
trip_dept_time <-   as.POSIXct(c("2014-05-02 03:37:00",
                                 "2014-05-06 11:54:00",
                                 "2014-08-10 18:54:00"), tz = "UTC")



# Calculate time sinse sunrise
x <- difftime(trip_dept_time,  sunrise_times, units = "hours")
hours.since.sunrise <- as.numeric(x)

# Calculate cos thing
time.since.sunrise.cos <-  (cos(pi*hours.since.sunrise/12))

#explanation of variables, also in meta#
#'time of day'is AM or PM (solar time, between local midnight and local midday is AM)
#'time since sunrise h' is what it sounds like, same day, if leave before sunrise then value is negative
#'time since sunrise/cos' 
#'tcdc_eatm_mean' is cloud cover, a mean of previous 24 hr (%)
# use 'prate_sfc_day_kg_m2'is the same as mm, its a total rainfall in previous 24 hr
# 'air_2m_mean_c' is in celsius, a mean from previous 24 hr

trips$ring_number<-as.factor(trips$ring_number)
trips$trip_id<-as.factor(trips$trip_id)
str(trips)
trips$sex<-trips$sex_tentative
trips$sex<-as.factor(trips$sex)
trips$ppt<-trips$prate_sfc_day_kg_m2
trips$cloud<-trips$tcdc_eatm_mean
trips$temp<-trips$air_2m_mean_c
trips$windNS<-trips$vwnd_10m_mean
trips$windEW<-trips$uwnd_10m_mean
trips$month<-as.factor(trips$month)
trips$year<-as.factor(trips$year)
trips$cloud_trans<-cloud_arcsine
trips$ppt_100<-trips$ppt*100

summary(trips$cloud_trans)

#delete 2 individuals with very few trips

f <- (trips$ring_number != 8114317) & (trips$ring_number != 8114320)
summary(f)
trips_f <- trips[f,]

library(lme4)

str(trips_f)
trips_f$month<-as.factor(trips_f$month)
trips_f$year<-as.factor(trips_f$year)

gotland_on <- rep(FALSE,length(trips_f$gotland_time_prop))
gotland_on[trips_f$gotland_time_prop > 0.2] <- TRUE
trips_f$gotland_on <- gotland_on

#trans.arcsine <- function(x){ asin(sign(x) * sqrt(abs(x/100))) } 
#gotland_arcsine <- trans.arcsine(trips$gotland_time_prop)
#hist(gotland_arcsine)
#trips$gotland_arcsine<-gotland_arcsine

#gotland_trans <- rep(FALSE,length(trips$gotland_arcsine))
#gotland_trans[trips$gotland_arcsine > 0.02] <- TRUE
#trips$gotland_trans<-gotland_trans

#models to test, approach 2

mod.1<- glmer(gotland_on~
          time_since_sunrise_cos
          +month
          +year
          +sex
          +ppt
          +cloud
          +temp
          +windNS
          +windEW
          +temp*month
          +windNS*windEW
          +(1|ring_number),family=binomial, data=trips_f) 
summary(mod.1)

#checking the sig of each variable with 3+ levels
mod.int<-glmer(gotland_on~(1|ring_number), family=binomial, data=trips_f)
summary(mod.int)

mod.month<-glmer(gotland_on~month+(1|ring_number), family=binomial, data=trips_f)
summary(mod.month)

mod.year<-glmer(gotland_on~year+(1|ring_number), family=binomial, data=trips_f)
summary(mod.year)

anova(mod.int, mod.year)
anova(mod.int, mod.month)

#standardizing the model???

library(arm)
stdz.model<-standardize(mod.1, standardize.y= FALSE)
summary(stdz.model)

library(MuMIn)
options(na.action="na.fail")
model.set<-dredge(stdz.model)
?dredge

summary(is.na(trips_f))



mod.2<-glmer(gotland_on~
            time_since_sunrise_cos
            +month
            +year
            +sex
            +ppt
            +cloud
            +temp
            +windNS
            +windEW
            +temp*month
            +(1|ring_number),family = binomial, data=trips_f) 
summary(mod.2)

mod.3<- glmer(gotland_on~
              time_since_sunrise_cos
              +month
              +year
              +ppt
              +cloud
              +temp
              +windNS
              +windEW
              +(1|ring_number),family=binomial, data=trips_f) 
summary(mod.3)


summary(gotland_on == trips$gotland_trans)

#transform cloud?
#there's a sig interaction bw temp and month which makes everything more significant

# Transform cloud (percentage) with arcsine transformation
trans.arcsine <- function(x){ asin(sign(x) * sqrt(abs(x/100))) } 
cloud_arcsine <- trans.arcsine(trips$cloud)
# See how the transformed cloud data looks:
hist(cloud_arcsine)




# Individuality -----

# SD for random effect
VarCorr(stdz.model9)

# Variance for random effect
print(VarCorr(stdz.model9), comp = "Variance")

# Variance and sd
as.data.frame(VarCorr(stdz.model9))



unclass(VarCorr(stdz.model9))


summary(stdz.model9)

# library("arm")

fixef(stdz.model9)

se.fixef(stdz.model9)

?se.fixef


se.ranef(stdz.model9)

plot(stdz.model9)


install.packages("rptR")


library("rptR")


install.packages("rptR", repos = "http://R-Forge.R-project.org", type = "source")
# ?install.packages

install.packages("VGAM")
install.packages("MCMCglmm")
install.packages("rptR", repos = "http://R-Forge.R-project.org", type = "source")



library("rptR")
?rpt.binomGLMM.multi

rpt.binomGLMM.multi(trips_f$gotland_on, )

gotland_yn <- trips_f$gotland_on*1

trips_f <- cbind(trips_f, gotland_yn)

rpt.thing <- rpt.binomGLMM.multi(gotland_yn, trips_f$ring_number, link=c("logit"))
rpt.thing.add <- rpt.binomGLMM.add(gotland_yn, trips_f$ring_number)



# summary(as.factor(gotland_yn))

rpt.thing
rpt.thing.add

may <- trips_f$month == "5"
# summary(may)
rpt.thing.may <- rpt.binomGLMM.multi(gotland_yn[may], trips_f$ring_number[may], link=c("logit"))
rpt.thing.may


june <- trips_f$month == "6"
summary(june)
rpt.thing.june <- rpt.binomGLMM.multi(gotland_yn[june], trips_f$ring_number[june], link=c("logit"))
rpt.thing.june


july <- trips_f$month == "7"
summary(july)
rpt.thing.july <- rpt.binomGLMM.multi(gotland_yn[july], trips_f$ring_number[july], link=c("logit"))
rpt.thing.july



y2013 <- trips_f$year == 2013
summary(y2013)
rpt.thing.2013 <- rpt.binomGLMM.multi(gotland_yn[y2013], trips_f$ring_number[y2013], link=c("logit"))
rpt.thing.2013




print(rpt.thing)







# vignette("rptR")

trips_f <- cbind(trips_f, gotland_yn)
# 
# 
# summary(stdz.model9)
# 
# 
# gotland_on ~ month + z.cloud + z.temp + z.ppt + z.time_since_sunrise_cos +  
#   year + z.temp * month + (1 | ring_number)
# 
# rpt.thing.adj <- rpt.adj (gotland_yn ~   
#                             year + month + (1 | ring_number), ring_number, trips_f,
#          datatype = "binomial",  
#          method = "GLMM.multi",  
#          link = "logit",
#          CI = 0.95, nboot = 100, npermut = 100) 
# 
# rpt.thing.adj <- rpt.adj (gotland_yn ~   (1 | ring_number), "ring_number", trips_f,
#                           datatype = "binomial",  
#                           method = "GLMM.multi") 
# 
# 
# data(Fledglings)
# Fledglings$sqrtFledge <- sqrt(Fledglings$Fledge)
# rpt.Fledge <- rpt.adj(sqrtFledge ~ Age + (1|MaleID), "MaleID", data=Fledglings, datatype="Gaussian", 
#                       method="REML", nboot=10, npermut=10)
# source("MyOwnAdjRpt.r")
# rpt.Fledge <- MyOwnAdjRpt(sqrtFledge ~ Age + (1|MaleID), "MaleID", data=Fledglings, datatype="Gaussian", 
#                       method="REML", nboot=10, npermut=10)
# 
# 
# rpt.adj
# 
# 
# 
# rpt.adj
# 
# 
# ?rpt.adj
