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

trips<-foraging_trip_info_filtered_may2015

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
            +(1|ring_number),family=binomial, data=trips_f) 
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

#####the table method or 'problem of too many models' method####
mod.1<- glmer(gotland_on~ month+cloud+temp+ppt+ time_since_sunrise_cos +year +windNS+windEW+sex+temp*month+windNS*windEW+(1|ring_number),family=binomial, data=trips_f)
mod.2<- glmer(gotland_on~ month+cloud+temp+ppt+ time_since_sunrise_cos +year +windNS+windEW+sex+temp*month+( 1|ring_number),family=binomial, data=trips_f)
mod.3<- glmer(gotland_on~ month+cloud+temp+ppt+ time_since_sunrise_cos +year +windNS+windEW+sex+( 1|ring_number),family=binomial, data=trips_f)
mod.4<- glmer(gotland_on~ month+cloud+temp+ppt+ time_since_sunrise_cos +year +windNS+windEW+( 1|ring_number),family=binomial, data=trips_f)
mod.5<- glmer(gotland_on~ month+cloud+temp+ppt+ time_since_sunrise_cos +year +( 1|ring_number),family=binomial, data=trips_f)
mod.6<- glmer(gotland_on~ month+cloud+temp+ppt+ time_since_sunrise_cos +( 1|ring_number),family=binomial, data=trips_f)
mod.7<- glmer(gotland_on~ month+cloud+temp+ppt+ ( 1|ring_number),family=binomial, data=trips_f)
mod.8<- glmer(gotland_on~ month+cloud+temp+ppt+temp*month+ ( 1|ring_number),family=binomial, data=trips_f)
mod.9 <-glmer(gotland_on~ month+cloud+temp+ppt+ time_since_sunrise_cos +year +temp*month+( 1|ring_number),family=binomial, data=trips_f)
mod.10<-glmer(gotland_on~ month+cloud+temp+ppt+ time_since_sunrise_cos +temp*month+( 1|ring_number),family=binomial, data=trips_f)
mod.int<-glmer(gotland_on~( 1|ring_number),family=binomial, data=trips_f)

anova(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8, mod.9, mod.10, mod.int)

aic.val <- AICc(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8, mod.9, mod.10, mod.int)
str(aic.val)
aic.val$dAICc <- aic.val$AICc - aic.val$AICc[9]
aic.val

r1<-r.squaredGLMM(mod.1)
r2<-r.squaredGLMM(mod.2)
r3<-r.squaredGLMM(mod.3)
r4<-r.squaredGLMM(mod.4)
r5<-r.squaredGLMM(mod.5)
r6<-r.squaredGLMM(mod.6)
r7<-r.squaredGLMM(mod.7)
r8<-r.squaredGLMM(mod.8)
r9<-r.squaredGLMM(mod.9)
r9.0<-r.squaredGLMM(stdz.model9)
r10<-r.squaredGLMM(mod.10)
rint<-r.squaredGLMM(mod.int)

r9
r9.0
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
rint

drop1(stdz.model9, test="Chi")

stdz.model9<-standardize(mod.9, standardize.y=FALSE)
summary(stdz.model9)

library(sjPlot)
library(Rcpp)
sjp.glmer(stdz.model9)
install.packages("Rcpp", type = "source")

######plotting month v season#####

table(trips_f$gotland_on, trips_f$year, trips_f$month)
monthyear<-matrix(c(61.8, 26.7, 17.2, 55.3,30.7, 13.2, 87.7, 21.2, 5.1), ncol=3, byrow=TRUE)
rownames(monthyear)<-c("2011", "2012", "2013")
colnames(monthyear)<-c("May", "June", "July")
monthyear<-as.table(monthyear)
monthyear

barplot(monthyear, xlab="Month", ylab="Gotland trips as % of total trips", ylim=c(0,100), 
        col=c("gray1", "gray47", "gray87"), beside=TRUE,
        names.arg=c("May", "June", "July"), axes=FALSE, cex.names=0.9)
axis(1, at = c(2.5,6.5,10.5), labels = FALSE)
axis(2, at=seq(0,100,10), cex.axis=0.9, las=1)
legend("topright", c("2011", "2012", "2013"), cex=0.8, col=c("gray1", "gray47", "gray87"), 
       fill=c("gray1", "gray47", "gray87"),
       bty="n")

#####plotting abiotics#####
library(sciplot)
str(trips_f)

plot(trips_f$gotland_on~ trips_f$ppt_100)

bargraph.CI(gotland_on, ppt_100,ylim=c(0,150),xlim=c(0,2.5),
            ylab="Precipitation (mm)", xlab="Gotland trips", 
            las=1, space= 0.4, width= 0.8, names.arg=c("False", "True"), data=trips_f)

plot(trips_f$gotland_on~trips_f$cloud)
bargraph.CI(gotland_on, cloud,ylim=c(0,50),xlim=c(0,2.5),
            ylab="Cloud cover (%)", xlab="Gotland trips", 
            las=1, space= 0.4, width= 0.8, names.arg=c("False", "True"), data=trips_f)

plot(trips_f$gotland_on~trips_f$temp)
bargraph.CI(gotland_on, temp,ylim=c(0,16),xlim=c(0,2.5),
            ylab="Air temperature (Celsius)", xlab="Gotland trips", 
            las=1, space= 0.4, width= 0.8, names.arg=c("False", "True"), data=trips_f)



LBBG=field$LBBG
veg.height=field$veg.height
library(sciplot)
?bargraph.CI
?barplot
bargraph.CI(LBBG, veg.height,ylim=c(0,50), 
            ylab="Vegetation height (cm)", xlab="LBBG absence/presence", 
            las=1, yaxs="r", data=field)


