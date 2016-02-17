#GPS LBBG with pre-laying analysis

#read in file 'trips_details_2016_02_16_detailed_extended.csv', it has pre-laying stage data
#and weather data extracted for it

trips <- read.csv("trips_details_2016_02_16_detailed_extended.csv", header = TRUE)

trips<-trips_details_2016_02_16_detailed_extended

#take a look at the structure to make sure it makes sense
str(trips)

#make variables into factors!

trips$year<-as.factor(trips$year)
trips$ring_number<-as.factor(trips$ring_number)

# I think this was thep problem, it's 'Pre_laying', not 'Pre-laying'
levels(trips$stage)
trips$stage <- factor(trips$stage,c("Pre_laying", "Incubation" ,"Chick_1" ,   "Chick_2"))
summary(trips$stage)
# levels(trips$stage) <- c("Incubation" ,"Chick_1" ,   "Chick_2")
str(trips$stage)
str(trips)

#filter out 2011
trips<-trips[as.factor(trips$year)!= "2011",]
head(trips)
View(trips)

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

#take out NA's

trips.na<-trips[is.na(trips$p_for_got),]
trips<-trips[!is.na(trips$p_for_got),]

#proportion terrestrial foraging for each trip
got_eps<-trips$p_for_got
got_eps[got_eps==0]<-0.0029
got_eps[got_eps==1]<-1-0.0029
summary(got_eps)
trips$got_eps<-got_eps

str(trips)

#fix variable names so it's less confusing!
trips$ppt<-trips$ecwf_ppt*100
trips$cloud<-trips$tcdc_eatm_mean
trips$windNS<-trips$uwnd_10m_mean
trips$windEW<-trips$vwnd_10m_mean
trips$temp<-trips$air_2m_mean_c 
trips$sex<-trips$sex_tentative

#do some stuff, install/load required packages

library(lme4)
library(MuMIn)
library(arm)

#most competitive models, plus intercept

mod.1<-glmer(got_eps~(1|ring_number), family=binomial(link='logit'), data=trips)
mod.2<-glmer(got_eps~stage+cloud+temp+ppt+sunrise_prox+(1|ring_number), family=binomial(link='logit'), data=trips)
mod.3<-glmer(got_eps~cloud+temp+ppt+sunrise_prox+(1|ring_number), family=binomial(link='logit'), data=trips)
mod.4<-glmer(got_eps~stage+cloud+temp+sunrise_prox+(1|ring_number),family=binomial(link='logit'), data=trips)
mod.5<-glmer(got_eps~cloud+temp+sunrise_prox+(1|ring_number), family=binomial(link='logit'), data=trips)

#compare AICc values
aic.val <- AICc(mod.1, mod.2, mod.3, mod.4, mod.5)
str(aic.val)
aic.val

aic.val$dAICc <- aic.val$AICc - aic.val$AICc[2]
aic.val[order(aic.val$dAICc),]

#get the R2 values

r1<-r.squaredGLMM(mod.1)
r2<-r.squaredGLMM(mod.2)
r3<-r.squaredGLMM(mod.3)
r4<-r.squaredGLMM(mod.4)
r5<-r.squaredGLMM(mod.5)

r1
r2
r3
r4
r5

#standardize the best model
stdz.model2<-standardize(mod.2, standardize.y=FALSE)
summary(stdz.model2)

drop1(stdz.model2, test="Chi")

###plotting the model####

# First make sure to use the standardized model to allow for comparison
stdz.mod.2<-standardize(mod.2, standardize.y=FALSE)

# Check this looks sensible
summary(stdz.mod.2)

# Get confidence intervals for coeficients
stdz.mod.2.ci.Wald <- confint(stdz.mod.2, method="Wald")

# Need this package for plots:

library(lattice)

# I worked this out based on code here:
# http://www.ashander.info/posts/2015/04/D-RUG-mixed-effects-viz/
# Make a data.frame of the CI
ci_dat <-stdz.mod.2.ci.Wald
ci_dat <- cbind(ci_dat, mean=rowMeans(ci_dat))
ci_df <- data.frame(coef = row.names(ci_dat), ci_dat)
names(ci_df)[2:3] <- c('lwr', 'upr')

# View current coeficient names:
ci_df$coef

# Make a new vector of coeficient names (need to change these to what is
# sensible based on the model)
ci_df$coef_new <- c(NA, "Intercept", "Incubation", "Chick-rearing: early", "Chick-rearing: late",
                    "Cloud cover", "Temperature", "Rain",
                    "Sunrise proximity")

# If you want the coeficients displayed in a different order to the current
# Here we sort them in order of the coeficient value, from low to high
ci_df_sort <- ci_df[order(ci_df$mean),]
ci_df_sort$coef_new <- factor(ci_df_sort$coef_new, levels = unique(ci_df_sort$coef_new))

# Plot the figure and export to wmf
win.metafile("GPSlogodds_new.wmf", width = 7, height= 8)
lattice::dotplot(coef_new ~ mean, ci_df_sort, xlim = c(-5,5),                 
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("Effect (log-odds of terrestrial foraging)",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df_sort$lwr, y, ci_df_sort$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)
dev.off()
#or png
png('GPSlogodds2_new.png', width = 6, height = 7, units="in", res = 300)
lattice::dotplot(coef_new ~ mean, ci_df_sort, xlim = c(-5,5),                 
                 #                  cexl.lab = 1.5, cex.axis = 1.5,
                 xlab = list("Effect (log-odds of terrestrial foraging)",cex=1.3),
                 panel = function(x, y) {
                   panel.segments(ci_df_sort$lwr, y, ci_df_sort$upr, y, lwd =2)
                   panel.xyplot(x, y, pch=18, cex = 1.2, col = "black")
                   panel.abline(v=0, lty=2)
                 },scales=list(y=list(cex=1.2), x = list(cex = 1.2))
)
dev.off()




source("mer-utils.R")


kappa.mer(stdz.mod.2)
vif.mer(stdz.mod.2)
