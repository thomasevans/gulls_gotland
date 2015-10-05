#read in file 'LBBGfield2.xlsx', all variables, no NA, seems to work better with lme4.

# setwd("D:/Dropbox/LBBG_agriculture/R_gotland_gulls")

load("D:/Dropbox/LBBG_terrestrial_foraging/R_gotland_gulls/LBBG_field_issues2.RData")
str(field)

plot(field$cloud, field$air.temp, las = 1)


field<-LBBGfield2
field
View(field)
str(field)
head(field)

#for some reason crop has 10 lvls when it should only have 10
#so use crop2 instead

field$Transect<-as.factor(field$Transect)
field$crop2<-as.factor(field$crop2)
field$veg.cover2<-as.factor(field$veg.cover2)
field$day<-as.factor(field$day)
field$season<-as.factor(field$season)
field$wind.dir4<-as.factor(field$wind.dir4)

field2$crop3_fac <- (as.factor(field2$crop3))

field2$veg.cover2_ord <- as.ordered(field2$veg.cover2)

# summary(field$crop2)


str(field)

# hist(field$wind.NS)

#convert ppt to mm from m 
#air temp seems to be in celsius already

ppt_mm<-field$ppt*1000
hist(ppt_mm)
field2<-cbind(field, ppt_mm)


str(field2)
head(field2)

field2$LBBG2 <- as.logical(field2$LBBG)

#Construct 8 models to test
#F: crop type, veg height, veg cover
#B:birds (gulls and waders)
#T: time of day and season
#A: abiotic variables (ppt, cloud, air temp, wind speed and direction)

library(lme4)

#model1: F+B+T+A
mod.1<-glmer(LBBG~crop3_fac+veg.height+veg.cover
               +gulls+waders
               +season+day
               +ppt_mm+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
               +veg.height*season+veg.cover*season
               +(1|Transect/Field), family=binomial, data=field2)
#                +(Field|Transect), family=binomial, data=field2)

summary(mod.1)
anova(mod.1)



# str(field2$waders)
# Waders ----
mod.waders<-glmer(LBBG ~ waders
             +(1|Transect/Field), family=binomial, data=field2)

summary(mod.waders)

str(field2$Field)
str(field2$Transect)
str(field2$waders)
str(field2$LBBG)
plot(field2$waders)

plot(mod.waders)

# Compute CI (fails!)
confint(mod.waders)


# gulls -----
mod.gulls<-glmer(LBBG ~ gulls
                  +(1|Transect/Field), family=binomial, data=field2)

str(field2$gulls)

summary(mod.gulls)
confint(mod.gulls)

plot(mod.gulls)
plot(jitter(field2$LBBG, amount = 0.1)  ~ field2$gulls)



?jitter




# hist(field2$cloud)

plot(mod.1)

# Compute CI (fails!)
confint(mod.1)
# warnings()


summary(field2$Transect)



#I get the Error: (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate

#model2:F+B+T
mod.2 <- glmer(LBBG~crop2+veg.height+veg.cover
             +gulls+waders
             +season
#              +day
#              +veg.height*season+veg.cover*season
             +(1|Transect/Field),
             family=binomial(link = logit),
              data=field2)
summary(mod.2)
# ?glmer
summary(field2$day)

#Error: (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate

#model3:F+B+A <--no time, not sure how meaningful this is, time is important to look at!
mod.3<-glmer(LBBG~crop2+veg.height+veg.cover
             +gulls+waders
             +ppt_mm+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
             +(1|Transect/Field), family=binomial, data=field2)
summary(mod.3)

#model4: F+T+A
mod.4<-glmer(LBBG~crop2+veg.height+veg.cover
             +season+day
             +ppt_mm+cloud+air.temp+wind.speed+ wind.dir.NS+wind.dir.EW
#              +veg.height*season+veg.cover*season
             +(1|Transect/Field), family=binomial, data=field2)
summary(mod.4)

#Error: (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate

#model5: F+B
mod.5<-glmer(LBBG~crop2+veg.height+veg.cover
             +gulls+waders
             +(1|Transect/Field), family=binomial, data=field2)
summary(mod.5)

#model6: F+T
mod.6<-glmer(LBBG~crop2+veg.height+veg.cover
             +season+day
             +veg.height*season+veg.cover*season
             +(1|Transect/Field), family=binomial, data=field2)
summary(mod.6)

#Error: (maxstephalfit) PIRLS step-halvings failed to reduce deviance in pwrssUpdate

#model7: F+A
mod.7<-glmer(LBBG~crop2+veg.height+veg.cover
             +ppt_mm+cloud+air.temp+wind.speed+wind.dir.NS+wind.dir.EW
             +(1|Transect/Field), family=binomial, data=field2)
summary(mod.7)


#model8: F
mod.8<-glmer(LBBG~crop3_fac +veg.height+veg.cover2_ord
             +(1|Transect/Field), family = binomial, data = field2)
summary(mod.8)
drop1(mod.8, test = "Chisq")



mod.8_int <-glmer(LBBG~crop3+veg.height+veg.cover
                  +(1|Transect/Field), family=binomial, data=field2)
summary(mod.8_int)
----
getwd()
setwd('C:/Users/Natalie/Desktop')
getwd()
save(field, file = "LBBG_field_issues2.RData")


# ?("MuMIn")

plot(field2$veg.height, field2$LBBG)
plot(field2$veg.cover2, field2$LBBG)


library("MuMIn")


mod.height <-glmer(LBBG~veg.height
                  +(1|Transect/Field),
                  family = binomial, data = field2)
summary(mod.height)

mod.cover <-glmer(LBBG~veg.cover2
                   +(1|Transect/Field),
                   family = binomial, data = field2)
summary(mod.cover)
summary(field2$veg.cover2)


# kappa()




LBBG.new <- as.logical(field2$LBBG)

field2 <- cbind(field2,LBBG.new)

library(reshape2)

aggregate( LBBG.new ~ veg.cover2,
          data = field2,
          FUN = length)
?aggregate




# Step-wise ----
mod.8_int <-glmer(LBBG~crop3+veg.height+veg.cover2
                  +(1|Transect/Field), family=binomial, data=field2)
summary(mod.8_int)

drop1(mod.8_int, test = "Chisq")
# ?drop1
test <- step(mod.8_int)

coef(mod.8_int)


library(Hmisc)
cor.test(field2$veg.height, as.numeric(field2$veg.cover2),
         method = "spearman")
?cor.test

field2$crop3 <- as.factor(field2$crop3)

anova(mod.8_int, test="Chisq")

anova(mod.8_int)
summary(mod.8_int)

boxplot(field2$veg.height~field2$veg.cover2,
        notch = TRUE)
?plot


setwd("D:/Dropbox/R_projects/gulls_gotland")

# Logistic plot thing ------
# install.packages("popbio")
source("logi.hist.plot.edit.fun.R")

library(popbio)
logi.hist.plot(field2$veg.height, field2$LBBG, 
               boxp = FALSE, type = "hist", col = "gray",
               xlabel = "Vegetation height (cm)")
logi.hist.plot.edit(field2$veg.height, field2$LBBG, 
                    boxp = FALSE, type = "hist", col = "gray",
                    xlabel = "Vegetation height (cm)",
                    line.fit = TRUE, rug = FALSE)


# Fit out own line:
x.new <- seq(min(field2$veg.height), max(field2$veg.height), len = 100)
y.new <- predict(mod3, data.frame(independ = x.new), 
                 type = "response")
lines(x.new, y.new, lwd = lwd.cur, lty = lty.cur, col = col.cur)




source("logi.hist.plot.edit.fun.R")



mod.veg.height <-glmer(LBBG~veg.height
                  +(1|Transect/Field), family=binomial, data=field2)

summary(mod.veg.height)



# PLOT FIGURE TO ILLUSTATE LOGISTIC MODEL --------
# To draw out own curve do the following
# Taking a tip from: http://stats.stackexchange.com/a/28650/35112
# Install this package for function 'invlogit'
# install.packages("arm")
library(arm)
model.coefs <- fixef(mod.veg.height)




win.metafile(filename = "veg_height_log_model.wmf", width = 5, height = 4, pointsize = 12)

logi.hist.plot.edit(field2$veg.height, field2$LBBG, 
               boxp = FALSE, type = "hist", col = "gray",
               xlabel = "Vegetation height (cm)",
               ylabel = expression(Probability~of~italic(L.~fuscus)~presence),
               ylabel2 = "N fields",
               line.fit = FALSE)
curve(invlogit( cbind(1, x) %*% model.coefs ), add = TRUE,
       lwd = 3, lty = 2)

dev.off()

summary(mod.veg.height)





# Veg.height vs cover correlation -----
str(field2)
f <- (field2$season == 2) & (field2$day ==1) 

mid.veg.height <- field2$veg.height[f]
mid.veg.cover <- as.numeric(field2$veg.cover2[f])

cor.test(mid.veg.height,mid.veg.cover,
         method = "spearman"
         )
boxplot(mid.veg.height~mid.veg.cover)




# veg.height vs crop type - middle period only ----
mid.crop.type <- field2$crop3[f]

# One-way ANOVA
mod.crop.height <- aov(mid.veg.height ~ as.factor(mid.crop.type))
summary(mod.crop.height)
plot(mod.crop.height)

# Post-hoc test to determine which crop types differ in height
TukeyHSD(mod.crop.height)

boxplot((mid.veg.height ~ mid.crop.type))



# abiotic x season ----
# Tom: I'm not sure about this bit!
f2 <- field2$day == 2
cor.test(as.numeric(field2$season[f2]), field2$air.temp[f2],
         method = "spearman"
)

plot(field2$air.temp[f2]~as.numeric(field2$season[f2]))



cor.test(as.numeric(field2$season[f2]), field2$cloud[f2],
         method = "spearman"
)

plot(field2$cloud[f2]~as.numeric(field2$season[f2]))



# Season ----




# mod.field4 ----

mod.field4 <- glmer(LBBG ~ veg.height + (1 | Transect/Field)
              , family=binomial, data = field2)
str(field2)
summary(mod.field4)
r.squaredGLMM(mod.field4)

mod.field.int <- glmer(LBBG ~ (1 | Transect/Field)
                    , family=binomial, data=field2)



anova(mod.field.int, mod.field4)

library("MuMIn")

r.squaredGLMM(mod.field4)
?r.squaredGLMM
plot(mod.field4)
summary(mod.field4)



# Waders -----

mod.wad <- glmer(LBBG ~ waders + (1 | Transect/Field)
                    , family=binomial, data=field2)

mod.wad.int <- glmer(LBBG ~ (1 | Transect/Field)
                       , family=binomial, data=field2)

anova(mod.wad.int, mod.wad)


model.coefs <- fixef(mod.wad)

logi.hist.plot.edit(field2$waders, field2$LBBG, 
                    boxp = FALSE, type = "hist", col = "gray",
                    xlabel = "N waders",
                    line.fit = FALSE)
curve(invlogit( cbind(1, x) %*% model.coefs ), add = TRUE,
      lwd = 2)

plot(mod.wad)



# Gulls -----


mod.gul <- glmer(LBBG ~ gulls + (1 | Transect/Field)
                 , family=binomial, data=field2)

mod.gul.int <- glmer(LBBG ~ (1 | Transect/Field)
                     , family=binomial, data=field2)

anova(mod.gul.int, mod.gul)


model.coefs <- fixef(mod.gul)

logi.hist.plot.edit(field2$gulls, field2$LBBG, 
                    boxp = TRUE, type = "hist", col = "gray",
                    xlabel = "N gulls",
                    line.fit = FALSE)
curve(invlogit( cbind(1, x) %*% model.coefs ), add = TRUE,
      lwd = 2)

plot(mod.wad)


# Season -----


mod.seas <- glmer(LBBG ~ season + (1 | Transect/Field)
                    , family=binomial, data=field2)

mod.int <- glmer(LBBG ~ (1 | Transect/Field)
                       , family=binomial, data=field2)



anova(mod.int, mod.seas)

library("MuMIn")

r.squaredGLMM(mod.seas)
?r.squaredGLMM
plot(mod.field4)
summary(mod.field4)



# Season and veg.height -------
mod.sea.veg <- glmer(LBBG ~ season + veg.height + (1 | Transect/Field)
                  , family=binomial, data=field2)

r.squaredGLMM(mod.sea.veg)

plot(mod.sea.veg)
summary(mod.sea.veg)


r.squaredGLMM(mod.int)



plot(LBBG ~ season + veg.height, data = field2)


library("rptR")
cite("rptR")






# NEW - Vegetation only

# Check variable structure:
str(field2$crop3_fac)
str(field2$veg.height)
str(field2$veg.cover2)

mod.veg.1 <-glmer(LBBG~crop3_fac +
                  + veg.height*veg.cover2
             +(1|Transect/Field), family = binomial, data = field2)


mod.veg.2 <-glmer(LBBG ~ crop3_fac +
                    + veg.height + veg.cover2
                  +(1|Transect/Field), family = binomial, data = field2)

mod.veg.3 <-glmer(LBBG ~
                    veg.height + crop3_fac
                  +(1|Transect/Field), family = binomial, data = field2)

mod.veg.4 <-glmer(LBBG ~ crop3_fac +
                    + veg.height 
                  +(1|Transect/Field), family = binomial, data = field2)

mod.veg.5 <-glmer(LBBG ~ crop3_fac 
                    + veg.cover2 
                  +(1|Transect/Field), family = binomial, data = field2)

mod.veg.6 <-glmer(LBBG ~ veg.cover2 +
                    + veg.height
                  +(1|Transect/Field), family = binomial, data = field2)

mod.veg.7 <-glmer(LBBG ~ crop3_fac 
                    +(1|Transect/Field), family = binomial, data = field2)

mod.veg.8 <-glmer(LBBG ~ veg.height
                  +(1|Transect/Field), family = binomial, data = field2)

mod.veg.9 <-glmer(LBBG ~ veg.cover2
                  +(1|Transect/Field), family = binomial, data = field2)

mod.veg.null <-glmer(LBBG ~ (1|Transect/Field), family = binomial, data = field2)


anova(mod.veg.1,mod.veg.2,mod.veg.3,mod.veg.4,
      mod.veg.5,mod.veg.6,mod.veg.7,
      mod.veg.8,mod.veg.9,mod.veg.null)
AICc(mod.veg.1,mod.veg.2,mod.veg.3,mod.veg.4,
     mod.veg.5,mod.veg.6,mod.veg.7,
     mod.veg.8,mod.veg.9,mod.veg.null)

summary(mod.veg.8)
drop1(mod.veg.8, test = "Chisq")
r.squaredGLMM(mod.veg.8)





# Vegetation height by season - box plot with notch true ------
# only include morning (i.e. one point per season)
fs <- field2$day == 1
names_seasons <- field2$season
levels(names_seasons) <- c("Beginning", "Middle", "End")
win.metafile(filename = "veg_height_season_boxplot.wmf", width = 5, height = 5, pointsize = 12)
boxplot(field2$veg.height[fs]~names_seasons[fs], notch = TRUE,
        xlab = "Observation period",
        ylab = "Vegetation height (cm)",
        cex.lab = 1.2, cex.axis = 1.1, las = 1
        )
dev.off()



# LBBG presence counts -----
boxplot(field2$LBBG ~ field2$season + field2$day)
