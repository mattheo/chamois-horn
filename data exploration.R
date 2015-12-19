library(mgcv)
library(psych)
library(VIF)

source("collinearity check.R")

load("db.RData")
attach(db)

# Getting to know the data

# female<-subset(db,sex==1)#1218
# male<-subset(db,sex==2)#1461
#The dataset represents 1218 female and 1461 male chamois



## horn length ####################

## differences in mean horn length between sexes
(horn_sex <- with(db, tapply(horn, f.sex, mean)))
diff(horn_sex) # difference = 22.4 mm


# change over the years
(horn_sexyear <- with(db, tapply(horn, list(year, f.sex), mean)))
apply(horn_sexyear, 2, summary)
apply(horn_sexyear, 2, sd)
# males have less variance in mean horn length over the years
boxplot(horn_sexyear, ylab="Hornlength [mm]", main="Mean Hornlength over years")



# Variance of horn size in sexes
with(db, tapply(horn, sex, sd))
# males have higher variance in general!

# graphical
boxplot(horn~f.sex, data = db, main= "Hornlength ",ylab= "length [mm]", xlab= "sex")

hist(horn[sex==1],breaks=40,freq=FALSE,col=rgb(1,0,0,0.1), main="Hornlength", xlab= "Length [mm]")
hist(horn[sex==2],breaks=40,freq=FALSE,add=TRUE,col=rgb(0,0,1,0.1))
lines(density(horn[sex==1]), col="red", lwd=2,lty =2)
lines(density(horn[sex==2]), col="blue", lwd=2, lty =1)
legend("topright",lwd=c(2,2), lty= c(2,1),col=c("red","blue"),legend=c("female", "male"))

#male and female horn length differs at an average of 22,4 mm. The boxplot of the hornlength distribution shows two outliers or extreme values respectively in both groups, which should be further investigated. The horn lengths are close to normally distributed



## weight ###################

## differences in weight between sexes
(weight_sex <- with(db, tapply(weight, f.sex, mean)))
diff(weight_sex) # difference = 0.34 kg
# difference in mean weight not as substantial as difference in mean horn length

# change over the years

boxplot(horn~year, main = "Hornlength", xlab="Year", ylab= "Hornlength")
# in different years the distribution of hornlength is different

(weight_sexyear <- with(db, tapply(weight, list(year, f.sex), mean)))
apply(weight_sexyear, 2, summary)
diff(apply(weight_sexyear, 2, range))
# as in horn length, males have smaller variance in mean weight over the years

#graphical
boxplot(weight~f.sex, data = db, ylab= "Weight [kg]", xlab= "sex")
# but males have a higher variance in weight for the individuals


hist(weight[sex==1],breaks=20,freq=FALSE,col=rgb(1,0,0,0.1), main="Weight", xlab= "Weight [kg]")
hist(weight[sex==2],breaks=20,freq=FALSE,add=TRUE,col=rgb(0,0,1,0.1))
lines(density(weight[sex==1]), col="red", lwd=2, lty = 2)
lines(density(weight[sex==2]), col="blue", lwd=2, lty=1)
legend("topright",lwd=c(2,2),lty= c(1,2),col=c("red","blue"),legend=c("female", "male"))

#The density distribution of the weight shows some irregularity in female chamois
plot(horn~weight,main= "Horn ~ Weight",xlab="Weight [kg]", ylab="Hornlength [mm]")
#The plot of horn length against weight shows some anomaly because of different scales of measurement. We analysed the scale detail  of the data.
tapply(weight, council_cod, unique)

# The Councils will be classified in thre classes according to the detalil of scale: 1 kg, 0.5kg or 0.1 kg


# many integer values and some extreme values
plot(horn~weight,main= "Horn ~ Weight",xlab="Weight [kg]", ylab="Hornlength [mm]")



# Problems with integers and floats in weight data
# selecting all weight data which are whole numbers
tol <- 1e-12
int_weight <- rep(0, nrow(db))

int_weight[sapply(db$weight, function(y) min(abs(c(y%%1, y%%1-1))) < tol)] <- 1

sum(int_weight) # whole numbers in weight
length(int_weight) - sum(int_weight) # floating point numbers

# are the integers and floating point weigths evenly distributed over the councils?
plot(with(db, tapply(int_weight, council_cod, function(y) sum(y)/length(y))), type="h", cex=1.4, main="weight: percentage whole numbers", xlab="council", ylab="")

plot(with(db, tapply(int_weight, area_cod, function(y) sum(y)/length(y))), type="h", cex=1.4, main="weight: percentage whole numbers", xlab="area", ylab="")



## spatial distribution of weight ###################
fm_spatial_weight <- gam(weight ~ s(x.council, y.council), data=db)
vis.gam(fm_spatial_weight, plot.type="contour", main="Weight")
# elevation, unweighted and interpolated!
fm_spatial_ele <- gam(q_media~s(x.council, y.council), data=db)
vis.gam(fm_spatial_ele, plot.type = "contour", main="Elevation", xlab="", ylab="")



## effect of weight on horn length ####################

flm1<-lm(horn~weight, data=db)
summary(flm1) # 11% of deviance explained; quite a lot for one predictor
plot(horn~weight, data=db)
abline(flm1)

# weight horn sex
mwhs<-gam(horn~s(weight)+f.sex+s(q_media),data=db)
summary(mwhs)
plot(mwhs)
vis.gam(mwhs,theta =-40, main ="Horn~Weight+Sex", zlab="hornlength",xlab ="female          male")
gam.check(mwhs)#model check, not great but accepable at this point. There are not all relevant variables included so far


## councils #####################

t(with(db, tapply(horn, council_cod, length)))
# council 15 has the most hunted anmimals
hist(db$council_cod, breaks=length(unique(db$council_cod)))

with(db, tapply(horn, list(council_cod, year), length))
# also consistently over the years, council 15 has the most kills



## Julian day ####################

range(Jday)#[1] 247 364
max(Jday) - min(Jday)#timespan 117 days

# The range of Julianday spans over a period of 117 days from day 247 to day 364

hist(Jday, main= "Julianday",xlab="day of the year")
#the data are well distributed



## effect of Julian day on weight ##################

fgam5 <- gam(weight ~ s(Jday, bs="cs"), data=db)
summary(fgam5)
plot(fgam5)
# apparantly the weight increases over the hunting period until ~ day 300 when it rapidly drops. This could  be explained that before October the animals find enough grass for feeding and grwoing, while later the winter has set on and they start to burn their body fat reserves.
# This on the other hand leads to the conclusion, that there is an interaction between weight and Jday

Julgam1<-gam(weight~s(Jday)+f.sex, data=db)
vis.gam(Julgam3,plot.type="persp",theta=45, main="Weight ~ Julianday + Sex",zlab="weight", xlab="female       male",ylab="Day of Hunting Season")




## effect of Julian day on horn length  male & female #########################

#Loess_horn_Julianday_male------------------------------------------------------------
fmLoess_Jday_m <- loess(male$horn~male$Jday,family="gaussian",span=0.8)
newday_m <- seq(from=min(male$Jday), to=max(male$Jday),by=1)
pred_Horn_m <- predict(fmLoess_Jday_m, newday_m, se=TRUE)
plot(horn~Jday, main ="Loess: Horn~Julianday",ylab="Hornlength [mm]",xlab="Day of the year",pch=1)
lines(pred_Horn_m$fit~newday_m,col="blue", lwd=2)


#Loess horn_Julianday_Female -------------------------------------------------------
fmLoess_Jday_f <- loess(female$horn~female$Jday,family="gaussian",span=0.8)
newday_f <- seq(from=min(female$Jday), to=max(female$Jday),by=1)
pred_Horn_f <- predict(fmLoess_Jday_f, newday_f, se=TRUE)
lines(pred_Horn_f$fit~newday_f,col="red", lwd=2, lty=2)

abline(h=140,col="darkgreen",lwd=2,lty=3)
abline(h=160,col="darkgreen",lwd=2,lty=3)
legend("topright",lwd=c(2,2,2),lty=c(1,2,3),col=c("blue","red","darkgreen"),legend=c("male","female", "fix value 140 mm & 160 mm"),cex=0.8)
# Loess regression shows decreasing hornlength at the beginning of the hunting season and increasing hornlength after day 275

Julgam4<-gam(horn~s(weight)+s(Jday),data=db)
vis.gam(Julgam4,theta=60,main="Hornlength ~ weight+Jday",zlab="Hornlength")

# Gam with Interaction
Julgam5<-gam(horn~s(weight,Jday),data=db)
vis.gam(Julgam5,theta=60,main="Hornlength ~ (weight, Jday)",zlab="Hornlength")
# the decrease could be effect of hunting selection but the effect has to be further invesigated including random effects of council.


#?? nochmal ausarbeiten??
fgam6 <- gamm(horn ~ s(Jday, bs="re") + s(Jday, weight, bs="re") + s(weight, bs="re") + f.sex, random=list(council_cod=~1, year=~1), data=db)
summary(fgam6$gam)
plot(fgam6$gam, page=1)
#vis.gam(fgam6)??????????
summary(fgam6$lme)


# less than 1 degree of freedom -> smoothing unnecessary -> line

# the effect seems to be minimal, also less than 1% deviance explained
# horns grow consistently over the season
# yet the random forest found Jday as second most important predictor on horn size. Whats going on?
flm1 <- lm(horn ~ Jday)
summary(flm1)
117*0.04 # over the hunting period, the horn is growing roughly about 5 mm
# that's less than 5%, but still something




# spatial distribution ###############

plot(db$x.council, db$y.council, col=db$area_cod, pch=20, cex=1.6, main = "Spatial Distribution", xlab="", ylab="")
legend("bottomright", legend=paste("Area", unique(db$area_cod)), col=unique(db$area_cod), pch=20)

#The spatial distribution of the data suggests spatial autocorrelation.This fact should be adressed in a model
# however, spatial autocorrelation might not be a problem after including x,y as predictors


# horn size compared to elevation at area level ################

with(db, tapply(horn, area_cod, mean)) # horn size per area
with(db, tapply(q_media, area_cod, mean)) # mean elevation per area
with(db, tapply(q_min, area_cod, mean)) # min elevation per area
with(db, tapply(q_max, area_cod, mean)) # max elevation per area


summary(with(db, tapply(horn, area_cod, mean)))
158.0197-137.3182 #20.7015
mean(horn) # 142.1995
sd(horn) #20.58488
mean(horn)/100*20.7015 #29.43743

#Animals hunted  at the lowest mean elevation area have the biggest mean horn length. Animals originating from the highest area have the smalest average horn length. There is a difference of 20.7015 mm between this groups. Thist span is 29% of the (overall) average horn length. The standard deviation of horn length is 20.6 mm

# visualization
# horn size, unweighted and interpolated
m1 <- gam(horn~s(x.council, y.council), data=db)
vis.gam(m1, plot.type = "contour", main="Horn length")

# distribution of councils by admin. area
with(db, points(x.council, y.council, col=area_cod, pch=20, cex=1.5))
with(db, legend("bottomright", legend=paste("Area", unique(area_cod)), col=unique(area_cod), pch=20))
# no data in south-east and north-west!

# elevation, unweighted and interpolated!
m2 <- gam(q_media~s(x.council, y.council), data=db)
vis.gam(m2, plot.type = "contour", main="Elevation", xlab="", ylab="")

with(db, points(x.council, y.council, col=area_cod, pch=20, cex=1.5))
with(db, legend("bottomright", legend=paste("Area", unique(area_cod)), col=unique(area_cod), pch=20))


#----Loess Weight elevation male------------------------------------

fmLoess_WE_m <- loess(male$weight~male$q_media,family="gaussian",span=0.75)
newEle_m <- seq(from=min(male$q_media), to=max(male$q_media),by=1)
pred_W_m <- predict(fmLoess_WE_m, newEle_m, se=TRUE)
plot(weight~q_media, main ="Loess Regression: Weight~Elevation",ylab="Weight [kg]",xlab="Elevation [m a.s.l.]",pch=1)
lines(pred_W_m$fit~newEle_m,col="blue", lwd=2)


#Loess Weight elevation Female -------------------------------------------------------


fmLoess_WE_f <- loess(female$weight~female$q_media,family="gaussian",span=0.75)
newEle_f <- seq(from=min(female$q_media), to=max(female$q_media),by=1)
pred_W_f <- predict(fmLoess_WE_f, newEle_f, se=TRUE)

lines(pred_W_f$fit~newEle_f,col="red", lwd=2, lty =2)
abline(h=15,lty=3,col="darkgreen",lwd=2)
legend("topright",lwd=c(2,2,2),lty=c(1,2,3),col=c("blue","red","darkgreen"),legend=c("male","female", "fix value 15 kg"),cex=0.8)

# LOESS Horn Elevation-----------------------------------



fmLoess_HE_m <- loess(male$horn~male$q_media,family="gaussian",span=0.75)
newEle_m <- seq(from=min(male$q_media), to=max(male$q_media),by=1)
pred_H_m <- predict(fmLoess_HE_m, newEle_m, se=TRUE)
plot(horn~q_media, main ="Loess Regression: Horn~Elevation",ylab="horn [mm]",xlab="Elevation [m a.s.l.]",pch=1)
lines(pred_H_m$fit~newEle_m,col="blue", lwd=2)
#Loess Horn Female---------------------------------

fmLoess_HE_f <- loess(female$horn~female$q_media,family="gaussian",span=0.75)
newEle_f <- seq(from=min(female$q_media), to=max(female$q_media),by=1)
pred_H_f <- predict(fmLoess_HE_f, newEle_f, se=TRUE)

lines(pred_H_f$fit~newEle_f,col="red", lwd=2,lty =2)
legend("topright",lwd=c(2,2),lty=c(1,2),col=c("blue","red"),legend=c("male","female"))


## effect of aspect on horn length ############
# use bs="cc" because asp is periodical (0 -> 360=0)
fgam2 <- gam(horn ~s(asp, bs="cc"), data=db)
summary(fgam2)
plot(fgam2)

fgam3 <- gam(horn ~s(asp, bs="cp"), data=db)
summary(fgam3)
plot(fgam3)


# problem: asp is not going from 0 to 360
summary(db$asp)
# but from 36 to 340 degrees
# -> the cycle is closed "too early"
fgam4 <- gam(horn ~ s(asp, bs="cs"), data=db, gamma=1.4)
summary(fgam4)
plot(fgam4)


fgam7 <- gam(horn ~ s(twinter.max1, k = 3, by=aspect), data=db)
gam.check(fgam7)
plot(fgam7, res=T, page=1, scale=F)
vis.gam(fgam7, theta=45)
summary(fgam7)

lm1 <- lm(horn ~ cos(asp))
summary(lm1)
# but this explaines least deviance and is has a worse p-value

# turn the aspect, so the gap is not a 0 <-> 360 °
newasp <- db$asp + 180
newasp[newasp > 360] <- newasp[newasp > 360] - 360
summary(newasp)

plot(gam(horn ~ s(newasp, bs="cc"), data=db))


## animal density###########################

with(db, tapply(density, list(year, area_cod), unique))
# we have an animal density per administrative area per year  which is then either confirmed in the next year or changed.




# Variance Inflation VIF ###############
preds <- c("Jday", "x.council", "y.council", "q_media", "q_max", "q_min","weight", "substrate", "density", "nao_w", "nao_a1", "nao_a2", "nao_d", "nao_j", "nao_f", "nao_m", "ndvi.maxincr1", "ndvi.summer1", "ndvi.slop2", "ndvi.maxincr2", "ndvi.summer2", "Perc.area.aperta", "ndvi.may1.new", "ndvi.may2.new", "snow_winter1", "Snow_cover_winter1", "r_spring1", "r_newsummer1", "r_autumn", "snow_winter2", "Snow_cover_winter2", "r_spring2", "r_newsummer2", "twinter.min1", "twinter.max1", "twinter.mean1", "tspring1.min", "tspring1.max", "tspring1.mean", "tsummer1.min", "tsummer1.max", "tsummer1.mean", "tautumn.min", "tautumn.max", "tautumn.mean", "twinter.min2", "twinter.max2", "twinter.mean2", "tspring2.min", "tspring2.max", "tspring2.mean", "tsummer2.min", "tsummer2.max", "tsummer2.mean", "category", "aspect", "f.sex", "f.substrate", "f.year", "f.council_cod", "log.ndvi.slop1", "log.ndvi.slop2", "q_range", "ndvi.m1m2.pc1", "ndvi.m1m2.pc2", "ndvi.m1m2s1s2.pc1", "ndvi.s1s2.pc1", "ndvi.m1s1.pc1", "ndvi.m2s2.pc1")

vif.sel <- with(db, vif(horn, db[, preds]))
