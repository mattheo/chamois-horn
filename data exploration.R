library(mgcv)


load("db.RData")
attach(db)

# Getting to know the data
class(sex) # integer
f.sex<-as.factor(sex)# transformation of sex data to class factor
levels(f.sex) <- c("female", "male") # relevel sex



## horn length ####################

## differences in mean horn length between sexes
(horn_sex <- with(db, tapply(horn, f.sex, mean)))
diff(horn_sex) # difference = 22.4 mm

# change over the years
(horn_sexyear <- with(db, tapply(horn, list(year, f.sex), mean)))
apply(horn_sexyear, 2, summary)
(apply(horn_sexyear, 2, sd))
# males have less variance in mean horn length over the years
boxplot(horn_sexyear, ylab="Hornlength [mm]", main="Mean Hornlength over years")


# Variance of horn size in sexes
with(db, tapply(horn, sex, sd))
# males have higher variance in general!

# graphical
boxplot(horn~f.sex, data = db, main= "Hornlength ",ylab= "length [mm]", xlab= "sex")

hist(horn[sex==1],breaks=40,freq=FALSE,col=rgb(1,0,0,0.1), main="Hornlength", xlab= "Length [mm]")
hist(horn[sex==2],breaks=40,freq=FALSE,add=TRUE,col=rgb(0,0,1,0.1))
lines(density(horn[sex==1]), col="red", lwd=2)
lines(density(horn[sex==2]), col="blue", lwd=2)
legend("topright",lwd=c(2,2),col=c("red","blue"),legend=c("female", "male"))

#male and female horn length differs at an average of 22,4 mm. The boxplot of the hornlength distribution shows two outliers or extreme values respectively in both groups, which should be further investigated. The horn lengths are close to normally distributed

# plots of horn lengths for each individual
plot (horn[sex==2], main = "Hornlength male", ylab = "Length [mm]")
plot (horn[sex==1],main = "Hornlength female", ylab = "Length [mm]")



## weight ###################

## differences in weight between sexes
(weight_sex <- with(db, tapply(weight, f.sex, mean)))
diff(weight_sex) # difference = 0.34 kg
# difference in mean weight not as substantial as difference in mean horn length

# change over the years
(weight_sexyear <- with(db, tapply(weight, list(year, f.sex), mean)))
apply(weight_sexyear, 2, summary)
diff(apply(weight_sexyear, 2, range))
# as in horn length, males have smaller variance in mean weight over the years

#graphical
boxplot(weight~f.sex, data = db, ylab= "Weight [kg]", xlab= "sex")
# but males have a higher variance in weight for the individuals
#In male chamois are again two outliers wereas there are none in female

hist(weight[sex==1],breaks=20,freq=FALSE,col=rgb(1,0,0,0.1), main="Weight", xlab= "Weight [kg]")
hist(weight[sex==2],breaks=20,freq=FALSE,add=TRUE,col=rgb(0,0,1,0.1))
lines(density(weight[sex==1]), col="red", lwd=2)
lines(density(weight[sex==2]), col="blue", lwd=2)
legend("topright",lwd=c(2,2),col=c("red","blue"),legend=c("female", "male"))

#the density distribution of the weight dara shows some irregularity in female chamois
# how can we tackle this ????
#The plot of horn length against weight shows some anomaly because of many integer values and some extreme values
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

fgam1<-gam(horn~s(weight, bs="cs"),data=db)
summary(fgam1) # 11% of deviance explained; quite a lot for one predictor
plot(horn~weight, data=db)
plot(fgam1)



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


## effect of Julian day on horn length #########################

fgam6 <- gamm(horn ~ s(Jday, bs="re") + s(Jday, weight, bs="re") + s(weight, bs="re") + f.sex, random=list(council_cod=~1, year=~1), data=db)
summary(fgam6$gam)
plot(fgam6$gam, page=1)
vis.gam(fgam6)
summary(fgam6$lme)


# less than 1 degree of freedom -> smoothing unnecessary -> line

# the effect seems to be minimal, also less than 1% deviance explained
# horns grow consistently over the season
# yet the random forest found Jday as second most important predictor on horn size. Whats going on?
flm1 <- lm(horn ~ Jday)
summary(flm1)
117*0.04 # over the hunting period, the horn is growing roughly about 5 mm
# that's less than 5%, but still something

# loess smoothing model
fmLoess_Jday <- loess(horn~Jday,family="gaussian",span=0.8)

# plotting the loess
newday <- seq(from=min(Jday), to=max(Jday),by=1)
pred_Horn <- predict(fmLoess_Jday, newday, se=TRUE)
plot(horn~Jday, main ="Loess Regression: Horn~Julianday",ylab="Hornlength [mm]",xlab="Day of the year",pch=1)
lines(pred_Horn$fit~newday,col="red", lwd=2)
abline(h=140,col="blue",lty=2,lwd=2)
legend("topright",lwd=c(2,2),lty=c(1,2),col=c("red","blue"),legend=c("loess regression", " fix value 140 mm"))
#The loess regression reveals about constant hornlength during the hunting season
# consistent with gam


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
fgam4 <- gam(horn ~ s(asp, bs="cs"), data=db)
summary(fgam4)
plot(fgam4)
# but this explaines least deviance and is has a worse p-value

# turn the aspect, so the gap is not a 0 <-> 360 °
newasp <- db$asp + 180
newasp[newasp > 360] <- newasp[newasp > 360] - 360
summary(newasp)

plot(gam(horn ~ s(newasp, bs="cc"), data=db))


## animal density###########################

with(db, tapply(density, list(year, area_cod), unique))
# we have an animal density per administrative area per year  which is then either confirmed in the next year or changed.


## Collinearity in predictors ##########################

# collinearity in elevation data
cor(db[c("q_media", "q_min", "q_max")])
q_range <- db$q_max - db$q_min
with(db, cor(q_media, q_range))
with(db, plot(q_media, q_range)) # not okay yet
# we still have to decide what we want to use for the model


# collinearity of NAO and others
cor(db$nao_w, db$twinter.mean2)
cor(db$nao_w, db$twinter.min2)
cor(db$nao_w, db$twinter.max2)
# nao is correlated with minimum winter temps

cor.test(db$nao_w, db$snow_winter2)
cor.test(db$nao_d, db$snow_winter2)
# and also with snow depth

# NAO is not a useful predictor since we have higher resolution on the highly correlated weather data

# first winter
source("collinearity check.R")
winter1 <- cbind(snow_winter1, Snow_cover_winter1, twinter.max1, twinter.mean1, twinter.min1)
pairs(winter1, lower.panel = panel.smooth2,upper.panel = panel.cor, diag.panel = panel.hist, main = "Winter1")

# collinearity of NDVI



