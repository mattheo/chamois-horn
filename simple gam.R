library(mgcv)
library(sp)
library(Hmisc)

load("db.RData")
db_male <-subset(db, f.sex == "male")
db_female <-subset(db, f.sex == "female")


# tiny models with single predictors ###############


# Null Model:
# just with council code and year
fcham_t0 <- gam(horn ~ 1 + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t0) # Deviance explained = 12.4%
plot(fcham_t0, page=1, scale=F, all=T)
AIC(fcham_t0) # 23472.28


# sex #############
fcham_t1 <- gam(horn ~ f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t1) # Deviance explained = 40.8%
plot(fcham_t1, page=1, scale=F, all=T)
AIC(fcham_t1) # 22431.79


# weight #############
fcham_t2 <- gam(horn ~ s(weight, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t2) # Deviance explained = 24.5%
plot(fcham_t2, page=1, scale=F)
AIC(fcham_t2) # 23085.26

fcham_t2.1 <- gam(horn ~ f.sex + s(weight, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t2.1) # Deviance explained = 49.6%
plot(fcham_t2.1, page=1, scale=F)
AIC(fcham_t2.1) # 22006.7

fcham_t2.2 <- gam(horn ~ f.sex + s(weight, bs="ts", by=f.sex) + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t2.2) # Deviance explained = 50.2%
plot(fcham_t2.2, page=1, scale=F)
AIC(fcham_t2.2) # 21980.35
vis.gam(fcham_t2.2, theta=-35)

fcham_t2.3 <- gam(horn ~ f.sex*poly(weight, 2) + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t2.3) # Deviance explained = 50.2%
plot(fcham_t2.3, page=1, scale=F, all=T)
AIC(fcham_t2.3) # 21978.95
gam.check(fcham_t2.3)
vis.gam(fcham_t2.3, theta=-35)
# this approach is even better.
# weight is a quadratic term interacting with sex
# here the first order slope is interacting with significant
vis.gam(fcham_t2.3, theta=-35)


# Jday ###############
fcham_t3 <- gam(horn ~ Jday + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t3) # Deviance explained = 13.5%
plot(fcham_t3, page=1, scale=F, all=T)
AIC(fcham_t3) # 23439.43

fcham_t3.1 <- gam(horn ~ Jday + f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t3.1) # Deviance explained = 42.7%
plot(fcham_t3.1, page=1, scale=F, all=T)
AIC(fcham_t3.1) # 22347.01

fcham_t3.2 <- gam(horn ~ Jday*f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t3.2) # Deviance explained = 42.7%
plot(fcham_t3.2, page=1, scale=F, all=T)
AIC(fcham_t3.2) # 22348.68
# no interaction between jday and sex



# weight, sex and Jday
fcham_t4 <- gam(horn ~ f.sex*poly(weight, 2) + Jday + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t4) # Deviance explained = 52.4%
AIC(fcham_t4) # 21860.34 best so far

plot(fcham_t4, page=1, all=T)

f_weight <- seq(from=min(db_female$weight), to=max(db_female$weight), len=100)
m_weight <- seq(from=min(db_male$weight), to=max(db_male$weight), len=100)

female.preds <- predict(fcham_t4, newdata = data.frame("weight"=f_weight, "f.sex"="female", "f.year"=median(db_female$year), "Jday"=mean(db_female$Jday), "f.council_cod"=median(db_female$council_cod)), se=T)

male.preds <- predict(fcham_t4, newdata = data.frame("weight"=m_weight, "f.sex"="male", "f.year"=median(db_male$year), "Jday"=mean(db_male$Jday), "f.council_cod"=median(db_male$council_cod)), se=T)

################### helper ##########################
addTrans <- function(color,trans)
{
    # This function adds transparancy to a color.
    # Define transparancy with an integer between 0 and 255
    # 0 being fully transparant and 255 being fully visable
    # Works with either color and trans a vector of equal length,
    # or one of the two of length 1.

    if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
    if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
    if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))

    num2hex <- function(x)
    {
        hex <- unlist(strsplit("0123456789ABCDEF",split=""))
        return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
    }
    rgb <- rbind(col2rgb(color),trans)
    res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
    return(res)
}
###############################

plot(horn ~ weight, cex=0.5, type="n")

with(db_female, points(weight + 0.05, horn, col="pink"))
with(db_male, points(weight - 0.05, horn, col="lightskyblue"))
lines(male.preds$fit ~ weightnew, col="lightskyblue2", lwd=2)
lines(male.preds$fit - 1.96*male.preds$se.fit ~ weightnew, col="red", lty=2)
lines(male.preds$fit + 1.96*male.preds$se.fit ~ weightnew, col="red", lty=2)

lines(female.preds$fit ~ weightnew, col="pink1", lwd=2)
lines(female.preds$fit - 1.96*female.preds$se.fit ~ weightnew, col="red", lty=2)
lines(female.preds$fit + 1.96*female.preds$se.fit ~ weightnew, col="red", lty=2)

# abline(v=
with(db, tapply(weight, f.sex, mean))
#, col=c("pink", "lightskyblue"), lwd=2)


vis.gam(fcham_t4, theta=-35)
vis.gam(fcham_t4, theta=-35, view=c("f.sex", "Jday"))

#check the gam
gam.check(fcham_t4) # all good
acf(residuals(fcham_t4)) # all good
# spatial autocorrelation
resids <-  residuals(fcham_t1)
resids.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council + runif(nrow(db), min=-150, max=300), db$y.council +  runif(nrow(db), min=-300, max=150)), data=data.frame(resids))
bubble(resids.spdf, maxsize=10)
# all good!

# best Model in explaining the most deviance;
# AIC 21860 is the set point other models have to surpass
fcham_t4.1 <- gam(horn ~ f.sex*poly(weight, 2) + Jday + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=T)
summary(fcham_t4.1) # Deviance explained = 52.4%
summary(fcham_t4)
AIC(fcham_t4.1)

# substrate ##############
fcham_t5 <- gam(horn ~ f.substrate + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t5) # Deviance explained = 12.3%
plot(fcham_t5, page=1, scale=F, all=T)
AIC(fcham_t5) # 23469.42

fcham_t5.1 <- gam(horn ~ f.substrate + f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t5.1) # Deviance explained = 40.7%
plot(fcham_t5.1, page=1, scale=F, all=T)
AIC(fcham_t5.1) # 22429.99

fcham_t5.2 <- gam(horn ~ f.substrate*f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t5.2) # Deviance explained = 40.7%
plot(fcham_t5.2, page=1, scale=F, all=T)
AIC(fcham_t5.2) # 22431.43
# No interaction!!


fcham_e1 <- update(fcham_t4, . ~ . + f.substrate)
summary(fcham_e1) # Deviance explained = 52.3%
plot(fcham_e1, page=1, scale=F, all=T)
AIC(fcham_e1) # 21856.68
AIC(fcham_t4) # 21860.34
# slightly better!


# elevation
fcham_t6 <- gam(horn ~ s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t6) # Deviance explained = 12.2%
plot(fcham_t6, page=1, scale=F, all=T)
AIC(fcham_t6) # 23468.93


fcham_e2 <- update(fcham_t4, . ~ . + q_media*f.sex)
summary(fcham_e2) # Deviance explained = 52.3%
plot(fcham_e2, page=1, scale=T, all=T)
AIC(fcham_e2) # 21860.11
AIC(fcham_t4) # 21860.34
# no difference


# perentage open area
fcham_t6 <- gam(horn ~ s(Perc.area.aperta, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t6) # Deviance explained = 12.2%
plot(fcham_t6, page=1, scale=F, all=T)
AIC(fcham_t6) # 23470.82

# density
fcham_t7 <- gam(horn ~ s(density, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t7) # Deviance explained = 12.4%
plot(fcham_t7, page=1, scale=F, all=T)
AIC(fcham_t7) # 23472.28



# NDVI pc may1 and may2
fcham_t9 <- gam(horn ~ s(ndvi.m1m2.pc1, bs="ts") + s(ndvi.m1m2.pc2, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t9) #Deviance explained = 12.2%
plot(fcham_t9, page=1, scale=F, all=T)
AIC(fcham_t9) # 23468.44
# best for environmental predictors so far

fcham_t9.1 <- gam(horn ~
        s(ndvi.m1m2.pc1, bs="ts") +
        s(ndvi.m1m2.pc2, bs="ts") +
        s(ndvi.maxincr1, bs="ts", k=4) +
        s(ndvi.maxincr2, bs="ts", k=4) +
        s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t9.1) #Deviance explained = 12.2%
plot(fcham_t9.1, page=1, scale=F, all=T)
AIC(fcham_t9.1) # 23468.44
# the same as before

fcham_t9.2 <- gam(horn ~
        s(ndvi.m1m2.pc1, bs="ts") +
        s(ndvi.m1m2.pc2, bs="ts") +
        s(ndvi.maxincr1, bs="ts", k=4) +
        s(ndvi.maxincr2, bs="ts", k=4) +
        s(log.ndvi.slop1, bs="ts") +
        s(log.ndvi.slop2, bs="ts") +
        s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t9.2) #Deviance explained = 12.6%
plot(fcham_t9.2, page=2, scale=F, all=T)
AIC(fcham_t9.2) # 23465.47
# slightly better

# NAO
fcham_t10 <- gam(horn ~ s(nao_a1, bs="ts", k=3) + s(nao_a2, bs="ts", k=3) + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_t10) #Deviance explained = 12.2%
plot(fcham_t10, page=1, scale=F, all=T)
AIC(fcham_t10) # 23472.18 no explanation

#



# first big model #########

fcham_s1 <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts") + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)

summary(fcham_s1) # Deviance explained = 51.8%
# q_media is not significant, has no effect
# Jday is linear and can be moved to the linear part
plot(fcham_s1, page=1)
vis.gam(fcham_s1, view=c("x.council", "y.council"), theta=-145)
gam.check(fcham_s1) # fine
AIC(fcham_s1) # 21879.94

# interaction sex and Jday
fcham_s1.1 <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts", by=f.sex) + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)

summary(fcham_s1.1) # Deviance explained = 51.8%
# q_media is not significant, has no effect
# Jday is linear and can be moved to the linear part
plot(fcham_s1.1, page=1)
vis.gam(fcham_s1.1, view=c("x.council", "y.council"), plot="contour")
# almost linear, explains not much
gam.check(fcham_s1.1) # fine
AIC(fcham_s1.1) # 21881.34

#remove council coords
fcham_s1.2 <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts", by=f.sex) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_s1.2)
plot(fcham_s1.2, page=1)
# almost linear, explains not much
gam.check(fcham_s1.2) # fine
AIC(fcham_s1.2) # 21889.72

# Jday with female + Jday with male are both significant, but are they different?
fcham_s1.3 <- gam(horn ~ f.sex + s(weight, bs="ts") + Jday*f.sex + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_s1.3)
plot(fcham_s1.3, page=1, scale=F, all=T)
# nope
gam.check(fcham_s1.3) # fine
AIC(fcham_s1.3) # 21889.37

# the interaction is not significantly explaining anything




# Jday in the linear part, x.y council removed, substrate and q_min and q_rangea dded  ###############
#check for collinearity

plot(varclus(horn ~ sex + substrate + aspect + year + council_cod + Jday + weight + q_media + q_min + q_range, data=db))
abline(h=0.5, lty=3)
# all good!

fcham_s2 <-  gam(horn ~ f.sex + f.substrate + Jday + s(f.year, bs="re") + s(f.council_cod, bs="re") + s(weight, bs="ts") + s(q_media, bs="ts") + s(q_min, bs="ts") + s(q_range, bs="ts"), data=db, REML=F)

summary(fcham_s2)
AIC(fcham_s2) # 21885.41
gam.check(fcham_s2) # fine
plot(fcham_s2, scale=F, page=2, all=T)

# check for spatial autocorrelation
resids_s2 <- residuals(fcham_s2)

resids_s2.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council + runif(nrow(db), min=-150, max=150), db$y.council +  runif(nrow(db), min=-150, max=150)), data=data.frame(resids_s2))
bubble(resids_s2.spdf, maxsize=10)
# all good!

# check for temporal auocorrelation
pacf(residuals(fcham_s2))
# all good


#remove q_range
fcham_s2.1 <-  gam(horn ~ f.sex + f.substrate + Jday + s(f.year, bs="re") + s(f.council_cod, bs="re") + s(weight, bs="ts") + s(q_media, bs="ts") + s(q_min, bs="ts"), data=db, REML=F)
summary(fcham_s2.1)
AIC(fcham_s2.1) # 21885.4 # same
# no effect, no difference
gam.check(fcham_s2.1) # fine
plot(fcham_s2.1, scale=F, page=1)

# remove q_min
fcham_s2.2 <-  gam(horn ~ f.sex + f.substrate + Jday + s(f.year, bs="re") + s(f.council_cod, bs="re") + s(weight, bs="ts") + s(q_media, bs="ts") + s(q_range, bs="ts"), data=db, REML=F)
summary(fcham_s2.2)
AIC(fcham_s2.2) # 21885.43 # same
# no effect, no difference
gam.check(fcham_s2.2) # fine
plot(fcham_s2.2, scale=F, page=1)

# remove q_min and q_range, keep q_media
fcham_s2.3 <-  gam(horn ~ f.sex + f.substrate + Jday +
        s(f.year, bs="re") +
        s(f.council_cod, bs="re") +
        s(weight, bs="ts") +
        s(q_media, bs="ts")
    , data=db, REML=F)
summary(fcham_s2.3)
AIC(fcham_s2.3) # 21885.43 # same
gam.check(fcham_s2.3) # fine
plot(fcham_s2.3, scale=F, page=1)
# there seems to be a breakpoint for elevation where hornsize suddenly decrease
plot(fcham_s2.3, select=4, scale=F)

# also remove q_media, no q_
fcham_s2.4 <-  gam(horn ~ f.sex + f.substrate + Jday + s(f.year, bs="re") + s(f.council_cod, bs="re") + s(weight, bs="ts"), data=db, REML=F)
summary(fcham_s2.4)
AIC(fcham_s2.4) # 21886.39
# no effect, no difference
AIC(fcham_s2) # 21885.41
gam.check(fcham_s2.4) # fine
plot(fcham_s2.4, scale=F, page=1)


# add interaction to all q_ predictors
fcham_s2.5 <-  gam(horn ~ f.sex + f.substrate + Jday + s(f.year, bs="re") + s(f.council_cod, bs="re") + s(weight, bs="ts") + s(q_media, bs="ts", by=f.sex) + s(q_min, bs="ts", by=f.sex) + s(q_range, bs="ts", by=f.sex), data=db, REML=F)

summary(fcham_s2.5)
AIC(fcham_s2.5) # 21883.3 slightly better
gam.check(fcham_s2.5) # fine
plot(fcham_s2.5, scale=F, page=2)

#
# Q_media is the only relevant predictor of class elevation



# q_media as lone predictor
fcham_s2.6 <-  gam(horn ~ f.sex + f.substrate + Jday +
        s(f.year, bs="re") +
        s(f.council_cod, bs="re") +
        s(q_media, bs="ts")
    , data=db, REML=F)
summary(fcham_s2.6)
plot(fcham_s2.6, page=1, scale=F)
# only with weight this breakpoint appears



# interaction of current predictors with sex ###########
# first: substrate
fcham_s4 <- gam(horn ~ f.substrate*f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday + s(weight, bs="ts") + s(q_media, bs="ts"), data=db, REML=F)

summary(fcham_s4)
vis.gam(fcham_s4, view=c("f.sex", "f.substrate"), theta=-55)
AIC(fcham_s4) # 21948.43
# no interaction, but better AIC

# Jday * sex
fcham_s5 <- gam(horn ~ f.substrate + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday*f.sex + s(weight, bs="ts") + s(q_media, bs="ts"), data=db, REML=F)

summary(fcham_s5)
AIC(fcham_s5) # 21949.99
# worse! no interaction

# weight and sex
fcham_s6 <- gam(horn ~ f.substrate + f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday + s(weight, bs="ts", by=f.sex) + s(q_media, bs="ts"), data=db, REML=F)
summary(fcham_s6)
AIC(fcham_s6) # 21918
# definitely an interaction!
gam.check(fcham_s6)
# looks good
plot(fcham_s6, page=1)
vis.gam(fcham_s6, view=c("f.sex", "weight"), theta=-55)
vis.gam(fcham_s6, view=c("f.sex", "weight"), theta=55, se=1.96)

fcham_s6 <- gam(horn ~ f.substrate + f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday + weight*f.sex + s(q_media, bs="ts"), data=db, REML=F)
summary(fcham_s6)

# males have longer horns getting heavier than females do;
# but males are in general heavier than females
# check for spatial autocorrelation
resids_s6 <- residuals(fcham_s6)

resids_s6.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council + runif(nrow(db), min=-300, max=150), db$y.council +  runif(nrow(db), min=-300, max=150)), data=data.frame(resids_s6))
bubble(resids_s6.spdf, maxsize=10)
# all good!

# elevation and sex
fcham_s7 <- gam(horn ~ f.substrate + Jday + f.sex +
        s(f.year, bs="re") +
        s(f.council_cod, bs="re") +
        s(weight, bs="ts", by=f.sex) +
        s(q_media, bs="ts", by=f.sex)
    , data=db, REML=F)
summary(fcham_s7)
AIC(fcham_s7) # 21919.13
gam.check(fcham_s7) # looks fine
# also an interaction here
plot(fcham_s7, scal=F, page=1)

# maybe as a linear term
fcham_s8 <- gam(horn ~ f.substrate + f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday + s(weight, bs="ts", by=f.sex) + q_media*f.sex, data=db, REML=F)
summary(fcham_s8)
AIC(fcham_s8) # 21919.19
gam.check(fcham_s8) # looks fine
# no interaction as linear model; probably nothing
plot(fcham_s8, scale=F, page=2, all=T, select=8)

vis.gam(fcham_s8, view=c("f.sex", "q_media"), theta=-55)
vis.gam(fcham_s8, view=c("f.sex", "q_media"), theta=55)
vis.gam(fcham_s8, view=c("f.sex", "q_media"), plot="contour")


# add environmental predictors #############

## ndvi may1, may2 pc
plot(varclus(horn ~ sex + substrate + aspect + year + council_cod + Jday + weight + q_media + q_min + q_range + ndvi.m1m2.pc1 + ndvi.m1m2.pc2, data=db))
abline(h=0.5, lty=3)
# here, q_media must go


fcham_s9 <- update(fcham_s6, . ~ . + s(ndvi.m1m2.pc1, bs="ts") + s(ndvi.m1m2.pc2, bs="ts") - s(q_media, bs="ts"), REML=F)
summary(fcham_s9)
#
AIC(fcham_s9) # 21900.65
# better model
gam.check(fcham_s9)
plot(fcham_s9, scale=F, page=1) # overfit!

# interaction?
fcham_s10 <- update(fcham_s6, . ~ . + s(ndvi.m1m2.pc1, bs="cs", by=f.sex) + s(ndvi.m1m2.pc2, bs="cs", by=f.sex) - s(q_media, bs="ts"), REML=F)
summary(fcham_s10)
gam.check(fcham_s10)
AIC(fcham_s10) # 21908
plot(fcham_s10, scale=F, page=1) # overfit!

fcham_s11 <-update(fcham_s6, . ~ . + s(ndvi.m1m2.pc1, bs="cs", k=4) - s(q_media, bs="ts"), REML=F)
summary(fcham_s11)
AIC(fcham_s11) # 21918.45
# its worse than before
plot(fcham_s11, scale=F, page=1)
# there's nothing left
