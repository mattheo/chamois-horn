library(mgcv)
library(sp)
library(Hmisc)
load("db.RData")



# gam with bs="re ################

fcham_s1 <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts") + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)

summary(fcham_s1) # Deviance explained = 51.8%
# q_media is not significant, has no effect
# Jday is linear and can be moved to the linear part
plot(fcham_s1, page=1)
vis.gam(fcham_s1, view=c("x.council", "y.council"))
gam.check(fcham_s1) # fine
AIC(fcham_s1) # 21943.3

# interaction sex and Jday
fcham_s1.1 <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts", by=f.sex) + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)

summary(fcham_s1.1) # Deviance explained = 51.8%
# q_media is not significant, has no effect
# Jday is linear and can be moved to the linear part
plot(fcham_s1.1, page=1)
vis.gam(fcham_s1.1, view=c("x.council", "y.council"), plot="contour")
# almost linear, explains not much
gam.check(fcham_s1.1) # fine
AIC(fcham_s1.1) # 21944.56

fcham_s1.2 <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts", by=f.sex) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)
summary(fcham_s1.2)
plot(fcham_s1.2, page=1)
# almost linear, explains not much
gam.check(fcham_s1.1) # fine
AIC(fcham_s1.1) # 21944.56




# Jday in the linear part, x.y council removed, substrate and added  ###############
#check for collinearity

plot(varclus(horn ~ sex + substrate + aspect + year + council_cod + Jday + weight + q_media + q_min + q_range, data=db))
abline(h=0.5, lty=3)
# all good!

fcham_s2 <-  gam(horn ~ f.sex + f.substrate + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday + s(weight, bs="ts") + s(q_media, bs="ts") + s(q_min, bs="ts") + s(q_range, bs="ts"), data=db, REML=F)

summary(fcham_s2)
AIC(fcham_s2) # 21948.53
gam.check(fcham_s2) # fine
plot(fcham_s2, scale=F, page=1)

# check for spatial autocorrelation
resids_s2 <- residuals(fcham_s2)

resids_s2.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council + runif(nrow(db), min=-150, max=150), db$y.council +  runif(nrow(db), min=-150, max=150)), data=data.frame(resids_s2))
bubble(resids_s2.spdf, maxsize=10)
# all good!


# More Models ############

fcham_s3 <-  gam(horn ~ f.sex + f.substrate + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday + s(weight, bs="ts") + s(q_media, bs="ts"), data=db, REML=F)

summary(fcham_s3)
AIC(fcham_s3) # 21948.82
gam.check(fcham_s2) # fine
plot(fcham_s3, scale=F, page=1)
# all good

# is there an interaction with sex?
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
vis.gam(fcham_s6, view=c("f.sex", "weight"), theta=-55, se=1.96)
vis.gam(fcham_s6, view=c("f.sex", "weight"), theta=55, se=1.96)

# males have longer horns getting heavier than females do;
# but males are in general heavier than females
# check for spatial autocorrelation
resids_s6 <- residuals(fcham_s6)

resids_s6.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council + runif(nrow(db), min=-300, max=150), db$y.council +  runif(nrow(db), min=-300, max=150)), data=data.frame(resids_s6))
bubble(resids_s6.spdf, maxsize=10)
# all good!

# elevation and sex
fcham_s7 <- gam(horn ~ f.substrate + f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday + s(weight, bs="ts", by=f.sex) + s(q_media, bs="ts", by=f.sex), data=db, REML=F)
summary(fcham_s7)
AIC(fcham_s7) # 21919.13
gam.check(fcham_s7) # looks fine
# also an interaction here; but AIC is worse
plot(fcham_s7, scal=F, page=1)

# maybe as a linear term
fcham_s8 <- gam(horn ~ f.substrate + f.sex + s(f.year, bs="re") + s(f.council_cod, bs="re") + Jday + s(weight, bs="ts", by=f.sex) + q_media*f.sex, data=db, REML=F)
summary(fcham_s8)
AIC(fcham_s8) # 21919.19
gam.check(fcham_s8) # looks fine
# no interaction as linear model; probably nothing

vis.gam(fcham_s8, view=c("f.sex", "q_media"), theta=-55, se=1.96)
vis.gam(fcham_s8, view=c("f.sex", "q_media"), theta=55, se=1.96)


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
