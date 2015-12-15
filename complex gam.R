library(mgcv)
library(sp)
library(gstat)
load("db.RData")

## complex gamms: how far can we go?

# first model, all predictors ################
system.time(
    fcham_c1 <-  gam(horn ~
            f.sex +
            s(weight, bs="ts") +
            s(Jday, bs="ts") +
            s(x.council, y.council) +
            s(q_media, bs="ts") +
            s(q_min, bs="ts") +
            s(q_max, bs="ts") +
            s(f.year, bs="re") +
            s(f.council_cod, bs="re") +
            f.substrate +
            s(density, bs="ts") +
            s(log.ndvi.slop1, bs="ts") +
            s(ndvi.maxincr1, bs="ts") +
            s(ndvi.may1.new, bs="ts") +
            s(log.ndvi.slop2, bs="ts") +
            s(ndvi.maxincr2, bs="ts") +
            s(ndvi.may2.new, bs="ts") +
            s(Perc.area.aperta, bs="ts") +
            s(snow_winter1, by=aspect, k=3, bs="ts") +
            s(r_spring1, k=3, bs="ts") +
            s(r_newsummer1, k=3, bs="ts") +
            s(r_autumn, k=3, bs="ts") +
            s(snow_winter2, by=aspect, k=3, bs="ts") +
            s(r_spring2, k=3, bs="ts") +
            s(r_newsummer2, k=3, bs="ts") +
            s(twinter.mean1, k=3, bs="ts") +
            s(tspring1.mean, k=3, bs="ts") +
            s(tsummer1.mean, k=3, bs="ts") +
            s(tautumn.mean, k=3, bs="ts") +
            s(twinter.mean2, k=3, bs="ts") +
            s(tspring2.mean, k=3, bs="ts") +
            s(tsummer2.mean, k=3, bs="ts"),
        data=db, REML=F)
)
summary(fcham_c1)
# Deviance explained = 52.9%
gam.check(fcham_c1)

par(mfrow=c(2, 2))
plot(fcham_c1, scale = F)

AIC(fcham_c1) # 21901.58


# NDVI summer instead of may #############
fcham_c2 <- update(fcham_c1, . ~ . - s(ndvi.may1.new, bs="ts") - s(ndvi.may2.new, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2, bs="ts"))

summary(fcham_c2)
# Deviance explained = 52.7%
gam.check(fcham_c2)
plot(fcham_c2, scale=F)

AIC(fcham_c2) # 21904.91


# collinear predictors: elevation, open area #################
# remove q_max
fcham_c3.1 <- update(fcham_c1, . ~ . - s(q_max, bs="ts"))
summary(fcham_c3.1)
# Deviance explained = 52.9%
AIC(fcham_c3.1) # 21901.84
# little worse, but nothin strange

# remove q_media
system.time(fcham_c3.2 <-  update(fcham_c1, . ~ . - s(q_media, bs="ts")))
summary(fcham_c3.2)
# Deviance explained = 52.9%
AIC(fcham_c3.2) # 21901.58
# nothing happened!

# remove q_min, q_max
fcham_c3.3 <-  update(fcham_c1, . ~ . - s(q_max, bs="ts") - s(q_min, bs="ts"))
summary(fcham_c3.3)
AIC(fcham_c3.3) # 21902.36
# worse

# remove all elevation data
fcham_c3.4 <- update(fcham_c1, . ~ . - s(q_media, bs="ts") - s(q_max, bs="ts") - s(q_min, bs="ts"))
summary(fcham_c3.4)
# Deviance explained = 52.9%
AIC(fcham_c3.4) # 21901.98
# the model performs better than the standard

# remove open area
fcham_c3.5 <- update(fcham_c3.4, . ~ . - s(Perc.area.aperta, bs="ts"), data=db, REML=F)
summary(fcham_c3.5)
# Deviance explained = 52.9%
AIC(fcham_c3.5) # 21902.24
# very slightly worse

## conclusion: removing elevation does not take away anything; open area is also highly correlated with elevation; ndvi, which is also highly correlated, is still in the model. it takes on the role of height.


# collinearity: remove r_spring1, r_spring2 and r_autumn ####################
system.time(fcham_c4 <- update(fcham_c3.5, . ~ . - s(r_spring1, k=3, bs="ts") - s(r_spring2, k=3, bs="ts") - s(r_autumn, k=3, bs="ts"), data=db, REML=F))
summary(fcham_c4)
# Deviance explained = 52.9%
AIC(fcham_c4) # 21902.35

# something happened
par(mfrow=c(2,2))
plot(fcham_c4, scale = F)


# new model without collinearity
# Julian day is linear
# without x and y coordinates
# no aspect

system.time(
    fcham_c5 <-  gam(horn ~
            f.sex +
            Jday +
            f.substrate +
            s(f.year, bs="re") +
            s(f.council_cod, bs="re") +
            s(weight, bs="ts") +
            s(density, bs="ts") +
            s(log.ndvi.slop1, bs="ts") +
            s(log.ndvi.slop2, bs="ts") +
            s(ndvi.maxincr1, bs="ts") +
            s(ndvi.maxincr2, bs="ts") +
            s(pc1.ndvi, bs="ts") +
            s(pc2.ndvi, bs="ts") +
            s(snow_winter1, k=3, bs="ts") +
            s(snow_winter2, k=3, bs="ts") +
            s(r_newsummer1, k=3, bs="ts") +
            s(r_newsummer2, k=3, bs="ts") +
            s(twinter.mean1, k=3, bs="ts") +
            s(twinter.mean2, k=3, bs="ts") +
            s(tspring1.mean, k=3, bs="ts") +
            s(tspring2.mean, k=3, bs="ts") +
            s(tsummer1.mean, k=3, bs="ts") +
            s(tsummer2.mean, k=3, bs="ts") +
            s(tautumn.mean, k=3, bs="ts"),
        data=db, REML=F)
)

summary(fcham_c5)
# 53.2%
AIC(fcham_c5) # 21894.78
gam.check(fcham_c5)

par(mfrow=c(2,2))
plot(fcham_c5, scale=F)

# check for spatial autocorrelation
resids_c5 <- residuals(fcham_c5)

resids.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council, db$y.council), data=data.frame(resids_c5))

bubble(resids.spdf, maxsize=10, main="non-spatial LM")

#all good
