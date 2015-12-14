library(mgcv)
library(gamm4)
load("db.RData")

## FIrst GAMM ##################


fcham1 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday) + s(q_media, bs="cs") + s(q_min, bs="cs") + s(weight) + f.substrate + s(density, bs="cs") + s(ndvi.slop1, bs="cs") + s(ndvi.maxincr1, bs="cs") + s(ndvi.may1.new, bs="cs") + s(ndvi.slop2, bs="cs") + s(ndvi.maxincr2, bs="cs") + s(Perc.area.aperta, bs="cs") + s(snow_winter1, by=aspect, k=3) + s(r_newsummer1, k=3) + s(snow_winter2, by=aspect, k=3) + s(r_spring2, k=3) + s(r_newsummer2, k=3) + s(twinter.mean1, k=3) + s(tspring1.mean, k=3) + s(tsummer1.mean, k=3) + s(twinter.mean2, k=3) + s(tspring2.mean, k=3) + s(tsummer2.mean, k=3), data=db)

# gamm(y ~  ..., random=list(council_cod=~1, year=~1))

summary(fcham1)

# simple GAMM with random effects and the most important predictors
fgamm_s1 <- gamm4(horn ~ f.sex + s(weight, bs="cs") + s(Jday, bs="cs") + s(x.council, y.council) + s(q_media, bs="ts"), random= ~ (1|council_cod) + (1|year), data=db, REML=F)

# gamm4 works, but is too slow
summary(fgamm_s1$gam)
AIC(fgamm_s1$lme)

gam.check(fgamm_s1$gam)
effects(fgamm_s1$gam)
plot(fgamm_s1$gam)
#plot(fgamm_s1$lme)

