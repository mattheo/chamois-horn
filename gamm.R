library(mgcv)
load("db.RData")

## FIrst GAMM ##################


fcham1 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday) + s(q_media, bs="cs") + s(q_min, bs="cs") + s(weight) + f.substrate + s(density, bs="cs") + s(ndvi.slop1, bs="cs") + s(ndvi.maxincr1, bs="cs") + s(ndvi.may1.new, bs="cs") + s(ndvi.slop2, bs="cs") + s(ndvi.maxincr2, bs="cs") + s(Perc.area.aperta, bs="cs") + s(snow_winter1, by=aspect, k=3) + s(r_newsummer1, k=3) + s(snow_winter2, by=aspect, k=3) + s(r_spring2, k=3) + s(r_newsummer2, k=3) + s(twinter.mean1, k=3) + s(tspring1.mean, k=3) + s(tsummer1.mean, k=3) + s(twinter.mean2, k=3) + s(tspring2.mean, k=3) + s(tsummer2.mean, k=3), data=db)

summary(fcham1)
