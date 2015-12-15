# spatial autocorrelation check
library(sp)

resids <- residuals(fcham_s1)

resids.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council, db$y.council), data=data.frame(resids))
bubble(resids.spdf, maxsize=5, main="non-spatial LM")

library(gstat)
plot(variogram(resids~1, data=resids.spdf))

plot(variogram(resids~1, data=resids.spdf, alpha=c(0,45, 90, 135)))

# check without x./y.council
fcham_s1re <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts") + s(q_media, bs="ts") + s(factor(db$year), bs="re") + s(factor(db$council_cod), bs="re"), data=db, REML=F)

summary(fcham_s1re) 


resids <- residuals(fcham_s1re)

resids.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council, db$y.council), data=data.frame(resids))
bubble(resids.spdf, maxsize=5, main="non-spatial LM")

library(gstat)
plot(variogram(resids~1, data=resids.spdf))

plot(variogram(resids~1, data=resids.spdf, alpha=c(0,45, 90, 135)))

# check without x./y.council, council_cod
fcham_s1re2 <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts") + s(q_media, bs="ts") + s(factor(db$year), bs="re"), data=db, REML=F)

summary(fcham_s1re2) 


resids <- residuals(fcham_s1re2)

resids.spdf <- SpatialPointsDataFrame(coords=cbind(db$x.council, db$y.council), data=data.frame(resids))
bubble(resids.spdf, maxsize=5, main="non-spatial LM")

library(gstat)
plot(variogram(resids~1, data=resids.spdf))

plot(variogram(resids~1, data=resids.spdf, alpha=c(0,45, 90, 135)))
