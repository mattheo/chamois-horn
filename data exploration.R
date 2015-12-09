library(mgcv)

# collinearity in elevation data
cor(db[c("q_media", "q_min", "q_max")])
db$q_range <- db$q_max - db$q_min
with(db, cor(q_media, q_range))
with(db, plot(q_media, q_range)) # not okay yet


# collinearity of NAO and others
cor.test(db$nao_w, db$twinter.mean2)
cor.test(db$nao_w, db$twinter.min2)
cor.test(db$nao_w, db$snow_winter2)

cor.test(db$nao_d, db$snow_winter2)


# data transformation
with(db_chamois1, tapply(snow_winter2, list(area_cod, year), mean)) # --> only 2 station, weather data in area1 are mixed

with(db_chamois1, tapply(twinter.mean, list(area_cod, year), mean))

with(db_chamois1, tapply(tspring1.mean, list(area_cod, year), mean)) # changed since 2009

# inconsistent relationship between areas

# how big is the variance between areas?
with(db, tapply(twinter.min2, year, summary)) # second winter, minimum T
with(db, tapply(twinter.max2, year, summary)) # second winter, max T

# summary of first winter
with(db, tapply(twinter.mean1, year, summary))

# values of first winter per year and area
with(db, tapply(twinter.mean1, list(area_cod, year), unique))


# aspect on horn size
# use bs="cc" because asp is periodical (0 -> 360=0)
fgam1 <- gam(horn ~s(asp, bs="cc"), data=db)
summary(fgam1)
plot(fgam1)

fgam2 <- gam(horn ~s(asp, bs="cp"), data=db)
summary(fgam2)
plot(fgam2)

# problem: asp is not going form 0 to 360
summary(db$asp)
# but from 36 to 340 degrees
# -> the cycle is closed "too early"
fgam3 <- gam(horn ~ s(asp, bs="cs"), data=db)
summary(fgam3)
plot(fgam3)
# but this explaines least deviance and is has a worse p-value
