library(mgcv)
load("db.RData")



# gam with bs="re ################

fcham_s1 <-  gam(horn ~ f.sex + s(weight, bs="ts") + s(Jday, bs="ts") + s(x.council, y.council) + s(q_media, bs="ts") + s(factor(db$year), bs="re") + s(factor(db$council_cod), bs="re"), data=db, REML=F)

summary(fcham_s1) # Deviance explained = 51.8%
# q_media is not significant, has no effect
# Jday is linear and can be moved to the linear part
plot(fcham_s1, page=1)
# vis.gam(fcham_s1, view=c("x.council", "y.council"))
gam.check(fcham_s1) # fine
AIC(fcham_s1) # 21943.3

# Jday in the linear part ###############
fcham_s2 <-  gam(horn ~ f.sex + s(weight, bs="ts") + Jday + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=db, REML=F)

summary(fcham_s2) # Deviance explained = 51.8%
AIC(fcham_s2) # 21942.99
gam.check(fcham_s2) # fine


# add substrate #################
fcham_s3 <-  gam(horn ~ f.sex + s(weight, bs="ts") + Jday + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re") + f.substrate, data=db, REML=F)

summary(fcham_s3) # Deviance explained = 51.8%
# substrate has a siginificant effect (almost 8mm longer horns on calc)
AIC(fcham_s3) # 21942.04
gam.check(fcham_s3) # fine


# interaction between sex and substrate? #################
fcham_s4 <-  gam(horn ~ f.sex + s(weight, bs="ts") + Jday + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re") + f.sex*f.substrate, data=db, REML=F)

summary(fcham_s4)
# no interaction between sex and substrate
AIC(fcham_s3) # 21942.04


# ndvi may in both years important? ################
fcham_s5 <-  gam(horn ~ f.sex + s(weight, bs="ts") + Jday + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re") + f.substrate + s(ndvi.may1.new, bs="ts") + s(ndvi.may2.new, bs="ts"), data=db, REML=F)

summary(fcham_s5) # Deviance explained =   52%
# it performs better, but may1 is definitley not significant
AIC(fcham_s5) # 21937.6
plot(fcham_s5)
gam.check(fcham_s5) # there is a problem with both ndvi


# remove may1 ###################
fcham_s6 <-  gam(horn ~ f.sex + s(weight, bs="ts") + Jday + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re") + f.substrate + s(ndvi.may2.new), data=db, REML=F)

summary(fcham_s6) # Deviance explained =   52%
# slightly better
plot(fcham_s6, select = 6)
gam.check(fcham_s6) # still problematic
AIC(fcham_s6) # 21937.58


# interaction ndvi may2 with sex #####################
fcham_s7 <-  gam(horn ~ f.sex + s(weight, bs="ts") + Jday + s(x.council, y.council) + s(q_media, bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re") + f.substrate + s(ndvi.may2.new, by=f.sex), data=db, REML=F)

summary(fcham_s7) # Deviance explained = 51.9%
# less deviance explained
AIC(fcham_s7) # 21939.81 slightly worse
