library(mgcv)
load("db.RData")
load("male_db.RData")
load("female_db.RData")

f.sex<-as.factor(db$sex)

## FIrst GAMM ##################


fcham1 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday) + s(q_media, bs="cs") + s(q_min, bs="cs") + s(weight) + f.substrate + s(density, bs="cs") + s(ndvi.slop1, bs="cs") + s(ndvi.maxincr1, bs="cs") + s(ndvi.may1.new, bs="cs") + s(ndvi.slop2, bs="cs") + s(ndvi.maxincr2, bs="cs") + s(Perc.area.aperta, bs="cs") + s(snow_winter1, by=aspect, k=3) + s(r_newsummer1, k=3) + s(snow_winter2, by=aspect, k=3) + s(r_spring2, k=3) + s(r_newsummer2, k=3) + s(twinter.mean1, k=3) + s(tspring1.mean, k=3) + s(tsummer1.mean, k=3) + s(twinter.mean2, k=3) + s(tspring2.mean, k=3) + s(tsummer2.mean, k=3), data=db)
vis.gam(fcham1,view = c("density","weight"),theta=25)


Chgam1 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday) + s(q_media) + s(q_min) + s(weight) + f.substrate + s(density) + s(ndvi.slop1) + s(ndvi.maxincr1) + s(ndvi.summer1) + s(ndvi.summer2) + s(ndvi.slop2) + s(ndvi.maxincr2) + s(Perc.area.aperta) , data=db)
vis.gam (Chgam1, view = c("q_media","weight"),theta =50)
summary(Chgam1)
vis.gam (Chgam1, view = c("density","weight"),theta =50)

vis.gam (Chgam1, view = c("f.sex","weight"),theta =50)
vis.gam (Chgam1, view = c("f.sex","density"),theta =50)
names(db)
# Gamm4 zu langsam
Chgam2 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min) + s(weight) + f.substrate + s(density) + s(ndvi.slop1) + s(ndvi.maxincr1) + s(ndvi.summer1) + s(ndvi.summer2) + s(ndvi.slop2) + s(ndvi.maxincr2) + s(Perc.area.aperta) + s(factor(db$year), bs="re") + s(factor(db$council_cod),bs="re"), data=db)

AIC(Chgam2)
plot(Chgam2)
plot(Chgam2,select=3)


Chgam3 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="cs") + s(weight, bs="cs") + f.substrate + s(density, bs="cs") + s(ndvi.slop1, bs="cs") + s(ndvi.maxincr1, bs="cs") + s(ndvi.summer1, bs="cs") + s(ndvi.summer2,bs="cs") + s(ndvi.slop2, bs="cs") + s(ndvi.maxincr2, bs="cs") + s(Perc.area.aperta ,bs="cs") + s(factor(db$year), bs="re") + s(factor(db$council_cod),bs="re"), data=db)

summary(Chgam3)


summary(Chgam2)


Chgam4 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts") + s(ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(factor(db$year), bs="re") + s(factor(db$council_cod),bs="re"), data=db)

summary(Chgam4)
#mit Interaktionen
Chgam5 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday, by= f.sex) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight,by = f.sex) + f.substrate + s(density, bs="ts") + s(ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(factor(db$year), bs="re") + s(factor(db$council_cod),bs="re"), data=db)

Chgam6 <- gam(horn ~ f.sex + s(x.council, y.council) + s(Jday, by= f.sex) + (q_media*f.sex) + I(q_media^2) + s(q_min,bs="ts") + s(weight,by = f.sex) + f.substrate + s(density, bs="ts") + s(ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(factor(db$year), bs="re") + s(factor(db$council_cod),bs="re"), data=db)
summary(Chgam6)


AIC(Chgam2)#21927.81
AIC(Chgam3)#21961.87
AIC(Chgam4)#21923.86
AIC(Chgam5)#21894.31
AIC(Chgam6)#21893.96
################################ sex separated #######################################


M_gam1 <- gam(horn ~  s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts") + s(ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(factor(male_db$year), bs="re") + s(factor(male_db$council_cod),bs="re"), data=male_db, REML=FALSE)

F_gam1 <- gam(horn ~  s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts") + s(ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(factor(female_db$year), bs="re") + s(factor(female_db$council_cod),bs="re"), data=female_db, REML=FALSE)
####FEHLER##########################################
#vis.gam(F_gam1,view = c("density","weight"),theta=25)
##########################################################
summary(M_gam1)
summary(F_gam1)
# differences in p values 

#s(density) male 0.00332 **
#s(density) female 0.095697 .

#s(ndvi.maxincr2) male 0.01412 *
#s(ndvi.maxincr2) female 0.115636

#s(Perc.area.aperta) male 0.00107 **
#s(Perc.area.aperta) female 0.059820 .

#significance of density, ndvi.max.incr2 and density is in male higer than in female
nrow(male_db)#1461
nrow((female_db)#1218
# but the  number of samles is also different

AIC(M_gam1) #11975.4
AIC(F_gam1) #9922.889

# model female has a better fit than male.

plot(F_gam1)
hist(female_db$density)
unique(female_db$density)#23 Werte
##########################################################################################################################





