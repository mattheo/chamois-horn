library(mgcv)
load("db.RData")
male_db<-subset(db,sex ==  2)
female_db<-subset(db,sex == 1)

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

M_gam1 <- gam(horn ~  s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts") + s(ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=male_db, REML=FALSE)


F_gam1 <- gam(horn ~  s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts") + s(ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=female_db, REML=FALSE)

plot(M_gam1,main="male")
plot(F_gam1,main="female")

vis.gam(F_gam1,view = c("density","weight"),theta=25)


?vis.gam

summary(db)
boxplot(female_db$horn~female_db$sex)



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
# but the  number of samples is also different

summary(M_gam1)#38.6%
summary(F_gam1)#33.2%
AIC(M_gam1)#11975.4
AIC(F_gam1)#9922.889

# model female has a better fit than male.

plot(F_gam1)
hist(female_db$density)
unique(female_db$density)#23 Werte
##########################################################################################################################
#adding k in density
M_gam2 <- gam(horn ~  s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts",k=5) + s(log.ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(log.ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod, bs="re"), data=male_db, REML=FALSE)

F_gam2 <- gam(horn ~  s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts",k=5) + s(log.ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(log.ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts") + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=female_db, REML=FALSE)

summary(M_gam2)#38.6%
summary(F_gam2)#32.6%
AIC(M_gam2)#11976.52
AIC(F_gam2)#9924.477

plot(M_gam2,main="male")
plot(F_gam2,main="female")

plot(F_gam2,main="female",select=1)
plot(M_gam2,main="male",select=1)

plot(F_gam2,main="female",select=2)
plot(M_gam2,main="male",select=2)

plot(F_gam2,main="female",select=3)
plot(M_gam2,main="male",select=3)

plot(F_gam2,main="female",select=4)
plot(M_gam2,main="male",select=4)

plot(F_gam2,main="female",select=5)
plot(M_gam2,main="male",select=5)

plot(F_gam2,main="female",select=6)
plot(M_gam2,main="male",select=6)

plot(F_gam2,main="female",select=7)
plot(M_gam2,main="male",select=7)

plot(F_gam2,main="female",select=8)
plot(M_gam2,main="male",select=8)

plot(F_gam2,main="female",select=9)
plot(M_gam2,main="male",select=9)

plot(F_gam2,main="female",select=10)
plot(M_gam2,main="male",select=10)

plot(F_gam2,main="female",select=11)
plot(M_gam2,main="male",select=11)

plot(F_gam2,main="female",select=12)
plot(M_gam2,main="male",select=12)

plot(F_gam2,main="female",select=13)
plot(M_gam2,main="male",select=13)

plot(F_gam2,main="female",select=14)
plot(M_gam2,main="male",select=14)

#strong effect of ndvi.slope1 on female horne, but not on male
# model explains only 32.6% ((female) and 38.6% (male)deviance
#strong effect of ndvi.slope1 (p = 0.01276 *) on female horn, but not on male

##########################################################################################################################
# adding k in ndvi.maxincr2
M_gam3 <- gam(horn ~  s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts",k=5) + s(log.ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(log.ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts",k=5) + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=male_db, REML=FALSE)

F_gam3 <- gam(horn ~  s(x.council, y.council) + s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts",k=5) + s(log.ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(log.ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts",k=5) + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=female_db, REML=FALSE)

summary(M_gam3)#37.4%
summary(F_gam3)#32.2%

MOD_F<-(F_gam3)
MOD_M<-(M_gam3)

#plot(MOD_M,main="male")
#plot(F_gam3,main="female")

plot(MOD_F,main="female",select=1)
plot(MOD_M,main="male",select=1)

plot(MOD_F,main="female",select=2)
plot(MOD_M,main="male",select=2)

plot(MOD_F,main="female",select=3)
plot(MOD_M,main="male",select=3)

plot(MOD_F,main="female",select=4)
plot(MOD_M,main="male",select=4)

plot(MOD_F,main="female",select=5)
plot(MOD_M,main="male",select=5)

plot(MOD_F,main="female",select=6)
plot(MOD_M,main="male",select=6)

plot(MOD_F,main="female",select=7)
plot(MOD_M,main="male",select=7)

plot(MOD_F,main="female",select=8)
plot(MOD_M,main="male",select=8)

plot(MOD_F,main="female",select=9)
plot(MOD_M,main="male",select=9)

plot(MOD_F,main="female",select=10)
plot(MOD_M,main="male",select=10)

plot(MOD_F,main="female",select=11)
plot(MOD_M,main="male",select=11)

plot(MOD_F,main="female",select=12)
plot(MOD_M,main="male",select=12)

plot(MOD_F,main="female",select=13)
plot(MOD_M,main="male",select=13)

plot(MOD_F,main="female",select=14)
plot(MOD_M,main="male",select=14)

summary(M_gam3)
summary(F_gam3)

AIC(M_gam3)#11994.66
AIC(F_gam3)# 9922.954
AIC(M_gam2)#11976.52
AIC(F_gam2)#9924.477

# models with higher k have the lower AIC
#in male the horn growth has a roughly constant increase, in female the plot shows a curve which sems to approximate a maximum value. But there are not many samles in the male high weight range

vis.gam(F_gam3,view = c("density","weight"),theta=35,main="female")
vis.gam(M_gam3,view = c("density","weight"),theta=35,main="male")

# without X-y-council 

M_gam4 <- gam(horn ~ s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts",k=5) + s(log.ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(log.ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts",k=5) + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=male_db, REML=FALSE)

F_gam4 <- gam(horn ~ s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts",k=5) + s(log.ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(log.ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts",k=5) + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=female_db, REML=FALSE)

summary(M_gam4)#37.3%
summary(F_gam4)#32.1%
AIC(M_gam4)#11994.66
AIC(F_gam4)#9922.954

#significant male:
#  (Intercept)      1.423e+01  1.396e+00  10.196  < 2e-16 ***
# q_media          1.368e-01  6.981e-03  19.593  < 2e-16 ***
#  I(q_media^2)    -3.305e-05  3.526e-06  -9.371  < 2e-16 ***
#  f.substratecalc  1.029e+01  1.540e+00   6.683 3.36e-11 ***
#  s(Jday)              1.00006      1  74.191  < 2e-16 ***
#  s(weight)            2.86993      9 135.002  < 2e-16 ***
#  s(density)           0.77587      4   3.202  0.00654 **
#  s(ndvi.summer1)      1.94303      9   7.910  0.06796 .
#  s(ndvi.summer2)      0.80382      9   5.234  0.06347 .
#  s(f.year)            5.80497      8   4.664 1.90e-05 ***
#  s(f.council_cod)    31.61936     49   2.605 6.63e-15 ***
  

#significant female:
#  (Intercept)      1.156e+01  1.294e+00   8.937  < 2e-16 ***
#  q_media          1.378e-01  9.314e-03  14.793  < 2e-16 ***
#  I(q_media^2)    -3.952e-05  4.813e-06  -8.212 5.74e-16 ***
#  f.substratecalc  8.527e+00  1.885e+00   4.524 6.70e-06 ***  
#  s(Jday)              1.000132      1 48.722 4.82e-12 ***
#  s(weight)            2.826967      9 37.854  < 2e-16 ***
#  s(density)           2.414158      4  8.720   0.0374 * 
#  s(ndvi.summer2)      1.869967      9  5.621   0.0699 .
#  s(f.year)            5.488166      8  5.185 3.98e-06 ***
#  s(f.council_cod)    31.431896     48  1.579 6.99e-07 ***

#ndvi.summer1 only in male, density different categories
MOD_F<-(F_gam4)
MOD_M<-(M_gam4)

#plot(MOD_M,main="male")
#plot(F_gam3,main="female")

plot(MOD_F,main="female",select=1)
plot(MOD_M,main="male",select=1)

plot(MOD_F,main="female",select=2)
plot(MOD_M,main="male",select=2)

plot(MOD_F,main="female",select=3)
plot(MOD_M,main="male",select=3)

plot(MOD_F,main="female",select=4)
plot(MOD_M,main="male",select=4)

plot(MOD_F,main="female",select=5)
plot(MOD_M,main="male",select=5)

plot(MOD_F,main="female",select=6)
plot(MOD_M,main="male",select=6)

plot(MOD_F,main="female",select=7)
plot(MOD_M,main="male",select=7)

plot(MOD_F,main="female",select=8)
plot(MOD_M,main="male",select=8)

plot(MOD_F,main="female",select=9)
plot(MOD_M,main="male",select=9)

plot(MOD_F,main="female",select=10)
plot(MOD_M,main="male",select=10)

plot(MOD_F,main="female",select=11)
plot(MOD_M,main="male",select=11)

plot(MOD_F,main="female",select=12)
plot(MOD_M,main="male",select=12)

plot(MOD_F,main="female",select=13)
plot(MOD_M,main="male",select=13)

plot(MOD_F,main="female",select=14)
plot(MOD_M,main="male",select=14)

M_gam5 <- gam(horn ~ s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts",k=5) + s(log.ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(log.ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts",k=5) + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=male_db, REML=FALSE)

F_gam5 <- gam(horn ~ s(Jday) + (q_media) + I(q_media^2) + s(q_min,bs="ts") + s(weight, bs="ts") + f.substrate + s(density, bs="ts",k=5) + s(log.ndvi.slop1, bs="ts") + s(ndvi.maxincr1, bs="ts") + s(ndvi.summer1, bs="ts") + s(ndvi.summer2,bs="ts") + s(log.ndvi.slop2, bs="ts") + s(ndvi.maxincr2, bs="ts",k=5) + s(Perc.area.aperta ,bs="ts") + s(f.year, bs="re") + s(f.council_cod,bs="re"), data=female_db, REML=FALSE)

MOD_F<-(F_gam5)
MOD_M<-(M_gam5)

plot(MOD_F,main="female",select=1)
plot(MOD_M,main="male",select=1)

plot(MOD_F,main="female",select=2)
plot(MOD_M,main="male",select=2)

plot(MOD_F,main="female",select=3)
plot(MOD_M,main="male",select=3)

plot(MOD_F,main="female",select=4)
plot(MOD_M,main="male",select=4)

plot(MOD_F,main="female",select=5)
plot(MOD_M,main="male",select=5)

plot(MOD_F,main="female",select=6)
plot(MOD_M,main="male",select=6)

plot(MOD_F,main="female",select=7)
plot(MOD_M,main="male",select=7)

plot(MOD_F,main="female",select=8)
plot(MOD_M,main="male",select=8)

plot(MOD_F,main="female",select=9)
plot(MOD_M,main="male",select=9)

plot(MOD_F,main="female",select=10)
plot(MOD_M,main="male",select=10)

plot(MOD_F,main="female",select=11)
plot(MOD_M,main="male",select=11)

plot(MOD_F,main="female",select=12)
plot(MOD_M,main="male",select=12)

plot(MOD_F,main="female",select=13)
plot(MOD_M,main="male",select=13)

plot(MOD_F,main="female",select=14)
plot(MOD_M,main="male",select=14)

summary(M_gam5)
summary(F_gam5)
# neues Gam ##############################################

#PC: (ndvi.may1.new, ndvi.may2.new, ndvi.summer1, ndvi.summer2)



system.time(
  F_fcham_c5 <-  gam(horn ~
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
                     data=female_db, REML=F)
)

summary(F_fcham_c5)# 32.2%
summary(M_fcham_c5)# 39.1%
AIC(F_fcham_c5)#  9911.109
AIC(M_fcham_c5)# 11960.16
#rename for plotting routine------------------------------------------
MOD_F<-(F_fcham_c5)
MOD_M<-(M_fcham_c5)

par(mfrow=c(1,2))

plot(MOD_F,main="female",select=1)
plot(MOD_M,main="male",select=1)

plot(MOD_F,main="female",select=2)
plot(MOD_M,main="male",select=2)

plot(MOD_F,main="female",select=3)
plot(MOD_M,main="male",select=3)

plot(MOD_F,main="female",select=4)
plot(MOD_M,main="male",select=4)

plot(MOD_F,main="female",select=5)
plot(MOD_M,main="male",select=5)

plot(MOD_F,main="female",select=6)
plot(MOD_M,main="male",select=6)

plot(MOD_F,main="female",select=7)
plot(MOD_M,main="male",select=7)

plot(MOD_F,main="female",select=8)
plot(MOD_M,main="male",select=8)

plot(MOD_F,main="female",select=9)
plot(MOD_M,main="male",select=9)

plot(MOD_F,main="female",select=10)
plot(MOD_M,main="male",select=10)

plot(MOD_F,main="female",select=11)
plot(MOD_M,main="male",select=11)

plot(MOD_F,main="female",select=12)
plot(MOD_M,main="male",select=12)

plot(MOD_F,main="female",select=13)
plot(MOD_M,main="male",select=13)

plot(MOD_F,main="female",select=14)
plot(MOD_M,main="male",select=14)

plot(MOD_F,main="female",select=15)
plot(MOD_M,main="male",select=15)

plot(MOD_F,main="female",select=16)
plot(MOD_M,main="male",select=16)

plot(MOD_F,main="female",select=17)
plot(MOD_M,main="male",select=17)

plot(MOD_F,main="female",select=18)
plot(MOD_M,main="male",select=18)

plot(MOD_F,main="female",select=19)
plot(MOD_M,main="male",select=19)

plot(MOD_F,main="female",select=20)
plot(MOD_M,main="male",select=20)

plot(MOD_F,main="female",select=21)
plot(MOD_M,main="male",select=21)

#adding Perc.area.aperta because of different effect on sexes in M_gam1 and F_gam1
F_fcham_c5.1 <- update(F_fcham_c5, . ~ . + s(Perc.area.aperta, bs="ts"))
M_fcham_c5.1 <- update(M_fcham_c5, . ~ . + s(Perc.area.aperta, bs="ts"))

summary(F_fcham_c5.1)# 32.3% (0.1 better than c5) but (F_gam2)#32.6%,F_gam1 33.2%
summary(M_fcham_c5.1)# 39.1% (same as c5)
AIC(F_fcham_c5.1)#  9910.396 (slightly better than c5)
AIC(M_fcham_c5.1)#  11960.16 (same as c5)

#compare also F_gam1 which has more deviance explained, buthigher AIC-----------------

#summary(M_gam1)#38.6%
#summary(F_gam1)#33.2%
#AIC(M_gam1)#11975.4
#AIC(F_gam1)#9922.889

M,main="male",select=22)
names(db)
#ndvi.summer1 insted of PC 
F_fcham_c5.2 <- update(F_fcham_c5.1, . ~ . - s(pc1.ndvi, bs="ts") -
                         s(pc2.ndvi, bs="ts") + s(ndvi.summer1, bs="ts"))
M_fcham_c5.2 <- update(M_fcham_c5.1, . ~ . - s(pc1.ndvi, bs="ts") -
                         s(pc2.ndvi, bs="ts") + s(ndvi.summer1, bs="ts"))

summary(F_fcham_c5.2)# 31.8% -> worse
AIC(F_fcham_c5.2)# 9918.308 -> worse
summary(M_fcham_c5.2)# 38.3%
AIC(M_fcham_c5.2)# 11965.76 better than c5.1

#q_media instead of ndvi.summer1 
F_fcham_c5.3 <- update(F_fcham_c5.2, . ~ . - s(ndvi.summer1, bs="ts") +
                         s(q_media, bs="ts"))
M_fcham_c5.3 <- update(M_fcham_c5.2, . ~ . - s(ndvi.summer1, bs="ts") +
                         s(q_media, bs="ts"))
                           
summary(F_fcham_c5.3)#32.1%  better than c5.2 but c5.1 better 32.3%
AIC(F_fcham_c5.3)#9908.092 best so far  
summary(M_fcham_c5.3)#38.3%
AIC(M_fcham_c5.3)#11965.76
    
# New gam differing PCI´s ################################
# Each model separately correlation ckecked###############

# only may1+ may2
#varclus
library(Hmisc)
attach(female_db)
vFx1 <- as.formula(horn ~

                       Jday +
                       f.substrate +
                       year +
                       council_cod +
                       weight +
                       density +
                       log.ndvi.slop1 +
                       log.ndvi.slop2 +
                       ndvi.maxincr1 +
                       ndvi.maxincr2 +
                       ndvi.m1m2.pc1 +
                       ndvi.m1m2.pc2 + 
                       snow_winter1 +
                       snow_winter2 +
                       r_newsummer1 +
                       r_newsummer2 +
                       twinter.mean1 +
                       twinter.mean2 +
                       tspring1.mean +
                       tspring2.mean +
                       tsummer1.mean +
                       tsummer2.mean +
                       tautumn.mean)
par(mfrow=c(1,1))
plot(varclus(vFx1, data=female_db))
abline(h=0.5,col="red")

attach(male_db)
vMx1 <- as.formula(horn ~
                     
                     Jday +
                     f.substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     ndvi.m1m2.pc1 +
                     ndvi.m1m2.pc2 + 
                     snow_winter1 +
                     snow_winter2 +
                     r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     tsummer1.mean +
                     tsummer2.mean +
                     tautumn.mean)

plot(varclus(vMx1, data=male_db))
abline(h=0.5,col="red")

#t_summer1.mean und snow winter2 correlated in male and female
#r_newsummer1 and tautum.mean correlated in male and female


#model omitting snow-winter2 and r_newsummer1

  M_x1.1 <-  gam(horn ~
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
                 s(ndvi.m1m2.pc1, bs="ts") +
                 s(ndvi.m1m2.pc2, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x1.1)# 11950.88

F_x1.1 <-  gam(horn ~
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
                 s(ndvi.m1m2.pc1, bs="ts") +
                 s(ndvi.m1m2.pc2, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=female_db, REML=F)



AIC(F_x1.1)# 9915.984

#model omitting snow winter and tautumn.mean

M_x1.2 <-  gam(horn ~
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
                 s(ndvi.m1m2.pc1, bs="ts") +
                 s(ndvi.m1m2.pc2, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x1.2)# 11950.85

F_x1.2 <-  gam(horn ~
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
                 s(ndvi.m1m2.pc1, bs="ts") +
                 s(ndvi.m1m2.pc2, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
                 #s(tautumn.mean, k=3, bs="ts")
               ,data=female_db, REML=F)



AIC(F_x1.2)# 9915.661

#omitting  tsummer1.mean and r_newsummer1

M_x1.3 <-  gam(horn ~
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
                 s(ndvi.m1m2.pc1, bs="ts") +
                 s(ndvi.m1m2.pc2, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x1.3)# 11952.13

F_x1.3 <-  gam(horn ~
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
                 s(ndvi.m1m2.pc1, bs="ts") +
                 s(ndvi.m1m2.pc2, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=female_db, REML=F)



AIC(F_x1.3)# 9917.68

#############
#omitting  tsummer1.mean and tautumn.mean

M_x1.4 <-  gam(horn ~
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
                 s(ndvi.m1m2.pc1, bs="ts") +
                 s(ndvi.m1m2.pc2, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
                 #s(tautumn.mean, k=3, bs="ts")
               ,data=male_db, REML=F)

AIC(M_x1.4)#  11952.24

F_x1.4 <-  gam(horn ~
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
                 s(ndvi.m1m2.pc1, bs="ts") +
                 s(ndvi.m1m2.pc2, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
                 #s(tautumn.mean, k=3, bs="ts")
               ,data=female_db, REML=F)



AIC(F_x1.4)# 9912.556



###  NDVI  only summer1 and summer2     ##########################################
#varclus

vMx2 <- as.formula(horn ~
                     
                     Jday +
                     substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     ndvi.s1s2.pc1 + 
                     snow_winter1 +
                     snow_winter2 +
                     r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     tsummer1.mean +
                     tsummer2.mean +
                     tautumn.mean)

plot(varclus(vMx2, data=male_db))
abline(h=0.5,col="red")

vFx2 <- as.formula(horn ~
                     
                     Jday +
                     substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     ndvi.s1s2.pc1 + 
                     snow_winter1 +
                     snow_winter2 +
                     r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     tsummer1.mean +
                     tsummer2.mean +
                     tautumn.mean)

plot(varclus(vFx2, data=female_db))
abline(h=0.5,col="red")

#same as M/F_x1:
#t_summer1.mean und snow winter2 correlated in male and female
#r_newsummer1 and tautum.mean correlated in male and female


#model omitting snow-winter2 and r_newsummer1

M_x2.1 <-  gam(horn ~
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
                 s(ndvi.s1s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x2.1)#11965.54 

F_x2.1 <-  gam(horn ~
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
                 s(ndvi.s1s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=female_db, REML=F)



AIC(F_x2.1)# 9918.739

#model omitting snow winter and tautumn.mean

M_x2.2 <-  gam(horn ~
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
                 s(ndvi.s1s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
                 #s(tautumn.mean, k=3, bs="ts")
               ,data=male_db, REML=F)

AIC(M_x2.2)# 11965.54

F_x2.2 <-  gam(horn ~
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
                 s(ndvi.s1s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=female_db, REML=F)



AIC(F_x2.2)# 9918.593

#omitting  tsummer1.mean and r_newsummer1

M_x2.3 <-  gam(horn ~
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
                 s(ndvi.s1s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x2.3)# 11965.75

F_x2.3 <-  gam(horn ~
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
                 s(ndvi.s1s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=female_db, REML=F)



AIC(F_x2.3)# 9916.444

#############
#omitting  tsummer1.mean and tautumn.mean

M_x2.4 <-  gam(horn ~
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
                 s(ndvi.s1s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=male_db, REML=F)

AIC(M_x2.4)# 11965.75

F_x2.4 <-  gam(horn ~
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
                 s(ndvi.s1s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=female_db, REML=F)



AIC(F_x2.4)# 9916.444
#####################################################################################################################################
#####################################################################################################################################
# PCA  may1 and summer1
#varclus

vMx3 <- as.formula(horn ~
                     
                     Jday +
                     substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     ndvi.m1s1.pc1 + 
                     snow_winter1 +
                     snow_winter2 +
                     r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     tsummer1.mean +
                     tsummer2.mean +
                     tautumn.mean)

plot(varclus(vMx3, data=male_db))
abline(h=0.5,col="red")

vFx3 <- as.formula(horn ~
                     
                     Jday +
                     substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     ndvi.m1s1.pc1 + 
                     snow_winter1 +
                     snow_winter2 +
                     r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     tsummer1.mean +
                     tsummer2.mean +
                     tautumn.mean)

plot(varclus(vFx3, data=female_db))
abline(h=0.5,col="red")

#same as M/F_x1:
#t_summer1.mean und snow winter2 correlated in male and female
#r_newsummer1 and tautum.mean correlated in male and female


#model omitting snow-winter2 and r_newsummer1

M_x3.1 <-  gam(horn ~
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
                 s(ndvi.m1s1.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x3.1)#  11966.55

F_x3.1 <-  gam(horn ~
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
                 s(ndvi.m1s1.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=female_db, REML=F)



AIC(F_x3.1)# 9919.03

#model omitting snow winter and tautumn.mean

M_x3.2 <-  gam(horn ~
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
                 s(ndvi.m1s1.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=male_db, REML=F)

AIC(M_x3.2)# 11966.55

F_x3.2 <-  gam(horn ~
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
                 s(ndvi.m1s1.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=female_db, REML=F)



AIC(F_x3.2)#9919.03 

#omitting  tsummer1.mean and r_newsummer1

M_x3.3 <-  gam(horn ~
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
                 s(ndvi.m1s1.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x3.3)#11965.3 

F_x3.3 <-  gam(horn ~
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
                 s(ndvi.m1s1.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=female_db, REML=F)



AIC(F_x3.3)# 9916.294

#############
#omitting  tsummer1.mean and tautumn.mean

M_x3.4 <-  gam(horn ~
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
                 s(ndvi.m1s1.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=male_db, REML=F)

AIC(M_x3.4)# 11965.3

F_x3.4 <-  gam(horn ~
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
                 s(ndvi.m1s1.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=female_db, REML=F)



AIC(F_x3.4)#9916.348
 

# no more gam problem (maxit= 600)

#############################################
##############################################


# PCA  may2 and summer2
#varclus

vMx4 <- as.formula(horn ~
                     
                     Jday +
                     substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     ndvi.m2s2.pc1 + 
                     snow_winter1 +
                     snow_winter2 +
                     r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     tsummer1.mean +
                     tsummer2.mean +
                     tautumn.mean)

plot(varclus(vMx4, data=male_db))
abline(h=0.5,col="red")

vFx4 <- as.formula(horn ~
                     
                     Jday +
                     substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     ndvi.m2s2.pc1 + 
                     snow_winter1 +
                     snow_winter2 +
                     r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     tsummer1.mean +
                     tsummer2.mean +
                     tautumn.mean)

plot(varclus(vFx4, data=female_db))
abline(h=0.5,col="red")

#same as M/F_x1:
#t_summer1.mean und snow winter2 correlated in male and female
#r_newsummer1 and tautum.mean correlated in male and female


#model omitting snow-winter2 and r_newsummer1

M_x4.1 <-  gam(horn ~
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
                 s(ndvi.m2s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x4.1)#  11963.36

F_x4.1 <-  gam(horn ~
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
                 s(ndvi.m2s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=female_db, REML=F)



AIC(F_x4.1)#9918.633

#model omitting snow winter and tautumn.mean

M_x4.2 <-  gam(horn ~
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
                 s(ndvi.m2s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=male_db, REML=F)

AIC(M_x4.2)# 11962.41

F_x4.2 <-  gam(horn ~
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
                 s(ndvi.m2s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 #s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=female_db, REML=F)



AIC(F_x4.2)#9918.758

#omitting  tsummer1.mean and r_newsummer1

M_x4.3 <-  gam(horn ~
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
                 s(ndvi.m2s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=male_db, REML=F)

AIC(M_x4.3)#11963.18 

F_x4.3 <-  gam(horn ~
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
                 s(ndvi.m2s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts") +
                 s(tautumn.mean, k=3, bs="ts"),
               data=female_db, REML=F)



AIC(F_x4.3)# 9917.623

#############
#omitting  tsummer1.mean and tautumn.mean

M_x4.4 <-  gam(horn ~
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
                 s(ndvi.m2s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=male_db, REML=F)

AIC(M_x4.4)# 11963.18

F_x4.4 <-  gam(horn ~
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
                 s(ndvi.m2s2.pc1, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
               #s(tautumn.mean, k=3, bs="ts")
               ,data=female_db, REML=F)

AIC(F_x4.4)#  9917.623
summary(F_x4.4)

#########################################################################################


# only may1+ may2
#varclus
library(Hmisc)
attach(female_db)
vFx1 <- as.formula(horn ~
                     
                     Jday +
                     f.substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     ndvi.m1m2.pc1 +
                     ndvi.m1m2.pc2 + 
                     snow_winter1 +
                     snow_winter2 +
                     r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     tsummer1.mean +
                     tsummer2.mean +
                     tautumn.mean)
par(mfrow=c(1,1))
plot(varclus(vFx1, data=female_db))
abline(h=0.5,col="red")

######################################
#resume in female omitting  tsummer1.mean and tautumn.mean (.4) has mostly the best AIC, in male no trend
#next models only using snow winter2 and r_newsummer1
#################################################################
#wintersnow and summerrain has more effect on female than on male !!!!!!!!!!!!!! see table
#################################################################
#next models only using snow winter2 and r_newsummer1

#M/F x1 using m1m2 has lowest AIC in male and female

# q _ media##############################################################################

names (male_db)
vMq <- as.formula(horn ~
                     
                     Jday +
                     f.substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     q_media + 
                     snow_winter1 +
                     snow_winter2 +
                     #r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     #tsummer1.mean +
                     tsummer2.mean 
                     #tautumn.mean
                  )

plot(varclus(vMq, data=male_db))
abline(h=0.5,col="red")

vFq <- as.formula(horn ~
                    
                    Jday +
                    f.substrate +
                    year +
                    council_cod +
                    weight +
                    density +
                    log.ndvi.slop1 +
                    log.ndvi.slop2 +
                    ndvi.maxincr1 +
                    ndvi.maxincr2 +
                    q_media + 
                    snow_winter1 +
                    snow_winter2 +
                    #r_newsummer1 +
                    r_newsummer2 +
                    twinter.mean1 +
                    twinter.mean2 +
                    tspring1.mean +
                    tspring2.mean +
                    #tsummer1.mean +
                    tsummer2.mean 
                  #tautumn.mean
                    )

plot(varclus(vFq, data=female_db))
abline(h=0.5,col="red")

#now all good


Mq1 <-  gam(horn ~
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
                 s(q_media, bs="ts") +
                 s(snow_winter1, k=3, bs="ts") +
                 s(snow_winter2, k=3, bs="ts") +
                 #s(r_newsummer1, k=3, bs="ts") +
                 s(r_newsummer2, k=3, bs="ts") +
                 s(twinter.mean1, k=3, bs="ts") +
                 s(twinter.mean2, k=3, bs="ts") +
                 s(tspring1.mean, k=3, bs="ts") +
                 s(tspring2.mean, k=3, bs="ts") +
                 #s(tsummer1.mean, k=3, bs="ts") +
                 s(tsummer2.mean, k=3, bs="ts")
                 #s(tautumn.mean, k=3, bs="ts")
               ,data=male_db, REML=F)
            
AIC(Mq1)#11966.58

Fq1 <-  gam(horn ~
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
              s(q_media, bs="ts") +
              s(snow_winter1, k=3, bs="ts") +
              s(snow_winter2, k=3, bs="ts") +
              #s(r_newsummer1, k=3, bs="ts") +
              s(r_newsummer2, k=3, bs="ts") +
              s(twinter.mean1, k=3, bs="ts") +
              s(twinter.mean2, k=3, bs="ts") +
              s(tspring1.mean, k=3, bs="ts") +
              s(tspring2.mean, k=3, bs="ts") +
              #s(tsummer1.mean, k=3, bs="ts") +
              s(tsummer2.mean, k=3, bs="ts")
              #s(tautumn.mean, k=3, bs="ts")
            ,data=female_db, REML=F)

AIC(Fq1)# 9913.439
summary(Fq1)


#Perc area aperta  #########################################################################################################


names (male_db)
vMaa <- as.formula(horn ~
                    
                    Jday +
                    f.substrate +
                    year +
                    council_cod +
                    weight +
                    density +
                    log.ndvi.slop1 +
                    log.ndvi.slop2 +
                    ndvi.maxincr1 +
                    ndvi.maxincr2 +
                    Perc.area.aperta + 
                    snow_winter1 +
                    snow_winter2 +
                    #r_newsummer1 +
                    r_newsummer2 +
                    twinter.mean1 +
                    twinter.mean2 +
                    tspring1.mean +
                    tspring2.mean +
                    #tsummer1.mean +
                    tsummer2.mean 
                   #tautumn.mean
)

plot(varclus(vMaa, data=male_db))
abline(h=0.5,col="red")

vFaa <- as.formula(horn ~
                     
                     Jday +
                     f.substrate +
                     year +
                     council_cod +
                     weight +
                     density +
                     log.ndvi.slop1 +
                     log.ndvi.slop2 +
                     ndvi.maxincr1 +
                     ndvi.maxincr2 +
                     Perc.area.aperta + 
                     snow_winter1 +
                     snow_winter2 +
                     #r_newsummer1 +
                     r_newsummer2 +
                     twinter.mean1 +
                     twinter.mean2 +
                     tspring1.mean +
                     tspring2.mean +
                     #tsummer1.mean +
                     tsummer2.mean 
                   #tautumn.mean
)

plot(varclus(vFaa, data=female_db))
abline(h=0.5,col="red")
#allgood

# all good


Maa1 <-  gam(horn ~
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
              s(Perc.area.aperta, bs="ts") +
              s(snow_winter1, k=3, bs="ts") +
              s(snow_winter2, k=3, bs="ts") +
              #s(r_newsummer1, k=3, bs="ts") +
              s(r_newsummer2, k=3, bs="ts") +
              s(twinter.mean1, k=3, bs="ts") +
              s(twinter.mean2, k=3, bs="ts") +
              s(tspring1.mean, k=3, bs="ts") +
              s(tspring2.mean, k=3, bs="ts") +
              #s(tsummer1.mean, k=3, bs="ts") +
              s(tsummer2.mean, k=3, bs="ts")
            #s(tautumn.mean, k=3, bs="ts")
            ,data=male_db, REML=F)

AIC(Maa1)#11965.76
summary(Maa1)

Faa1 <-  gam(horn ~
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
              s(Perc.area.aperta, bs="ts") +
              s(snow_winter1, k=3, bs="ts") +
              s(snow_winter2, k=3, bs="ts") +
              #s(r_newsummer1, k=3, bs="ts") +
              s(r_newsummer2, k=3, bs="ts") +
              s(twinter.mean1, k=3, bs="ts") +
              s(twinter.mean2, k=3, bs="ts") +
              s(tspring1.mean, k=3, bs="ts") +
              s(tspring2.mean, k=3, bs="ts") +
              #s(tsummer1.mean, k=3, bs="ts") +
              s(tsummer2.mean, k=3, bs="ts")
              #s(tautumn.mean, k=3, bs="ts")
            ,data=female_db, REML=F)

AIC(Faa1)# 9916.232
