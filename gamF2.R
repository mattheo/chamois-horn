library(mgcv)
load("db.RData")
male_db<-subset(db,sex ==  2)
female_db<-subset(db,sex == 1)




# New gam differing PCI´s #############################################################################################################
# Each model separately correlation ckecked###########################################################################################

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
                 
                 s(density,k=3, bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(Jday,bs="ts") + 
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2,k=4, bs="ts") +
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

AIC(M_x1.1)# 11923.48


F_x1.1 <-  gam(horn ~
                 s(density,k=3, bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(Jday,bs="ts") + 
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2,k=4, bs="ts") +
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



AIC(F_x1.1)# 9924.014
gam.check(F_x1.1)
par(mfrow=c(1,1))
vis.gam(F_x1.1,view=c("weight","density"),theta=50)


#rename for plotting routine------------------------------------------
MOD_F<-(F_x1.1)
MOD_M<-(M_x1.1)

par(mfrow=c(1,2))

plot(MOD_F,main="female",all=T,select=1)
plot(MOD_M,main="male",all=T,select=1)

plot(MOD_F,main="female",all=T,select=2)
plot(MOD_M,main="male",all=T,select=2)

plot(MOD_F,main="female",all=T,select=3)
plot(MOD_M,main="male",all=T,select=3)

plot(MOD_F,main="female",all=T,select=4)
plot(MOD_M,main="male",all=T,select=4)

plot(MOD_F,main="female",all=T,select=5)
plot(MOD_M,main="male",all=T,select=5)

plot(MOD_F,main="female",all=T,select=6)
plot(MOD_M,main="male",all=T,select=6)

plot(MOD_F,main="female",all=T,select=7)
plot(MOD_M,main="male",all=T,select=7)

plot(MOD_F,main="female",all=T,select=8)
plot(MOD_M,main="male",all=T,select=8)

plot(MOD_F,main="female",all=T,select=9)
plot(MOD_M,main="male",all=T,select=9)

plot(MOD_F,main="female",all=T,select=10)
plot(MOD_M,main="male",all=T,select=10)

plot(MOD_F,main="female",all=T,select=11)
plot(MOD_M,main="male",all=T,select=11)

plot(MOD_F,main="female",all=T,select=12)
plot(MOD_M,main="male",all=T,select=12)

plot(MOD_F,main="female",all=T,select=13)
plot(MOD_M,main="male",all=T,select=13)

plot(MOD_F,main="female",all=T,select=14)
plot(MOD_M,main="male",all=T,select=14)

plot(MOD_F,main="female",all=T,select=15)
plot(MOD_M,main="male",all=T,select=15)

plot(MOD_F,main="female",all=T,select=16)
plot(MOD_M,main="male",all=T,select=16)

plot(MOD_F,main="female",all=T,select=17)
plot(MOD_M,main="male",all=T,select=17)

plot(MOD_F,main="female",all=T,select=18)
plot(MOD_M,main="male",all=T,select=18)

plot(MOD_F,main="female",all=T,select=19)
plot(MOD_M,main="male",all=T,select=19)

plot(MOD_F,main="female",all=T,select=20)
plot(MOD_M,main="male",all=T,select=20)

plot(MOD_F,main="female",all=T,select=21)
plot(MOD_M,main="male",all=T,select=21)


#model omitting snow winter and tautumn.mean

M_x1.2 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2,k=4, bs="ts") +
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

AIC(M_x1.2)# 11923.48

F_x1.2 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2,k=4, bs="ts") +
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



AIC(F_x1.2)# 9922.036

#omitting  tsummer1.mean and r_newsummer1

M_x1.3 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2,k=4, bs="ts") +
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

AIC(M_x1.3)# 11926.67

F_x1.3 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2,k=4, bs="ts") +
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



AIC(F_x1.3)# 9923.202

#############
#omitting  tsummer1.mean and tautumn.mean

M_x1.4 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2, k=4, bs="ts") +
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

AIC(M_x1.4)#  11926.67

F_x1.4 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2,k=4, bs="ts") +
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



AIC(F_x1.4)# 9921.959



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
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.s1s2.pc1,k=4, bs="ts") +
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

AIC(M_x2.1)# 11925.25

F_x2.1 <-  gam(horn ~
                 s(Jday,bs="ts")+
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.s1s2.pc1,k=4, bs="ts") +
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



AIC(F_x2.1)# 9923.85
#model omitting snow winter and tautumn.mean

M_x2.2 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.s1s2.pc1,k=4, bs="ts") +
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

AIC(M_x2.2)#  11925.9

F_x2.2 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=2, bs="ts") +
                 s(ndvi.s1s2.pc1,k=2, bs="ts") +
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



AIC(F_x2.2)# 9922.789

#omitting  tsummer1.mean and r_newsummer1

M_x2.3 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.s1s2.pc1,k=4, bs="ts") +
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

AIC(M_x2.3)# 11927.6

F_x2.3 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.s1s2.pc1,k=4, bs="ts") +
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



AIC(F_x2.3)# 9925.405

#############
#omitting  tsummer1.mean and tautumn.mean

M_x2.4 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.s1s2.pc1,k=4, bs="ts") +
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

AIC(M_x2.4)# 11927.6

F_x2.4 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.s1s2.pc1,k=4, bs="ts") +
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



AIC(F_x2.4)# 9925.488
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
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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

AIC(M_x3.1)#  11926.46

F_x3.1 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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



AIC(F_x3.1)# 9924.476

#model omitting snow winter and tautumn.mean

M_x3.2 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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

AIC(M_x3.2)# 11926.46

F_x3.2 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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



AIC(F_x3.2)#9922.43 

#omitting  tsummer1.mean and r_newsummer1

M_x3.3 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density, k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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

AIC(M_x3.3)#11928.63

F_x3.3 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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



AIC(F_x3.3)# 9921.486

#############
#omitting  tsummer1.mean and tautumn.mean

M_x3.4 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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

AIC(M_x3.4)# 11928.61

F_x3.4 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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



AIC(F_x3.4)#9921.486
 

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
                 s(Jday,bs="ts")+
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m2s2.pc1,k=4, bs="ts") +
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

AIC(M_x4.1)#  11924.08

F_x4.1 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m2s2.pc1,k=4, bs="ts") +
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



AIC(F_x4.1)#9924.199

#model omitting snow winter and tautumn.mean

M_x4.2 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m2s2.pc1,k=4, bs="ts") +
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

AIC(M_x4.2)# 11924.08

F_x4.2 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=2, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m2s2.pc1,k=4, bs="ts") +
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



AIC(F_x4.2)#9923.389

#omitting  tsummer1.mean and r_newsummer1

M_x4.3 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m2s2.pc1,k=4, bs="ts") +
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

AIC(M_x4.3)#11927.15 

F_x4.3 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m2s2.pc1,k=4, bs="ts") +
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



AIC(F_x4.3)# 9926.057

#############
#omitting  tsummer1.mean and tautumn.mean

M_x4.4 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m2s2.pc1,k=4, bs="ts") +
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

AIC(M_x4.4)# 11924.93

F_x4.4 <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m2s2.pc1,k=4, bs="ts") +
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

AIC(F_x4.4)#  9924.151
summary(F_x4.4)

#########################################################################################


# only may1+ may2
#varclus
library(Hmisc)
attach(female_db)
vFx1 <- as.formula(horn ~
                     
                     s(Jday,bs="ts") +
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
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(q_media,k=4, bs="ts") +
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

par(mfrow=c(1,1))
plot(Mq1,select = 10)
#for q_media  bs="ts" would be sufficient but k=4 because due to necessarity in the female mode

AIC(Mq1)#11928.13


Fq1 <-  gam(horn ~
              s(Jday,bs="ts") +
              f.substrate +
              s(f.year, bs="re") +
              s(f.council_cod, bs="re") +
              s(weight, bs="ts") +
              s(density,k=3, bs="ts") +
              s(log.ndvi.slop1, bs="ts") +
              s(log.ndvi.slop2, bs="ts") +
              s(ndvi.maxincr1, bs="ts") +
              s(ndvi.maxincr2,k=4, bs="ts") +
              s(q_media,k=4, bs="ts") +
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

plot(Fq1,select = 10)
#q_media k=4 applied 
AIC(Fq1)# 9922.87
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
              s(Jday,bs="ts") +
              f.substrate +
              s(f.year, bs="re") +
              s(f.council_cod, bs="re") +
              s(weight, bs="ts") +
              s(density, k=3, bs="ts") +
              s(log.ndvi.slop1, bs="ts") +
              s(log.ndvi.slop2, bs="ts") +
              s(ndvi.maxincr1, bs="ts") +
              s(ndvi.maxincr2,k=4, bs="ts") +
              s(Perc.area.aperta,k=3, bs="ts") +
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

plot(Maa1,select = 10)
#ok --

AIC(Maa1)#11927.25
summary(Maa1)

Faa1 <-  gam(horn ~
              s(Jday,bs="ts") +
              f.substrate +
              s(f.year, bs="re") +
              s(f.council_cod, bs="re") +
              s(weight, bs="ts") +
              s(density,k=3, bs="ts") +
              s(log.ndvi.slop1, bs="ts") +
              s(log.ndvi.slop2, bs="ts") +
              s(ndvi.maxincr1, bs="ts") +
              s(ndvi.maxincr2, bs="ts") +
              s(Perc.area.aperta,k=3, bs="ts") +
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

plot(Faa1,select = 10)
#k was reduce in M/Faa1 to k=3 because of a better modelling in Faa1
gam.check(Faa1)
summary(Faa1)

AIC(Faa1)# 9924.67


#Best models with REML=T#########################################################################

#best male model with REML=T

M_x1.1T <-  gam(horn ~
                 
                 s(density,k=3, bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(Jday,bs="ts") + 
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1m2.pc1,k=4, bs="ts") +
                 s(ndvi.m1m2.pc2,k=4, bs="ts") +
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
               data=male_db, REML=T)

AIC(M_x1.1T)#11923.48

# best female model with REML=T
F_x3.4T <-  gam(horn ~
                 s(Jday,bs="ts") +
                 f.substrate +
                 s(f.year, bs="re") +
                 s(f.council_cod, bs="re") +
                 s(weight, bs="ts") +
                 s(density,k=3, bs="ts") +
                 s(log.ndvi.slop1, bs="ts") +
                 s(log.ndvi.slop2, bs="ts") +
                 s(ndvi.maxincr1, bs="ts") +
                 s(ndvi.maxincr2,k=4, bs="ts") +
                 s(ndvi.m1s1.pc1,k=4, bs="ts") +
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
               ,data=female_db, REML=T)

AIC(F_x3.4T)#9921.486
