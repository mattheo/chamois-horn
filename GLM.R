
load("db.RData")

# male and female subsets------------------------------------------
male_db<-subset(db,sex ==  2)
female_db<-subset(db,sex == 1)

Glmhorn_m<-glm(horn~q_media+weight+Jday+ndvi.summer1+Perc.area.aperta,data=male_db,family="gaussian")
par(mfrow=c(1,1))

library(effects)
plot(effect("q_media",Glmhorn_m),color="blue",main="Horn Male GLM",lwd=3,ylab="Hornlength [mm]",xlab="Elevation")
summary(Glmhorn_m)

Glmhorn_f<-glm(horn~q_media+weight+Jday+ndvi.summer1+Perc.area.aperta,data=female_db,family="gaussian")
par(mfrow=c(1,1))

plot(effect("q_media",Glmhorn_f),color="red",main="Horn Female GLM",lwd=3,ylab="Hornlength [mm]",xlab="Elevation")

summary(Glmhorn_f)
