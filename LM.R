
load("db.RData")

# male and female subsets------------------------------------------
male_db<-subset(db,sex ==  2)
female_db<-subset(db,sex == 1)

# Analysis of elevation effect with a simlified GLM, sexes separated,
# useing variables which are ranked as relevant in the randomfores Importance analysis------------------------------------


Glmhorn_m<-glm(horn~q_media+weight+Jday+ndvi.summer1+Perc.area.aperta,data=male_db,family="gaussian")
summary(Glmhorn_m)#Estimate  q_media  -0.008671

library(effects)
plot(effect("q_media",Glmhorn_m),color="blue",main="Horn Male GLM",lwd=3,ylab="Hornlength [mm]",xlab="Elevation")

Glmhorn_f<-glm(horn~q_media+weight+Jday+ndvi.summer1+Perc.area.aperta,data=female_db,family="gaussian")
plot(effect("q_media",Glmhorn_f),color="red",main="Horn Female GLM",lwd=3,ylab="Hornlength [mm]",xlab="Elevation")
summary(Glmhorn_f)# Estimate q_media  -0.009838

#combined plot of two lines for glm male and female with fixed further variables
attach(db)

newele_f<- seq(from=min(female_db$q_media), to=max(female_db$q_media),by=1)
Preds_f<-predict(Glmhorn_f, newdata=data.frame("q_media"=newele_f, weight= median(weight), Jday= median(Jday),ndvi.summer1 = median(ndvi.summer1),Perc.area.aperta=median(Perc.area.aperta) ),se=TRUE)

newele_m<- seq(from=min(male_db$q_media), to=max(male_db$q_media),by=2)
Preds_m<-predict(Glmhorn_m, newdata=data.frame("q_media"=newele_m, weight= median(weight), Jday= median(Jday),ndvi.summer1 = median(ndvi.summer1),Perc.area.aperta=median(Perc.area.aperta) ),se=TRUE)


plot(horn~q_media,main= "LM Hornlength", xlab="Elevation",ylab="Hornlenght [mm]")
lines(Preds_f$fit~newele_f,col="red", lwd =2, lty =2)
lines(Preds_m$fit~newele_m,col="blue",lwd =2, lty =1)
legend("topright",lwd=c(2,2,2,2),lty= c(1,2,3,3),col=c("blue","red","blue","red"),legend=c("male","female","conf. male"," conf.female"))
text(c(900,900),c(122,180), labels =c("-0.009838", "-0.008671"), col= c("red","blue"))
lines(newele_m,Preds_m$fit+1.96*Preds_m$se.fit,col="blue",lwd =2, lty =3)
lines(newele_m,Preds_m$fit-1.96*Preds_m$se.fit,col="blue",lwd =2, lty =3)
lines(newele_f,Preds_f$fit+1.96*Preds_f$se.fit,col="red",lwd =2, lty =3)
lines(newele_f,Preds_f$fit-1.96*Preds_f$se.fit,col="red",lwd =2, lty =3)

# with increasing elevation female hornlength decreases slightly more than in male chamois.
# but the difference is small regarding the confidence intervall


#  LMER    ########################################################################
library(lme4)
LMERhorn_m<-lmer(horn~q_media+weight+Jday+ndvi.summer1+Perc.area.aperta+(1|council_cod)+(1|year),data=male_db, REML=FALSE)
summary(LMERhorn_m)
#q_media-0.005832
#ndvi.summer1 1.552955
#Perc.area.aperta -0.080928

LMERhorn_f<-lmer(horn~q_media+weight+Jday+ndvi.summer1+Perc.area.aperta+(1|council_cod)+(1|year),data=female_db, REML=FALSE)
summary(LMERhorn_f)
# q_media -0.011583
#ndvi.summer1 1.055086
#Perc.area.aperta -0.037595

#How to plot lmer ?
