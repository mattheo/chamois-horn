
path.data<-"C:/Users/Franz/Documents/Uni/A3/R_Projects/Chamois/chamois-horn/"

db_chamois1<- read.table(paste(path.data, "chamois1.csv", sep=""), dec=".", sep=";", header=T)
db_chamois2<- read.table(paste(path.data, "chamois2.csv", sep=""), dec=".", sep=";", header=T)

#data exploration---horn, weight, sex ------------

#Horn--------------------------------------------

attach (db)

mean (horn[sex==2]) # male 152.3881
mean (horn[sex==1]) #female 129.9782
mean (horn[sex==2])-mean (horn[sex==1]) #22.40985



boxplot(horn~sex, data = db, main= "Hornlength ",ylab= "lengt [mm]", xlab= "female                                                                   male")
hist(horn[sex==1],breaks=40,freq=FALSE,col=rgb(1,0,0,0.1), main="Hornlength", xlab= "Length [mm]")
hist(horn[sex==2],breaks=40,freq=FALSE,add=TRUE,col=rgb(0,0,1,0.1))
?hist
lines(density(horn[sex==1]), col="red", lwd=2)
lines(density(horn[sex==2]), col="blue", lwd=2)
legend("topright",lwd=c(2,2),col=c("red","blue"),legend=c("female", "male"))

#male and female horn length differs at an average of 22,4 mm. The boxplot of the hornlength distribution shows two outliers or extreme values respectively in both groups, which should be further investigated 

plot (horn[sex==2], main = "Hornlength male", ylab = "Length [mm]")
plot (horn[sex==1],main = "Hornlength female", ylab = "Length [mm]")

# weight--------------------------------------------------
boxplot(weight~sex, data = db,main= "Weight ",ylab= "Weight [kg]", xlab= "female                                                                   male")
#In male chamois are again two outliers wereas there are none in female

hist(weight[sex==1],breaks=20,freq=FALSE,col=rgb(1,0,0,0.1), main="Weight", xlab= "Weight [kg]")
hist(weight[sex==2],breaks=20,freq=FALSE,add=TRUE,col=rgb(0,0,1,0.1))
lines(density(weight[sex==1]), col="red", lwd=2)
lines(density(weight[sex==2]), col="blue", lwd=2)
legend("topright",lwd=c(2,2),col=c("red","blue"),legend=c("female", "male"))

#the density distribution of the weight dara shows some irregularity in female chamois
# how can we tackle this ????
#The plot of horn length against weight shows some anomaly because of many integer values and some extreme values
plot(horn~weight,main= "Horn ~ Weight",xlab="Weight [kg]", ylab="Hornlengt [mm]")

#spatial distribution---------------------------------------
plot(x.council,y.council,col=area_cod,pch=20,cex=1.6, main = "Spatial Distribution")
legend("bottomright", legend=paste("Area", unique(db$area_cod)), col=unique(db$area_cod), pch=20)

#The spatial distribution of the data suggests spatial autocorrelation.This fact should be adressed in a model


#   analyse horn--- elevation--------------------------
tapply(horn,area_cod,mean)    
#  1        2        3        4        6 
#140.0377 142.6841 158.0197 143.0943 137.3182 
tapply(q_media,area_cod,mean)
# 1        2        3        4        6 
#1923.472 1700.743 1186.555 1736.614 2138.228

158.0197-137.3182 #20.7015
mean (horn)# 142.1995
sd(horn)#20.58488
mean (horn)/100*20.7015 #29.43743

#Animals hunted  at the lowest mean elevation area have the biggest mean horn length.Animals originating from the highest area have the smalest average horn length. There is a difference of 20.7015 mm between this groups. Thist span is 29% of the (overall) average horn length. The standard deviation of horn length is 20.6 mm

plot(horn~q_media)
hist (q_media,main= "Elevation",freq=FALSE, xlab="elevation a.s.l. [m]")
lines(density(q_media), col="purple", lwd=3)

library(mgcv)

class(sex) #integer
f.sex<-as.factor(sex)# transformation of sex data to class factor

#first simple model--------horn sex  weight -------------------------------------------
m1<-gam(horn~s(weight)+f.sex+s(q_media),data=db)
vis.gam(m1,theta =40, main ="Horn~Weight+Sex", zlab="hornlength",xlab ="female          male")
gam.check(m1)#model check, not great but accepable at this point. There are not all relevant variables included so far

# spatial model---------------------------------------
terrmod<-gam(horn~s(x.council,y.council),data=db)
vis.gam(terrmod,plot.type="contour",main="Spatial Distribution of Hornlength" )
points(x.council,y.council,col=area_cod,pch=20,cex=1.6)
legend("bottomright", legend=paste("Area", unique(db$area_cod)), col=unique(db$area_cod), pch=20)


# explore influence of Julianday---------------------------------------------------------------

range (Jday)#[1] 247 364
max(Jday)-min(Jday)#timespan 117 days

# The range of Julianday spans over a period of 117 days from day 247 to day 364 

hist(Jday, main= "Julianday",xlab="day of the year")
#the data are well distributed

plot (horn~Jday)

#Juldayloess-----

#horn
fmLoess_Jday <- loess(horn~Jday,family="gaussian",span=0.8)
newday <- seq(from=min(Jday), to=max(Jday),by=1)
pred_Horn <- predict(fmLoess_Jday, newday, se=TRUE)
plot(horn~Jday, main ="Loess Regression: Horn~Julianday",ylab="Hornlength [mm]",xlab="Day of the year",pch=1)
lines(pred_Horn$fit~newday,col="red", lwd=2)
abline(h=140,col="blue",lty=2,lwd=2)
legend("topright",lwd=c(2,2),lty=c(1,2),col=c("red","blue"),legend=c("loess regression", " fix value 140 mm"))
#The loess regression reveals about constant hornlength during the hunting season


#Weight
fmLoess_Weight <- loess(weight~Jday,family="gaussian",span=0.8)
newday <- seq(from=min(Jday), to=max(Jday),by=1)
pred_Weight <- predict(fmLoess_Weight, newday, se=TRUE)
plot(weight~Jday, main ="Loess Regression: Weight~Julianday",ylab="Weight [kg]",xlab="Day of the year",pch=1)
lines(pred_Weight$fit~newday,col="red", lwd=2)
abline(h=16,col="blue",lty=2,lwd=2)
legend("topright",lwd=c(2,2),lty=c(1,2),col=c("red","blue"),legend=c("loess regression", " fix value 16 kg"))
#The loess regression reveals about constant hornlength during the hunting season

female<-subset(db,sex==1)#1218
male<-subset(db,sex==2)#1461
#The dataset represents 1218 female and 1461 male chamois


#Loess_horn_male------------------------------------------------------------
fmLoess_Jday_m <- loess(male$horn~male$Jday,family="gaussian",span=0.8)
newday_m <- seq(from=min(male$Jday), to=max(male$Jday),by=1)
pred_Horn_m <- predict(fmLoess_Jday_m, newday_m, se=TRUE)
plot(horn~Jday, main ="Loess Regression: Horn~Julianday",ylab="Hornlength [mm]",xlab="Day of the year",pch=1)
lines(pred_Horn_m$fit~newday_m,col="blue", lwd=2)


#Loess horn Female -------------------------------------------------------
fmLoess_Jday_f <- loess(female$horn~female$Jday,family="gaussian",span=0.8)
newday_f <- seq(from=min(female$Jday), to=max(female$Jday),by=1)
pred_Horn_f <- predict(fmLoess_Jday_f, newday_f, se=TRUE)
lines(pred_Horn_f$fit~newday_f,col="red", lwd=2)

abline(h=140,col="darkgreen",lty=2,lwd=2)
abline(h=160,col="darkgreen",lty=2,lwd=2)
legend("topright",lwd=c(2,2,2),lty=c(1,1,2),col=c("blue","red","darkgreen"),legend=c("male","female", "fix value 140 mm & 160 mm"),cex=0.8)

#############################################################################################

#simple models-----------------

Julgam1<-gam(horn~s(Jday)+s(weight), data=db)
vis.gam(Julgam1,plot.type="persp",theta=45, main="horn~Jday + weight",zlab="horn")
summary(Julgam1)


Julgam2<-gam(horn~s(Jday, bs="cs")+s(weight,bs="cs"), data=db_chamois1)
vis.gam(Julgam2,plot.type="persp",theta=45)

Julgam3<-gam(weight~s(Jday)+f.sex, data=db_chamois1)
vis.gam(Julgam3,plot.type="persp",theta=45, main="weight~Jday + sex",zlab="weight")
summary(Julgam3)
par(mfrow=c(1,1))
plot(Julgam3)



db$data<-NULL#delete couumn containing data formate
# male and female subsets------------------------------------------
male_db<-subset(db,sex = 2)
female_db<-subset(db,sex = 1)
#scaled subsets---------------------------------------------------------------
sc_male<-as.data.frame(scale(male_db))
sc_female<-as.data.frame(scale(female_db))


list(names(sc_female))

#Randomforest NDVI
library(randomForest)
?randomForest
#watch computing time n= 2000 
RandomNDVI_male<-randomForest(horn~ (MEAN_105) + (MEAN_121) + (MEAN_137) + (MEAN_153) + (MEAN_169) + (MEAN_185) + (MEAN_201) + (MEAN_217) + (MEAN_233) + (MEAN_249) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.summer1) + (ndvi.may1) + (ndvi.slop2) + (ndvi.maxincr2 ) + (ndvi.summer2) + (ndvi.may2) + (ndvi.may1.new) + (ndvi.may2.new), ntree=2000, data = sc_male)
varImpPlot(RandomNDVI_male)

RandomNDVI_female<-randomForest(horn~ (MEAN_105) + (MEAN_121) + (MEAN_137) + (MEAN_153) + (MEAN_169) + (MEAN_185) + (MEAN_201) + (MEAN_217) + (MEAN_233) + (MEAN_249) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.summer1) + (ndvi.may1) + (ndvi.slop2) + (ndvi.maxincr2 ) + (ndvi.summer2) + (ndvi.may2) + (ndvi.may1.new) + (ndvi.may2.new), ntree=2000, data = sc_female)
varImpPlot

RandomNDVI_maleMEAN<-randomForest(horn~ (MEAN_105) + (MEAN_121) + (MEAN_137) + (MEAN_153) + (MEAN_169) + (MEAN_185) + (MEAN_201) + (MEAN_217) + (MEAN_233) + (MEAN_249), ntree=2000, data = sc_male)
varImpPlot(RandomNDVI_maleMEAN)

RandomNDVI_femaleMEAN<-randomForest(horn~ (MEAN_105) + (MEAN_121) + (MEAN_137) + (MEAN_153) + (MEAN_169) + (MEAN_185) + (MEAN_201) + (MEAN_217) + (MEAN_233) + (MEAN_249), ntree=2000, data = sc_female)
varImpPlot(RandomNDVI_femaleMEAN)

# NDVI Summer2, summer1, may1new, may2, MEAN 185 and MEAN 137 are the most important effects

#explore year effect
boxplot(weight~year, main = "Weight")
tapply(weight,list(year),mean)
#2005     2006     2007     2008     2009     2010     2011     2012 
#15.46646 15.40549 15.30142 15.95851 16.06113 15.27224 15.98993 15.67872

boxplot(horn~year,, main = "Horn")
tapply(horn,list(year),mean)
#2005     2006     2007     2008     2009     2010     2011     2012 
#142.3703 142.1738 145.4103 144.4403 140.2986 138.1275 139.7987 144.8367

#2009 max average weight but 2007 max average horn ???

# explore spatial effect

with(db,tapply(weight,list(area_cod,year),mean))


boxplot(weight~area_cod)
boxplot(horn~area_cod)
boxplot(horn~council_cod)
#Model open area , elevation
OE<-gam(horn~s(Perc.area.aperta,bs="cs")+s(q_media,bs="cs"))
vis.gam(OE,theta=45,phi=50,zlab="horn")
