
path.data<-"C:/Users/Franz/Documents/Uni/A3/R_Projects/Chamois/chamois-horn/"

db_chamois1<- read.table(paste(path.data, "chamois1.csv", sep=""), dec=".", sep=";", header=T)
db_chamois2<- read.table(paste(path.data, "chamois2.csv", sep=""), dec=".", sep=";", header=T)

#data exploration---horn weight sex ------------

boxplot(horn~sex, data = db_chamois1)
boxplot(weight~sex, data = db_chamois1)

attach(db_chamois1)
hist(weight,breaks =50)
plot(horn~weight,data=db_chamois1)

#spatial distribution---------------------------------------
plot(x.council,y.council,col=area_cod,pch=20,cex=1.6)


#   analyse horn--- elevation--------------------------
tapply(horn,area_cod,mean)    
#  1        2        3        4        6 
#140.0377 142.6841 158.0197 143.0943 137.3182 
tapply(q_media,area_cod,mean)
# 1        2        3        4        6 
#1923.472 1700.743 1186.555 1736.614 2138.228

#lowest mean elevation biggest mean horn

library(mgcv)

class(sex) #integer
f.sex<-as.factor(sex)# transform to factor

#first simple model--------horn sex  weight -------------------------------------------
m1<-gam(horn~s(weight)+f.sex+s(q_media),data=db_chamois1)
vis.gam(m1,theta =40)
gam.check(m1)#model check

# spatial model---------------------------------------
terrmod<-gam(horn~s(x.council,y.council),data=db_chamois1)
vis.gam(terrmod,plot.type="contour")
points(x.council,y.council,col=area_cod,pch=20,cex=1.6)


#analyse Weather area data -----------------------------------------------------
with(db_chamois1,tapply(tsummer2.mean, list(area_cod), unique))

#$`1`
#[1] 21.52 19.59 19.38 18.50 20.21 19.54 16.54 19.86

#$`2`
#[1] 19.21 18.71 18.81 19.92 18.44 18.54 15.69 18.66

#$`3`
#[1] 19.21 18.71 18.81 19.92 20.11 19.89 17.99 21.02

#$`4`
#[1] 21.52 19.59 19.38 18.50 20.21 19.54 16.54 19.86

#$`6`
#[1] 17.56 16.07 17.26 18.10 18.44 18.54 16.54 18.66

with(db_chamois1,tapply(snow_winter,list(area_cod,year),unique))

with(db_chamois1,tapply(snow_winter,list(council_cod,year,area_cod),unique))
#long call#

# explore influence of Julianday---------------------------------------------------------------

range (Jday)#[1] 247 364
max(Jday)-min(Jday)#timespan 117 days
#simple models-----------------
Julgam1<-gam(horn~s(Jday)+s(weight), data=db_chamois1)
?vis.gam
vis.gam(Julgam1,plot.type="persp",theta=45, main="horn~Jday + weight",zlab="horn")
summary(Julgam1)


Julgam2<-gam(horn~s(Jday, bs="cs")+s(weight,bs="cs"), data=db_chamois1)
vis.gam(Julgam2,plot.type="persp",theta=45)

Julgam3<-gam(weight~s(Jday)+f.sex, data=db_chamois1)
vis.gam(Julgam3,plot.type="persp",theta=45, main="weight~Jday + sex",zlab="weight")
summary(Julgam3)
par(mfrow=c(1,1))
plot(Julgam3)
##################################  continue with merged set db  ############################################################


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

  