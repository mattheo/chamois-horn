library(randomForest)
load("db.RData")

#Randomforest of selected Variables which are used in fcham1

RF1<-randomForest(horn ~ f.sex + (x.council) + (y.council) + (Jday) + (q_media) + (q_min) + (weight) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=db)
varImpPlot(RF1)
# Weight ist the most relevant predictor of horn size followed by Julianday 

# male and female subsets------------------------------------------
male_db<-subset(db,sex == 2)
female_db<-subset(db,sex == 1)

save(male_db, file="male_db.RData")
save(female_db, file="female_db.RData")


#Randomforest male
RF_male1<-randomForest(horn ~ (x.council) + (y.council) + (Jday) + (q_media) + (q_min) + (weight) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=male_db)
varImpPlot(RF_male1)
#Randomforest female
RF_female1<-randomForest(horn ~ (x.council) + (y.council) + (Jday) + (q_media) + (q_min) + (weight) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=female_db)
varImpPlot(RF_female1)
#more influence of Julianday on females

# Randomforest, both sexes, ommiting weight and Julianday 
RF2<-randomForest(horn ~ f.sex + (x.council) + (y.council) + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=db)
varImpPlot(RF2)
names(db)

#adding correlated ndvi.summer1
RF3<-randomForest(horn ~ f.sex + (x.council) + (y.council) + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) +(ndvi.may1.new) + (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=db)
varImpPlot(RF3)
# ndvi.summer.1 has more influence than ndvi.may.1.new 

#Sexes separated, ndvi summer added, Weight and Julianday ommitted---------------------------------------------------
#Randomforest male
RF_male2<-randomForest(horn ~ (x.council) + (y.council) + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new) + (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=male_db)
varImpPlot(RF_male2)
#Randomforest female
RF_female2<-randomForest(horn ~ (x.council) + (y.council) + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new) + (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=female_db)
varImpPlot(RF_female2
# for female less effect of may1'

#For both sexes  the rank of variables is (staring with the most relevant): weight, julianday, q_ media, ndvi.summer.1, ndvi.may.1.new

# ommitting weight,Julianday, ndvi.may.1 (correlated with summer 1),x council and y.council (correlated with topograpy)
#both sexes
RF4<-randomForest(horn ~ f.sex + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=db)
varImpPlot(RF4)


# the same only male #
RF_male3<-randomForest(horn ~  (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1)  + (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=male_db)
varImpPlot(RF_male3)
#only female # ntree = 5000 ca 1 min computing time
RF_female3<-randomForest(horn ~  (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=female_db)
varImpPlot(RF_female3)
#area aperta has more effect on female, q_ min has more effect on male

# both sexes
#omitting weight,Jday,x-council and y-council
#including both, ndvi.may1.new and ndvisummer1
RF5<-randomForest(horn ~ f.sex + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new)+ (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=db)
varImpPlot(RF5)


#Conclusion:(omitted x an y council)
#Weight is the most relevant predictor followed by Julianday. The average elevation is predictor on rank 3 followed by ndvisummer1and ndvi.may1.new. Thereafter follow percentage of open area and  minimum elevation (q_min).Density has about the same relevance as ndvimax.incr. As well ndvi.maxincrement as ndvi.slope are more relevant predictors than temperature, rain or snow. Also substrate and aspect play a minor role. The relevance of the selected variables for the horngrowth is about the same in both sexes. The small variability in weather data could be a reason of this ranking


#including x-council and y council

RF6<-randomForest(horn ~  (x.council) + (y.council) + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new)+ (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=db)
varImpPlot(RF6)

#    Weight   ##############################################################
RF_weight<-randomForest(horn ~  (x.council) + (y.council) + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new)+ (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=db)
par(mfrow=c(1,1))
varImpPlot(RF_weight)
RF_weight_m<-randomForest(horn ~  (x.council) + (y.council) + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new)+ (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=500, data=male_db)

par(mfrow=c(1,2))

RF_weight_f<-randomForest(horn ~  (x.council) + (y.council) + (q_media) + (q_min) + f.substrate + (density) + (ndvi.slop1) + (ndvi.maxincr1) + (ndvi.may1.new)+ (ndvi.summer1) + (ndvi.slop2) + (ndvi.maxincr2) + (Perc.area.aperta) + (snow_winter1) + (aspect) + (r_newsummer1) + (snow_winter2) + (r_spring2) + (r_newsummer2) + (twinter.mean1) + (tspring1.mean) + (tsummer1.mean) + (twinter.mean2) + (tspring2.mean) + (tsummer2.mean), ntree=1000, data=female_db)

varImpPlot(RF_weight_m)
varImpPlot(RF_weight_f)
# without q-media
RF_weight_m2 <- update(RF_weight_m, . ~ . - (q_media))
RF_weight_f2 <- update(RF_weight_f, . ~ . - (q_media))

varImpPlot(RF_weight_m2)
varImpPlot(RF_weight_f2)

# without council coordinates
RF_weight_m3 <- update(RF_weight_m2, . ~ . - x.council- y.council)
RF_weight_f3 <- update(RF_weight_f2, . ~ . - x.council -y.council)

varImpPlot(RF_weight_m3)
varImpPlot(RF_weight_f3)

# adding  ndvi.summer2-------------
RF_weight_m4 <- update(RF_weight_m3, . ~ . + ndvi.summer2)
RF_weight_f4 <- update(RF_weight_f3, . ~ . + ndvi.summer2)

varImpPlot(RF_weight_m4)
varImpPlot(RF_weight_f4)

#summer1 less effect on male, perc area aperta less effect on male,density less effect on male

RF_weight_m4 <- update(RF_weight_m3, . ~ . + ndvi.summer2)
RF_weight_f4 <- update(RF_weight_f3, . ~ . + ndvi.summer2)

varImpPlot(RF_weight_m4)
varImpPlot(RF_weight_f4)


