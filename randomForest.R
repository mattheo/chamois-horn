library(randomForest)
load("db.RData")
f.sex<-as.factor(db$sex)

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


#Conclusion:
#Weight is the most relevant predictor followed by Julianday. The average elevation is predictor on rank 3 followed by ndvisummer1. Thereafter follow percentage of open area, ndvi.may1.new, sex and  minimum elevation (q_min).Density has about the same relevance as ndvimax.incr. As well ndvi.maxincrement as ndvi.slope are more relevant predictors than temperature, rain or snow. Also substrate and aspect play a minor role. The relevance of the selected variables for the horngrowth is about the same in both sexes. The small variability in weather data could be a reason of this ranking
