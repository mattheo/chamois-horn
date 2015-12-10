

#require(Hmisc)
library(Hmisc)
#calculate a correlation matrix (round to 3 decimal places):
round(cor(db, use="complete.obs", method="kendall"),3)
# correlation cluster
v <- as.formula(horn ~ Jday + council_cod + x.council + y.council + area_cod + asp + q_max + q_media + q_min + sex + weight + substrate + density + ndvi.slop1 + ndvi.maxincr1 + ndvi.may1.new + ndvi.slop2 + ndvi.maxincr2 + ndvi.may2.new + Perc.area.aperta + snow_winter1 + Snow_cover_winter1 + r_spring1 + r_newsummer1 + r_autumn + snow_winter2 + Snow_cover_winter2 + r_spring2 + r_newsummer2 + twinter.min1 + twinter.mean1 + twinter.max1 + tspring1.min + tspring1.mean + tspring1.max + tsummer1.min + tsummer1.mean + tsummer1.max + tautumn.min + tautumn.mean + tautumn.max + twinter.min2 + twinter.mean2 + twinter.max2 + tspring2.min + tspring2.mean + tspring2.max + tsummer2.min + tsummer2.mean + tsummer2.max)
plot(varclus(v, data=db))
#r 0.7^2=0.5
abline(h=0.5,col="red")
