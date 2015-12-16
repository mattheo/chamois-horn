library(Hmisc)
attach(db)


# First, we test for correlation inside of logical groups of predictors. There we assume that a correlation coefficient lower than 0.7 for Pearsons r are not collinear.
# on the other hand, we use a varclus plot to indicate collinear clusters. The threshhold here is 0.5 for spearmans rho²


# collinearity in elevation data ###########

pairs.panels(cbind(q_media, q_min, q_range, q_max))
# here only q_max is not good

# calculate elevation relative to mean
q_relmin <- q_min - q_media
q_relmax <- q_max - q_media

pairs.panels(cbind(q_relmin, q_media, q_relmax))
# this approach is worse.


####
# Conclusion
####
#
# q_media, q_min and q_range can stay as pedictors for elevation




# collinearity of NAO and NAO with other climate data ##################

pairs.panels(cbind(nao_a1, nao_a2, nao_d, nao_f, nao_j, nao_m, nao_w))
# NAO for months is correlated; NAO for winter, year1 and year2 could be useful

# how is the correlation of nao with temperature?
cor(db$nao_w, db$twinter.mean2)
cor(db$nao_w, db$twinter.min2)
cor(db$nao_w, db$twinter.max2)
# nao is correlated with minimum winter temps

cor.test(db$nao_w, db$snow_winter2)
# and also with snow depth

# Varcluster of all climate data
f1 <- formula(horn ~ nao_a1 + nao_a2 + nao_w + r_apr_mag_1 + r_giu_lug_1 + r_ago_set_1 + r_apr_mag_2 + r_giu_lug_2 + r_ago_set_2 + r_spring1 + r_spring2 + r_newsummer1 + r_newsummer2 + r_autumn + snow_winter1 + snow_winter2 + Snow_cover_winter1 + Snow_cover_winter2 + twinter.min1 + twinter.max1 + twinter.mean1 + twinter.min2 + twinter.max2 + twinter.mean2 + tautumn.min + tautumn.max + tautumn.mean + tspring1.min + tspring1.max + tspring1.mean + tspring2.min + tspring2.max + tspring2.mean + tsummer1.min + tsummer1.max + tsummer1.mean + tsummer2.min + tsummer2.max + tsummer2.mean)

plot(varclus(f1, data=db))
abline(h=0.5, col="red")

# a lot of climate predictors are highly collinear. some of them are not logically interpretable like r_spring1 with snow_cover_winter2

# too much information, how to simplify?

# different approach: first check for correlation inside smaller groups



# precipitation data ############
prec1 <- cbind(r_apr_mag_1, r_giu_lug_1, r_ago_set_1, r_spring1, r_newsummer1, r_autumn)
prec2 <- cbind(r_apr_mag_2, r_giu_lug_2, r_ago_set_2, r_spring2, r_newsummer2)

pairs.panels(prec1, main="precipitation 1")
pairs.panels(prec2, main="precipitation 2")

# bimonthly precipitation for both years
prec_all <- cbind(r_apr_mag_1, r_giu_lug_1, r_ago_set_1, r_apr_mag_2, r_giu_lug_2, r_ago_set_2)
pairs.panels(prec_all)
# all is good except for r_ago_set and r_giu_lug2. pick one, but test for both

# what about rain and snow?
pairs.panels(cbind(prec_all, snow_winter1, snow_winter2))
pairs.panels(cbind(prec_all, Snow_cover_winter1, Snow_cover_winter2))
# snow cover winter is not correlated! snow winter is with rain data

# approach: either bimonthly data or seasonal data
# high correlation between r_summer1 and r_autumn1
# bimonthly data is uncorrelated
# so bimonthly prec data mmight be better



# temperature data ############

winter1 <- cbind(horn, twinter.min1, twinter.mean1, twinter.max1)
pairs.panels(winter1, main = "Winter1")
# ok

# second winter
winter2 <- cbind(horn, twinter.min2, twinter.mean2, twinter.max2)
pairs.panels(winter2, main = "Winter2")
# ok

# first spring
spring1 <- cbind(horn, tspring1.min, tspring1.mean, tspring1.max)
pairs.panels(spring1, main = "Spring1")
# here, everything can stay

# second spring
spring2 <- cbind(horn, tspring2.min, tspring2.mean, tspring2.max)
pairs.panels(spring2, main = "Spring2")
# here, everything can stay

# first summer
summer1 <- cbind(horn, tsummer1.min, tsummer1.mean, tsummer1.max)
pairs.panels(summer1, main = "summer1")
# tsummer1_mean is highly correlated with both max and min. it should not be used.

# second summer
summer2 <- cbind(horn, tsummer2.min, tsummer2.mean, tsummer2.max)
pairs.panels(summer2, main = "summer2")
# here, everything can stay

# autumn
autumn <- cbind(horn, tautumn.min, tautumn.mean, tautumn.max)
pairs.panels(autumn, main = "Autumn")
# tautumn.mean with tautumn.max at the threshold

plot(varclus(autumn))
abline(h=0.5)



## conclusion so far:
# for snow: snow_cover_winter
#
# for Temperature:
#       twinter.min1, twinter.mean1, twinter.max1
#       twinter.min2, twinter.mean2, twinter.max2
#       tspring1.min, tspring1.mean, tspring1.max
#       tspring2.min, tspring2.mean, tspring2.max
#       tsummer1.min, tsummer1.max
#       tsummer2.min, tsummer2.mean, tsummer2.max
#       tautumn.min, tautumn.mean, tautumn.max, but at threshold
#
# for precipitation:
#       r_apr_mag_1, r_giu_lug_1, r_apr_mag_2, r_ago_set_2
#       either r_ago_set_1 or r_giu_lug_2


ftemp1 <- formula(horn ~ twinter.min1 + twinter.max1 + twinter.mean1 + tspring1.min + tspring1.max + tspring1.mean + tsummer1.min + tsummer1.max + tautumn.min + tautumn.max + tautumn.mean)

ftemp2 <- formula(horn ~ twinter.min2 + twinter.max2 + twinter.mean2 + tspring2.min + tspring2.max + tspring2.mean + tsummer2.min + tsummer2.max + tsummer2.mean)


plot(varclus(ftemp1))
abline(h=0.5, col="red")
# tautumn.max collinear with twinter.min1; tautumn.max probably less important

ftemp1.1 <- update(ftemp1, . ~ . - tautumn.max)
plot(varclus(ftemp1.1))
abline(h=0.5, col="red")

ftemp1.2 <- update(ftemp1.1, . ~ . - tspring1.min)
plot(varclus(ftemp1.2))
abline(h=0.5, col="red")


plot(varclus(ftemp2))
abline(h=0.5, col="red")


# collnearity of NDVI
ndvi1 <- cbind(horn, ndvi.maxincr1, ndvi.may1.new, ndvi.slop1, ndvi.summer1)
ndvi2 <- cbind(horn, ndvi.maxincr2, ndvi.may2.new, ndvi.slop2, ndvi.summer2)

ndvi1 <- cbind(horn, ndvi.maxincr1, ndvi.may1.new, log.ndvi.slop1, ndvi.summer1)
ndvi2 <- cbind(horn, ndvi.maxincr2, ndvi.may2.new, log.ndvi.slop2, ndvi.summer2)
pairs.panels(ndvi1, main = "NDVI1", method = "pearson")

# as expected, NDVI in may and summer are highly correlated. Pick One.
# however, ndvi.slop is heavily skewed towards 0
summary(ndvi.slop1)
sum(ndvi.slop1 == 0) # only 1!
sum(ndvi.slop1 < 0.1) # but more then 2000 are smaller than 0.1
sum(ndvi.slop1 > 0.1)
# log transform?
log.ndvi.slop1 <- log(ndvi.slop1 + 0.5*0.001)
hist(log.ndvi.slop1)
summary(log.ndvi.slop1)
# maybe better, ask simone!

pairs.panels(ndvi2, main = "NDVI2")
# same for the second year: may and summer highly correlated
summary(ndvi.slop2) # no zeros
log.ndvi.slop2 <- log(ndvi.slop2)
hist(log.ndvi.slop2)
# maybe better?


ndvib <- cbind(ndvi.maxincr1, ndvi.may1.new, log.ndvi.slop1, ndvi.maxincr2, ndvi.may2.new, log.ndvi.slop2)
pairs.panels(ndvib, main = "both NDVI")



## PCA ###################

# may ndvis
pca1 <- prcomp(cbind(ndvi.may1.new, ndvi.may2.new), scale=T)
summary(pca1)
# two PCS explain enough variance
round(pca1$rotation, 2)
biplot(pca1)

# all ndvis
pca2 <- prcomp(cbind(ndvi.may1.new, ndvi.may2.new, ndvi.summer1, ndvi.summer2), scale=T)
summary(pca2)
# two are enough
biplot(pca2)

# summer ndvis
pca3 <- prcomp(cbind(ndvi.summer1, ndvi.summer2), scale=T)
summary(pca3)
# pc1 is enough
biplot(pca3)

# ndvi year one and ndvi year two seperated
pca4.1 <- prcomp(cbind(ndvi.may1.new, ndvi.summer1), scale=T)
pca4.2 <- prcomp(cbind(ndvi.may2.new, ndvi.summer2), scale=T)

summary(pca4.1)
summary(pca4.2)
# for both, pc1 is enough
cor(pca4.1$x[, 1], pca4.2$x[, 1])
# cannot use both, highly correlated
