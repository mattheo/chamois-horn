load("db.RData")
library(Hmisc)
library(psych)
attach(db)


# correlation cluster
v <- as.formula(horn ~ sex + Jday + weight + x.council + y.council + q_media + q_min + q_max + substrate + density + log.ndvi.slop1 + ndvi.maxincr1 + ndvi.may1.new + log.ndvi.slop2 + ndvi.maxincr2 + ndvi.may2.new + Perc.area.aperta + snow_winter1 + r_spring1 + r_newsummer1 + r_autumn + snow_winter2 + r_spring2 + r_newsummer2 + twinter.mean1 + tspring1.mean + tsummer1.mean + tautumn.mean + twinter.mean2 + tspring2.mean + tsummer2.mean)

# pdf(file="varclus.pdf", height = 8, width = 14, pointsize = 12)
par(mar=c(1, 5, 1, 2))
plot(varclus(v, data=db, sim="pearson"), lwd=1.2)
abline(h=0.5, col="red")
# dev.off()

par(mar=c(1, 5, 1, 2))
plot(varclus(v, data=db, sim="spearman"), lwd=1.2)
abline(h=0.5, col="red")



with(db, cor(data.frame(q_media, q_max, q_min, q_range), method = "pearson"))
# substrate density
with(db, cor(substrate, density, method = "pearson"))
with(db, cor(substrate, density, method = "spearman"))
# temp summer rain autumn
cor(tsummer1.mean, r_autumn)

with(db, pairs.panels(data.frame(ndvi.may1.new, Perc.area.aperta, ndvi.may2.new, q_max, q_media, q_min, q_range), method = "spearman"))
# highly correlated
with(db, pairs.panels(data.frame(ndvi.may1.new, Perc.area.aperta, ndvi.may2.new, q_max, q_media, q_min, q_range), method = "pearson"))
# highly correlated
with(db, pairs.panels(data.frame(pc1.ndvi, pc2.ndvi, Perc.area.aperta, q_max, q_media, q_min, q_range), method = "pearson"))




v2 <- as.formula(horn ~ sex + Jday + weight + substrate + density + log.ndvi.slop1 + log.ndvi.slop2 + ndvi.maxincr1 + ndvi.maxincr2 + ndvi.m1m2.pc1 + ndvi.m1m2.pc2 + snow_winter1 + snow_winter2 + r_newsummer1 + r_newsummer2 + twinter.mean1 + tspring1.mean + tsummer1.mean + tautumn.mean + twinter.mean2 + tspring2.mean + tsummer2.mean)


par(mar=c(1, 5, 1, 2))
plot(varclus(v2, data=db, sim="pearson"), lwd=1.2)
abline(h=0.5, col="red")
# dev.off()

par(mar=c(1, 5, 1, 2))
plot(varclus(v2, data=db, sim="spearman"), lwd=1.2)
abline(h=0.5, col="red")



v3 <- as.formula(horn ~ f.sex + x.council + y.council + Jday + q_media + q_min + weight + f.substrate + density + ndvi.slop1 + ndvi.maxincr1 + ndvi.may1.new + ndvi.slop2 + ndvi.maxincr2 + ndvi.may2.new + Perc.area.aperta + snow_winter1 + r_newsummer1 + r_autumn + snow_winter2 + r_spring2 + r_newsummer2 + twinter.mean1 + tspring1.mean + tsummer1.mean + twinter.mean2 + tspring2.mean + tsummer2.mean)

plot(varclus(v2, data=db))
#r 0.7^2=0.5
abline(h=0.5,col="red")
