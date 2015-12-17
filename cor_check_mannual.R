# Collinearity check for singel elements

single <- cbind(twinter.min1, twinter.max1, twinter.mean1, twinter.mean2, tspring1.min, tspring1.max, tspring1.mean,
                tspring2.min, tspring2.max, tspring2.mean, tsummer2.min, tsummer2.max, tsummer2.mean, tautumn.min,
                ndvi.maxincr1 , ndvi.slop1, ndvi.maxincr2, ndvi.slop2, q_min)


cor(single)


'tspring1.min/twinter.min1   tspring2.mean/twinter.min1  
tspring1.max/twinter.max1  tspring2.max/twinter.max1 tautumn.min/twinter.max1
tspring1.min/twinter.mean1  tspring2.mean/twinter.mean1  tautumn.min/twinter.mean1
tsummer2.min/tspring1.min
tautumn.min/tspring1.max  ndvi.maxincr2/tspring1.max
tspring2.max/tspring1.mean tautumn.min/tspring1.mean
tsummer2.max/tspring2.min
tautumn.min/tspring2.max
tsummer2.max/tsummer2.min  ndvi.slop1/tsummer2.min
ndvi.slop1/tsummer2.mean'

db3 <- subset(db, sel= -c(horn, id, data, x.council, y.council, area_cod, MEAN_105, MEAN_121, MEAN_137, MEAN_153, MEAN_169, MEAN_185, MEAN_201, MEAN_217, MEAN_233, MEAN_249, value, category, asp, aspect, f.sex, f.substrate, f.year, f.council_cod, ndvi.slop1, ndvi.slop2, ndvi.may1.new, ndvi.may2.new, ndvi.summer1, ndvi.summer2, q_max, ndvi.m1m2s1s2.pc1, ndvi.m1s1.pc1, ndvi.m2s2.pc1, ndvi.s1s2.pc1))


 vif_func(in_frame = db3, thresh = 5, trace = T)
