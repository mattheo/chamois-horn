

#require(Hmisc)
library(Hmisc)
#calculate a correlation matrix (round to 3 decimal places):
round(cor(db, use="complete.obs", method="kendall"),3)
# correlation cluster
v <- as.formula(paste("~", names(db), collapse="+"))
plot(varclus(v, data=db))
#r 0.7^2=0.5
abline(h=0.5,col="red")