# helper function: inverse of intersect
outersect <- function(x, y) {
    sort(c(setdiff(x, y),
        setdiff(y, x)))
}

# read data
db_chamois1 <- read.csv("chamois1.csv", sep=";")
db_chamois2 <- read.csv("chamois2.csv", sep=";")


#rename columns so merging can work its magic
db_chamois1$Snow_cover_winter2 <- db_chamois1$Snow_cover_winter
db_chamois1$snow_winter2 <- db_chamois1$snow_winter
db_chamois1$twinter.max2 <- db_chamois1$twinter.max
db_chamois1$twinter.mean2 <- db_chamois1$twinter.mean
db_chamois1$twinter.min2 <- db_chamois1$twinter.min

# delete the old cloumns
db_chamois1$Snow_cover_winter <- NULL
db_chamois1$snow_winter <- NULL
db_chamois1$twinter.max <- NULL
db_chamois1$twinter.mean <- NULL
db_chamois1$twinter.min <- NULL

# delete columns excel has added
db_chamois2$X <- NULL
db_chamois2$X.1 <- NULL
db_chamois2$X.2 <- NULL

# merge the databases
db_merge <- merge(db_chamois1, db_chamois2, all.x=T)

# what columns are new in the new data set?
outersect(colnames(db_chamois1), colnames(db_chamois2))

# delete all unnecessary columns
drops <- c(
    "exc.date2", # excel numeric date
    "Julia.date", # year + Julian day
    "date", # year # Julian day + random number
    "x", # coordniates + small random step for spatial autocorrelation
    "y", # same
    "x2", # same
    "y2", # same
    "y_r", #same
    "x_r", #same
    "NS", # North-south facing component of aspect
    "EO", # East-West facing component of aspect
    "Nweight", # normalized weight
    "Nhorn", # normalized horn length
    "h_w", # ratio hor length to weight
    "index" # Indice of above ratio
)
db_merge <- db_merge[, !names(db_merge) %in% drops]
names(db_merge)


# consistency of kills in councils: not every year a chamois was shot in every council
with(db_merge, tapply(horn, list(council_cod, year), length))
# we need the consistent council to select which weather data to use


# make weather data consistent
# select all weather data
weather_data <- c(
    "snow_winter1",
    "Snow_cover_winter1",
    "r_apr_mag_1",
    "r_giu_lug_1",
    "r_ago_set_1",
    "r_spring1",
    "r_newsummer1",
    "r_autumn",
    "snow_winter2",
    "Snow_cover_winter2",
    "r_apr_mag_2",
    "r_giu_lug_2",
    "r_ago_set_2",
    "r_spring2",
    "r_newsummer2",
    "twinter.min1",
    "twinter.max1",
    "twinter.mean1",
    "tspring1.min",
    "tspring1.max",
    "tspring1.mean",
    "tsummer1.min",
    "tsummer1.max",
    "tsummer1.mean",
    "tautumn.min",
    "tautumn.max",
    "tautumn.mean",
    "twinter.min2",
    "twinter.max2",
    "twinter.mean2",
    "tspring2.min",
    "tspring2.max",
    "tspring2.mean",
    "tsummer2.min",
    "tsummer2.max",
    "tsummer2.mean"
)

# weather data from council 27 for each year
station27 <- unique(db_merge[db_merge$council_cod==27, c("year", weather_data)]) # not okay: year2007
# for the weather data in autumn, council27 has 2 values for the year 2007
station10 <- unique(db_merge[db_merge$council_cod==10, c("year", weather_data)])
station8 <- unique(db_merge[db_merge$council_cod==8, c("year", weather_data)])
# the weather stations used in council 8 and 10 are not representative for the whole area

station39 <- unique(db_merge[db_merge$council_cod==39, c("year", weather_data)]) # ok, snow, T, and Prec are from the higher stations both
station49 <- unique(db_merge[db_merge$council_cod==49, c("year", weather_data)]) # same as 39
# representative!



# merge the new weather data set into the old one
# removing the old weather columns first
db <- merge(db_merge[, !names(db_merge) %in% weather_data], station39, by="year", all=F)

# remove the old summer precipitation data
db$r_summer_1 <- NULL
db$r_summer_2 <- NULL

# remove old may ndvi. it's incorrect
db$ndvi.may1 <- NULL
db$ndvi.may2 <- NULL

# check for NA
with(db, tapply(snow_winter1, year, function(y) sum(is.na(y))))
summary(db$snow_winter1)
summary(db$r_autumn)
summary(db$tautumn.min)
# No NAs!


# merge count: which councils have integer weight, which have 0.5 and which have 0.1
# adds category
count <- read.csv("count.csv", sep=";")
db <- merge(db, count, by="council_cod", all.y=T)

# aspect is an unreliable predictor since its not equally distributed
# include aspect as factr: facing south and facing north
db$aspect <- as.factor(ifelse(db$asp < 90 | db$asp > 270, "N", "S"))

# transform sex to factors
db$f.sex <- as.factor(db$sex)
levels(db$f.sex) <- c("female", "male") # relevel sex

#transform substrate to factors
db$f.substrate <- as.factor(db$substrate)
levels(db$f.substrate) <- c("sili", "calc")

#factorize year
db$f.year <- as.factor(db$year)

# factorize council code
db$f.council_cod <- as.factor(db$council_cod)


#calculate log ndvi slope
head(sort(unique(db$ndvi.slop1)))
db$log.ndvi.slop1 <- log(db$ndvi.slop1 + 0.5*0.001)
db$log.ndvi.slop2 <- log(db$ndvi.slop2)

# add q_range to db
db$q_range <- db$q_max - db$q_min


# add pca for ndvi
with(db, {
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
})

# PCA of NDVI may year 1 and 2
db$ndvi.m1m2.pc1 <- pca1$x[, 1]
db$ndvi.m1m2.pc2 <- pca1$x[, 2]

# PCA of NDVI of summer year 1 and 2 and may year 1 and 2
db$ndvi.m1m2s1s2.pc1 <- pca2$x[, 1]

# PCA of NDVI of summer year and and 2
db$ndvi.s1s2.pc1 <-pca3$x[, 1]

# PCA of NDVI may1 and summer 1
db$ndvi.m1s1.pc1 <- pca4.1$x[, 1]
# PCA of NDVI may2 and summer 2
db$ndvi.m2s2.pc1 <- pca4.2$x[, 2]



save(db, file="db.RData")
