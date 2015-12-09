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

# check for NA
with(db, tapply(snow_winter1, year, function(y) sum(is.na(y))))
summary(db$snow_winter1)
summary(db$r_autumn)
summary(db$tautumn.min)
# No NAs!








# collinearity in elevation data
cor(db[c("q_media", "q_min", "q_max")])
db$q_range <- db$q_max - db$q_min
with(db, cor(q_media, q_range))
with(db, plot(q_media, q_range)) # not okay yet


# collinearity of NAO and others
cor.test(db$nao_w, db$twinter.mean2)
cor.test(db$nao_w, db$twinter.min2)
cor.test(db$nao_w, db$snow_winter2)

cor.test(db$nao_d, db$snow_winter2)




# what columns are new in the new data set?
outersect(colnames(db_chamois1), colnames(db_chamois2))





# data transformation
with(db_chamois1, tapply(snow_winter2, list(area_cod, year), mean)) # --> only 2 station, weather data in area1 are mixed

with(db_chamois1, tapply(twinter.mean, list(area_cod, year), mean))

with(db_chamois1, tapply(tspring1.mean, list(area_cod, year), mean)) # changed since 2009

# inconsistent relationship between areas

# how big is the variance between areas?
with(db, tapply(twinter.min2, year, summary)) # second winter, minimum T
with(db, tapply(twinter.max2, year, summary)) # second winter, max T

# summary of first winter
with(db, tapply(twinter.mean1, year, summary))

# values of first winter per year and area
with(db, tapply(twinter.mean1, list(area_cod, year), unique))
