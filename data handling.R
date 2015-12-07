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

# delete columns in second dataset: coordinates
db_chamois2$x_r <- NULL
db_chamois2$y_r <- NULL

# create new Database
db <- merge(db_chamois1, db_chamois2, all.x=T)


# delete all unnecessary columns
drops <- c(
    "exc.date2", # excel numeric date
    "Julia.date", # year + Julian day
    "date", # year # Julian day + random number
    "x", # coordniates + small random step for spatial autocorrelation
    "y", # same
    "x2", # same
    "y2", # same
    "NS", # North-south facing component of aspect
    "EO", # East-West facing component of aspect
    "NWeight", # normalized weight
    "Nhorn", # normalized horn length
    "h_w", # ratio hor length to weight
    "index" # Indice of above ratio
)

db <- db[, !names(db) %in% drops]
head(db)



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
