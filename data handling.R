db_chamois1 <- read.csv("chamois1.csv", sep=";")
db_chamois2 <- read.csv("chamois2.csv", sep=";")

db <- merge(db_chamois1, db_chamois2, all.x=T)

outersect <- function(x, y) {
    sort(c(setdiff(x, y),
        setdiff(y, x)))
}

# what columns are new in the new data set?
outersect(colnames(db_chamois1), colnames(db_chamois2))


# where are the data holes




# data transfprmation
with(db_chamois1, tapply(snow_winter, list(area_cod, year), mean)) # --> only 2 station, weather data in area1 are mixed

with(db_chamois1, tapply(twinter.mean, list(area_cod, year), mean))

with(db_chamois1, tapply(tspring1.mean, list(area_cod, year), mean)) # changed since 2009