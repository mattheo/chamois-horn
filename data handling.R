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

