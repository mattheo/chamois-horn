db_chamois1 <- read.csv("~/Studium/R Projects/topic 5 - chamois horn/chamois1.csv", sep=";")

db_chamois2 <- read.csv("~/Studium/R Projects/topic 5 - chamois horn/chamois2.csv", sep=";")

db <- merge(db_chamois1, db_chamois2, all.x=T)

outersect <- function(x, y) {
    sort(c(setdiff(x, y),
        setdiff(y, x)))
}

# what columns are new in the new data set?
outersect(colnames(db_chamois1), colnames(db_chamois2))


# where are the data holes

with(db_chamois2, tapply(twinter.m)
