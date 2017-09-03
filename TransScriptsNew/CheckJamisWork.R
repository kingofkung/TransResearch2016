## So in this script, I'll check if Dr. Taylor's calculations are sensible.

## read in Jami's work
source("/Users/bjr/GitHub/TransResearch2016/ScriptsForNewData/ReadInNewData.R")
ls()

getwd()
fileLoc <- "/Users/bjr/Dropbox/LGBT Interest group data/TransTeamDatFall17"


## begin by verifying the 1990 census numbers

## downloaded from https://www2.census.gov/census_1990/other/90partners.txt

ninetyP <- readLines("90partners.txt")
head(ninetyP, 100)

## line 27 has the col Letters, 29 has the 1st row
## easy reference location
ref <- ninetyP[1:26]

colLtrs <- ninetyP[27]

colDat <- ninetyP[29:length(ninetyP)]

## grab only the ones that have state code in the 1st 10 chars
stateInfo <- colDat[grepl("04000", substr(colDat, 1, 10))]
## split into columns
stateInfo <- trimws(stateInfo)

stList <- strsplit(stateInfo, " ")

## having split the string by spaces, we cut it down into only the elements we'll use.
stList <- lapply(seq_along(stList), function(z) stList[[z]][!stList[[z]] %in% ""])
## Now, we paste any values past 7 together into a single string
stList <- lapply(stList, function(x) c(x[1:7], paste(x[8:length(x)], collapse = " ")))

colDat <- do.call(rbind, stList)

## Better names, and get rid of the column that shows they're state rows
colnames(colDat) <- c("", "FIPS", "Total", "unmarriedOpp", "unmarMale", "unmarFem", "PostalCode", "state")
## That helps the census people, but we do not need it as a column
colDat <- colDat[, -1]
## make the numeric columns numeric
colDat[, 1:5] <- as.numeric(colDat[, 1:5])

## Whatever Dr. Taylor did checks out.
## here we make sure the columns are
all(unlist(lapply(1:ncol(colDat), function(u) all(colDat[, u] == census[, u]))))
