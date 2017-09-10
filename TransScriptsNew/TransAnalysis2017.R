## Having read in the data on transgender policy, let us now get a few things taken care of and begin our analyses

## read in the data
source("/Users/bjr/GitHub/TransResearch2016/TransScriptsNew/ReadInNewData.R")

## we need to join the datasets together. That means converting from long to wide format in the case of the ssph files

head(ourdat)
## reshape williams measures to be a long dataset
head(ssphWill)

colnames(ssphWill) <- gsub("\\s", "", colnames(ssphWill))

library(reshape2)
library(dplyr)
library(tidyr)

## get the start/ending time varying columns' locations in the data
vrCls <- which(colnames(ssphWill) %in% c("1991ssph", "2015ssph", "Williamscomputed1990", "Williamscomputed2015"))

## grab just the time varying columns, see if that'll work
timeVar <- grep("Williamscomputed|\\dssph", colnames(ssphWill), value = TRUE)
ssphWill[, c("State" )]

## convert ssph williams measures from wide to long format
ssphWlLng <- gather(data = ssphWill[,c("State", timeVar) ], key = "ComputedValue", timeVar, -State )
head(ssphWlLng)
## fix measure
cVal <- ssphWlLng$ComputedValue
## way to just extract the digits?
## gsub did it for me
ssphWlLng$Year <- as.numeric(gsub("\\D", "", cVal))
## then, did the same with the measure information
ssphWlLng$measure <- gsub("\\d", "", cVal)
## create variable for accurate merging by state and Year
ssphWlLng$syr <- paste0(ssphWlLng$State, ssphWlLng$Year)

## repeat for ssphdt
head(ssphdt)
colnames(ssphdt) <- gsub("\\s", "", colnames(ssphdt))
## if you don't see ssphdt in the first column name, add it to all the column names...
## ... except for 3, which is state name
if (!grepl("ssphdt", colnames(ssphdt)[1])) colnames(ssphdt)[-3] <- paste0(colnames(ssphdt)[-3], "ssphdt")
##
## find the names of the time varying columns
timeVarDt <- grep("Williamscomputed|^\\d{4,}ssphdt", colnames(ssphdt), value = TRUE)
## And gather
ssphDtLng <- gather(ssphdt[, c("State", timeVarDt)], "CompVal", timeVarDt, -State)

ssphDtLng$yrDt <- gsub("\\D", "", ssphDtLng$CompVal)
ssphDtLng$measureDt <- gsub("\\d", "", ssphDtLng$CompVal)
## create same merger variable syr as in ssphWlLng
ssphDtLng$syrDt <- paste0(ssphDtLng$State, ssphDtLng$yrDt)


## merge in the two long datasets we just made
mergeDat <- ourdat
## get dimensions... when we need them. Working now...
## dim(mergeDat)
## table(ourdat$statename, ourdat$year)
mergeDat$stateYr <- paste0(mergeDat$statename, mergeDat$year)
##
ssphDtMatch <- match(mergeDat$stateYr, ssphDtLng$syrDt)
mergeDat <- data.frame(mergeDat, ssphDtLng[ssphDtMatch, ])
##
##
## get williams Measures in separately, Make the match as a variable
## Turns out the best way to do this is to create a subset of the long data,
## With Williams measures only
ssphDWlLng <- ssphDtLng[grepl("Williams", ssphDtLng$CompVal),]
WillDtMatch <- match(mergeDat$stateYr, ssphDWlLng$syrDt)
##
mergeDat <- data.frame(mergeDat,
                       "CompValWillDt" = ssphDWlLng[WillDtMatch, "CompVal"],
                       "timeVarDtWill" = ssphDWlLng[WillDtMatch, "timeVarDt"])
##
## add the merged data to the data.frame
mergeDat <- data.frame(mergeDat, ssphWlLng[match(mergeDat$stateYr, ssphWlLng$syr), ])
##
## and match the williams measure as well
ssphWWWlLng <- ssphWlLng[grepl("Williams", ssphWlLng$ComputedValue),]
WillssphMatch <- match(mergeDat$stateYr, ssphWWWlLng$syr)
## complete the merge
mergeDat <- data.frame(mergeDat, "CompValWill" = ssphWWWlLng[WillssphMatch, "ComputedValue"], "timeVarWill" = ssphWWWlLng[WillssphMatch, "timeVar"])
##
##
##
## verify that stateYr and syr/syrDt are the same. The diagonal of the
## table should always equal 1. It does! Success.
## with(mergeDat, table(stateYr, syrDt.2)[diag(length(unique(stateYr)))])
## clean up a bit... I have so many merged measures
head(mergeDat)
mergeDat <- mergeDat[, !grepl("syr|State\\.|CompVal", colnames(mergeDat))]

## see if we can't lag those last 2 variables
## get only the columns we want to lag (and the columns we'll need to do it)
splittrCols <- c('realastpercap_smallno234', 'realastpercapall', "timeVar", "timeVarWill",  "timeVarDt", "timeVarDtWill", 'Year', "statename", "stateYr")
toSplit <- mergeDat[, ]
##
## split the data
splitDat <- split(toSplit, f = toSplit$statename)
##
## get rid of the index columns
lagCols <- splittrCols[-grep("Year|statename|Yr", splittrCols)]
## do the lag
splitDat <- lapply(splitDat, function(x, lagNo = 1, colsToLag = lagCols){
    for(u in colsToLag) x[, paste0(u, "Lag")] <- Hmisc::Lag(x[, u], lagNo)
    ##
    return(x[, order(colnames(x))])
})
##
## and recombine using do.call
splitDat <- do.call(rbind, splitDat)
## returning to merge
mergeDat <- splitDat

## figure out a regional diffusion variable
## I made a regional diffusion variable and have added it to the merged data set.
mergeDat <- data.frame( mergeDat, "censusRegion" = censusRegions[ match(mergeDat$statename, censusRegions$state), "censusRegion"])
