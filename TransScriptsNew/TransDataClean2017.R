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
## ssphWill[, c("State" )]

## convert ssph williams measures from wide to long format
## ssphWlLng came from "ssphh over time with williams.xlsx"
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

## ssphDtLng came from ssphdt, which came from
## ssphh over time with williams straight line 1990 2008.xlsx
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

mergeDat <- mergeDat[, -1]
mergeDat$abb <- zoo::na.locf(mergeDat[, "abb"])

mergeDat <- as.data.frame(apply(mergeDat, 2, function(x){
    x[x %in% "#N/A"] <- NA
    x
}))

## having done the regional diffusion variable, we need to figure out how to get a percentage of regions with the policies in question

## how do I get the % of states, by region by year that have the policy in question

## number of states by region
mergeDat$Year <- as.numeric(as.character(mergeDat$Year))

compNo <- table(mergeDat[!duplicated(mergeDat$statename), "censusRegion"])

range(as.numeric(as.character(mergeDat$Year)))

getRegPercs <- function(dvName, dat = yrDat){
    percTab <- table(dat[, dvName], dat$censusRegion)
    ## percTab <- prop.table(percTab, 2)
    percTab
}

## add at thing for data from 1994
dat94 <- matrix(0, 4, 4)
colnames(dat94) <- colnames(datLs[[1]])
rownames(dat94) <- rownames(datLs[[1]])
## so minnesota had a gender ID nondisc, and it's region 2. As for
## sexual orientation, WI (2), MN (2), MA (1), DC (3), CA (4), HI (4),
## VT (1), NJ (1) and CT (1) all had one. For region 1, 4 people. For region 2, 2, for region 3, 1. For region 4, 2
dat94["trans_dis", "Region 2"] <- 1
dat94["gay_disc", 1:4] <- c(4, 2, 1, 2)


## get diffusion and save it:
datLs <- list()
## get data from 1994 that we found from Jami as the 1st list element
datLs[['1994']] <- dat94
deevs <- c("superdoma", "doma", "trans_dis", "gay_disc")
##
for(i in 1995:2015){
    ## make data for just that year
    yrDat <- mergeDat[mergeDat$Year == i, ]
    ## get the percentages of states/regions that passed something in
    ## the dv that year
    yrLs <- lapply(deevs, function(x) getRegPercs(x)["1" , ])
    ## get the names based on the dvs
    names(yrLs) <- deevs
    ## making it i - 1994 starts it at 1... Push it back to 1993 to
    ## acommodate our new information from 1994.
    datLs[[i-1993]] <- t(as.data.frame(yrLs))
    names(datLs)[i-1993] <- i
}

rmNans <- function(y){
    y[!is.finite(y)] <- 0
    y
}





datLs <- lapply(datLs, rmNans)
## do a cumulative sum of the data.frames
cumulDatLs <- list()
for(i in seq(names(datLs))){
    ## if it's the first one, it's the same as datLs
    if(i == 1) cumulDatLs[[i]] <- datLs[[i]] else {
        ## if it's not the first one, then take the last one, add the
        ## current datLs to it, and save to that position.
        cumulDatLs[[i]] <- cumulDatLs[[i - 1]] + datLs[[i]]
    }
}
## and keep those names neat!
names(cumulDatLs) <- names(datLs)
## next, we'll get the values divided by compNo
cumulDatLs <- lapply(cumulDatLs, function(j) apply(j, 1,  function(x) x/compNo))

diffNames <- paste0(deevs, "Diff")
mergeDat[, diffNames] <- NA




i <- 2007
for(i in 1995:2015){
    ## pop out a given year
    yrPpt <- mergeDat[mergeDat$Year %in% i, ]
    ## make variables with names diffNames
    ## pop out the dataFrame with the diffusion data
    diffPpt <- cumulDatLs[[as.character(i)]]
    ## match to get the appropriate census region
    diffCensusRegions <- match(yrPpt$censusRegion, rownames(diffPpt))
    yrPpt[, diffNames] <- diffPpt[diffCensusRegions, ]
    ## and return
    mergeDat[mergeDat$Year %in% i, ] <- yrPpt
}

write.csv(mergeDat, "JamisDataMerged.csv", row.names = FALSE)
## and make one for Jami's stata file, using read.csv to get the numbers into a semi decent format.
foreign::write.dta(read.csv("JamisDataMerged.csv", stringsAsFactors = FALSE), "JamisDataMerged.dta")
