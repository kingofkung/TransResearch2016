## This script will use the data that we downloaded on the census' website, with the goal of making a nice .csv file

## go to the right wd
setwd("/Users/bjr/Dropbox/LGBT Interest group data/TransTeamDatFall17")

## read in the dataset
cenReg <- readLines("CensusRegions.txt")

## remove blank lines
cenReg <- cenReg[!cenReg %in% ""]

## remove first, last 2 lines (neither divison nor region info)
badLineInfo <- grep("Census Bureau Regions|Prior to June|North Central Region", cenReg, value = F)
##
cenReg <- cenReg[-badLineInfo]

## get rid of these lines that talk about divisions. Note that I was
## going to put a digit in here, but it turns out that one's an I
cenReg <- cenReg[-grep("Division\\s", cenReg, value = F)]

## get rid of numbers with parentheses, and one asterisk
cenReg <- gsub("\\(\\d\\d\\)|\\*","", cenReg)
## deal with the white space too
cenReg <- trimws(cenReg)

## find each region's location in the lines
regLocs <- grep("region", cenReg, ignore.case = T)
## having found the location, generate appropriate sequences
stSeq <- list()
for(i in 1:length(regLocs)) {
    firstState <- regLocs[i] + 1
    ## We want to get a final sequence that includes the 4th region.
    ## If it's before that, we want everything up to the final state in sequence
    ## if it isn't, then we'll get that last set of states.
    lastState <- ifelse(i != length(regLocs),
                        regLocs[i + 1] - 1,
                        length(cenReg))
    ## get the regions assembled
    stSeq[[i]] <- firstState:lastState
}
##
## get the state names
stSeq <- lapply(stSeq, function(x) cenReg[x])
## then, take the data and make them into a matrix with column 1 being the state, column 2 as the region based on the sequence we generated
stDf <- lapply(seq(stSeq), function(i) cbind(stSeq[[i]], rep(i, length(stSeq[[i]]))))

stDf <- do.call(rbind, stDf)
stDf <- as.data.frame(stDf)
colnames(stDf) <- c("state", "censusRegion")

## I just realized that I didn't want to worry about R thinking the region's an integer variable, so I'm pasting region in here to fix that.
stDf$censusRegion <- paste0("Region ", stDf$censusRegion)

## census regions
write.csv(stDf, "CensusRegions.csv", row.names = FALSE)
