## Open the Files DHM is having me look at
rm(list = ls())
## loc <- "/Users/bjr/KUDropbox/Dropbox/LGBT Interest group data/"
loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"
library(readstata13)
library(xlsx)
cleaneddata <- read.dta13(paste0(loc, "jami and don lgbt data cleaned 5.20.16.dta"))

## Read in files with nccs data
lgbtallcoll <- read.dta13(paste0(loc, "jami and don all lgbt group collapse 5.21.16.dta"))
lgbtcollno234 <- read.dta13(paste0(loc, "jami and don lgbt group collapse no 2 3 4 groups  5.21.16.dta"))

## So what do you say we create some codes with the correct naming
## conventions? Now that we have given taylorlgbt state abbreviations,
## we should be able to make codes to do some matching.
lgbtallcoll$matchcode <- paste0(lgbtallcoll$state, lgbtallcoll$bmfyear)
lgbtcollno234$matchcode <- paste0(lgbtcollno234$state, lgbtcollno234$bmfyear)

## All rows have a unique match code. You can tell based on this command.
## which(table(taylorlgbt$matchcode)>1)

colnames(lgbtallcoll) <- paste0(colnames(lgbtallcoll), "all")
colnames(lgbtcollno234) <- paste0(colnames(lgbtcollno234), "no234")



## read in taylor lgbt measure

taylorlgbt <- read.dta13(paste0(loc, "taylor lgbt measure 3.20.14.dta"))


## So basically we want to add a column that has the state as either a
## name in the collapse files or an abbreviation in the taylor file.
taylorlgbt$stateabb <- NA
taylorlgbt$stateabb <- unlist(lapply(taylorlgbt$statename, function(x){state.abb[match(x, state.name)]}))
## Appears to have worked exactly as intended
## table(taylorlgbt$stateabb, taylorlgbt$statename)



taylorlgbt$matchcode <- paste0(taylorlgbt$stateabb, taylorlgbt$year)


## read in pop/price index
popnprice <- read.dta13(paste0(loc, "pop and priceindex.dta"))
popnprice$matchcode <- paste0(popnprice$state, popnprice$bmfyear)


## read in HRC's policy index
HRC <- read.csv(paste0(loc, "Index2015.csv"))
HRC$abb <- unlist(lapply(HRC$State, function(u) state.abb[match(u, state.name)]))
HRC$matchcode <- paste0(HRC$abb, HRC$Year)


## read in state public opinion data
stateopp <- read.xlsx(paste0(loc, "StateOpp.xlsx"), 1)


## read in newest state/citizen ideology data
stcit <- read.xlsx2(paste0(loc, "ideo6014.xlsx"), 1)
stcit$abb <- unlist(lapply(as.character(stcit$statename),  function(u) state.abb[match(u, state.name)]))
stcit$matchcode <- paste0(stcit$abb, stcit$year)
