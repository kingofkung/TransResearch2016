## Open the Files DHM is having me look at
rm(list = ls())
## loc <- "/Users/bjr/KUDropbox/Dropbox/LGBT Interest group data/"
loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"
library(readstata13)
cleaneddata <- read.dta13(paste0(loc, "jami and don lgbt data cleaned 5.20.16.dta"))

## Read in files with nccs data
lgbtallcoll <- read.dta13(paste0(loc, "jami and don all lgbt group collapse 5.21.16.dta"))
lgbtcollno234 <- read.dta13(paste0(loc, "jami and don lgbt group collapse no 2 3 4 groups  5.21.16.dta"))

## read in taylor lgbt measure

taylorlgbt <- read.dta13(paste0(loc, "taylor lgbt measure 3.20.14.dta"))

## read in pop/price index
popnprice <- read.dta13(paste0(loc, "pop and priceindex.dta"))


