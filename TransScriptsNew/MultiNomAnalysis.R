rm(list = ls(all.names = TRUE))
## Don got me a copy of some data he'd like me to take a look at. I'm
## going to at least get it all set up here.

library(readstata13)
library(MASS)

## the first thing we'll need to do is read in the file.
datWd <- paste0("/Users/bjr/Dropbox/LGBT Interest group data",
                "/TransTeamDat18")

setwd(datWd)

f <- read.dta13("taylor rogers haider-markel  1.16.18.dta")

head(f)
colnames(f)[grepl("equality", colnames(f))]

f$hrc2015net
