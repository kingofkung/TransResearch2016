## So here we'll take the data that Don and Jami Sent over and try to
## read it in. Later, we'll see what they actually want me to do with
## it.

library(readstata13)
library(xlsx)
library(readxl)

fileIn <- "~/Dropbox/LGBT Interest group data/TransTeamDatFall17/"
setwd(path.expand(fileIn))

## read in the .xlsx files, taking care to make them data.frames each time.
## The 1990 Census file
census <- read_excel("1990 Census unmarried partner  households by state.xlsx", 1)
census <- as.data.frame(census)

## The measures of group resources
grpstuff <- read_excel("Copy of JT DHM LGBT Group Resources  jkt 8.5.17.xlsx", 1)
grpstuff <- as.data.frame(grpstuff)

## Same sex partner households over time
ssphdt <- read_excel("ssphh over time with williams  straight line 1990 2008.xlsx", 1)
ssphdt <- as.data.frame(ssphdt)

## and Same sex partner households with williams measure
ssphWill <- read_excel("ssphh over time with williams.xlsx", 1)
ssphWill <- as.data.frame(ssphWill)

## now let us read in the stata 13 file
ourdat <- read.dta13("jami ben and don sppq data  8.6.17.dta")

## ok they're read in. Now to figure out what it is Don wants me to do with them...
