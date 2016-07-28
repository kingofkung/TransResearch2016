## This code will get the data in EqualityCodebook.xlsx and JT DHM LGBT Group Resources.csv, figure out which variables are and aren't listed in the former, and add those that arent

datloc <- "/Users/bjr/Dropbox/LGBT Interest group data/"

library(xlsx)
eqcd <- read.xlsx2(paste0(datloc, "EqualityCodebook.xlsx"), 1)
eqvals <- as.character( eqcd[,1])
eqvals <- gsub("\\s",'', eqvals)
eqvals <- tolower(eqvals)

dat <- read.csv(paste0(datloc, "JT DHM LGBT Group Resources.csv"))
colnames(dat)
valstoadd <- colnames(dat)[ !tolower(colnames(dat)) %in% eqvals]
write.csv(valstoadd, "/Users/bjr/GitHub/TransResearch2016/valuestoadd.csv", row.names = F, col.names = F)
