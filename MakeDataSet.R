## Make the dataset that we'll use to enter/create state level measures from HRC and Lax and Phillips' MRP
rm(list = ls())
state.name

emptystyr <- lapply(1982:2015, function(x) data.frame(state.name, year = x))

library(plyr)
emptystyr <- rbind.fill(emptystyr)
write.csv(emptystyr, "FillwithMeasures.csv", row.names = F)
