## Combine the HRC Data with the other information that we have available.
location <- "/Users/bjr/GitHub/TransResearch2016/"
source(paste0(location,"OpenFiles.R"))

head(HRC)

HRC$State[46]
colnames(HRC)
HRCInt <- c("State", "Year", "ScoreCardCats", "matchcode")

## merge HRC with popnprice
hrcmerge <- merge(HRC[, HRCInt], popnprice, by.x = "matchcode", by.y = "matchcode", all.x = TRUE)

##merge hrc with stateopp
hrcmerge <- merge(hrcmerge, stateopp, by = "State", all.x = TRUE)

## merge hrc with the lgbt collapsed data

hrcmerge <- merge(hrcmerge, lgbtallcoll, by.x = "matchcode", by.y = "matchcodeall", all.x = TRUE)

hrcmerge <- merge(hrcmerge, lgbtcollno234, by.x = "matchcode", by.y = "matchcodeno234", all.x = TRUE)


hrcmerge <- hrcmerge[,-grep('matchcode', colnames(hrcmerge))]
hrcmerge <- hrcmerge[,-grep('bmfyear', colnames(hrcmerge))]

## So what if I decide that I want to remove all states but the one at
## the start? This works.
hrcmerge <- hrcmerge[,-grep('state', colnames(hrcmerge))[-1]]



write.csv(hrcmerge, "/Users/bjr/Dropbox/LGBT Interest group data/hrcCrossSec.csv", row.names = F)
