## Having created a file that lets us read in all of the files into R,
## I'm working on trying to figure out how to pull in the new measures
## of NCCS data.

source("/Users/bjr/GitHub/TransResearch2016/OpenFiles.R")
ls()
head(lgbtallcoll, 3)
head(lgbtcollno234, 3)
head(taylorlgbt, 3)

## Until we know what NCCS means, lets try and assume we're matching all data from the coll variables to the taylorlgbt file

statematcher <- data.frame("name" = state.name, "abb" = state.abb)

## So basically we want to add a column that has the state as either a
## name in the collapse files or an abbreviation in the taylor file.
taylorlgbt$stateabb <- NA
taylorlgbt$stateabb <- unlist(lapply(taylorlgbt$statename, function(x){
    statematcher$abb[statematcher$name %in% x]
    }

))

## Appears to have worked exactly as intended
## table(taylorlgbt$stateabb, taylorlgbt$statename)

## So what do you say we create some codes with the correct naming
## conventions? Now that we have given taylorlgbt state abbreviations,
## we should be able to make codes to do some matching.
taylorlgbt$matchcode <- paste0(taylorlgbt$stateabb, taylorlgbt$year)
lgbtallcoll$matchcode <- paste0(lgbtallcoll$state, lgbtallcoll$bmfyear)
lgbtcollno234$matchcode <- paste0(lgbtcollno234$state, lgbtcollno234$bmfyear)

## All rows have a unique match code. You can tell based on this command.
## which(table(taylorlgbt$matchcode)>1)

colnames(lgbtallcoll) <- paste0(colnames(lgbtallcoll), "all")
colnames(lgbtcollno234) <- paste0(colnames(lgbtcollno234), "no234")

## allcollmatcher <- match(taylorlgbt$matchcode, lgbtallcoll$matchcodeall)
## taylorlgbt <- cbind(taylorlgbt, lgbtallcoll[allcollmatcher,])

## no234matcher <- match(taylorlgbt$matchcode, lgbtcollno234$matchcodeno234)
## taylorlgbt <- cbind(taylorlgbt, lgbtcollno234[no234matcher,])

## So DHM wants me to make sure all of the data is in the file, not
## just the stuff that matches. I Need to figure out how to match the
## values that are a. not in taylorlgbt and b. the same. Turns out we
## can do it by using the merge instead of the match command. The
## merge command combines the values that are the same, but we can
## make it append values that differ on their codes by setting the
## all.x and all.y arguments to TRUE


taylorlgbt2 <- merge(taylorlgbt, lgbtallcoll, by.x = "matchcode", by.y = "matchcodeall", all.x = TRUE, all.y = TRUE)
taylorlgbt2 <- merge(taylorlgbt2, lgbtcollno234, by.x = "matchcode", by.y = "matchcodeno234", all.x = TRUE, all.y = TRUE)


## So here's the thing. Right now, the year and states are all messed
## up. There are missing values in the states and missing values in
## the year, and we should try and fix that.

taylorlgbt2$newyear <- paste0(taylorlgbt2$year, taylorlgbt2$bmfyearall)
taylorlgbt2$newyear <- gsub("NA", "", taylorlgbt2$newyear)
taylorlgbt2$year <- as.numeric(substr(taylorlgbt2$newyear, 1, 4))

## And maybe the state...

taylorlgbt2[,c("statename", "stateall")]


taylorlgbt2$statename <- ifelse(is.na(taylorlgbt2$statename), taylorlgbt2$stateall, taylorlgbt2$statename)
abbsinorder <- taylorlgbt2$statename[which(taylorlgbt2$statename %in% state.abb)]
## Given a list of state abbreviations, how do I make it return a list
## of state names, if I have a dataframe with that info side by side?
namesinorder <- as.character(statematcher$name[ match(abbsinorder, statematcher$abb)])
taylorlgbt2$statename[which(taylorlgbt2$statename %in% state.abb)] <- namesinorder


taylorlgbt2$bmfyearno234

colnames(taylorlgbt)

head(taylorlgbt)


## Get rid of the match codes, abbreviations and bmfyear, as they've done their part
taylorlgbt2 <- taylorlgbt2[, -grep("matchcode", colnames(taylorlgbt2))]

## The Goal: to create a file called JT DHM LGBT Group Resources, with NCCS measures added
write.csv(taylorlgbt2, paste0(loc,"JT DHM LGBT Group Resources.csv"), row.names = F)
