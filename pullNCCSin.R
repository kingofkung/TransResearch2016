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

allcollmatcher <- match(taylorlgbt$matchcode, lgbtallcoll$matchcode)
taylorlgbt <- cbind(taylorlgbt, lgbtallcoll[allcollmatcher,])

no234matcher <- match(taylorlgbt$matchcode, lgbtcollno234$matchcode)
taylorlgbt <- cbind(taylorlgbt, lgbtcollno234[no234matcher,])

colnames(taylorlgbt)

head(taylorlgbt)
## Get rid of the match codes, abbreviations and bmfyear, as they've done their part
taylorlgbt <- taylorlgbt[, -grep("matchcode", colnames(taylorlgbt))]
taylorlgbt <- taylorlgbt[, -grep('bmfyear',colnames(taylorlgbt))]
taylorlgbt <- taylorlgbt[, !colnames(taylorlgbt) %in% c("stateall", "stateabb", "stateno234")]

## The goal: to create a file called JT DHM LGBT Group Resources, with NCCS measures added
write.csv(taylorlgbt, paste0(loc,"JT DHM LGBT Group Resources.csv"))
