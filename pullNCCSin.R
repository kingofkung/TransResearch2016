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


## Merge in pop and price

taylorlgbt2 <- merge(taylorlgbt2, popnprice, by.x = "matchcode", by.y = "matchcode", all.x = TRUE, all.y = TRUE)


## ## Now to get the HRC Index in
## head(HRC)
HRC$St.abb <- rep(state.abb,2)
HRC$sy <- paste0(HRC$St.abb, HRC$Year)
taylorlgbt2 <- merge(taylorlgbt2, HRC[,c("ScoreCardCats", "sy")], by.x = "matchcode", by.y = "sy", all.x = TRUE)

## make hrccols something we can use.
hrccols <- colnames(taylorlgbt2)[grepl("ScoreCardCats", colnames(taylorlgbt2))]
taylorlgbt2[, hrccols]
hrclevels <- levels(taylorlgbt2[, hrccols])
hrcdums <- sapply(hrclevels, function(x) ifelse(taylorlgbt2[ ,hrccols] == x, 1, 0))

## Get column names just so
colnames(hrcdums) <- paste0(colnames(hrcdums), "hrc")
colnames(hrcdums) <- gsub("\\s", ".", colnames(hrcdums))

## add hrc dummys to the full dataset
taylorlgbt2[,colnames(hrcdums)] <- hrcdums



## So here's the thing. If I want to use the HRC Data for the majority
## of the files, I need to make it so there's an index in each row by
## the state. It doesn't make a whole lot of sense to use the 2015
## data across all of them. I think the best option is to make it so
## the 2014 categorical index result is used across the rest of the state rows



## In order to do the pooled time series analysis that
## Dr. Haider-Markel wants, we'll need to change the format of the
## dependent variables. When they came in, the DVS had 0's if the
## policy hadn't been adopted, and 1's if it had. We need to make it
## so that every 1 after the first is an NA...

## taylorlgbt2[, c("statename", "year", "doma")]
devees <- c("doma", "gay_disc", "trans_dis", "superdoma")
taysplit <- split(taylorlgbt2, f = taylorlgbt2$statename)
head(taysplit)

## first goal, try to get the toy example, with just doma, to have only 1 one
## Iterate over devee list that I create above

for(i in devees){
    taysplit <- lapply(taysplit,
                       function(x, dvname = i){
                           ## Return which values in dvname equal to 1, except for the first one
                           extraones <- which(x[,dvname]==1)[-1]
                           x[extraones, dvname] <- NA
                           x
                       })
    }

library(plyr)
taylorlgbt2 <- rbind.fill(taysplit)

## get state public opinion appended to the end of the data

head(stateopp)
colnames(stateopp) <- paste0(colnames(stateopp), "LP")

taylorlgbt2 <- merge(taylorlgbt2, stateopp, by.x = "statename", by.y = "StateLP", all.x = TRUE)

## Get rid of the match codes, abbreviations and bmfyear, as they've done their part
taylorlgbt2 <- taylorlgbt2[, -grep("matchcode", colnames(taylorlgbt2))]
taylorlgbt2 <- taylorlgbt2[, -grep("newyear", colnames(taylorlgbt2))]
taylorlgbt2 <- taylorlgbt2[, -grep("bmfyear", colnames(taylorlgbt2))]
taylorlgbt2 <- taylorlgbt2[, -grep("state", colnames(taylorlgbt2))[-1]]

## sort it so I don't have to keep doing it when I'm reading the data

taylorlgbt2 <- taylorlgbt2[order(taylorlgbt2$year),]
taylorlgbt2 <- taylorlgbt2[order(taylorlgbt2$state),]

## The Goal: to create a file called JT DHM LGBT Group Resources, with NCCS measures added
write.csv(taylorlgbt2, paste0(loc,"JT DHM LGBT Group Resources.csv"), row.names = F)
