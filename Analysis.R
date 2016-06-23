## We'll store the analyses of the data we collect in here
library(plyr)
library(MASS)
library(zoo)

loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"

dat <- read.csv(paste0(loc,"JT DHM LGBT Group Resources.csv"))

head(dat)
colnames(dat)
## Have read in data, will conduct some analyses now

## lgbtrevpercapita is Dr. Taylor's measure

## correlations between IVs, dvs
## make a file with output


realhrccols <- colnames(dat)[grepl("hrc", colnames(dat))]
laxPhillips <- colnames(dat)[grepl("LP", colnames(dat))]

nofacs <- !unlist(lapply(dat, is.factor))
deveesofint <- c("doma", "superdoma", "trans_dis", 'gay_disc')

allcols <- colnames(dat)[grepl("all", colnames(dat))]
no234cols <- colnames(dat)[grepl("no234", colnames(dat))]

ivsofint <- c("citi6008", "inst6008_adacope", "inst6008_nom", "evangelical", "census", "ssph", "south", "lowerdem", "upperdem", 'squire', 'partneradj', 'ssphh2000', "ssphhacs3", "Williams", "lgbtpop", "lgbtrev2010adj", "lgbtrevpercapita", "lgbtrevperlgbtcapita", "sexorientnd2013", "genderidentnd2013", "loglgbtrev", "ssphh200per", "ssphhacs3per", "Williamsper", "logrevperlgbtcap", "evangper", "stdlgbtperlgbtcapita", "ssphh2010per", "percentssphh", "lgbtrevperlgbtcapita2", "loglgbtrevperlgbtcapita2", "incomeall", "assetsall", "orgcountall", allcols, no234cols, realhrccols)

allcors <- cor(dat[, nofacs], use = "pairwise.complete")

allcors[unique(ivsofint) , deveesofint]

allcors <- cor(dat[, c(deveesofint, ivsofint)], use = "pairwise.complete.obs")
## write.csv(allcors, paste0(loc,"correlations.csv"))

## Extrapolate Dr. Fording's data to the years 2014-2015
colstoex <- colnames(dat)[grep("601", colnames(dat))]
datsub <- dat[, c("statename", colstoex )]

exsplit <- split(dat[, c("statename", colstoex )], dat$statename)

## Apparently, na.locf fills na values with the last non-NA value,
## fixing the problem we were having!
exsplit <- lapply(exsplit, na.locf)
exsplit <- rbind.fill(exsplit)

dat[, colnames(exsplit)[2:4]] <- exsplit[2:4]


## Get ScoreCardCats into the correct Order
SCCLvls <- c("High Priority To Achieve Basic Equality", "Building Equality", "Solidifying Equality", "Working Toward Innovative Equality")
dat$ScoreCardCats <- factor(dat$ScoreCardCats, levels = SCCLvls)
levels(dat$ScoreCardCats)

SCCdat <- dat[ !is.na(dat$ScoreCardCats),]
head(SCCdat)

m1 <- polr(ScoreCardCats ~ MeanOppLP, data = SCCdat, method = "logistic")
summary(m1)

apply(SCCdat, 2, function(x) all(is.na(x)))


tr1 <- glm(trans_dis ~ citi6008, data = dat, family = "binomial")
summary(tr1)

tr2 <- update(tr1, .~. + inst6008_adacope + inst6008_nom)
summary(tr2)

tr3 <- update(tr2, .~. + evangelical)
summary(tr3)

tr4 <- update(tr3, .~. + Williams)
summary(tr4)

tr5 <- update(tr4, .~. + squire)
summary(tr5)

tr6 <- update(tr5, .~. + percentssphh)
summary(tr6)

tr7 <- update(tr6, gay_disc ~ .)
summary(tr7)
