## We'll store the analyses of the data we collect in here
loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"

dat <- read.csv(paste0(loc,"JT DHM LGBT Group Resources.csv"))

head(dat)
colnames(dat)
## Have read in data, will conduct some analyses now

lgbtrevpercapita

## correlations between IVs, dvs
## make a file with output

## make hrccols something we can use.
hrccols <- colnames(dat)[grepl("ScoreCardCats", colnames(dat))]
dat[, hrccols]
hrclevels <- levels(dat[, hrccols])
hrcdums <- sapply(hrclevels, function(x) ifelse(dat[ ,hrccols] == x, 1, 0))

## Get column names just so
colnames(hrcdums) <- paste0(colnames(hrcdums), "hrc")
colnames(hrcdums) <- gsub("\\s", ".", colnames(hrcdums))

## add hrc dummys to the full dataset
dat[,colnames(hrcdums)] <- hrcdums

realhrccols <- colnames(dat)[grepl("hrc", colnames(dat))]

nofacs <- !unlist(lapply(dat, is.factor))
deveesofint <- c("doma", "superdoma", "trans_dis", 'gay_disc')

allcols <- colnames(dat)[grepl("all", colnames(dat))]
no234cols <- colnames(dat)[grepl("no234", colnames(dat))]

ivsofint <- c("citi6008", "inst6008_adacope", "inst6008_nom", "evangelical", "census", "ssph", "south", "lowerdem", "upperdem", 'squire', 'partneradj', 'ssphh2000', "ssphhacs3", "Williams", "lgbtpop", "lgbtrev2010adj", "lgbtrevpercapita", "lgbtrevperlgbtcapita", "sexorientnd2013", "genderidentnd2013", "loglgbtrev", "ssphh200per", "ssphhacs3per", "Williamsper", "logrevperlgbtcap", "evangper", "stdlgbtperlgbtcapita", "ssphh2010per", "percentssphh", "lgbtrevperlgbtcapita2", "loglgbtrevperlgbtcapita2", "incomeall", "assetsall", "orgcountall", allcols, no234cols, realhrccols)

allcors <- cor(dat[, nofacs], use = "pairwise.complete")

allcors[unique(ivsofint) , deveesofint]

allcors <- cor(dat[, c(deveesofint, ivsofint)], use = "pairwise.complete.obs")
write.csv(allcors, paste0(loc,"correlations.csv"))

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
