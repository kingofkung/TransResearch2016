## We'll store the analyses of the data we collect in here
library(plyr)
library(MASS)
library(zoo)

loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"
outlocgit <- "/Users/bjr/GitHub/TransResearch2016/Output/"
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

dat[, colnames(exsplit)[2:4]] <- sapply(exsplit[2:4], as.numeric)
str(dat[,colnames(exsplit)[2:4]])


## Get ScoreCardCats into the correct Order
SCCLvls <- c("High Priority To Achieve Basic Equality", "Building Equality", "Solidifying Equality", "Working Toward Innovative Equality")
dat$ScoreCardCats <- factor(dat$ScoreCardCats, levels = SCCLvls)
levels(dat$ScoreCardCats)

SCCdat <- dat[ !is.na(dat$ScoreCardCats),]
head(SCCdat)

nacols <- apply(SCCdat, 2, function(x) all(is.na(x)))
names(nacols[!nacols==T])


m1 <- polr(ScoreCardCats ~ MeanOppLP, data = SCCdat, method = "logistic")
summary(m1)


m2 <- polr(ScoreCardCats ~ inst6013_adacope, data = SCCdat, method = "logistic")
summary(m2)

m3 <- polr(ScoreCardCats ~ inst6014_nom, data = SCCdat, method = "logistic")
summary(m3)

m4 <- polr(ScoreCardCats ~ citi6013, data = SCCdat, method = "logistic")
summary(m4)

## Incomeall isn't working correctly, but its square root and percentages return a result
SCCdat$iasqrt <- sqrt(SCCdat$incomeall)
SCCdat$iaplus <- SCCdat$incomeall + 1
SCCdat$iaperc <- SCCdat$incomeall/sum(as.numeric(SCCdat$incomeall))
incomeall <- as.numeric(SCCdat$incomeall)
m5 <- polr(ScoreCardCats ~ iaperc, data = SCCdat, method = "logistic", Hess = T)
summary(m5)

m5.5 <- polr(ScoreCardCats ~ realincpercapall, data = SCCdat, method = "logistic")
summary(m5.5)

m6 <- polr(ScoreCardCats ~ orgcountall, data = SCCdat, method = "logistic")
summary(m6)

## [1] "SecondParentLP"   "HateCrimesLP"     "HealthBenefitsLP" "HousingLP"
## [5] "JobsLP"           "MarriageLP"       "SodomyLP"         "CivUnionsLP"
## [9] "MeanOppLP"


m7 <- polr(ScoreCardCats ~ HateCrimesLP, data = SCCdat, method = "logistic")
summary(m7)

m8 <- polr(ScoreCardCats ~ HealthBenefitsLP, data = SCCdat, method = "logistic")
m9 <- polr(ScoreCardCats ~ HousingLP, data = SCCdat, method = "logistic")
m10 <- polr(ScoreCardCats ~ JobsLP, data = SCCdat, method = "logistic")
m11 <- polr(ScoreCardCats ~ MarriageLP, data = SCCdat, method = "logistic")
m12 <- polr(ScoreCardCats ~ SodomyLP, data = SCCdat, method = "logistic")
m13 <- polr(ScoreCardCats ~ CivUnionsLP, data = SCCdat, method = "logistic")




mlist1 <- list(m1, m2, m3, m4, "Income As Percentage" = m5, m5.5, m6, m7, m8, m9, m10, m11, m12,m13)
mlist1 <- lapply(mlist1, FUN = extract, include.thresholds = TRUE)




library(texreg)
latextext1 <- texreg(mlist1,  reorder.coef = c(1, 5:(length(mlist1) + 3) , 2:4), caption.above = TRUE, caption = "Ordered Logistic Regressions using the HRC's classifications"   )
## The columns ordered properly
write.table(latextext1 , file = paste0(outloc, "HRCmlist.txt"), quote = F, row.names = F, col.names = F)


## Reconsideration of trans_dis
citi

colnames(dat)


dv1 <- "trans_dis"
ivs <- list("MeanOppLP", "citi6013", m3 = c("citi6013", "inst6013_adacope"), "inst6014_nom", "realincpercapall", "orgcountall", laxPhillips[1:8])


customglm <- function(x, deev = dv1){
    x <- ifelse(length(x) > 1, paste0(x, collapse = "+"), x)
    u <- as.formula(paste(deev, x, sep = " ~ "))
    ## eval(bquote(.(u))) lets us get u out and evaluate it not as a thing passed in, but as the formula as it exists
    eval(bquote(glm(.(u), family = "binomial", data = dat)))
    }


tdmods <- lapply(ivs[1:5], customglm)

tdlatex1 <- texreg(tdmods, caption.above = T, caption = "Event History Analysis of Transgender Discrimination Policy")
write.table(tdlatex1, file = paste0(outlocgit, 'tdmods.txt'), quote = F, row.names = F, col.names = F)

## get income/Revenue/assets variables
revVars <- grep("rev", colnames(dat), value = T)
incVars <- grep("inc", colnames(dat), value = T)
## Fun fact: | is the or command, and it works in regex the same as elsewhere
assetVars <- grep("asset|ast", colnames(dat), value = T)
## grep("ast", colnames(dat), value = T)

incasstrevVars <- c(revVars, incVars, assetVars)

##Typical control variables
typcont <- c("citi6013", "inst6013_adacope", "inst6014_nom", laxPhillips[9])
typcont <- lapply(seq_along(typcont), function(x) typcont[1:x])

## bind typcont[[4]] to revVars
toreg <- lapply(incasstrevVars, function(x) c(x, typcont[[4]]) )

transdisregsNconts <- lapply(toreg, customglm)
tdlatex2 <- texreg(transdisregsNconts, caption.above = T, caption = "Event History Analysis of Transgender Discrimination Policy \n with Controls")
write.table(tdlatex2, file = paste0(outlocgit, 'transdisregsNconts.txt'), quote = F, row.names = F, col.names = F)



dv2 <- "gay_disc"

gdmods <- lapply(toreg, customglm, deev = dv2)
gdlatex1 <- texreg(gdmods, caption.above = T, caption = "Event History Analysis of Gay Discrimination Policy with added Controls")
write.table(gdlatex1, file = paste0(outlocgit, "gdlatex1.txt"), quote = F, row.names = F, col.names = F)

## source("/Users/bjr/GitHub/BeSiVaImp/R/BeSiVaFunctions.R")
## mtake <- lapply(1:20, function(i) y <- besiva("gay_disc", unique(unlist(toreg)), dat = dat, perc = .33,  iters = 5, thresh = .0001, sampseed = i))
## names(mtake[[1]])


## lapply(1:20, function(u) sort(mtake[[u]]$pcps))
## lapply(1:20, function(u) mtake[[u]]$predvals)

## nrow(model.frame(transdisregsNconts[[1]]))

## take2 <- lapply(1:45, function(i) besiva(dv1, incasstrevVars, dat, sampseed = i, perc = .1))
## lapply(1:45, function(gum) sort(take2[[gum]]$pcps))
