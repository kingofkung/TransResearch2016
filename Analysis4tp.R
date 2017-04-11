rm(list = ls())

## We'll store the analyses of the data we collect in here
library(plyr)
library(MASS)
library(texreg)
library(rockchalk)
library(stargazer)
library(arm)
library(xlsx)
library(pscl)



##' create custom glm function suitable for bulk regression orders
##' @title custom glm
##' @param x a vector containing the independent variables that we want to consider
##' @param deev A character version of our dv of choice.
##' @return a glm model with deev ~ x, where x is a the of variables of interest
##' @author Benjamin Rogers
customglm <- function(x, deev, dat = dat){
    x <- ifelse(length(x) > 1, paste0(x, collapse = "+"), x)
    u <- as.formula(paste(deev, x, sep = " ~ "))
    eval(bquote(glm(.(u), family = "binomial", data = dat)))
}

##' create custom Ordered logistic regression for bulk regressions
##' @title custom polr
##' @param x the independent variable or independent variables of interest as a vector
##' @param deev the dependent variable of interest
##' @param thedata the data to model the formula on
##' @return a polr model in the form deev ~ x
##' @author Benjamin Rogers
custompolr <- function(x, deev, dat = dat){
    x <- ifelse(length(x) > 1, paste0(x, collapse = "+"), x)
    u <- as.formula(paste(deev, x, sep = " ~ "))
    eval(bquote(polr(.(u), data = dat, method = "logistic")))
}

loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"
outlocgit <- "/Users/bjr/GitHub/TransResearch2016/Output/"
outlocdb <- "/Users/bjr/Dropbox/LGBT Interest group data/BensOutput/"
thedate <- substr(Sys.time(), 1, 10)

dat <- read.csv(paste0(loc,"JT DHM LGBT Group Resources.csv"))
head(dat)

stateWill <- split(dat[, c('statename', "Williams")], f = dat$statename)
stateWill <- lapply(stateWill, function(x){
    ##
    ## browser()
    valCol <- x[, 2]
    ## replace na's with non-missing value
    repVal <- rep(valCol[!is.na(valCol)][1], times = length(valCol))
    return(data.frame('statename' = as.character(x[, 1]), 'Williams' = round(repVal, 3)))
})
stateWill <- do.call(rbind, stateWill)
dat$Williams <- stateWill$Williams
## table(dat$statename, dat$Williams)

## Have read in data, will conduct some analyses now
## lgbtrevpercapita is Dr. Taylor's measure
## correlations between IVs, dvs, and make a file with output

laxPhillips <- colnames(dat)[grepl("LP", colnames(dat))]

nofacs <- !unlist(lapply(dat, is.factor))
deveesofint <- c("doma", "superdoma", "trans_dis", 'gay_disc')

allcols <- colnames(dat)[grepl("all", colnames(dat))]
no234cols <- colnames(dat)[grepl("no234", colnames(dat))]

## Here are some typical control variables. Note, I had "inst6014_nom"
## in here, but decided to remove it as it appeared to be
## multicollinear with both our ivs of interest as well as the other
## institutional variable. We can put it back in if we'd like later
## though.
typcont <- c("citi6013", "inst6013_adacope", laxPhillips[5], "evangelical")
typcont <- lapply(seq_along(typcont), function(x) typcont[1:x])

## ## get income/Revenue/assets variables and combine them into a list
revVars <- grep("rev", colnames(dat), value = T)
incVars <- grep("inc", colnames(dat), value = T)
## Fun fact: | is the or command, and it works in regex the same as elsewhere
assetVars <- grep("asset|ast", colnames(dat), value = T)
## Need to add ssph variables that Dr. Taylor just made

nussphVars <- grep("ssph", colnames(dat)[(ncol(dat)-10):ncol(dat)], value = T)

incasstrevVars <- c(revVars, incVars, assetVars, nussphVars)

## Do a correlation matrix on the variables of interest
typcont[[4]] %in% colnames(dat)
"Williams" %in% colnames(dat)

dsub <- dat[, unique(unlist(c(deveesofint, typcont[[4]], incasstrevVars, nussphVars))) ]
incContsCors <- cor(dsub, use = "pairwise.complete.obs")
write.csv(incContsCors, file = paste0(outlocdb, 'ControlsAndGroupresourcescorrelations.csv'))

dsubSmall <- dat[, c(typcont[[4]], "realastpercapall",  "realastpercap_smallno234", nussphVars, "Williams")]
littleCors <- cor(dsubSmall, use = "pairwise.complete.obs")


cor.test(dsubSmall[, "realastpercapall"], dsubSmall[, "acs5ssph2012"])


lcLabs <- c("Citizen Ideology", "ADA COPE Inst Ideo", "Jobs", "Evangelical",
            "Real Assets PC All", "Real Assets PC Small, No 234 Groups", "SSPH Census 2000", "ACS SSPH 2012", "Williams")

rownames(littleCors) <- lcLabs
colnames(littleCors) <- lcLabs

stargazer(littleCors, type = 'html', out = paste0(outlocdb, "LCHTML.html"))


library(xlsx)
write.xlsx(littleCors, file = paste0(outlocdb, "BensDocs.xlsx"))

## Get ScoreCardCats into the correct Order
SCCLvls <- c("High Priority To Achieve Basic Equality", "Building Equality", "Solidifying Equality", "Working Toward Innovative Equality")
dat$ScoreCardCats <- factor(dat$ScoreCardCats, levels = SCCLvls)
## levels(dat$ScoreCardCats)

SCCdat <- dat[ !is.na(dat$ScoreCardCats),]

SCCdat <- SCCdat[, unlist(lapply(SCCdat, function(x) !all(is.na(x))))]

## head(SCCdat)

## Models for ScoreCardCats ordinal Logit regression
## Incomeall isn't working correctly, but its square root and percentages return a result

## Create the IVs we'd like
ivls <- c( "inst6013_adacope", "inst6014_nom", "citi6013", "iaperc", "realincpercapall", "orgcountall", laxPhillips[5])


## Start adding files that show how HRC is affected by


## Model trans_dis (transgender antidiscrimination statutes) using the
## full breadth of the data. Trying to do the same as ScoreCardCats,
## but without repeating this same code over and over.

dv1 <- "trans_dis"

simplervar <- lapply(ivls[-4], customglm, dv1, dat)

## Bind control variables we want to the variables we want to consider in a variable called toreg.
## Note to self, it appears that we get what we want if we list the variable of interest last rather than first.
toreg <- lapply(incasstrevVars, function(x) c(typcont[[length(typcont)]], x) )

td4tp <- lapply(toreg[c(19, 21)], customglm, deev = dv1, dat = dat)

## see what years are used in one of the td4tp measures
tRows <- as.numeric(rownames(model.frame(td4tp[[1]])))
## unique(dat[as.numeric(tRows), "year"])
cbind(dat$statename[tRows], model.frame(td4tp[[1]]))

## outreg(tdregsNconts, type = "html")


## Begin working on gay discrimination variables
dv2 <- "gay_disc"


# some isolated varsofint

smallno234ivs <- lapply(typcont, c, "realastpercap_smallno234")
no234mods <- lapply(smallno234ivs, customglm, deev = "trans_dis", dat = dat)

no234mods <- lapply(smallno234ivs, customglm, deev = "gay_disc", dat = dat)

realastivs <- lapply(typcont, c, "realastpercapall")
## These shouldn't need to change, but we might want to add one for the ssph
censussphivs <- lapply(typcont, c, "censsph2000")
acs5ssphivs <- lapply(typcont, c, "acs5ssph2012")

IVs <- c(censussphivs[4], acs5ssphivs[4], realastivs[4], smallno234ivs[4])
gdmods4tp <- lapply(IVs, customglm, deev = "gay_disc", dat)
tdmods4tp <- lapply(IVs, customglm, deev = "trans_dis", dat)

## Need to grab pseudo-R^2s
gdpR2s <- lapply(gdmods4tp, function(x) round(pR2(x)['McFadden'], 3))
tdpR2s <- lapply(tdmods4tp, function(x) round(pR2(x)['McFadden'], 3))


myCovLabs <- c("Constant", "Citizen Ideology", "Insitutional Ideology", "Jobs LP", "Evangelical Population", "SSPH 2000 Census", "SSPH 2012 ACS", "Real Assets", "Real Assets, no 234 groups")

dvLabs <- c(paste0("Sexual Orientation Anti-Discrimination Law: Models 1-", length(gdmods4tp)),
            paste0("Gender Identity Anti-Discrimination Law: Models ", 1 + length(tdmods4tp), "-", length(gdmods4tp) + length(tdmods4tp)))

stargazer(c(gdmods4tp, tdmods4tp), type = "html",
          dep.var.labels = dvLabs,
          model.numbers = T,
          covariate.labels = myCovLabs,
          intercept.bottom = F,
          add.lines = list(unlist(c("Pseudo R-Squared", gdpR2s, tdpR2s))),
          out = paste0(outlocdb, "htmlReg.html")
          )


table(dat$censsph2000, dat$state)
table(dat$realastpercapall, dat$state)

## Make a smaller one for the paper

rm(atMeanLs)
atMeanLs <- list("citi6013" = mean(dat$citi6013, na.rm = T),
     "inst6013_adacope" = mean(dat$inst6013_adacope, na.rm = T),
     "JobsLP" = mean(dat$JobsLP, na.rm = T),
     "evangelical" = mean(dat$evangelical, na.rm = T))
##
## Get the model we'd like to use in predictOMatic and the name of the variable
intMods <- c(gdmods4tp, tdmods4tp)
for(i in seq(intMods)){
    intMod <- intMods[[i]]
    intVar <- names(coef(intMod)[6])
    ##
    atMeanLs[[intVar]] <- mean(dat[, intVar], na.rm = TRUE) + sd(dat[, intVar], na.rm = TRUE) * -2:2
    ## ## Craft mfx
    mfx <- predictOMatic(intMod, predVals = atMeanLs, n = 5)[, ]
    mfx$fitAsPerc <-  (mfx$fit) * 100
    mfx$fitAsFactor <- (mfx$fit)/min(mfx$fit)
    ## make sure we know which DV goes with which table. A little hacky
    mfx$dv <- as.character(formula(intMod)[[2]])
##
    ##Write the Results to a table, but only the one time
    ifelse(i == 1, myAppend <- FALSE, myAppend <- TRUE)
    ## And write to a .csv file, using write.table so we can append
    write.table(mfx[,], file = paste0(outlocdb, "MFX.csv"), append = myAppend, sep = ",", row.names = F)
    ## Needed to do some cleanup. If we don't get rid of the intVar element, it'll just stick around and be a problem
    atMeanLs[[intVar]] <- NULL
}




## Make formula, one time
intMod <- tdmods4tp[[4]]
myForm <- formula(intMod)
##
confMats <- lapply(1:100, function(x, data = dat){
    set.seed(x)
    tstRows <- sample(1:nrow(data), nrow(data) * .1)
    myGlm <- glm(myForm, data[-tstRows,], family = "binomial")
    myPreds <- predict(myGlm, data[tstRows,], type = "response")
    preds <- sapply(myPreds, function(i) {ifelse(!is.na(i), rbinom(1, 1,  i), NA)})
    obs <- data[tstRows, "gay_disc"]
    cmProp <- prop.table(table(obs, preds))
})

getTruePositives <- function(cMat) ifelse(nrow(cMat) == 2 && ncol(cMat) == 2,
                                          cMat["0", "0"] + cMat["1", "1"],
                                          cMat["0", "0"])
TPVals <- sapply(confMats, getTruePositives)
summarize(TPVals)


intMod <- gdmods4tp[[3]]
coefplot(intMod, vertical = TRUE)
