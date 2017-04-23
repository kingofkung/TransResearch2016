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


loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"
outlocgit <- "/Users/bjr/GitHub/TransResearch2016/Output/"
outlocdb <- "/Users/bjr/Dropbox/LGBT Interest group data/BensOutput/"
thedate <- substr(Sys.time(), 1, 10)

dat <- read.csv(paste0(loc,"JT DHM LGBT Group Resources.csv"))
## head(dat)

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

## Need to lag the measures:: realastpercap_smallno234 and realastpercapall
stateAst <- split(dat[, c('statename', 'realastpercap_smallno234', 'realastpercapall', 'year')], f = dat$statename)
stateAst <- lapply(stateAst, function(x){
    x$realastpercap_smallno234Lagged <- Hmisc::Lag(x$realastpercap_smallno234, 1)
    x$realastpercapalllagged <- Hmisc::Lag(x$realastpercapall, 1)
##
    return(x[,  c('statename', 'year', 'realastpercap_smallno234', 'realastpercap_smallno234Lagged',
                  'realastpercapall', 'realastpercapalllagged') , drop = FALSE])
##
})

write.csv(stateAst$Wyoming, file = paste0( outlocdb, "WyomingLagged.csv"), row.names = F)

## Have read in data and played with some variables, will conduct some analyses now
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

dsub <- dat[, unique(unlist(c(deveesofint, typcont[[length(typcont)]], incasstrevVars, nussphVars))) ]
incContsCors <- cor(dsub, use = "pairwise.complete.obs")
write.csv(incContsCors, file = paste0(outlocdb, 'ControlsAndGroupresourcescorrelations.csv'))

dsubSmall <- dat[, c(typcont[[length(typcont)]], "realastpercapall",  "realastpercap_smallno234", nussphVars, "Williams")]
littleCors <- cor(dsubSmall, use = "pairwise.complete.obs")


cor.test(dsubSmall[, "realastpercapall"], dsubSmall[, "acs5ssph2012"])


lcLabs <- c("Citizen Ideology", "ADA COPE Inst Ideo", "Jobs", "Evangelical",
            "Real Assets PC All", "Real Assets PC Small, No 234 Groups", "SSPH Census 2000", "ACS SSPH 2012", "Williams")

rownames(littleCors) <- lcLabs
colnames(littleCors) <- lcLabs

stargazer(littleCors, type = 'html', out = paste0(outlocdb, "LCHTML.html"))


library(xlsx)
write.xlsx(littleCors, file = paste0(outlocdb, "BensDocs.xlsx"))


## Create the IVs we'd like
ivls <- c( "inst6013_adacope", "inst6014_nom", "citi6013", "iaperc", "realincpercapall", "orgcountall", laxPhillips[5])


## Start adding files that show how HRC is affected by


## Model trans_dis (transgender antidiscrimination statutes) using the
## full breadth of the data. Trying to do the same as ScoreCardCats,
## but without repeating this same code over and over.

dv1 <- "trans_dis"
dv2 <- "gay_disc"


## make lists of IVs for the paper
smallno234ivs <- lapply(typcont, c, "realastpercap_smallno234")

realastivs <- lapply(typcont, c, "realastpercapall")
## These shouldn't need to change, but we might want to add one for the ssph
censussphivs <- lapply(typcont, c, "censsph2000")
acs5ssphivs <- lapply(typcont, c, "acs5ssph2012")
williamsivs <- lapply(typcont, c, "Williams")

## Just grab the last, sans controls, of the data
finalTypCont <- length(typcont)
IVs <- c(censussphivs[finalTypCont],
         acs5ssphivs[finalTypCont],
         realastivs[finalTypCont],
         smallno234ivs[finalTypCont],
         williamsivs[finalTypCont])

## make mods for the paper
gdmods4tp <- lapply(IVs, customglm, deev = "gay_disc", dat)
tdmods4tp <- lapply(IVs, customglm, deev = "trans_dis", dat)

## Need to grab pseudo-R^2s
gdpR2s <- lapply(gdmods4tp, function(x) round(pR2(x)['McFadden'], 3))
tdpR2s <- lapply(tdmods4tp, function(x) round(pR2(x)['McFadden'], 3))


myCovLabs <- c("Constant", "Citizen Ideology", "Insitutional Ideology", "Jobs LP", "Evangelical Population", "SSPH 2000 Census", "SSPH 2012 ACS", "Real Assets", "Real Assets, no 234 groups", "Williams Measure")

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

