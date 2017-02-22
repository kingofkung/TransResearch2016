## We'll store the analyses of the data we collect in here
library(plyr)
library(MASS)
library(texreg)
library(rockchalk)

##' create custom glm function suitable for bulk regression orders
##' @title custom glm
##' @param x a vector containing the independent variables that we want to consider
##' @param deev A character version of our dv of choice.
##' @return a glm model with deev ~ x, where x is a the of variables of interest
##' @author Benjamin Rogers
customglm <- function(x, deev, thedata = dat){
    x <- ifelse(length(x) > 1, paste0(x, collapse = "+"), x)
    u <- as.formula(paste(deev, x, sep = " ~ "))
    eval(bquote(glm(.(u), family = "binomial", data = thedata)))
}

##' create custom Ordered logistic regression for bulk regressions
##' @title custom polr
##' @param x the independent variable or independent variables of interest as a vector
##' @param deev the dependent variable of interest
##' @param thedata the data to model the formula on
##' @return a polr model in the form deev ~ x
##' @author Benjamin Rogers
custompolr <- function(x, deev, thedata = dat){
    x <- ifelse(length(x) > 1, paste0(x, collapse = "+"), x)
    u <- as.formula(paste(deev, x, sep = " ~ "))
    eval(bquote(polr(.(u), data = thedata, method = "logistic")))
}

loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"
outlocgit <- "/Users/bjr/GitHub/TransResearch2016/Output/"
outlocdb <- "/Users/bjr/Dropbox/LGBT Interest group data/"
thedate <- substr(Sys.time(), 1, 10)

dat <- read.csv(paste0(loc,"JT DHM LGBT Group Resources.csv"))
foreign::write.dta(dat, paste0(loc, "JT DHM LGBT Group Resources.dta"))
## Spot check Williams Measures
## willdat <- unique(dat[!is.na(dat$Williams), c("statename", "Williams")])
## willdat$Williams
## write.csv(willdat, file = paste0(outlocdb, "Spotchecker.csv"), row.names = F)


## Have read in data, will conduct some analyses now
## lgbtrevpercapita is Dr. Taylor's measure
## correlations between IVs, dvs, and make a file with output

realhrccols <- colnames(dat)[grepl("hrc", colnames(dat))]
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

## get income/Revenue/assets variables and combine them into a list
revVars <- grep("rev", colnames(dat), value = T)
incVars <- grep("inc", colnames(dat), value = T)
## Fun fact: | is the or command, and it works in regex the same as elsewhere
assetVars <- grep("asset|ast", colnames(dat), value = T)
## Need to add ssph variables that Dr. Taylor just made

nussphVars <- grep("ssph", colnames(dat)[(ncol(dat)-10):ncol(dat)], value = T)

incasstrevVars <- c(revVars, incVars, assetVars, nussphVars)
dat[,nussph]

## Do a correlation matrix on the variables of interest
typcont[[4]] %in% colnames(dat)

dsub <- dat[, unique(unlist(c(deveesofint, typcont, incasstrevVars, nussph))) ]
incContsCors <- cor(dsub, use = "pairwise.complete.obs")
write.csv(incContsCors, file = paste0(outlocdb, 'ControlsAndGroupresourcescorrelations.csv'))


## Get ScoreCardCats into the correct Order
SCCLvls <- c("High Priority To Achieve Basic Equality", "Building Equality", "Solidifying Equality", "Working Toward Innovative Equality")
dat$ScoreCardCats <- factor(dat$ScoreCardCats, levels = SCCLvls)
## levels(dat$ScoreCardCats)

SCCdat <- dat[ !is.na(dat$ScoreCardCats),]

SCCdat <- SCCdat[, unlist(lapply(SCCdat, function(x) !all(is.na(x))))]

## head(SCCdat)

## Models for ScoreCardCats ordinal Logit regression
## Incomeall isn't working correctly, but its square root and percentages return a result
SCCdat$iasqrt <- sqrt(SCCdat$incomeall)
SCCdat$iaplus <- SCCdat$incomeall + 1
SCCdat$iaperc <- SCCdat$incomeall/sum(as.numeric(SCCdat$incomeall))
incomeall <- as.numeric(SCCdat$incomeall)

## Create the IVs we'd like
ivls <- c( "inst6013_adacope", "inst6014_nom", "citi6013", "iaperc", "realincpercapall", "orgcountall", laxPhillips[5])

## And apply across all single function levels
onevarHRCmods <- lapply(ivls, function(x) {
    r <- as.formula(paste0("ScoreCardCats ~ ",x))
    eval(bquote(polr(.(r), data = SCCdat, method = "logistic")))

})
onevarHRCmods <- lapply(onevarHRCmods, FUN = extract, include.thresholds = TRUE)

## A symbol for all latex documents indicating what we'd like for the .1 threshold
dotsym <- "\\dagger"

HRCCoefOrder <- c(1, 5:(length(onevarHRCmods) + 3), 2:4)
latextext1 <- texreg(onevarHRCmods,  reorder.coef = HRCCoefOrder, caption.above = TRUE, caption = "Ordered Logistic Regressions using the HRC's classifications", stars = c(.001, .01, .05, .1), symbol = dotsym, file = paste0(outlocgit, "HRCmlist.txt"))


## Start adding files that show how HRC is affected by


## Model trans_dis (transgender antidiscrimination statutes) using the
## full breadth of the data. Trying to do the same as ScoreCardCats,
## but without repeating this same code over and over.

dv1 <- "trans_dis"

simplervar <- lapply(ivls[-4], customglm, dv1)
texreg(simplervar, file = paste0(outlocgit, "simplevartransdisc.txt"), caption.above = T, caption = "Event History Analysis of Transgender Anti-Discrimination")

## Bind control variables we want to the variables we want to consider in a variable called toreg.
## Note to self, it appears that we get what we want if we list the variable of interest last rather than first.
toreg <- lapply(incasstrevVars, function(x) c(typcont[[length(typcont)]], x) )

tdregsNconts <- lapply(toreg, customglm, deev = dv1)
## outreg(tdregsNconts, "html")

tdmodsub1 <- tdregsNconts[1:10]
tdmodsub2 <- tdregsNconts[11:length(tdregsNconts)]

## outreg(tdregsNconts, type = "html")

tdcap1 <- "Event History Analysis of Transgender Anti-Discrimination Policy with Controls, Models 1-10"
tdlatex1 <- texreg(tdmodsub1, caption.above = T, caption = tdcap1, stars = c(.001, .01, .05, .1), symbol = dotsym, file = paste0(outlocgit, 'tdmodsub1.txt'))

tdcap2 <- paste0("Event History Analysis of Transgender Anti-Discrimination Policy with Controls, Models 11-", length(tdregsNconts))
tdmodnames <- paste("Model", 11:length(tdregsNconts))
tdlatex2 <- texreg(tdmodsub2, caption.above = T, caption = tdcap2, custom.model.names = tdmodnames, stars = c(.001, .01, .05, .1), symbol = dotsym, file = paste0(outlocgit, 'tdmodsub2.txt'))

## write the results to tables
## write.table(tdlatex1, file = paste0(outlocgit, 'tdmodsub1.txt'), quote = F, row.names = F, col.names = F)
## write.table(tdlatex2, file = paste0(outlocgit, 'tdmodsub2.txt'), quote = F, row.names = F, col.names = F)



## Begin working on gay discrimination variables
dv2 <- "gay_disc"
gdmods <- lapply(toreg, customglm, deev = dv2)


## Write gay discrimination mods to latex tables
gdmodsub1 <- gdmods[1:10]
gdmodsub2 <- gdmods[11:length(gdmods)]
names(gdmodsub2) <- paste("Model", 11:length(gdmods))

predictOMatic(gdmods[[length(gdmods)]], interval = "confidence")

## outreg(gdmodsub1, type = 'html')
gdlatex1 <- texreg(gdmodsub1, caption.above = T, caption = "Event History Analysis of Gay Anti-Discrimination Policy with added Controls, 1-10", stars = c(.001, .01, .05, .1), symbol = dotsym, file = paste0(outlocgit, "gdlatex1.txt"))


modcap2 <- paste0("Event History Analysis of Gay Anti-Discrimination Policy with added Controls, 11-", length(gdmods))
gdlatex2 <- texreg(gdmodsub2, caption.above = T, caption = modcap2, custom.model.names = paste("Model", 11:length(gdmods)), stars = c(.001, .01, .05, .1), symbol = dotsym, file = paste0(outlocgit, "gdlatex2.txt"))


# some isolated varsofint

smallno234ivs <- lapply(typcont, c, "realastpercap_smallno234")
no234mods <- lapply(smallno234ivs, customglm, deev = "trans_dis")
no234tex <- texreg(no234mods, caption.above = T, caption = "A further examination of real assets per capita on Transgender Anti-discrimination, sans certain groups", stars = c(.001, .01, .05, .1), symbol = dotsym, file = paste0(outlocgit, "no234textrans.txt"))
## write.table(no234tex, file = paste0(outlocgit, "no234textrans.txt"), quote = F, row.names = F, col.names = F)

no234mods <- lapply(smallno234ivs, customglm, deev = "gay_disc")
no234tex <- texreg(no234mods, caption.above = T, caption = "A further examination of real assets per capita on Gay Anti-discrimination, sans certain groups", stars = c(.001, .01, .05, .1), symbol = dotsym, file = paste0(outlocgit, "no234texgay.txt"))
## write.table(no234tex, paste0(outlocgit, "no234texgay.txt"), quote = F, row.names = F, col.names = F)

realastivs <- lapply(typcont, c, "realastpercapall")
for(i in c("gay_disc", "trans_dis")[1:2]){
    mods <- lapply(realastivs, customglm, deev = i)
    ifelse(i == "trans_dis", mcap <- "An Examination of Real Assets Per Capita on Transgender Anti-discrimination", mcap <- "An Examination of Real Assets Per Capita on Gay Anti-discrimination")
    ifelse(i == "gay_disc", modsavr <- mods, modsavr <- c(modsavr, mods))
    mtex <- texreg(mods, caption.above = T, caption = mcap, stars = c(.001, .01, .05, .1), symbol = dotsym, paste0(outlocgit, "realastpercapall",i, ".txt"))
}

## These shouldn't need to change, but we might want to add one for the ssph
censussphivs <- lapply(typcont, c, "censsph2000")
for(i in c("gay_disc", "trans_dis")[1:2]){
    mods <- lapply(censussphivs, customglm, deev = i)
    ifelse(i == "trans_dis", mcap <- "An Examination of the effect of Same Sex Couples Per household, Census 2000 Measures on Transgender Anti-discrimination", mcap <- "An Examination of the effect of Same Sex Couples Per household, Census 2000 Measures on Gay Anti-discrimination")
    ifelse(i == "gay_disc", modsavr <- mods, modsavr <- c(modsavr, mods))
    mtex <- texreg(mods, caption.above = T, caption = mcap, stars = c(.001, .01, .05, .1), symbol = dotsym, paste0(outlocgit, "censsph2000",i, ".txt"))
}

acs5ssphivs <- lapply(typcont, c, "acs5ssph2012")
for(i in c("gay_disc", "trans_dis")[1:2]){
    mods <- lapply(acs5ssphivs, customglm, deev = i)
    ifelse(i == "trans_dis", mcap <- "An Examination of the effect of Same Sex Couples Per household, ACS 2012 Measures on Transgender Anti-discrimination", mcap <- "An Examination of the effect of Same Sex Couples Per household, ACS 2012 Measures on Gay Anti-discrimination")
    ifelse(i == "gay_disc", modsavr <- mods, modsavr <- c(modsavr, mods))
    mtex <- texreg(mods, caption.above = T, caption = mcap, stars = c(.001, .01, .05, .1), symbol = dotsym, paste0(outlocgit, "acs5ssph2012",i, ".txt"))
}




## So now let's take another look at that HRC data
typcont[[1]] %in% colnames(SCCdat)

incasstrevVars %in% colnames(SCCdat)

unique(unlist(toreg)) %in% colnames(SCCdat)

dv3 <- "ScoreCardCats"

lapply(toreg[[1]], custompolr, dv3, thedat = SCCdat)

dv3 %in% colnames(SCCdat)
SCCdat[, toreg[[1]]]


## joined F-test for two measures of same-sex resources
compvars <- c(nussphVars, "Williams", "realastpercap_smallno234", "realastpercapall")
ivsforF <- lapply(compvars, function(u) c( typcont[[4]], u))

freemodtrans <- customglm(typcont[[4]], dv1)
freemodgay <- customglm(typcont[[4]], dv2)


fmodtrans1 <- customglm(typcont[[4]], dv1, model.frame(compmodstrans[[1]]))


compmodstrans <- lapply(ivsforF, customglm, dv1, freemodtrans$model)
compmodsgay <- lapply(ivsforF, customglm, dv2)

lapply(compmodstrans, function(x){
    freemod <- customglm(typcont[[4]], dv1, model.frame(x))
    anova(freemod, x, test = "Chisq")
})

lapply(compmodsgay, function(x){
    freemod <- customglm(typcont[[4]], dv2, model.frame(x))
    anova(freemod, x, test = "Chisq")
})

## Possibly what he's looking for?
droptab <- drop1(customglm( c(compvars, typcont[[4]]), dv1), test = "F")

str(droptab)


library(xtable)
print(xtable(droptab), type = "latex", sanitize.text.function = identity)
