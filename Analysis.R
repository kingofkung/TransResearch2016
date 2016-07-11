## We'll store the analyses of the data we collect in here
library(plyr)
library(MASS)
library(zoo)
library(texreg)
library(stargazer)
library(rockchalk)

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

##Typical control variables
## Note, I had  "inst6014_nom" in here, but decided to remove it as it appeared to be multicollinear with both our ivs of interest as well as the other institutional variable. We can put it back in if we'd like later though
typcont <- c("citi6013", "inst6013_adacope", laxPhillips[9], "ssph", "Williams", "squire", "evangelical")
typcont <- lapply(seq_along(typcont), function(x) typcont[1:x])

## get income/Revenue/assets variables and combine them into a list
revVars <- grep("rev", colnames(dat), value = T)
incVars <- grep("inc", colnames(dat), value = T)
## Fun fact: | is the or command, and it works in regex the same as elsewhere
assetVars <- grep("asset|ast", colnames(dat), value = T)
incasstrevVars <- c(revVars, incVars, assetVars)

## Do a correlation matrix on the variables of interest
incContsCors <- cor(dat[, unique(unlist(c(typcont, incasstrevVars))) ], use = "pairwise.complete.obs")
## write.csv(incContsCors, file = paste0(outlocgit, 'ControlsAndGroupresourcescorrelations.csv'))

oc,"JT DHM LGBT Group Resources.csv"), row.names = F)

## Get ScoreCardCats into the correct Order
 SCCLvls <- c("High Priority To Achieve Basic Equality", "Building Equality", "Solidifying Equality", "Working Toward Innovative Equality")
dat$ScoreCardCats <- factor(dat$ScoreCardCats, levels = SCCLvls)
levels(dat$ScoreCardCats)

SCCdat <- dat[ !is.na(dat$ScoreCardCats),]
head(SCCdat)

nacols <- apply(SCCdat, 2, function(x) all(is.na(x)))
names(nacols[!nacols==T])

## Models for ScoreCardCats ordinal Logit regression
## Incomeall isn't working correctly, but its square root and percentages return a result
SCCdat$iasqrt <- sqrt(SCCdat$incomeall)
SCCdat$iaplus <- SCCdat$incomeall + 1
SCCdat$iaperc <- SCCdat$incomeall/sum(as.numeric(SCCdat$incomeall))
incomeall <- as.numeric(SCCdat$incomeall)

ivls <- c("MeanOppLP", "inst6013_adacope", "inst6014_nom", "citi6013", "iaperc", "realincpercapall", "orgcountall", laxPhillips[2:8])
onevarHRCmods <- lapply(ivls, function(x) {
    r <- as.formula(paste0("ScoreCardCats ~ ",x))
    eval(bquote(polr(.(r), data = SCCdat, method = "logistic")))

})
onevarHRCmods <- lapply(onevarHRCmods, FUN = extract, include.thresholds = TRUE)
latextext1 <- texreg(onevarHRCmods,  reorder.coef = NULL, caption.above = TRUE, caption = "Ordered Logistic Regressions using the HRC's classifications")
## The columns ordered properly
write.table(latextext1 , file = paste0(outlocgit, "HRCmlist.tex"), quote = F, row.names = F, col.names = F)


## Model trans_dis (transgender antidiscrimination statutes) using the
## full breadth of the data. Trying to do the same as ScoreCardCats,
## but without repeating this same code over and over.

dv1 <- "trans_dis"

customglm <- function(x, deev = dv1){
    x <- ifelse(length(x) > 1, paste0(x, collapse = "+"), x)
    u <- as.formula(paste(deev, x, sep = " ~ "))
    eval(bquote(glm(.(u), family = "binomial", data = dat)))
    }



## bind control variables we want to the variables we want to consider in a variable called toreg
toreg <- lapply(incasstrevVars, function(x) c(x, typcont[[length(typcont)]]) )

tdregsNconts <- lapply(toreg, customglm)
## outreg(tdregsNconts, "html")

tdmodsub1 <- tdregsNconts[1:10]
tdmodsub2 <- tdregsNconts[11:length(tdregsNconts)]

## outreg(tdregsNconts, type = "html")

tdcap1 <- "Event History Analysis of Transgender Discrimination Policy with Controls, Models 1-10"
tdlatex1 <- texreg(tdmodsub1, caption.above = T, caption = tdcap1)

tdcap2 <- paste0("Event History Analysis of Transgender Discrimination Policy with Controls, Models 11-", length(tdregsNconts))
tdmodnames <- paste("model", 11:length(tdregsNconts))
tdlatex2 <- texreg(tdmodsub2, caption.above = T, caption = tdcap2, custom.model.names = tdmodnames)

## write the results to tables
write.table(tdlatex1, file = paste0(outlocgit, 'tdmodsub1.txt'), quote = F, row.names = F, col.names = F)
write.table(tdlatex2, file = paste0(outlocgit, 'tdmodsub2.txt'), quote = F, row.names = F, col.names = F)



dv2 <- "gay_disc"
gdmods <- lapply(toreg, customglm, deev = dv2)
## outreg(gdmods, 'html')

## outreg(gdmods, type = 'html')
## Write gay discrimination mods to latex tables
gdmodsub1 <- gdmods[1:10]
gdcoefOrder1 <- c(1, 3:(length(typcont) + 2), 2, (length(typcont) + 3):(length(gdmodsub1) + 5))


gdmodsub2 <- gdmods[11:length(gdmods)]
names(gdmodsub2) <- paste("Model", 11:length(gdmods))
gdcoefOrder2 <- c(1, 3:(length(typcont) + 2), 2, (length(typcont) + 3):(length(gdmodsub1) + 4))

## outreg(gdmodsub1, type = 'html')
gdlatex1 <- texreg(gdmodsub1, caption.above = T,
                   caption = "Event History Analysis of Gay Discrimination Policy with added Controls, 1-10",
                   reorder.coef = gdcoefOrder1)

write.table(gdlatex1, file = paste0(outlocgit, "gdlatex1.txt"), quote = F, row.names = F, col.names = F)

modcap2 <- paste0("Event History Analysis of Gay Discrimination Policy with added Controls, 11-", length(gdmods))
gdlatex2 <- texreg(gdmodsub2, caption.above = T, caption = modcap2, reorder.coef = gdcoefOrder2, custom.model.names = paste("Model", 11:length(gdmods))



write.table(gdlatex2, file = paste0(outlocgit, "gdlatex2.txt"), quote = F, row.names = F, col.names = F)


m <- glm(gay_disc ~ citi6013 + inst6014_nom  + MeanOppLP + ssph + realastpercap_smallno234, data = dat, "binomial")
summary(m)

malt <- glm(gay_disc ~ citi6013  + inst6014_nom   + realastpercap_smallno234 + evangelical + Williams + squire + ssph, data = dat, "binomial")
summary(malt)
