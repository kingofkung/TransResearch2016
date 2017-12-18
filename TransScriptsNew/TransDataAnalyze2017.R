rm(list = ls(all.names = TRUE))

library(stargazer)
library(survival)

## let us do the usual thing, in which we take the variables, and transform them into a custom glm function
customGlm <- function(DVName, IVnames, dat){
    ## need to create the Right hand side first. Putting in all
    ## independent variable names as text before
    rhs <- paste(IVnames, collapse = " + ")
    ## craft formula from dv, rhs
    theForm <- as.formula(paste(DVName, "~", rhs))
    ## and Run glm on that formula
    glm(theForm, data = dat, family = 'binomial')
}


## Having done the work on getting the data together, let us begin with our analysis

## grab the data:
setwd("/Users/bjr/Dropbox/LGBT Interest group data/TransTeamDatFall17")
## read in the merged data as .csv
## mergeDat <- read.csv("JamisDataMerged.csv", na = c("#N/A", "NA"), stringsAsFactors = FALSE)
mergeDat <- readstata13::read.dta13("taylor rogers haider markel  11.5.17.dta")
## note to self, if it says dt, then it refers to the "ssphh over time
## with williams straight line 1990 2008.xlsx" dataset
str(mergeDat)

## grab dependent variables
dvsInt <- c("doma", "superdoma", "trans_dis", 'gay_disc')
## and the controls of interest we had last time
typCont <- c("citi6013", "inst6013_adacope", "jobslp", "evangelical", 'censusRegion')

## make lists of IVs for the paper
mainIVLs <- c('realastpercap_smallno234', 'realastpercap_smallno234Lag', 'realastpercapall', 'realastpercapallLag', 'censsph2000', 'acs5ssph2012', 'williams', "timeVar", "timeVarLag", "timeVarDt", "timeVarDtLag", "timeVarDtWill", "timeVarDtWillLag", "timeVarWill", "timeVarWillLag")
mainIVLs %in% colnames(mergeDat)
colnames(mergeDat)


lapply(dvsInt, customGlm, typCont, mergeDat)

lapply(mainIVLs, function(x) customGlm(dvsInt[1], x, mergeDat))



r1 <- coxph(Surv(Year, event = trans_dis, origin = 1995) ~
            realastpercap_smallno234 + cluster(State),
            data = mergeDat, ties = "breslow")
print(r1)


lhs <- "Surv(Year, event = gay_disc)"
rhs <- paste(mainIVLs[1])  ##,  "+", paste( typCont, collapse = " + "))
##
##
ehaForm <- as.formula( paste(lhs, rhs, sep = "~"))
##
eha1 <- coxph(ehaForm, data = mergeDat, ties = "breslow")
summary(eha1)

## make my left hand sides for the eha
lhsVec <- vapply(seq(dvsInt), function(x) paste0("Surv(Year, event = ", dvsInt[x], ")"), character(1))

## and paste together the right hand side as well
rhsVec <- vapply(mainIVLs, function(x){
    paste(x, paste(typCont, collapse = " + "), sep = "+")
}, character(1))


## so putting 2 dots in setwd moves one up, and looks for the directory. Why does it work when I'm already there though?
sg <- function(x, ...) stargazer(x, ..., type = "html")

## create list for eventual DV information
dvTiList <- c("DOMA" = "doma", "Super DOMA" = "superdoma", "Trans Antidiscrimination Legislation" = "trans_dis", "Gay Antidiscrimination Legislation" = "gay_disc")

myCovLabs <- c("Constant",
               "Census Region 4", "Census Region 3", "Census Region 2",
               "Evangelical Population", "Jobs LP",
               "Insitutional Ideology", "Citizen Ideology",
               "timeVarWillLag", "timeVarWill",
               "timeVarDtWillLag", "timeVarDtWill",
               "timeVarDtLag", "timeVarDt", "timeVarLag", "timeVar", "Williams Measure", "SSPH 2012 ACS", "Census Same Sex 2000", "Real Assets Per Capita Lagged","Real Assets Per Capita Lagged", "Real Assets, no 234 groups Lagged", "Real Assets, no 234 groups")


setwd("../BensOutput")
getwd()
for(i in seq(lhsVec)){
    ## customglm
    custGlms <- lapply(rhsVec, function(x) customGlm(dvsInt[i], x, mergeDat))
    custEhas <- lapply(rhsVec, function(x, lh = lhsVec){
        frm <- as.formula(paste(lh[i], x, sep = "~"))
        coxph(frm, data = mergeDat, ties = "breslow")
    })
    ## Release the output.
    ## get the name of the dv
    dvName <- names(dvTiList)[grepl(dvsInt[i], dvTiList)]
    capture.output(stargazer(custGlms,
                             out = paste0(dvsInt[i], "glmRegs.html"),
                             type = "html",
                             covariate.labels = rev(myCovLabs),
                             title = paste("Predicting", dvName, "Via Generalized Linear Model")))
    capture.output(stargazer(custEhas,
                             out = paste0(dvsInt[i], "ehaRegs.html"),
                             type = "html",
                             ## there's no constant in this one...
                             covariate.labels = rev(myCovLabs[-1]),
                             title = paste("Predicting", dvName, "Via Cox Proportional Hazards Model")))
##
}


