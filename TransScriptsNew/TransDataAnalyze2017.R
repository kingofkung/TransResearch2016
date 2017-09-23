rm(list = ls(all.names = TRUE))

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
mergeDat <- read.csv("JamisDataMerged.csv", na = c("#N/A", "NA"), stringsAsFactors = FALSE)
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

