rm(list = ls(all.names = TRUE))
## So Dr. Taylor has asked me to do a new analysis using the
## regressions she did, and I'm working on them now...
library(readstata13)
library(rockchalk)

## the first thing we'll need to do is read in the file.
datWd <- paste0("/Users/bjr/Dropbox/LGBT Interest group data",
                "/TransTeamDatFall17")

outWd <- "/Users/bjr/GitHub/TransResearch2016/TransOutputNewer/"
setwd(datWd)
##
##
f <- read.dta13("taylor rogers haider-markel  11.27.17.dta")

head(f)

## replicate gayempnondisc models
gnd1 <- glm(gayempnondisc ~ realastpercap_smallno234 + citi6013 +
                inst6014_nom + evangldsper + gayempdiff,
            data = f, family = binomial)

gnd2 <- glm(gayempnondisc ~ realincpercap_smallno234 + citi6013 +
                inst6014_nom + evangldsper + gayempdiff,
            data = f, family = binomial)

gnd3 <- glm(gayempnondisc ~ realperkassetsno234perlgbtpop + citi6013 +
                inst6014_nom + evangldsper + gayempdiff,
            data = f, family = binomial)

cat(outreg(list(gnd1, gnd2, gnd3), type = "html"),
    file = paste0(outWd, "gayempnondisc.html"))
## transpublicaccomm models
tpb1 <- glm(transpublicaccomm  ~
            realastpercap_smallno234 + citi6013 +
                inst6014_nom + evangldsper + transpublicaccomdiffusion,
            data = f, family = binomial)

tpb2 <- glm(transpublicaccomm  ~
            realastpercapall + citi6013 +
                inst6014_nom + evangldsper + transpublicaccomdiffusion,
            data = f, family = binomial)

tpb3 <- glm(transpublicaccomm  ~
            realincpercap_smallno234 + citi6013 +
                inst6014_nom + evangldsper + transpublicaccomdiffusion,
            data = f, family = binomial)

tpb4 <- glm(transpublicaccomm  ~
            realincpercapall + citi6013 +
                inst6014_nom + evangldsper + transpublicaccomdiffusion,
            data = f, family = binomial)

tpb5 <- glm(transpublicaccomm  ~
            realperkassetsno234perlgbtpop  + citi6013 +
                inst6014_nom + evangldsper + transpublicaccomdiffusion,
            data = f, family = binomial)
cat(outreg(list(tpb1, tpb2, tpb3, tpb4, tpb5), type = "html"),
    file = paste0(outWd, "transpublicaccomm.html"))




## transempnondiscrim models
tnd1 <- glm(transempnondiscrim ~
            realastpercap_smallno234 + citi6013 +
                inst6014_nom + evangldsper + transempdiffusion,
            data = f, family = binomial)

tnd2 <- glm(transempnondiscrim ~
            realincpercap_smallno234 + citi6013 +
                inst6014_nom + evangldsper + transempdiffusion,
            data = f, family = binomial)

tnd3 <- glm(transempnondiscrim ~
            realastpercapall + citi6013 +
                inst6014_nom + evangldsper + transempdiffusion,
            data = f, family = binomial)

tnd4 <- glm(transempnondiscrim ~
            realincpercapall + citi6013 +
                inst6014_nom + evangldsper + transempdiffusion,
            data = f, family = binomial)

tnd5 <- glm(transempnondiscrim ~
            realperkassetsno234perlgbtpop + citi6013 +
                inst6014_nom + evangldsper + transempdiffusion,
            data = f, family = binomial)

cat(outreg(list(tnd1, tnd2, tnd3, tnd4, tnd5), type = "html"),
    file = paste0(outWd, "transempnondiscrim.html"))

