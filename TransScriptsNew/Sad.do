use "/Users/bjr/Dropbox/LGBT Interest group data/TransTeamDatFall17/JamisDataMerged.dta"

stset Year, failure(doma == 1)

// now remember that everything that goes into stcox is actually an independent variable...
stcox realastpercap_smallno234


//stcox realastpercap_smallno234 citi6013 inst6013_adacope jobslp evangelical censusRegion

