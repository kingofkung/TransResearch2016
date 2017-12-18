use "/Users/bjr/Dropbox/LGBT Interest group data/TransTeamDatFall17/taylor rogers haider markel 11.5.17  stata 12.dta"

stset Year, id(State) failure(doma == 1) origin(Year == 1995)

// now remember that everything that goes into stcox is actually an independent variable...
stcox timeVar, exactp


//stcox realastpercap_smallno234 citi6013 inst6013_adacope jobslp evangelical censusRegion

