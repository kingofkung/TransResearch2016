## Combine the HRC Data with the other information that we have available.
location <- "/Users/bjr/GitHub/TransResearch2016/"
source(paste0(location,"OpenFiles.R"))

head(HRC)

HRC$State[46]


HRC$abb <- unlist(lapply(HRC$State, function(u) state.abb[match(u, state.name)]))

HRC$Codes <- paste0(HRC$abb, HRC$Year)

popnprice$matchcodes <- paste0(popnprice$state, popnprice$bmfyear)
