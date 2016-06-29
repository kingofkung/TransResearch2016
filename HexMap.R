## Try and do this hexagonal map, with a minimum of help.

library(ggplot2)
library(dplyr)
library(maptools)
library(rgeos)

## read in my shapefile
maploc <- "/Users/bjr/Dropbox/R_Projects/MapSomething/us_states_hexgrid/"
hexin <- readShapePoly(paste0(maploc, "us_states_hexgrid.shp"))
## head(hexin)

hexmap <- fortify(hexin, region = "iso3166_2")
head(hexmap)

## Remove DC
## hexmap <- hexmap[!hexmap$id %in% "DC",]
## add DC to state.abb
which( state.abb == "DE")
state.abb2 <- c(state.abb[1:7], "DC", state.abb[8:length(state.abb)])

## Read in some data that will be colors

loc <- "/Users/bjr/Dropbox/LGBT Interest group data/"
dat <- read.csv(paste0(loc,"JT DHM LGBT Group Resources.csv"))
din <- dat[complete.cases(dat$incomeall) , c("statename", "year", "incomeall")]

incall <- aggregate(din[,3], by = list(din$statename), function(x) log(mean(x, na.rm = T), 10))


prefcols <- data.frame(state.abb2, col = incall)
colnames(prefcols)[colnames(prefcols) == "col.x"] <- "colr"


hexmap2 <- merge(hexmap, prefcols, by.x = "id", by.y = "state.abb2", all.x = TRUE)
## Looks like in order to
hexmap2 <- hexmap2[order(hexmap2$order),]
head(hexmap2)

writeloc <- "/Users/bjr/GitHub/TransResearch2016/Output/"

dev.new()
pdf(paste0(writeloc, "hexmap1.pdf"))

## coord_map() give us a mercator projection. It's quite nice.
hex <- ggplot(hexmap2) + geom_polygon(aes(x = long, y = lat, group = group, fill = colr), color = "darkgray") + coord_map()

brkseq <- quantile(prefcols$colr, probs = seq(0, 1, .1), na.rm = TRUE)

hex <- hex + scale_fill_gradient(low = "green", high = "blue", breaks = brkseq,  guide = guide_legend(title = "Incomes") )
##
##
## Add labeling. gCentroid is supposed to give us the central
## locations of the polygons. Byid means it separates them out by id
## before returning them.
labcoords <- gCentroid(hexin, byid = TRUE)
labs <- data.frame(cbind(data.frame(labcoords), id = hexin@data$iso3166_2))
##
hex <- hex + geom_text(data = labs, aes(label = id, x = x, y = y), color = "white", size = 2)
##
plotdesc <- "Logged Mean group Income in Dollars between 1995 and 2015"
hex <- hex + theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())  + ggtitle(paste("Benjamin Rogers' Hexgrid Demonstration:\n", plotdesc))
##
print(hex)

## Found out that unlike dev.off(), graphics.off() actually closes all the windows R creates
graphics.off()
