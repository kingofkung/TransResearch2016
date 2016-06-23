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
hexmap <- hexmap[!hexmap$id %in% "DC",]

## Read in some data that will be colors

dat <- read.csv(paste0(loc,"JT DHM LGBT Group Resources.csv"))


prefcols <- data.frame(state.abb, col = state.x77[, "Income"])

hexmap2 <- merge(hexmap, prefcols, by.x = "id", by.y = "state.abb", all.x = TRUE)
## Looks like in order to
hexmap2 <- hexmap2[order(hexmap2$order),]
head(hexmap2)

writeloc <- "/Users/bjr/Dropbox/R_Projects/MapSomething/"

dev.new()
pdf(paste0(writeloc, "hexmap1.pdf"))

## coord_map() give us a mercator projection. It's quite nice.
hex <- ggplot(hexmap2) + geom_polygon(aes(x = long, y = lat, group = group, fill = col), color = "darkgray") + coord_map()
hex <- hex + scale_fill_gradient(high = "#132B43", low = "#56B1F7", breaks = seq(6000, 3000, -500), guide = guide_legend(title = "Incomes") )
##
##
## Add labeling. gCentroid is supposed to give us the central
## locations of the polygons. Byid means it separates them out by id
## before returning them.
labcoords <- gCentroid(hexin, byid = TRUE)
labs <-data.frame(cbind(data.frame(labcoords), id = hexin@data$iso3166_2))
##
hex <- hex + geom_text(data = labs, aes(label = id, x = x, y = y), color = "white", size = 2)
##
hex <- hex + theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())  + ggtitle("Benjamin Rogers' Hexgrid Demonstration:\nPer Capita Income in Dollars Circa 1974\n")
##
print(hex)

## Found out that unlike dev.off(), graphics.off() actually closes all the windows R creates
graphics.off()
