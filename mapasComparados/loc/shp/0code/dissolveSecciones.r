library(rgdal)

rm(list = ls())

# get mun names and votes
#load("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/aymu1977-present.RData")
d <- read.csv(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/aymu1977-present.csv", stringsAsFactors=FALSE)
munvot <- d
rm(list=setdiff(ls(), "munvot")) # clean
#
wd2 <- c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/data/")
setwd(wd2)
load(file="elDatForMaps.RData")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/maps/0code/")
setwd(wd)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/")
md <- c("/home/eric/Dropbox/data/mapas/cartografia28feb2013rojano/")
md2 <- "../" # c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/maps/")
edo <- "cps"
edon <- 7

# geospatial data 
library(spdep); library(maptools); library(OpenStreetMap)
# used to determine what datum rojano data has
library(rgdal)
#gpclibPermit()
tmp <- paste("../../../fed/shp/", edo, sep = "") # archivo con shapefiles ine 2017
#tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
se.map <- readOGR(dsn = tmp, layer = 'SECCION')
summary(se.map)
#plot(se.map)
# projects to a different datum with long and lat
se.map <- spTransform(se.map, osm()) # project to osm native Mercator


# read comparative district maps
# a. from seccion2dis map, in order to export into se.map for sub-setting
#sec2dis <- read.csv("/home/eric/Dropbox/data/mapas/reseccionamiento/equivSecc/tablaEquivalenciasSeccionales1994-2010.2013.csv", stringsAsFactors = FALSE)
#sec2dis <- sec2dis[sec2dis$edon == 18,]
sec2dis <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/cpsLoc.csv", stringsAsFactors = FALSE)
#sec2dis <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cpsLoc(indig)24Dist.csv", stringsAsFactors = FALSE) # archivo Julia
colnames(sec2dis)
# send to seccion map
tmp <- data.frame(SECCION = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
head(tmp)
sec2dis$seccion
tmp <- merge(x = tmp, y = sec2dis, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
tmp <- tmp[, grep("^dis.+$", colnames(tmp))]
#tmp <- tmp[order(tmp$orden), grep("SECCION|^dis.+$", colnames(tmp))]
se.map@data <- cbind(se.map@data, tmp)
rm(tmp)
# df2006.map <- unionSpatialPolygons(se.map, se.map$disn) # proper way to get federal district objects... if only seccion shapefiles had no problems

names(se.map@data)
table(se.map$disloc2018)
table(se.map$juanZepedaPerez1)
colnames(se.map@data)


# Now the dissolve
library(rgeos)
tmp <- gUnaryUnion(se.map, id = se.map@data$disloc2012)
tmp <- gUnaryUnion(se.map, id = se.map@data$antonioPerezGomez1)
plot(tmp)
## #
## tmp2 <- unionSpatialPolygons(se.map, se.map$disloc2018) # proper way to get federal district objects... if only seccion shapefiles had no problems
## plot(tmp2)

# If you want to recreate an object with a data frame
# make sure row names match
row.names(tmp) <- as.character(1:length(tmp))

# Extract the data you want (the larger geography)
lu <- data.frame()
lu <- rbind(lu, se.map@data)
names(se.map@data)
lu <- unique(lu$antonioPerezGomez1)
lu <- as.data.frame(lu)
colnames(lu) <- "antonioPerezGomez1"  # your data will probably have more than 1 row!

# And add the data back in
tmp <- SpatialPolygonsDataFrame(tmp, lu)

# Check it all worked
plot(tmp)

getwd()
d <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/2clean/cps"
writeOGR(tmp, d, "antonioPerezGomez1", driver="ESRI Shapefile")
