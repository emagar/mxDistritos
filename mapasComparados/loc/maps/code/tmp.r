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
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/maps/code/")
setwd(wd)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/")
md <- c("/home/eric/Dropbox/data/mapas/cartografia28feb2013rojano/")
md2 <- "../" # c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/maps/")
edo <- "cps"
edon <- 7

# geospatial data 
library(spdep); library(maptools)
# used to determine what datum rojano data has
library(rgdal)
#gpclibPermit()
tmp <- paste("../../../fed/shp/", edo, sep = "") # archivo con mapas 2018
#tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
se.map <- readOGR(dsn = tmp, layer = 'SECCION')
summary(se.map)
# projects to a different datum with long and lat
se.map <- spTransform(se.map, osm()) # project to osm native Mercator


# read comparative district maps
# a. from seccion2dis map, in order to export into se.map for sub-setting
#sec2dis <- read.csv("/home/eric/Dropbox/data/mapas/reseccionamiento/equivSecc/tablaEquivalenciasSeccionales1994-2010.2013.csv", stringsAsFactors = FALSE)
#sec2dis <- sec2dis[sec2dis$edon == 18,]
sec2dis <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/cpsLoc.csv", stringsAsFactors = FALSE)
# send to seccion map
tmp <- data.frame(SECCION = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
dim(tmp)
tmp <- merge(x = tmp, y = sec2dis, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), grep("^dis.+$", colnames(tmp))]
#tmp <- tmp[order(tmp$orden), grep("SECCION|^dis.+$", colnames(tmp))]
se.map@data <- cbind(se.map@data, tmp)
rm(tmp)
# df2006.map <- unionSpatialPolygons(se.map, se.map$disn) # proper way to get federal district objects... if only seccion shapefiles had no problems

head(se.map$seccion)
head(tmp$)



# Now the dissolve
region <- gUnaryUnion(region, id = region@data$country)

# If you want to recreate an object with a data frame
# make sure row names match
row.names(region) <- as.character(1:length(region))

# Extract the data you want (the larger geography)
lu <- unique(lu$country)
lu <- as.data.frame(lu)
colnames(lu) <- "country"  # your data will probably have more than 1 row!

# And add the data back in
region <- SpatialPolygonsDataFrame(region, lu)

# Check it's all worked
plot(region)



writeOGR(counties.rg, ".", "counties-rgdal", driver="ESRI Shapefile")
