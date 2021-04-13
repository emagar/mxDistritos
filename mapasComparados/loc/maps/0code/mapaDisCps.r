# script is adapted from one in /data/elecs/MXelsCalendGovt/atlasDis/code/

#source(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/code/mapPrep.r") # sólo correr si hubiera cambios en los datos

## # OJO: when using spTranform in script, use line below for google earth, or next line for OSM/google maps
#x.map <- spTransform(x.map, CRS("+proj=longlat +datum=WGS84"))
#x.map <- spTransform(x.map, osm()) # project to osm native Mercator

# to use osm backgrounds
library(rJava)
library(OpenStreetMap)
library(rgdal)


rm(list = ls())
# get mun names and votes
#load("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/aymu1977-present.RData")
d <- read.csv(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/aymu1977-present.csv", stringsAsFactors=FALSE)
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
library(spdep); library(maptools)
# used to determine what datum rojano data has
library(rgdal)
#gpclibPermit()
tmp <- paste("../../../fed/shp/disfed2018/", edo, sep = "") # archivo con mapas 2017
#tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
se.map <- readOGR(dsn = tmp, layer = 'SECCION')
summary(se.map)
# projects to a different datum with long and lat
se.map <- spTransform(se.map, osm()) # project to osm native Mercator
plot(se.map)

# read all state borders from rojano
ed.map <- list()
## tmp <- "../../../fed/shp/ags" # archivo con mapas 2018
## # tmp <- paste(md, "ags", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ags <- tmp
## #
## tmp <- "../../../fed/shp/bc" # archivo con mapas 2018
## # ## tmp <- paste(md, "bc", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$bc <- tmp
## #
## tmp <- "../../../fed/shp/bcs" # archivo con mapas 2018
## # ## tmp <- paste(md, "bcs", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$bcs <- tmp
## #
tmp <- "../../../fed/shp/disfed2018/cam" # archivo con mapas 2018
## # tmp <- paste(md, "cam", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$cam <- tmp
## #
## tmp <- "../../../fed/shp/coa" # archivo con mapas 2018
## # ## tmp <- paste(md, "coa", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$coa <- tmp
## #
## tmp <- "../../../fed/shp/col" # archivo con mapas 2018
## # ## tmp <- paste(md, "col", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$col <- tmp
## #
tmp <- "../../../fed/shp/disfed2018/cps" # archivo con mapas 2018
# ## tmp <- paste(md, "cps", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$cps <- tmp
## #
## tmp <- "../../../fed/shp/cua" # archivo con mapas 2018
## # ## tmp <- paste(md, "cua", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cua <- tmp
##
## tmp <- "../../../fed/shp/df" # archivo con mapas 2018
## # ## tmp <- paste(md, "df", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$df <- tmp
##
## tmp <- "../../../fed/shp/dgo" # archivo con mapas 2018
## # ## tmp <- paste(md, "dgo", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$dgo <- tmp
## #
## tmp <- "../../../fed/shp/gua" # archivo con mapas 2018
## # ## tmp <- paste(md, "gua", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$gua <- tmp
## #
## tmp <- "../../../fed/shp/gue" # archivo con mapas 2018
## # ## tmp <- paste(md, "gue", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$gue <- tmp
## #
## tmp <- "../../../fed/shp/hgo" # archivo con mapas 2018
## # ## tmp <- paste(md, "hgo", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$hgo <- tmp
## #
## tmp <- "../../../fed/shp/jal" # archivo con mapas 2018
## # ## tmp <- paste(md, "jal", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$jal <- tmp
## #
## tmp <- "../../../fed/shp/mex" # archivo con mapas 2018
## # ## tmp <- paste(md, "mex", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$mex <- tmp
## #
## tmp <- "../../../fed/shp/mic" # archivo con mapas 2018
## # ## tmp <- paste(md, "mic", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$mic <- tmp
## #
## tmp <- "../../../fed/shp/mor" # archivo con mapas 2018
## # ## tmp <- paste(md, "mor", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$mor <- tmp
## #
## tmp <- "../../../fed/shp/nay" # archivo con mapas 2018
## # ## tmp <- paste(md, "nay", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nay <- tmp
## #
## tmp <- "../../../fed/shp/nl" # archivo con mapas 2018
## # ## tmp <- paste(md, "nl", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nl <- tmp
## #
tmp <- "../../../fed/shp/disfed2018/oax" # archivo con mapas 2018
## # tmp <- paste(md, "oax", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$oax <- tmp
## #
## tmp <- "../../../fed/shp/pue" # archivo con mapas 2018
## # ## tmp <- paste(md, "pue", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$pue <- tmp
## #
## tmp <- "../../../fed/shp/que" # archivo con mapas 2018
## # ## tmp <- paste(md, "que", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$que <- tmp
## #
## tmp <- "../../../fed/shp/qui" # archivo con mapas 2018
## # ## tmp <- paste(md, "qui", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$qui <- tmp
## #
## tmp <- "../../../fed/shp/san" # archivo con mapas 2018
## # ## tmp <- paste(md, "san", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$san <- tmp
## #
## tmp <- "../../../fed/shp/sin" # archivo con mapas 2018
## # ## tmp <- paste(md, "sin", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$sin <- tmp
## #
## tmp <- "../../../fed/shp/son" # archivo con mapas 2018
## # ## tmp <- paste(md, "son", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$son <- tmp
## #
tmp <- "../../../fed/shp/disfed2018/tab" # archivo con mapas 2018
## # tmp <- paste(md, "tab", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$tab <- tmp
## #
## tmp <- "../../../fed/shp/tam" # archivo con mapas 2018
## # ## tmp <- paste(md, "tam", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tam <- tmp
## ## #
## tmp <- "../../../fed/shp/tla" # archivo con mapas 2018
## # ## tmp <- paste(md, "tla", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tla <- tmp
## #
tmp <- "../../../fed/shp/disfed2018/ver" # archivo con mapas 2018
## # tmp <- paste(md, "ver", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$ver <- tmp
## #
## tmp <- "../../../fed/shp/yuc" # archivo con mapas 2018
## # ## tmp <- paste(md, "yuc", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$yuc <- tmp
## #
## tmp <- "../../../fed/shp/zac" # archivo con mapas 2018
## # ## tmp <- paste(md, "zac", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$zac <- tmp

# read municipios
#tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
tmp <- paste("../../../fed/shp/disfed2018/", edo, sep = "") # archivo con mapas 2018
mu.map <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# polygon area in km2
crs(mu.map) # projection is in m
mu.map$km2 <- round(area(mu.map) / 1000000, 1)
mu.map@data[, c("municipio","nombre","km2")]
# projects to a different datum with long and lat
mu.map <- spTransform(mu.map, osm())
mu.map$mun <- mu.map$nombre # mun names
# read cabeceras municipales
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
cab <- readOGR(dsn = tmp, layer = 'CABECERA_MUNICIPAL')
names(cab) <- tolower(names(cab))
# projects to a different datum with long and lat
cab <- spTransform(cab, osm())
#cab$LOCALIDAD_.1 # names
#
# read shapefiles distritos locales 
tmp <- paste("/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, sep = "") # archivo con mapas locales
dl.map <- readOGR(dsn = tmp, layer = 'disloc2018')
colnames(dl.map@data) <- c("edon","tipo","disloc","id")
# projects to a different ./datum with long and lat
dl.map <- spTransform(dl.map, osm()) # project to osm native Mercator
# read disloc2012
dl2012.map <- readOGR(dsn = tmp, layer = 'disloc2012')
colnames(dl2012.map@data) <- c("disloc")
# projects to a different datum with long and lat
dl2012.map <- spTransform(dl2012.map, osm()) # project to osm native Mercator

# add father/son info and dsi of mapLoc
dsi <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/simIndex/dist_cps.csv"
dsi <- read.csv(file = dsi, stringsAsFactors = FALSE)
head(dsi)
#
#dl.map$ord <- 1:nrow(dl.map@data)
dl.map@data <- merge(x = dl.map@data, y = dsi, by.x = "disloc", by.y = "disloc2018", all.x = TRUE, all.y = FALSE)
rm(dsi)

# read comparative district maps
# a. from seccion2dis map, in order to export into se.map for sub-setting
#sec2dis <- read.csv("/home/eric/Dropbox/data/mapas/reseccionamiento/equivSecc/tablaEquivalenciasSeccionales1994-2010.2013.csv", stringsAsFactors = FALSE)
#sec2dis <- sec2dis[sec2dis$edon == 18,]
sec2dis <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/cpsLoc.csv", stringsAsFactors = FALSE)
head(sec2dis)
# send to seccion map
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = sec2dis, by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), grep("^dis.+$", colnames(tmp))]
#tmp <- tmp[order(tmp$orden), grep("SECCION|^dis.+$", colnames(tmp))]
se.map@data <- cbind(se.map@data, tmp)
rm(tmp)
# df2006.map <- unionSpatialPolygons(se.map, se.map$disn) # proper way to get federal district objects... if only seccion shapefiles had no problems

# aa. Add Julia's lenguas
li <- read.csv(file = "../../cpsJulia/07_poblacion_lenguas_seccion.csv", stringsAsFactors = FALSE)
summary(li$homb + li$muj - li$pob) # check pob ok
li$ne <- NULL # drop no especificado, suspiciously close to my linhabOther totals
sel <- which(li$dif!=0)
li$linhabOther[sel] <- li$linhabOther[sel] + li$dif[sel] # adds difference with p5li as unspecified lengua
#
## # subset cols
## tmp <- li[, c("tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "mame", "kanjobal", "cluj", "zapoteco", "maya", "akatako", "chinanteco", "nahuatl", "linhabOther")]
## sel <- which(rowSums(tmp) - li$p5li !=0)
## data.frame(li$seccion[sel], rowSums(tmp)[sel] - li$p5li[sel], li$p5li[sel])
#
# número efectivo de lenguas indígenas
# subset cols
tmp <- li[, c("tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "mame", "kanjobal", "cluj", "zapoteco", "maya", "akatako", "chinanteco", "nahuatl", "linhabOther")]
sel <- which(rowSums(tmp)==0)
tmp <- tmp/rowSums(tmp) # shares
tmp.hh <- rowSums(tmp^2)
nel <- 1/tmp.hh
tmp.max <- apply(tmp, 1, max)
nel.moli <- 1 + nel * (tmp.hh - tmp.max^2) / tmp.hh
li$neli <- nel
li$neliMoli <- nel.moli
li$neli[sel] <- li$neliMoli[sel] <- 0
rm(tmp, tmp.hh, nel, tmp.max, nel.moli, sel)
#
# número efectivo de lenguas incluyendo español
li$soloEsp <- li$p5 - li$p5li
tmp <- li[, c("tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "mame", "kanjobal", "cluj", "zapoteco", "maya", "akatako", "chinanteco", "nahuatl", "linhabOther","soloEsp")]
sel <- which(rowSums(tmp)==0) # secciones wo lengua data
tmp <- tmp/li$p5 # shares including spanish
tmp.hh <- rowSums(tmp^2)
nel <- 1/tmp.hh
tmp.max <- apply(tmp, 1, max)
nel.moli <- 1 + nel * (tmp.hh - tmp.max^2) / tmp.hh
li$nelwEsp <- nel
li$nelwEspMoli <- nel.moli
li$nelwEsp[sel] <- li$nelwEspMoli[sel] <- 0 # secciones wo lengua data have NAs, remove them
rm(tmp, tmp.hh, nel, tmp.max, nel.moli, sel)
#
# lengua predominante
# subset cols
tmp <- li[, c("tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "mame", "kanjobal", "cluj", "zapoteco", "maya", "akatako", "chinanteco", "nahuatl", "linhabOther","soloEsp")]
li$lengMax <- colnames(tmp)[max.col(tmp, ties.method = "first")] ## returns label of row's max 
#
# shares
li$homb <- li$homb / li$pob
li$muj <- li$muj / li$pob
li$p5li <- li$p5li / li$pob
#
# make colors
library(RColorBrewer)
nclr <- 5                                    #CATEGORÍAS DE COLOR (MIN=3 MAX=9)
mauve <- brewer.pal(nclr,"BuPu")             #GENERA CODIGOS DE COLOR QUE CRECEN CON GRADO
redgreen6 <- brewer.pal(6, "RdYlGn"); redgreen6 <- rev(redgreen6)
redgreen7 <- brewer.pal(7, "RdYlGn"); redgreen7 <- rev(redgreen7)
redgreen8 <- brewer.pal(8, "RdYlGn"); redgreen8 <- rev(redgreen8)
redgreen9 <- brewer.pal(9, "RdYlGn"); redgreen9 <- rev(redgreen9)
catcol <- brewer.pal(9, "Set1")
catcol <- c(catcol[9], catcol[2:8], catcol[1]) # invert colors for esp and oth

library(plyr)
tmp <- as.numeric(cut(li$p5li, seq(0,1,by = .2), include.lowest = TRUE)) # cut p5li into 5 categories
li$p5licat <- mapvalues ( tmp, from = 1:5, to = mauve)
#
# next do same for neli etc
li$nel.i.woEsp <- NA; sel <- which(li$p5li>=.4)
  table(round(li$nel.i.woEsp[sel],1), useNA = "always")   # debug
  leg.nel.i.woEsp <- seq(1, 3.4, .3); leg.nel.i.woEsp # debug
li$nel.i.woEsp[sel] <- cut( round(li$neli[sel],1), breaks = leg.nel.i.woEsp, include.lowest = TRUE)
li$nel.i.woEsp[sel] <- mapvalues ( li$nel.i.woEsp[sel], from = 1:8, to =redgreen8  )
#
li$mol.i.woEsp <- NA; sel <- which(li$p5li>=.4)
  table(round(li$mol.i.woEsp[sel],1), useNA = "always") # debug
  leg.mol.i.woEsp <- seq(1, 2.2, .2); leg.mol.i.woEsp   # debug
li$mol.i.woEsp[sel] <- cut( round(li$neliMoli[sel],1), breaks = leg.mol.i.woEsp, include.lowest = TRUE)
li$mol.i.woEsp[sel] <- mapvalues ( li$mol.i.woEsp[sel], from = 1:6, to =redgreen6  )
#
li$mol.all.wEsp <- NA; sel <- which(li$nelwEspMoli==.0) # drop zeroes, but compute for all secciones in cps
  table(round(li$mol.all.wEsp[-sel],1), useNA = "always") # debug
  leg.mol.all.wEsp <- seq(1, 2.8, .2); leg.mol.all.wEsp     # debug
li$mol.all.wEsp[-sel] <- cut( round(li$nelwEspMoli[-sel],1), breaks = leg.mol.all.wEsp, include.lowest = TRUE)
li$mol.all.wEsp[-sel] <- mapvalues ( li$mol.all.wEsp[-sel], from = 1:9, to =redgreen9  )
#
li$nel.all.wEsp <- NA; sel <- which(li$nelwEsp==.0) # drop zeroes, but compute for all secciones in cps
  table(round(li$nel.all.wEsp[-sel],1), useNA = "always") # debug
  leg.nel.all.wEsp <- seq(1, 3.8, .4); leg.nel.all.wEsp # debug
li$nel.all.wEsp[-sel] <- cut( round(li$nelwEsp[-sel],1), breaks = leg.nel.all.wEsp, include.lowest = TRUE)
li$nel.all.wEsp[-sel] <- mapvalues ( li$nel.all.wEsp[-sel], from = 1:7, to =redgreen7  )
#
li$nel.i.wEsp <- NA; sel <- which(li$p5li>=.4)
  table(round(li$nel.i.wEsp[sel],1), useNA = "always")   # debug
  leg.nel.i.wEsp <- seq(1, 3.4, .3); leg.nel.i.wEsp # debug
li$nel.i.wEsp[sel] <- cut( round(li$neli[sel],1), breaks = leg.nel.i.wEsp, include.lowest = TRUE)
li$nel.i.wEsp[sel] <- mapvalues ( li$nel.i.wEsp[sel], from = 1:8, to =redgreen8  )
#
li$mol.i.wEsp <- NA; sel <- which(li$p5li>=.4)
  table(round(li$mol.i.wEsp[sel],1), useNA = "always") # debug
  leg.mol.i.wEsp <- seq(1, 2.2, .2); leg.mol.i.wEsp   # debug
li$mol.i.wEsp[sel] <- cut( round(li$neliMoli[sel],1), breaks = leg.mol.i.wEsp, include.lowest = TRUE)
li$mol.i.wEsp[sel] <- mapvalues ( li$mol.i.wEsp[sel], from = 1:6, to =redgreen6  )
#
# colors for majority tongue
li$lengMaxcat <- mapvalues ( li$lengMax, from = c("soloEsp", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "linhabOther"), to = catcol )

# export to se.map
tmp <- se.map@data
tmp$order <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = li, by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$order),]
tmp$order <- NULL
se.map@data <- tmp
rm(tmp)

# b. from rojano's 2006 distrito map, which has good-looking shapefiles
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
df2006.map <- readOGR(dsn = tmp, layer = 'DISTRITO')
# projects to a different datum with long and lat
df2006.map <- spTransform(df2006.map, osm())

# c. from ine's 2018 distrito maps
tmp <- paste("../../../fed/shp/disfed2018/", edo, sep = "") # archivo con mapas 2018
df.map <- readOGR(dsn = tmp, layer = 'DISTRITO')
# projects to a different datum with long and lat
df.map <- spTransform(df.map, osm())
head(df.map@data)

## # read cabeceras distritales (via vocal ejecutivo)
## tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
## cabDis <- readOGR(dsn = tmp, layer = 'VOCAL_EJECUTIVO_DISTRITAL')
## # projects to a different datum with long and lat
## cabDis <- spTransform(cabDis, osm())
## #
## cabDisNames <- read.csv(paste(wd2, "cabeceras2006.csv", sep = ""), stringsAsFactors = FALSE)

# add casillas in 2012
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
cas.map <- readOGR(dsn = tmp, layer = 'CASILLA')
names(cas.map) <- tolower(names(cas.map))
# projects to a different datum with long and lat
cas.map <- spTransform(cas.map, osm()) # project to osm native Mercator
#
# add districts for subsetting
tmp <- cas.map@data; tmp$ord <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = se.map[,c("seccion","disfed2006","disfed2018","disloc2018","disloc2012")], by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$ord),]
cas.map@data <- tmp
#
# drop casillas from missing secciones to avoid indeterminate subsetting
sel <- which(is.na(cas.map$disloc2018)==TRUE)
if (length(sel)>0) cas.map <- cas.map[-sel,] # drop missing cases
rm(sel)

# add ncasillas in 2012 to seccion map
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = ncasillas[ncasillas$edon==edon, c("seccion","e12")], by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("seccion","e12")]; 
se.map$ncasillas <- tmp$e12
# make colors
library(RColorBrewer)
nclr <- 6                                    #CATEGORÍAS DE COLOR (MIN=3 MAX=9)
purples <- brewer.pal(nclr,"Purples")            #GENERA CODIGOS DE COLOR QUE CRECEN CON GRADO
#display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, colorblindFriendly=TRUE)
library(plyr)
se.map$ncascol[se.map$ncasillas==1] <- purples[1]
se.map$ncascol[se.map$ncasillas==2] <- purples[2]
se.map$ncascol[se.map$ncasillas>=3  & se.map$ncasillas<=5]  <- purples[3]
se.map$ncascol[se.map$ncasillas>=6  & se.map$ncasillas<=10] <- purples[4]
se.map$ncascol[se.map$ncasillas>=11 & se.map$ncasillas<=20] <- purples[5]
se.map$ncascol[se.map$ncasillas>=21]                        <- purples[6]

# add nwin to seccion map
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = nwin[nwin$edon==edon,], by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("seccion","pan","pri","prd")]
se.map$nwinpan <- tmp$pan
se.map$nwinpri <- tmp$pri
se.map$nwinprd <- tmp$prd
rm(tmp)
#
# make colors
library(RColorBrewer)
nclr <- 7                                    #CATEGORÍAS DE COLOR (MIN=3 MAX=9)
blues <- brewer.pal(nclr,"Blues")            #GENERA CODIGOS DE COLOR QUE CRECEN CON GRADO
reds <- brewer.pal(nclr,"Reds")              
yellows <- brewer.pal(nclr,"YlOrBr")         
library(plyr)
se.map$pancol <- mapvalues ( se.map$nwinpan, from = 0:6, to = blues )
se.map$pricol <- mapvalues ( se.map$nwinpri, from = 0:6, to = reds )
se.map$prdcol <- mapvalues ( se.map$nwinprd, from = 0:6, to = yellows )
# bastiones = 4+ wins
se.map$bastion <- rgb(190,190,190, maxColorValue = 255) # "gray"
## se.map$bastion[se.map$nwinpan>=4] <- "blue"
## se.map$bastion[se.map$nwinpri>=4] <- "red"
## se.map$bastion[se.map$nwinprd>=4] <- "gold"
se.map$bastion[se.map$nwinpan==4] <- blues[4]
se.map$bastion[se.map$nwinpan==5] <- blues[5]
se.map$bastion[se.map$nwinpan==6] <- blues[6]
se.map$bastion[se.map$nwinpri==4] <- reds[4]
se.map$bastion[se.map$nwinpri==5] <- reds[5]
se.map$bastion[se.map$nwinpri==6] <- reds[6]
se.map$bastion[se.map$nwinprd==4] <- yellows[4]
se.map$bastion[se.map$nwinprd==5] <- yellows[5]
se.map$bastion[se.map$nwinprd==6] <- yellows[6]

# add seccion volat 2012-2105 = max change
# add lisnom2015
tmp1 <- v12[v12$edon==edon,]
tmp1$pri <- tmp1$pri + tmp1$pric; tmp1 <- tmp1[, c("seccion","pan","pri","prdc","pvem","panal")]
tmp1$pt <- tmp1$mc <- tmp1$morena <- tmp1$ph <- tmp1$ps <- tmp1$indep1 <- tmp1$indep2 <- 0
tmp1 <- tmp1[, c("seccion","pan","pri","prdc","pvem","pt","mc","panal","morena","ph","ps","indep1","indep2")]
tmp1[,-1] <- round(tmp1[,-1] / v12[v12$edon==edon, c("efec")], 3) # vote shares
#
tmp2 <- v15[v15$edon==edon,]
tmpv <- tmp2[,c("seccion","lisnom")] # save to add lisnom below
tmp2$pri <- tmp2$pri + tmp2$pric;
tmp2$prd <- tmp2$prd + tmp2$prdc; tmp2 <- tmp2[, c("seccion","pan","pri","prd","pvem","pt","mc","panal","morena","ph","ps","indep1","indep2")]
tmp2[,-1] <- round(tmp2[,-1] / v15[v15$edon==edon, c("efec")], 3) # vote shares
#
tmp <- apply(X = abs(tmp2[,-1] - tmp1[,-1]), MARGIN = 1, FUN = max) # absolute 2015-2012 vote shares by party
tmp3 <- cbind(seccion=tmp1$seccion, volat=tmp, lisnom=tmpv$lisnom)
#
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = tmp3, by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
se.map$volat1215 <- tmp$volat
se.map$ln15 <- tmp$lisnom
#
# add seccion fch, epn, amlo
# handy function to sort one data frame by order of another, matching data frame
sortBy <- function(target, By){
    t <- target; b <- By;
    do.call(rbind, lapply(seq_len(nrow(b)), 
            function(i) as.character(unlist(t[i,])[order(unlist(-b[i,]))]))) # change to -b if decreasing wished
}
# 2006
tmpv <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/elecReturns/datosBrutos/resultSecciones/prSeccion2006.csv", stringsAsFactors = FALSE)
tmpv <- tmpv[tmpv$edon==edon,]
tmpv$edon <- tmpv$disn <- tmpv$munn <- tmpv$id_elec <- tmpv$nr <- tmpv$nul <- tmpv$tot <- tmpv$lisnom <- NULL
colnames(tmpv)[grep("pan$", colnames(tmpv))] <- "fch"
colnames(tmpv)[grep("apm", colnames(tmpv))] <- "pri"
colnames(tmpv)[grep("pbt", colnames(tmpv))] <- "amlo"
tmpv2 <- tmpv[,-1] # votes only
etiq <- data.frame(matrix(rep(colnames(tmpv2), nrow(tmpv2)), nrow=nrow(tmpv2), byrow = TRUE), stringsAsFactors = FALSE)
etiq <- sortBy(target = etiq, By = tmpv2)
tmpv2 <- t(apply(tmpv2, 1, function(x) sort(x, decreasing = TRUE)))
#
pres <- data.frame(seccion=tmpv$seccion, win06p=etiq[,1], mg06p=round( (tmpv2[,1] - tmpv2[,2])/rowSums(tmpv2), 3), fch=round(tmpv$fch/rowSums(tmpv2), 3), amlo06=round(tmpv$amlo/rowSums(tmpv2), 3))
#
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = pres, by.x = "seccion", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
#
# 2012
tmpv <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/elecReturns/datosBrutos/resultSecciones/prSeccion2012.csv", stringsAsFactors = FALSE)
tmpv <- tmpv[tmpv$edon==edon,]
tmpv$edon <- tmpv$disn <- tmpv$munn <- tmpv$urbrur <- tmpv$nr <- tmpv$nul <- tmpv$lisnom <- NULL
tmpv$amlo <- tmpv$prd + tmpv$pt + tmpv$mc + tmpv$prdptmc + tmpv$prdpt + tmpv$prdmc + tmpv$ptmc
tmpv$epn <- tmpv$pri + tmpv$pvem + tmpv$pripvem
tmpv$prd <- tmpv$pt <- tmpv$mc <- tmpv$prdptmc <- tmpv$prdpt <- tmpv$prdmc <- tmpv$ptmc <- tmpv$pri <- tmpv$pvem <- tmpv$pripvem <- NULL
tmpv2 <- tmpv[,-1] # votes only
etiq <- data.frame(matrix(rep(colnames(tmpv2), nrow(tmpv2)), nrow=nrow(tmpv2), byrow = TRUE), stringsAsFactors = FALSE)
etiq <- sortBy(target = etiq, By = tmpv2)
tmpv2 <- t(apply(tmpv2, 1, function(x) sort(x, decreasing = TRUE)))
#
pres <- data.frame(seccion=tmpv$seccion, win12p=etiq[,1], mg12p=round( (tmpv2[,1] - tmpv2[,2])/rowSums(tmpv2), 3), epn=round(tmpv$epn/rowSums(tmpv2), 3), amlo12=round(tmpv$amlo/rowSums(tmpv2), 3))
tmp <- merge(x = tmp, y = pres, by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
#
se.map@data <- cbind(se.map@data, tmp)
colnames(se.map@data)
rm(tmp, tmp1, tmp2, tmp3, tmpv)

# add district ptot, rri proj at each election
load(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/votPobDis0018.RData")
tmp <- votPobDis0018$pob.distMap2006
tmp <- tmp[tmp$edon==edon,]
tmp1 <- df2006.map@data
tmp1$ord <- 1:nrow(tmp1)
tmp1 <- merge(x=tmp1, y=tmp, by.x="DISTRITO", by.y="disn")
tmp1[, grep("rri", colnames(tmp1))] <- round(tmp1[, grep("rri", colnames(tmp1))],2)
tmp1 <- tmp1[order(tmp1$ord), grep("ptot|rri", colnames(tmp1))]
df2006.map@data <- cbind(df2006.map@data, tmp1)
df2006.map$disrri06 <- paste(df2006.map$DISTRITO, " (", df2006.map$rris2006, ")", sep="")
df2006.map$disrri15 <- paste(df2006.map$DISTRITO, " (", df2006.map$rris2015, ")", sep="")
                         
## # export attributes for maps with other software
## se.map$bastion2 <- "swing"
## se.map$bastion2[se.map$nwinpan==4] <- "pan4"
## se.map$bastion2[se.map$nwinpan==5] <- "pan5"
## se.map$bastion2[se.map$nwinpan==6] <- "pan6"
## se.map$bastion2[se.map$nwinpri==4] <- "pri4"
## se.map$bastion2[se.map$nwinpri==5] <- "pri5"
## se.map$bastion2[se.map$nwinpri==6] <- "pri6"
## se.map$bastion2[se.map$nwinprd==4] <- "izq4"
## se.map$bastion2[se.map$nwinprd==5] <- "izq5"
## se.map$bastion2[se.map$nwinprd==6] <- "izq6"
## #
## # add centroids
## tmp <- coordinates(se.map)
## tmp <- data.frame(seccion=se.map$SECCION, edon=se.map$ENTIDAD, disn=se.map$disn, munn=se.map$MUNICIPIO, bastion=se.map$bastion2, ncasillas=sqrt(se.map$ncasillas), lon=tmp[,1], lat=tmp[,2], stringsAsFactors = FALSE)
## write.csv(tmp, file = paste(md, edo, "/magar.csv", sep = "") )
## tmp <- c("\"Integer\"",    "\"Integer\"",    "\"Integer\"",      "\"Integer\"",       "\"Integer\"",        "\"String\"",          "\"Integer\"",                   "\"Real\"", "\"Real\"") #,        "\"Integer\"",      "\"String\"")
## write(tmp, file = paste(md, edo, "/magar.csvt", sep = ""), ncolumns = length(tmp), sep = "," )


# (use 1984 long/lat for this map when mercator projection was chosen)
p84 <- function(x = NA){
    x <- x
    x <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
}
portray  <- se.map$bastion        # elegir qué reportará el mapa 1
portray2 <- se.map$p5licat        # elegir qué reportará el mapa 2
#
portray4 <- se.map$lengMaxcat     # elegir qué reportará el mapa 4
#
portray01 <- se.map$nel.i.woEsp    # elegir qué reportará el mapa 01
portray02 <- se.map$mol.i.woEsp    # elegir qué reportará el mapa 02
portray03 <- se.map$nel.all.wEsp   # elegir qué reportará el mapa 03
portray04 <- se.map$mol.all.wEsp   # elegir qué reportará el mapa 04
portray05 <- se.map$nel.i.wEsp     # elegir qué reportará el mapa 05
portray06 <- se.map$mol.i.wEsp     # elegir qué reportará el mapa 06

gray <- rgb(190,190,190, maxColorValue = 255)

# function to re-compute square bbox
sqbbox <- function(bbox = NULL){
    b <- bbox
    maxrange <- max(b$max[2] - b$min[2], b$max[1] - b$min[1])
    m1 <- (b$min[1] + b$max[1])/2
    m2 <- (b$min[2] + b$max[2])/2
    #
    b2 <- data.frame(min=c(NA,NA), max=c(NA,NA)); rownames(b2) <- c("x","y")
    b2$min[1] <- m1 - maxrange/2
    b2$min[2] <- m2 - maxrange/2
    b2$max[1] <- m1 + maxrange/2
    b2$max[2] <- m2 + maxrange/2
    return(b2)
}

############################################################################
############################################################################
## all state's municipios and secciones, colored by % indigenas, in one map 
############################################################################
############################################################################
m <- p84(ed.map$cps)  # subsetted map
b <- as.data.frame(m@bbox)
b <- sqbbox(b)
## dn <- 2                  # elegir un distrito
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, dn, "-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##                                         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
#pdf(file = paste(md2, edo, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Secciones y municipios indígenas de Chiapas")#, bg = "lightblue")
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .95))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=mauve, title = "% indígena", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomleft")
#dev.off()

########################
## historia electoral ##
########################
#pdf(file = paste(md2, edo, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), axes = TRUE, main = "Historia electoral de municipios y secciones de Chiapas")#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .67))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
legend(x="bottomleft", bg = "white", legend=c("municipios","secciones"), col=c("black","gray"), lty = c(1,1), lwd = c(1,1), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()


# distritos c indígenas
disIndig <- c(4, 5, 7, 8, 9, 11, 20, 21, 22)

######################################################################
## número efectivo d lenguas -- nel en subset indígena sin español  ##
######################################################################
#pdf(file = paste(md2, edo, "-nel.i.woEsp.pdf", sep = ""))
#png(file = paste(md2, edo, "-nel.i.woEsp.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Pluralidad lingüística de secciones indígenas (excl. español)")#, bg = "lightblue")
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray01, .95))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
tmp <- leg.nel.i.woEsp[1:8] # select legend labels
legend(x="bottomright", bg = "white", legend=tmp, fill=redgreen8, title = "nMolinar lenguas", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomleft")
#dev.off()

##########################################################################
## número efectivo d lenguas -- Molinar en subset indígena sin español  ##
##########################################################################
#pdf(file = paste(md2, edo, "-mol.i.woEsp.pdf", sep = ""))
#png(file = paste(md2, edo, "-mol.i.woEsp.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Pluralidad lingüística de secciones indígenas (excl. español)")#, bg = "lightblue") # "Pluralidad lingüística de secciones y municipios indígenas"
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .95))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
tmp <- leg.mol.i.woEsp[1:6] # select legend labels
legend(x="bottomright", bg = "white", legend=tmp, fill=redgreen6, title = "nMolinar lenguas", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomleft")
#dev.off()

###########################################################
## número efectivo d lenguas --- nel en todo con español ##
###########################################################
#pdf(file = paste(md2, edo, "-nel.all.wEsp.pdf", sep = ""))
#png(file = paste(md2, edo, "-nel.all.wEsp.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Pluralidad lingüística de secciones y municipios (incl. español)")#, bg = "lightblue")
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray03, .95))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
tmp <- leg.nel.all.wEsp[1:7] # select legend labels
legend(x="bottomright", bg = "white", legend=tmp, fill=redgreen7, title = "N efec. lenguas", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomleft")
#dev.off()

#########################################################
## número efectivo d lenguas --- Molinar w esp en todo ##
#########################################################
#pdf(file = paste(md2, edo, "-mol.all.wEsp.pdf", sep = ""))
#png(file = paste(md2, edo, "-mol.all.wEsp.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Pluralidad lingüística de secciones y municipios (incl. español)")#, bg = "lightblue")
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray04, .95))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
tmp <- leg.mol.all.wEsp[1:9] # select legend labels
legend(x="bottomright", bg = "white", legend=tmp, fill=redgreen9, title = "nMolinar lenguas", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomleft")
#dev.off()

######################################################################
## número efectivo d lenguas -- nel en subset indígena con español  ##
######################################################################
#pdf(file = paste(md2, edo, "-nel.i.wEsp.pdf", sep = ""))
#png(file = paste(md2, edo, "-nel.i.wEsp.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Pluralidad lingüística de secciones indígenas (incl. español)")#, bg = "lightblue")
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray05, .95))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
tmp <- leg.nel.i.wEsp[1:8] # select legend labels
legend(x="bottomright", bg = "white", legend=tmp, fill=redgreen8, title = "nMolinar lenguas", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomleft")
#dev.off()

##########################################################################
## número efectivo d lenguas -- Molinar en subset indígena con español  ##
##########################################################################
#pdf(file = paste(md2, edo, "-mol.i.wEsp.pdf", sep = ""))
#png(file = paste(md2, edo, "-mol.i.wEsp.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Pluralidad lingüística de secciones indígenas (incl. español)")#, bg = "lightblue") # "Pluralidad lingüística de secciones y municipios indígenas"
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray06, .95))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
tmp <- leg.mol.i.wEsp[1:6] # select legend labels
legend(x="bottomright", bg = "white", legend=tmp, fill=redgreen6, title = "nMolinar lenguas", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomleft")
#dev.off()



# lengua predominante
#pdf(file = paste(md2, edo, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Lenguas predominantes en Chiapas (secciones y municipios)")#, bg = "lightblue")
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .95))
# municipal borders
plot(p84(mu.map), add = TRUE, border = "white", lwd = 2.5, lty = 1)
plot(p84(mu.map), add = TRUE, border = "black", lwd = 1, lty = 1)
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
# add municipio numbers
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.89, col = "white")
## text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=catcol, title = "lengua predominante", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomleft")
#dev.off()

##########################
# mapas de las consultas #
##########################
# read shapefiles 1er escenario 2018
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/escenario1", sep = "") # archivo con mapas locales
dle1.map <- readOGR(dsn = tmp, layer = 'escenario1', stringsAsFactors = FALSE)
colnames(dle1.map@data) <- c("id")
# polygon area in km2
crs(dle1.map) # projection is in m
dle1.map$km2 <- round(area(dle1.map) / 1000000, 1)
dle1.map@data
# projects to a different ./datum with long and lat
dle1.map <- spTransform(dle1.map, osm()) # project to osm native Mercator
#



# read shapefiles MagdalenaGironLopez
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/magdalenaGironLopez1", sep = "") # archivo con mapas locales
dlmgl.map <- readOGR(dsn = tmp, layer = 'magdalenaGironLopez1', stringsAsFactors = FALSE)
colnames(dlmgl.map@data) <- c("id")
# projects to a different ./datum with long and lat
dlmgl.map <- spTransform(dlmgl.map, osm()) # project to osm native Mercator
#

# read shapefiles AntonioPerezGomez
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/antonioPerezGomez1", sep = "") # archivo con mapas locales
dlapg.map <- readOGR(dsn = tmp, layer = 'antonioPerezGomez1', stringsAsFactors = FALSE)
colnames(dlapg.map@data) <- c("id")
# projects to a different ./datum with long and lat
dlapg.map <- spTransform(dlapg.map, osm()) # project to osm native Mercator
#

# read shapefiles JoseDomingoVazquezLopez1a
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/joseDomingoVazquezLopez1a", sep = "") # archivo con mapas locales
dljdvla.map <- readOGR(dsn = tmp, layer = 'JoseDomingoVazquezLopez1a', stringsAsFactors = FALSE)
colnames(dljdvla.map@data) <- c("id")
# projects to a different ./datum with long and lat
dljdvla.map <- spTransform(dljdvla.map, osm()) # project to osm native Mercator
#

# read shapefiles JoseDomingoVazquezLopez1b
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/joseDomingoVazquezLopez1b", sep = "") # archivo con mapas locales
dljdvlb.map <- readOGR(dsn = tmp, layer = 'JoseDomingoVazquezLopez1b', stringsAsFactors = FALSE)
colnames(dljdvlb.map@data) <- c("id")
# projects to a different ./datum with long and lat
dljdvlb.map <- spTransform(dljdvlb.map, osm()) # project to osm native Mercator
#

# read shapefiles juanZepedaPerez1
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/juanZepedaPerez1", sep = "") # archivo con mapas locales
dljzp.map <- readOGR(dsn = tmp, layer = 'juanZepedaPerez1', stringsAsFactors = FALSE)
colnames(dljzp.map@data) <- c("id")
# projects to a different ./datum with long and lat
dljzp.map <- spTransform(dljzp.map, osm()) # project to osm native Mercator

 
# read shapefiles leonardoLopezPerez1
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/leonardoLopezPerez1", sep = "") # archivo con mapas locales
dlllp.map <- readOGR(dsn = tmp, layer = 'leonardoLopezPerez1', stringsAsFactors = FALSE)
colnames(dlllp.map@data) <- c("id")
# projects to a different ./datum with long and lat
dlllp.map <- spTransform(dlllp.map, osm()) # project to osm native Mercator

# read shapefiles lorenzoLopezMéndez1a
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/lorenzoLopezMendez1a", sep = "") # archivo con mapas locales
dlllma.map <- readOGR(dsn = tmp, layer = 'lorenzoLopezMendez1a', stringsAsFactors = FALSE)
colnames(dlllma.map@data) <- c("id")
# projects to a different ./datum with long and lat
dlllma.map <- spTransform(dlllma.map, osm()) # project to osm native Mercator

# read shapefiles lorenzoLopezMéndez1b
tmp <- paste("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, "/lorenzoLopezMendez1b", sep = "") # archivo con mapas locales
dlllmb.map <- readOGR(dsn = tmp, layer = 'lorenzoLopezMendez1b', stringsAsFactors = FALSE)
colnames(dlllmb.map@data) <- c("id")
# projects to a different ./datum with long and lat
dlllmb.map <- spTransform(dlllmb.map, osm()) # project to osm native Mercator


# add escenario 1 and indigenous amendments to sec map
tmp <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cpsLoc(indig)ABC.csv"
tmp <- read.csv(file = tmp, stringsAsFactors = FALSE)
colnames(se.map@data)
#
se.map$ord <- 1:nrow(se.map@data)
tmp.se <- se.map@data
tmp.se <- merge(x = tmp.se, y = tmp, by = "seccion", all.x = TRUE, all.y = FALSE)
tmp.se <- tmp.se[order(tmp.se$ord),]
tmp.se$ord <- NULL
se.map@data <- tmp.se
rm(tmp,tmp.se)

############################################################################
############################################################################
## close-up of consulta munic and secc, colored by % indigenas
## MagdalenaGironLopez cambia 2 distritos 17 18
############################################################################
############################################################################
name <- "Magdalena Girón López"
abrev <- "mgl"
zemap <- dlmgl.map
sel1 <- which(dle1.map$id==17 | dle1.map$id==18) # relevant 4 MagdaGiron
sel1s <- which(se.map$escenario1==17 | se.map$escenario1==18)
selm <- which(zemap$id==25 | zemap$id==26)
#plot(dle1.map[sel1,])
## m <- p84(dle1.map[sel1,])  # subsetted map
## b <- as.data.frame(m@bbox)
## b <- sqbbox(b)
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "mgl-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
#pdf(file = paste(md2, edo, "-", abrev, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.57, y = 17.44, labels = "TAB.", col = "darkgray", cex = .9 , srt = -5)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray2[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=alpha(mauve, .67), title = "% indígena", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-", abrev, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.57, y = 17.44, labels = "TAB.", col = "darkgray", cex = .9 , srt = -5)
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray02[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=alpha(redgreen, .67), title = "nMolinar lenguas", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# lengua predominante
#pdf(file = paste(md2, edo, "-", abrev, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.57, y = 17.44, labels = "TAB.", col = "darkgray", cex = .9 , srt = -5)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray4[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=alpha(catcol, .67), title = "lengua predominante", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# historia electoral
#pdf(file = paste(md2, edo, "-", abrev, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.57, y = 17.44, labels = "TAB.", col = "darkgray", cex = .9 , srt = -5)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()

############################################################################
############################################################################
## close-up of consulta munic and secc, colored by % indigenas
## AntonioPerezGomez cambia 3 distritos 17 21 23
############################################################################
############################################################################
name <- "Antonio Pérez Gómez"
abrev <- "apg"
zemap <- dlapg.map
sel1 <- which(dle1.map$id==17 | dle1.map$id==21 | dle1.map$id==23) # relevant districts
sel1s <- which(se.map$escenario1==17 | se.map$escenario1==21 | se.map$escenario1==23)
selm <- which(zemap$id==25 | zemap$id==26)
#plot(dle1.map[sel1,])
## m <- p84(dle1.map[sel1,])  # subsetted map
## b <- as.data.frame(m@bbox)
## b <- sqbbox(b)
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "mgl-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
#pdf(file = paste(md2, edo, "-", abrev, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.75, y = 17.58, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.6, y = 17.555, labels = "TAB.", col = "darkgray", cex = .9 , srt = -90)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray2[sel1s], .67))
#
# escenario1 borders and consulta
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
#plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
#text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
#text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=alpha(mauve, .67), title = "% indígena", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-", abrev, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.75, y = 17.58, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.6, y = 17.555, labels = "TAB.", col = "darkgray", cex = .9 , srt = -90)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray02[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
#plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
#text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
#text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=alpha(redgreen, .67), title = "nMolinar lenguas", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# lengua predominante
#pdf(file = paste(md2, edo, "-", abrev, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.75, y = 17.58, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.6, y = 17.555, labels = "TAB.", col = "darkgray", cex = .9 , srt = -90)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray4[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
#plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
#text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
#text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=alpha(catcol, .67), title = "lengua predominante", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# historia electoral
#pdf(file = paste(md2, edo, "-", abrev, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.75, y = 17.58, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.6, y = 17.555, labels = "TAB.", col = "darkgray", cex = .9 , srt = -90)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
#plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
#text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
#text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()

############################################################################
############################################################################
## close-up of consulta munic and secc, colored by % indigenas
## JoseDomingoVazquezLopez1a cambia distritos 6 y 7 
############################################################################
############################################################################
name <- "José Domingo Vázquez López v1" 
abrev <- "jdvla"
zemap <- dljdvla.map
## # locate districts
## plot(zemap)
## xy <- coordinates(zemap)
## text(xy[,1],xy[,2],zemap$id,col="red",cex=.7)
sel1 <- which(dle1.map$id==6 | dle1.map$id==7) # relevant 4 case
sel1s <- which(se.map$escenario1==6 | se.map$escenario1==7)
selm <- which(zemap$id==25 | zemap$id==26)
#plot(dle1.map[sel1,])
## m <- p84(dle1.map[sel1,])  # subsetted map
## b <- as.data.frame(m@bbox)
## b <- sqbbox(b)
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "mgl-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
#pdf(file = paste(md2, edo, "-", abrev, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.5, y = 15.85, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray2[sel1s], .67))
#
# escenario1 borders and case borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=alpha(mauve, .67), title = "% indígena", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-", abrev, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.5, y = 15.85, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray02[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=alpha(redgreen, .67), title = "nMolinar lenguas", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# lengua predominante
#pdf(file = paste(md2, edo, "-", abrev, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.5, y = 15.85, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray4[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=alpha(catcol, .67), title = "lengua predominante", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# historia electoral
#pdf(file = paste(md2, edo, "-", abrev, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.5, y = 15.85, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()


############################################################################
############################################################################
## close-up of consulta munic and secc, colored by % indigenas
## JoseDomingoVazquezLopez1a cambia distritos 6 7 9 18 19 
############################################################################
############################################################################
name <- "José Domingo Vázquez López v2" 
abrev <- "jdvlb"
zemap <- dljdvlb.map
## # locate districts
## plot(zemap)
## xy <- coordinates(zemap)
## text(xy[,1],xy[,2],zemap$id,col="red",cex=.7)
sel1 <- which(dle1.map$id==6 | dle1.map$id==7 | dle1.map$id==9 | dle1.map$id==18 | dle1.map$id==19) # relevant 4 case
sel1s <- which(se.map$escenario1==6 | se.map$escenario1==7 | se.map$escenario1==9 | se.map$escenario1==18 | se.map$escenario1==19)
selm <- which(zemap$id==25 | zemap$id==26 | zemap$id==27)
#plot(dle1.map[sel1,])
## m <- p84(dle1.map[sel1,])  # subsetted map
## b <- as.data.frame(m@bbox)
## b <- sqbbox(b)
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "mgl-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
#pdf(file = paste(md2, edo, "-", abrev, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.185, y = 17.325, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray2[sel1s], .67))
#
# escenario1 borders and case borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=alpha(mauve, .67), title = "% indígena", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-", abrev, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.185, y = 17.325, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray02[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=alpha(redgreen, .67), title = "nMolinar lenguas", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# lengua predominante
#pdf(file = paste(md2, edo, "-", abrev, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.185, y = 17.325, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray4[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=alpha(catcol, .67), title = "lengua predominante", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# historia electoral
#pdf(file = paste(md2, edo, "-", abrev, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.185, y = 17.325, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()

############################################################################
############################################################################
## close-up of consulta munic and secc, colored by % indigenas
## juanZepedaPerez1 cambia distritos 6 7 9 18 19 
############################################################################
############################################################################
name <- "Juan Zepeda Pérez" 
abrev <- "jzp"
zemap <- dljzp.map
## # locate districts
## plot(zemap)
## xy <- coordinates(zemap)
## text(xy[,1],xy[,2],zemap$id,col="red",cex=.7)
sel1 <- which(dle1.map$id==17 | dle1.map$id==18 | dle1.map$id==19 | dle1.map$id==21 | dle1.map$id==22 | dle1.map$id==23 | dle1.map$id==24) # relevant 4 juanZapedaPerez
sel1s <- which(se.map$escenario1==17 | se.map$escenario1==18 | se.map$escenario1==19 | se.map$escenario1==21 | se.map$escenario1==22 | se.map$escenario1==23 | se.map$escenario1==24)
selm <- which(zemap$id==25 | zemap$id==26 | zemap$id==27 | zemap$id==28 | zemap$id==29 | zemap$id==30)
#plot(dle1.map[sel1,])
## m <- p84(dle1.map[sel1,])  # subsetted map
## b <- as.data.frame(m@bbox)
## b <- sqbbox(b)
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "mgl-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
#pdf(file = paste(md2, edo, "-", abrev, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.3, y = 17.525, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -92.7, y = 17.825, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray2[sel1s], .67))
#
# escenario1 borders and case borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=alpha(mauve, .67), title = "% indígena", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-", abrev, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.3, y = 17.525, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -92.7, y = 17.625, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 25)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray02[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=alpha(redgreen, .67), title = "nMolinar lenguas", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# lengua predominante
#pdf(file = paste(md2, edo, "-", abrev, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.3, y = 17.525, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -92.7, y = 17.625, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 25)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray4[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=alpha(catcol, .67), title = "lengua predominante", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# historia electoral
#pdf(file = paste(md2, edo, "-", abrev, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.3, y = 17.525, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -92.7, y = 17.625, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 25)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()

############################################################################
############################################################################
## close-up of consulta munic and secc, colored by % indigenas
## Leonardo López Pérez cambia distritos 20 21 22 23
############################################################################
############################################################################
name <- "Leonardo López Pérez" 
abrev <- "llp"
zemap <- dlllp.map
## # locate districts
## plot(zemap)
## xy <- coordinates(zemap)
## text(xy[,1],xy[,2],zemap$id,col="red",cex=.7)
sel1 <- which(dle1.map$id==20 | dle1.map$id==21 | dle1.map$id==22 | dle1.map$id==23) # relevant 4 case
sel1s <- which(se.map$escenario1==20 | se.map$escenario1==21 | se.map$escenario1==22 | se.map$escenario1==23)
selm <- which(zemap$id==25 | zemap$id==26 | zemap$id==27)
#plot(dle1.map[sel1,])
## m <- p84(dle1.map[sel1,])  # subsetted map
## b <- as.data.frame(m@bbox)
## b <- sqbbox(b)
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "mgl-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
pdf(file = paste(md2, edo, "-", abrev, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.4, y = 17.525, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -92.7, y = 17.825, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray2[sel1s], .67))
#
# escenario1 borders and case borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="topright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=alpha(mauve, .67), title = "% indígena", bty="o", cex=.9)
#
legend(x="topleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "bottomleft", scale=.75)
addscalebar(style = "ticks", pos = "bottomright")
dev.off()

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-", abrev, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.3, y = 17.525, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -92.7, y = 17.625, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 25)
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray02[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=alpha(redgreen, .67), title = "nMolinar lenguas", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# lengua predominante
#pdf(file = paste(md2, edo, "-", abrev, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.3, y = 17.525, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -92.7, y = 17.625, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 25)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray4[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=alpha(catcol, .67), title = "lengua predominante", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# historia electoral
#pdf(file = paste(md2, edo, "-", abrev, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -91.3, y = 17.525, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
text( x = -92.7, y = 17.625, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 25)
text( x = -91.25, y = 15.7, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = 0)
text( x = -90.75, y = 17, labels = "G U A T E M A L A", col = "darkgray", cex = .9 , srt = -35)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()

############################################################################
############################################################################
## close-up of consulta munic and secc, colored by % indigenas
## lorenzoLopezMéndez1a cambia distritos 1 18 21 22 23 24
############################################################################
############################################################################
name <- "Lorenzo López Méndez v1" 
abrev <- "llma"
zemap <- dlllma.map
## # locate districts
## plot(zemap)
## xy <- coordinates(zemap)
## text(xy[,1],xy[,2],zemap$id,col="red",cex=.7)
sel1 <- which(dle1.map$id==1 | dle1.map$id==18 | dle1.map$id==21 | dle1.map$id==22 | dle1.map$id==23 | dle1.map$id==24) # relevant 4 case
sel1s <- which(se.map$escenario1==1 | se.map$escenario1==18 | se.map$escenario1==21 | se.map$escenario1==22 | se.map$escenario1==23 | se.map$escenario1==24)
selm <- which(zemap$id==25 | zemap$id==26 | zemap$id==27 | zemap$id==28 | zemap$id==29)
#plot(dle1.map[sel1,])
## m <- p84(dle1.map[sel1,])  # subsetted map
## b <- as.data.frame(m@bbox)
## b <- sqbbox(b)
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "mgl-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
#pdf(file = paste(md2, edo, "-", abrev, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.8, y = 17.55, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray2[sel1s], .67))
#
# escenario1 borders and case borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=alpha(mauve, .67), title = "% indígena", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-", abrev, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.8, y = 17.55, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray02[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=alpha(redgreen, .67), title = "nMolinar lenguas", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# lengua predominante
#pdf(file = paste(md2, edo, "-", abrev, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.8, y = 17.55, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray4[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=alpha(catcol, .67), title = "lengua predominante", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# historia electoral
#pdf(file = paste(md2, edo, "-", abrev, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.8, y = 17.55, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()

############################################################################
############################################################################
## close-up of consulta munic and secc, colored by % indigenas
## lorenzoLopezMéndez1b cambia distritos 1 17 18 22 23 24
############################################################################
############################################################################
name <- "Lorenzo López Méndez v2" 
abrev <- "llmb"
zemap <- dlllmb.map
## # locate districts
## plot(zemap)
## xy <- coordinates(zemap)
## text(xy[,1],xy[,2],zemap$id,col="red",cex=.7)
sel1 <- which(dle1.map$id==1 | dle1.map$id==17 | dle1.map$id==18 | dle1.map$id==22 | dle1.map$id==23 | dle1.map$id==24) # relevant 4 case
sel1s <- which(se.map$escenario1==1 | se.map$escenario1==17 | se.map$escenario1==18 | se.map$escenario1==22 | se.map$escenario1==23 | se.map$escenario1==24)
selm <- which(zemap$id==25 | zemap$id==26 | zemap$id==27 | zemap$id==28 | zemap$id==29 | zemap$id==30)
#plot(dle1.map[sel1,])
## m <- p84(dle1.map[sel1,])  # subsetted map
## b <- as.data.frame(m@bbox)
## b <- sqbbox(b)
## # gets xx degrees more than bbox (decimal defines share of max range)
## xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
##     # checks if basemap (type os, saved as R data) is in disk
##     bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
##     if (paste(edo, "mgl-os.RData", sep = "") %in% bmps) {
##         load(file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # gets bg.os
##         bg <- bg.os
##     } else {
##         # choose one of four background picture types
##         #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
##         #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
##         #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
##         bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
##         save(bg.os, file = paste(md2, "basemaps/", edo, "mgl-os.RData", sep = "")) # save a copy of the basemap for future use
##         bg <- bg.os
##     }
#
#pdf(file = paste(md2, edo, "-", abrev, "-p5licat.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-p5licat.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.8, y = 17.65, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray2[sel1s], .67))
#
# escenario1 borders and case borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=alpha(mauve, .67), title = "% indígena", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-", abrev, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.8, y = 17.65, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray02, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray02[sel1s], .67))
#
# escenario1 borders and magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=alpha(redgreen, .67), title = "nMolinar lenguas", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# lengua predominante
#pdf(file = paste(md2, edo, "-", abrev, "-lengMax.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-lengMax.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.8, y = 17.65, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray4, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray4[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "green", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomright", bg = "white", legend=c("español", "tzeltal", "tzotzil", "chol", "zoque", "tojolabal", "kanjobal", "maya", "otras"), fill=alpha(catcol, .67), title = "lengua predominante", bty="o", cex=.9)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
#dev.off()

# historia electoral
#pdf(file = paste(md2, edo, "-", abrev, "-core.pdf", sep = ""))
#png(file = paste(md2, edo, "-", abrev, "-core.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(dle1.map[sel1,]), axes = TRUE, main = name)#, bg = "lightblue")
#plot(bg, add = TRUE)
#
# add neighboring states
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
text( x = -92.8, y = 17.65, labels = "TABASCO", col = "darkgray", cex = .9 , srt = 0)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray[sel1s], .67))
#
# escenario1 borders
# magda giron borders
plot(p84(dle1.map[sel1,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "white", lwd = 4.5)
plot(p84(zemap[selm,]), add = TRUE, border = "red", lwd = 3)
plot(p84(dle1.map[sel1,]), add = TRUE, lwd = 1.5)
# municipal borders
plot(p84(mu.map), add = TRUE, border = "forestgreen", lwd = .75, lty = 3)
# nombres municipios
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.51, col = "green")
## text(coordinates(p84(mu.map)), labels=mu.map$mun, cex=.5)
#
legend(x="bottomleft", bg = "white", legend=c("1er escenario","consulta","municipales"), col=c("black","red","green"), lty = c(1,1,3), lwd = c(4,4,2), title = "Límites", bty="o", cex=.9)
library(prettymapr)
addnortharrow(pos = "topleft", scale=.75)
addscalebar(style = "ticks", pos = "topright")
##################################################################################################################
# bastion legend for three parties
# size reduction for secondary plot (serves as legend) conditional on placement
par(fig = c( .8,  1,  0, .2), new = TRUE) 
clr <- data.frame(pan = blues[c(5,6,7)], pri = reds[c(5,6,7)], prd = yellows[c(5,6,7)], stringsAsFactors = FALSE)
sz <- .75
par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
# par(bg = "white")
plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
for (r in 1:3){
    for (c in 1:3){
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = "white", border = "white", lwd = 4)
        polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .67), border = "white", lwd = 4)
    }
}
for (c in 1:3){
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = "white", border = "white", lwd = 4)
    polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .67), border = "white", lwd = 4)
}
text(x = 1.5, y = 4.2, label = "pan", cex = sz)
text(x = 2.5, y = 4.2, label = "pri", cex = sz)
text(x = 3.5, y = 4.2, label = "izq.", cex = sz)
#text(x = 5,   y = 4.2, label = "won", cex = sz)
text(x = 4.75, y = 3.5, label = "6de6", pos = NULL, cex = sz)
text(x = 4.75, y = 2.5, label = "5de6", pos = NULL, cex = sz)
text(x = 4.75, y = 1.5, label = "4de6", pos = NULL, cex = sz)
text(x = 4.75, y = 0.5, label = "menos", pos = NULL, cex = sz)
###################################################################################################################
#dev.off()



# grafica distritos locales 1 por 1
dn <- 2                  # elegir un distrito
## for (dn in 30:45){
##     print(paste("disn =", dn))
## # plot state map with highlighted district
#png(file = paste(md2, edo, dn, "-1.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Chiapas (mapa local 2018)")#, bg = "lightblue")
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
# 
plot(p84(dl.map), add = TRUE, border = "gray")
plot(p84(dl.map[dl.map$disloc==dn,]), add = TRUE, border = "gray", col = "hotpink")
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
text(coordinates(p84(dl.map)), labels=dl.map$disloc, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#dev.off()

# plot same distrito only
# need to merge disn info into mun and sec object, in order to select just those belonging to dis
# get openstreetmap background
m <- p84(dl.map[dl.map$disloc==dn,])  # subsetted map
b <- as.data.frame(m@bbox)
# gets xx degrees more than bbox (decimal defines share of max range)
xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
#bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
#bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
#bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))
bg <- bg.os
#
#png(file = paste(md2, edo, dn, "-2.png", sep = ""), width=15, height=15, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
tmp <-  dl.map$cab[which(dl.map$disloc==dn)]
tmp2 <- dl.map$dsi[which(dl.map$disloc==dn)]
plot(dl.map[dl.map$disloc==dn,], axes = TRUE, main = paste("Chiapas ", dn, " - ", tmp, " (DSI = ", tmp2, ")", sep = ""))
plot(bg, add = TRUE)
#plot(dl.map[dl.map$disloc==dn,], lwd = 5, add = TRUE) # drop
plot(ed.map$cps, add = TRUE)
library(scales) # has function alpha()
plot(se.map,                         add = TRUE, border = "darkgray", col = alpha(portray, .25)) # color nwin
# plot(se.map[se.map$disn==dn,], add = TRUE, border = "darkgray", col = portray[se.map$disn==dn]) # color nwin -- se.map$disn is disfed
#plot(ffcc, add = TRUE, lwd = .75)
#
#plot(ed.map$nay, add = TRUE, lty = 1, col = rgb(1,1,1, alpha = .5)) # blurs colors inside state
plot(se.map[se.map$disloc2018==dn,], add = TRUE, border = "darkgray", col = alpha(portray[se.map$disloc2018==dn], .5)) # color nwin
# add casillas
points(cas.map, pch = 20, col = "white" , cex = .3)
#points(cas.map[cas.map$disloc2017==dn,], pch = 20, col = rgb(1,1,1,.33), cex = .3)
#
plot(ed.map$oax, add = TRUE, lty = 1)
plot(ed.map$ver, add = TRUE, lty = 1)
plot(ed.map$tab, add = TRUE, lty = 1)
plot(ed.map$cam, add = TRUE, lty = 1)
#
sel <- which(dl2012.map$disloc==dl.map$father[dl.map$disloc==dn])
plot(dl2012.map[sel,], add = TRUE, lwd = 6, border = "red")
#
plot(dl.map[dl.map$disloc==dn,], add = TRUE, lwd = 4)
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
#
plot(ed.map$cps, add = TRUE, lwd = 3)
plot(ed.map$cps, add = TRUE, border = "red", lty = 3, lwd = 2)
#
text(coordinates(mu.map), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(mu.map), labels=mu.map$mun, cex=.5)
lp <- c("bottomright", #1 
        "bottomright", #2 
        "bottomleft",  #3 
        "bottomright", #4 
        "bottomright", #5 
        "bottomright", #6 
        "bottomright", #7 
        "bottomright", #8 
        "bottomright", #9 
        "bottomleft",  #10
        "bottomright", #11
        "bottomright", #12
        "bottomright", #13
        "bottomleft",  #14
        "bottomleft",  #15
        "bottomleft",  #16
        "bottomleft",  #17
        "bottomleft",  #18
        "bottomright", #19
        "topleft",     #20
        "bottomleft",  #21
        "bottomright", #22
        "bottomright", #23
        "bottomleft",  #24
        "bottomleft",  #25
        "bottomleft")  #26
legend(x=lp[dn], bg = "white", legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","black","black","gray"), lty = c(1,1,1,1,1), pch = c(NA,NA,NA,NA,19), lwd = c(6,6,2,2,0), bty="o", cex=.75)
legend(x=lp[dn], bg = NULL,    legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","red","green","white"),  lty = c(1,1,3,3,1), pch = c(NA,NA,NA,NA,20), lwd = c(2,2,2,2,0), bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = ifelse(lp[dn]=="topright", "topleft", "topright"), scale=.75)
addscalebar(style = "ticks", pos = ifelse(lp[dn]=="bottomright", "bottomleft", "bottomright"))

# plot same distrito only: p5li
#png(file = paste(md2, edo, munn, "-p5li.png", sep = ""))
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
tmp <-  dl.map$cab[which(dl.map$disloc==dn)]
tmp2 <- dl.map$dsi[which(dl.map$disloc==dn)]
plot(dl.map[dl.map$disloc==dn,], axes = TRUE, main = paste("Chiapas ", dn, " - ", tmp, " (DSI = ", tmp2, ")", sep = ""))
plot(bg, add = TRUE)
#plot(dl.map[dl.map$disloc==munn,], lwd = 5, add = TRUE) # drop
plot(ed.map$cps, add = TRUE)
library(scales) # has function alpha()
plot(se.map, add = TRUE, border = "darkgray", col = alpha(portray2, .5)) # color p5li
# plot(se.map[se.map$disn==munn,], add = TRUE, border = "darkgray", col = portray[se.map$disn==munn]) # color nwin -- se.map$disn is disfed
#plot(ffcc, add = TRUE, lwd = .75)
#
#plot(ed.map$nay, add = TRUE, lty = 1, col = rgb(1,1,1, alpha = .5)) # blurs colors inside state
plot(se.map[se.map$disloc2018==dn,], add = TRUE, border = "darkgray", col = alpha(portray2[se.map$disloc2018==dn], .5)) # color nwin
# add casillas
points(cas.map, pch = 20, col = "white" , cex = .3)
#points(cas.map[cas.map$disloc2017==munn,], pch = 20, col = rgb(1,1,1,.33), cex = .3)
#
plot(ed.map$oax, add = TRUE, lty = 1)
plot(ed.map$ver, add = TRUE, lty = 1)
plot(ed.map$tab, add = TRUE, lty = 1)
plot(ed.map$cam, add = TRUE, lty = 1)
#
sel <- which(dl2012.map$disloc==dl.map$father[dl.map$disloc==dn])
plot(dl2012.map[sel,], add = TRUE, lwd = 6, border = "red")
#
plot(dl.map[dl.map$disloc==dn,], add = TRUE, lwd = 4)
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
#
plot(ed.map$cps, add = TRUE, lwd = 3)
plot(ed.map$cps, add = TRUE, border = "red", lty = 3, lwd = 2)
#
sel <- which(mu.map$municipio==munn)
text(coordinates(mu.map[-sel,]), labels=mu.map$mun[-sel], cex=.51, col = "green")
text(coordinates(mu.map[-sel,]), labels=mu.map$mun[-sel], cex=.5)
legend(x=lp[dn], bg = "white", legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","black","black","gray"), lty = c(1,1,1,1,1), pch = c(NA,NA,NA,NA,19), lwd = c(6,6,2,2,0), bty="o", cex=.75)
legend(x=lp[dn], bg = NULL,    legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","red","green","white"),  lty = c(1,1,3,3,1), pch = c(NA,NA,NA,NA,20), lwd = c(2,2,2,2,0), bty="o", cex=.75)
#
legend(x="bottom", bg = "white", legend=c("0-.2",".2-.4",".4-.6",".6-.8",".8-1"), fill=mauve, title = "Prop. indígena", bty="o", cex=.75)
#
library(prettymapr)
addnortharrow(pos = ifelse(lp[dn]=="topright", "topleft", "topright"), scale=.75)
addscalebar(style = "ticks", pos = ifelse(lp[dn]=="bottomright", "bottomleft", "bottomright"))
#dev.off()
#}



################################
################################
## grafica municipios 1 por 1 ##
################################
################################
md3 <- "../../../mun/maps/"
# (use 1984 long/lat for this map when mercator projection was chosen)
p84 <- function(x = NA){
    x <- x
    x <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
}
portray <- se.map$bastion  # elegir qué reportará el mapa 2
portray2 <- se.map$p5licat # elegir qué reportará el mapa 3
M <- nrow(mu.map@data)    # number of municipalities
## munn <- 14                  # elegir un municipio
## for (munn in 15:M){
##     print(paste("munn =", munn))
## # plot state map with highlighted district
## png(file = paste(md3, edo, munn, "-1.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Chiapas (municipios)")#, bg = "lightblue")
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
# 
plot(p84(mu.map), add = TRUE, border = "gray")
plot(p84(mu.map[mu.map$municipio==munn,]), add = TRUE, border = "black", col = "hotpink")
# thick state border
plot(p84(ed.map$cps), add = TRUE, lwd = 3)
plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -93.6, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -40 )
text( x = -91.25, y = 15.25, labels = "GUATEMALA", col = "darkgray", cex = .9)
text( x = -90.75, y = 17, labels = "GUATEMALA", col = "darkgray", cex = .9, srt = -35)
text( x = -94.35, y = 16.6, labels = "OAXACA", col = "darkgray", cex = .9, srt = 90)
text( x = -94.1, y = 17.5, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -35)
text( x = -92.65, y = 17.85, labels = "TABASCO", col = "darkgray", cex = .9 )
text( x = -91.8, y = 18.05, labels = "CAMP.", col = "darkgray", cex = .9, srt = -35)
text( x = -90.6, y = 18, labels = "CAMPECHE", col = "darkgray", cex = .9 )
#dev.off()

# plot same municipio only: bastiones
# need to merge disn info into mun and sec object, in order to select just those belonging to dis
# get openstreetmap background
m <- p84(mu.map[mu.map$municipio==munn,])  # subsetted map
b <- as.data.frame(m@bbox)
# gets xx degrees more than bbox (decimal defines share of max range)
xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
#bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
#bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
#bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))
bg <- bg.os
#
#png(file = paste(md3, edo, munn, "-2.png", sep = ""))
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
tmp <-  as.character(mu.map$nombre[which(mu.map$municipio==munn)])
plot(mu.map[mu.map$municipio==munn,], axes = TRUE, main = paste("Chiapas ", munn, " - ", tmp, sep = ""))
plot(bg, add = TRUE)
#plot(dl.map[dl.map$disloc==munn,], lwd = 5, add = TRUE) # drop
plot(ed.map$cps, add = TRUE)
library(scales) # has function alpha()
plot(se.map, add = TRUE, border = "darkgray", col = alpha(portray, .25)) # color nwin
# plot(se.map[se.map$disn==munn,], add = TRUE, border = "darkgray", col = portray[se.map$disn==munn]) # color nwin -- se.map$disn is disfed
#plot(ffcc, add = TRUE, lwd = .75)
#
#plot(ed.map$nay, add = TRUE, lty = 1, col = rgb(1,1,1, alpha = .5)) # blurs colors inside state
plot(se.map[se.map$municipio==munn,], add = TRUE, border = "darkgray", col = alpha(portray[se.map$municipio==munn], .5)) # color nwin
# add casillas
points(cas.map, pch = 20, col = "white" , cex = .3)
#points(cas.map[cas.map$disloc2017==munn,], pch = 20, col = rgb(1,1,1,.33), cex = .3)
#
plot(ed.map$oax, add = TRUE, lty = 1)
plot(ed.map$ver, add = TRUE, lty = 1)
plot(ed.map$tab, add = TRUE, lty = 1)
plot(ed.map$cam, add = TRUE, lty = 1)
#
plot(dl.map, add = TRUE, lwd = 4)
#
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
#
plot(ed.map$cps, add = TRUE, lwd = 3)
plot(ed.map$cps, add = TRUE, border = "red", lty = 3, lwd = 2)
#
sel <- which(mu.map$municipio==munn)
text(coordinates(mu.map[-sel,]), labels=mu.map$mun[-sel], cex=.51, col = "green")
text(coordinates(mu.map[-sel,]), labels=mu.map$mun[-sel], cex=.5)
lp <- c("bottomright", #1 
        "bottomright", #2 
        "bottomright", #3 
        "bottomright", #4 
        "bottomright", #5 
        "bottomright", #6 
        "bottomright", #7 
        "bottomright", #8 
        "bottomright", #9 
        "bottomright", #10
        "bottomright", #11
        "bottomright", #12
        "bottomright", #13
        "bottomright", #14
        "bottomright", #15
        "bottomleft",  #16
        "bottomright", #17
        "bottomright", #18
        "bottomright", #19
        "bottomright", #20
        "bottomright", #21
        "bottomright", #22
        "bottomright", #23
        "bottomright", #24
        "bottomright", #25
        "bottomright", #26
        "bottomright", #27
        "bottomright", #28
        "bottomright", #29
        "bottomright", #30
        "bottomright", #31
        "bottomright", #32
        "bottomright", #33
        "bottomright", #34
        "bottomright", #35
        "bottomright", #36
        "bottomright", #37
        "bottomright", #38
        "bottomright", #39
        "bottomright", #40
        "bottomright", #41
        "bottomright", #42
        "bottomright", #43
        "bottomright", #44
        "bottomright", #45
        "bottomright", #46
        "bottomright", #47
        "bottomright", #48
        "bottomright", #49
        "bottomright", #50
        "bottomright", #51
        "bottomright", #52
        "bottomright", #53
        "bottomright", #54
        "bottomright", #55
        "bottomright", #56
        "bottomright", #57
        "bottomright", #58
        "bottomright", #59
        "bottomleft",  #60
        "bottomright", #61
        "bottomright", #62
        "bottomright", #63
        "bottomright", #64
        "bottomright", #65
        "bottomright", #66
        "bottomright", #67
        "bottomright", #68
        "bottomright", #69
        "bottomright", #70
        "bottomright", #71
        "bottomleft",  #72
        "bottomright", #73
        "bottomright", #74
        "bottomright", #75
        "bottomright", #76
        "bottomleft",  #77
        "bottomright", #78
        "bottomright", #79
        "bottomleft",  #80
        "bottomright", #81
        "bottomright", #82
        "bottomright", #83
        "bottomright", #84
        "bottomright", #85
        "bottomright", #86
        "bottomright", #87
        "bottomright", #88
        "bottomright", #89
        "bottomright", #90
        "bottomright", #91
        "bottomright", #92
        "bottomleft",  #93
        "bottomright", #94
        "bottomright", #95
        "bottomright", #96
        "bottomleft",  #97
        "bottomright", #98
        "bottomright", #99
        "bottomright", #100
        "bottomright", #101
        "bottomright", #102
        "bottomright", #103
        "bottomright", #104
        "bottomright", #105
        "bottomright", #106
        "bottomright", #107
        "bottomleft",  #108
        "bottomright", #109
        "bottomright", #110
        "bottomright", #111
        "bottomright", #112
        "bottomright", #113
        "bottomright", #114
        "bottomright", #115
        "bottomright", #116
        "bottomright", #117
        "bottomright") #118
legend(x=lp[munn], bg = "white", legend=c("lím. munic.", "dist. local","lím. edo.","casilla"), col=c("black","black","black","gray"), lty = c(1,1,1,1), pch = c(NA,NA,NA,19), lwd = c(2,6,2,0), bty="o", cex=.75)
legend(x=lp[munn], bg = NULL,    legend=c("lím. munic.","dist. local","lím. edo.","casilla"), col=c("green","black","red","white"),  lty = c(3,1,3,1,1), pch = c(NA,NA,NA,20), lwd = c(2,2,2,0), bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = ifelse(lp[munn]=="topright", "topleft", "topright"), scale=.75)
addscalebar(style = "ticks", pos = ifelse(lp[munn]=="bottomright", "bottomleft", "bottomright"))
#dev.off()

# plot same municipio only: p5li
#png(file = paste(md3, edo, munn, "-p5li.png", sep = ""))
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
tmp <-  as.character(mu.map$nombre[which(mu.map$municipio==munn)])
plot(mu.map[mu.map$municipio==munn,], axes = TRUE, main = paste("Chiapas ", munn, " - ", tmp, sep = ""))
plot(bg, add = TRUE)
#plot(dl.map[dl.map$disloc==munn,], lwd = 5, add = TRUE) # drop
plot(ed.map$cps, add = TRUE)
library(scales) # has function alpha()
plot(se.map, add = TRUE, border = "darkgray", col = alpha(portray2, .25)) # color nwin
# plot(se.map[se.map$disn==munn,], add = TRUE, border = "darkgray", col = portray[se.map$disn==munn]) # color nwin -- se.map$disn is disfed
#plot(ffcc, add = TRUE, lwd = .75)
#
#plot(ed.map$nay, add = TRUE, lty = 1, col = rgb(1,1,1, alpha = .5)) # blurs colors inside state
plot(se.map[se.map$municipio==munn,], add = TRUE, border = "darkgray", col = alpha(portray2[se.map$municipio==munn], .5)) # color nwin
# add casillas
points(cas.map, pch = 20, col = "white" , cex = .3)
#points(cas.map[cas.map$disloc2017==munn,], pch = 20, col = rgb(1,1,1,.33), cex = .3)
#
plot(ed.map$oax, add = TRUE, lty = 1)
plot(ed.map$ver, add = TRUE, lty = 1)
plot(ed.map$tab, add = TRUE, lty = 1)
plot(ed.map$cam, add = TRUE, lty = 1)
#
plot(dl.map, add = TRUE, lwd = 4)
#
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
#
plot(ed.map$cps, add = TRUE, lwd = 3)
plot(ed.map$cps, add = TRUE, border = "red", lty = 3, lwd = 2)
#
sel <- which(mu.map$municipio==munn)
text(coordinates(mu.map[-sel,]), labels=mu.map$mun[-sel], cex=.51, col = "green")
text(coordinates(mu.map[-sel,]), labels=mu.map$mun[-sel], cex=.5)
lp <- c("bottomright", #1 
        "bottomright", #2 
        "bottomright", #3 
        "bottomright", #4 
        "bottomright", #5 
        "bottomright", #6 
        "bottomright", #7 
        "bottomright", #8 
        "bottomright", #9 
        "bottomright", #10
        "bottomright", #11
        "bottomright", #12
        "bottomright", #13
        "bottomright", #14
        "bottomright", #15
        "bottomleft",  #16
        "bottomright", #17
        "bottomright", #18
        "bottomright", #19
        "bottomright", #20
        "bottomright", #21
        "bottomright", #22
        "bottomright", #23
        "bottomright", #24
        "bottomright", #25
        "bottomright", #26
        "bottomright", #27
        "bottomright", #28
        "bottomright", #29
        "bottomright", #30
        "bottomright", #31
        "bottomright", #32
        "bottomright", #33
        "bottomright", #34
        "bottomright", #35
        "bottomright", #36
        "bottomright", #37
        "bottomright", #38
        "bottomright", #39
        "bottomright", #40
        "bottomright", #41
        "bottomright", #42
        "bottomright", #43
        "bottomright", #44
        "bottomright", #45
        "bottomright", #46
        "bottomright", #47
        "bottomright", #48
        "bottomright", #49
        "bottomright", #50
        "bottomright", #51
        "bottomright", #52
        "bottomright", #53
        "bottomright", #54
        "bottomright", #55
        "bottomright", #56
        "bottomright", #57
        "bottomright", #58
        "bottomright", #59
        "bottomleft",  #60
        "bottomright", #61
        "bottomright", #62
        "bottomright", #63
        "bottomright", #64
        "bottomright", #65
        "bottomright", #66
        "bottomright", #67
        "bottomright", #68
        "bottomright", #69
        "bottomright", #70
        "bottomright", #71
        "bottomleft",  #72
        "bottomright", #73
        "bottomright", #74
        "bottomright", #75
        "bottomright", #76
        "bottomleft",  #77
        "bottomright", #78
        "bottomright", #79
        "bottomleft",  #80
        "bottomright", #81
        "bottomright", #82
        "bottomright", #83
        "bottomright", #84
        "bottomright", #85
        "bottomright", #86
        "bottomright", #87
        "bottomright", #88
        "bottomright", #89
        "bottomright", #90
        "bottomright", #91
        "bottomright", #92
        "bottomleft",  #93
        "bottomright", #94
        "bottomright", #95
        "bottomright", #96
        "bottomleft",  #97
        "bottomright", #98
        "bottomright", #99
        "bottomright", #100
        "bottomright", #101
        "bottomright", #102
        "bottomright", #103
        "bottomright", #104
        "bottomright", #105
        "bottomright", #106
        "bottomright", #107
        "bottomleft",  #108
        "bottomright", #109
        "bottomright", #110
        "bottomright", #111
        "bottomright", #112
        "bottomright", #113
        "bottomright", #114
        "bottomright", #115
        "bottomright", #116
        "bottomright", #117
        "bottomright") #118
legend(x=lp[munn], bg = "white", legend=c("lím. munic.", "dist. local","lím. edo.","casilla"), col=c("black","black","black","gray"), lty = c(1,1,1,1), pch = c(NA,NA,NA,19), lwd = c(2,6,2,0), bty="o", cex=.75)
legend(x=lp[munn], bg = NULL,    legend=c("lím. munic.","dist. local","lím. edo.","casilla"), col=c("green","black","red","white"),  lty = c(3,1,3,1,1), pch = c(NA,NA,NA,20), lwd = c(2,2,2,0), bty="o", cex=.75)
#
legend(x="bottom", bg = "white", legend=c("0-.2",".2-.4",".4-.6",".6-.8",".8-1"), fill=mauve, title = "Prop. indígena", bty="o", cex=.75)
#
library(prettymapr)
addnortharrow(pos = ifelse(lp[munn]=="topright", "topleft", "topright"), scale=.75)
addscalebar(style = "ticks", pos = ifelse(lp[munn]=="bottomright", "bottomleft", "bottomright"))
#dev.off()
#}


