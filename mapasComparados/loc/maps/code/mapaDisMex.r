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
edo <- "mex"
edon <- 15

# geospatial data 
library(spdep); library(maptools)
# used to determine what datum rojano data has
library(rgdal)
#gpclibPermit()
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
se.map <- readOGR(dsn = tmp, layer = 'SECCION')
summary(se.map)
# projects to a different datum with long and lat
se.map <- spTransform(se.map, osm()) # project to osm native Mercator

# read all state borders from rojano
ed.map <- list()
## tmp <- paste(md, "ags", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ags <- tmp
## #
## tmp <- paste(md, "bc", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$bc <- tmp
## #
## tmp <- paste(md, "bcs", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$bcs <- tmp
## #
## tmp <- paste(md, "cam", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cam <- tmp
## #
## tmp <- paste(md, "coa", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$coa <- tmp
## #
## tmp <- paste(md, "col", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$col <- tmp
## #
## tmp <- paste(md, "cps", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cps <- tmp
## #
## tmp <- paste(md, "cua", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cua <- tmp
#
tmp <- paste(md, "df", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$df <- tmp
#
## tmp <- paste(md, "dgo", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$dgo <- tmp
## #
tmp <- paste(md, "gua", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$gua <- tmp
#
tmp <- paste(md, "gue", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$gue <- tmp
#
tmp <- paste(md, "hgo", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$hgo <- tmp
#
## tmp <- paste(md, "jal", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$jal <- tmp
#
tmp <- paste(md, "mex", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$mex <- tmp
#
tmp <- paste(md, "mic", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$mic <- tmp
#
tmp <- paste(md, "mor", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$mor <- tmp
#
## tmp <- paste(md, "nay", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nay <- tmp
## #
## tmp <- paste(md, "nl", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nl <- tmp
## #
## tmp <- paste(md, "oax", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$oax <- tmp
#
tmp <- paste(md, "pue", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$pue <- tmp
#
tmp <- paste(md, "que", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$que <- tmp
#
## tmp <- paste(md, "qui", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$qui <- tmp
## #
## tmp <- paste(md, "san", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$san <- tmp
## #
## tmp <- paste(md, "sin", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$sin <- tmp
## #
## tmp <- paste(md, "son", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$son <- tmp
## #
## tmp <- paste(md, "tab", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tab <- tmp
## #
## tmp <- paste(md, "tam", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tam <- tmp
## #
tmp <- paste(md, "tla", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$tla <- tmp
#
## tmp <- paste(md, "ver", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ver <- tmp
## #
## tmp <- paste(md, "yuc", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$yuc <- tmp
## #
## tmp <- paste(md, "zac", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$zac <- tmp

# read municipios
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
mu.map <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
mu.map <- spTransform(mu.map, osm())
# read cabeceras municipales
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
cab <- readOGR(dsn = tmp, layer = 'CABECERA_MUNICIPAL')
# projects to a different datum with long and lat
cab <- spTransform(cab, osm())
#cab$LOCALIDAD_.1 # names
#
# edit manually to shorten mun names (*need to do this outside!!!*) and plug into mu.map
#tmp <- munvot[munvot$edon==13 & munvot$yr==2008,]
#tmp <- tmp[,c("mun","munn")]
## OJO: EL ORDEN NO JALA
## data.frame(N=mu.map$NOMBRE, M=mu.map$mun)
mu.map$mun <- mu.map$NOMBRE
mu.map$mun <- gsub(pattern = "[0-9]+", replacement = "", mu.map$mun) # en algunos casos NOMBRE trae números, hay que quitarlos

# read shapefiles distritos locales 
tmp <- paste("/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/", edo, sep = "") # archivo con mapas locales
dl.map <- readOGR(dsn = tmp, layer = 'disloc2018')
colnames(dl.map@data) <- c("edon","tipo","disloc","id")
# projects to a different datum with long and lat
dl.map <- spTransform(dl.map, osm()) # project to osm native Mercator
# read disloc2005
dl1996.map <- readOGR(dsn = tmp, layer = 'disloc1996')
colnames(dl1996.map@data) <- c("edon","disloc")
# projects to a different datum with long and lat
dl1996.map <- spTransform(dl1996.map, osm()) # project to osm native Mercator

library(foreign)
tmp2 <- read.dbf(paste(tmp, "disloc1996.dbf", sep=""))
head(tmp2)
dl.map$disloc

# add father/son info and dsi of mapLoc
dsi <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/simIndex/dist_mex.csv"
dsi <- read.csv(file = dsi, stringsAsFactors = FALSE)
#
#dl.map$ord <- 1:nrow(dl.map@data)
dl.map@data <- merge(x = dl.map@data, y = dsi, by.x = "disloc", by.y = "disloc2018", all.x = TRUE, all.y = FALSE)
rm(dsi)

# read comparative district maps
# a. from seccion2dis map, in order to export into se.map for sub-setting
#sec2dis <- read.csv("/home/eric/Dropbox/data/mapas/reseccionamiento/equivSecc/tablaEquivalenciasSeccionales1994-2010.2013.csv", stringsAsFactors = FALSE)
#sec2dis <- sec2dis[sec2dis$edon == 18,]
sec2dis <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/mexLoc.csv", stringsAsFactors = FALSE)
# send to seccion map
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = sec2dis, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), grep("^dis.+$", colnames(tmp))]
#tmp <- tmp[order(tmp$orden), grep("SECCION|^dis.+$", colnames(tmp))]
se.map@data <- cbind(se.map@data, tmp)
rm(tmp)
# di.map <- unionSpatialPolygons(se.map, se.map$disn) # proper way to get federal district objects... if only seccion shapefiles had no problems
#
# b. from rojano's 2006 distrito map, which has good-looking shapefiles
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
di.map <- readOGR(dsn = tmp, layer = 'DISTRITO')
# projects to a different datum with long and lat
di.map <- spTransform(di.map, osm())
# read cabeceras distritales (via vocal ejecutivo)
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
cabDis <- readOGR(dsn = tmp, layer = 'VOCAL_EJECUTIVO_DISTRITAL')
# projects to a different datum with long and lat
cabDis <- spTransform(cabDis, osm())
#
cabDisNames <- read.csv(paste(wd2, "cabeceras2006.csv", sep = ""), stringsAsFactors = FALSE)

# add casillas in 2012
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
cas.map <- readOGR(dsn = tmp, layer = 'CASILLA')
# projects to a different datum with long and lat
cas.map <- spTransform(cas.map, osm()) # project to osm native Mercator
#
# add districts for subsetting
tmp <- cas.map@data; tmp$ord <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = se.map[,c("SECCION","disfed2006","disfed2018","disloc1996","disloc2018")], by = "SECCION", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$ord),]
cas.map@data <- tmp
#
# drop casillas from missing secciones to avoid indeterminate subsetting
sel <- which(is.na(cas.map$disloc2018)==TRUE)
if (length(sel)>0) cas.map <- cas.map[-sel,] # drop missing cases
rm(sel)

# add ncasillas in 2012 to seccion map
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = ncasillas[ncasillas$edon== 9, c("seccion","e12")], by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("SECCION","e12")]; 
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
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = nwin[nwin$edon==9,], by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("SECCION","pan","pri","prd")]
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

# add disloc --- DONE ABOVE
#tmp <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/subnat/disloc/df2015.csv", stringsAsFactors = FALSE) # 2015 districts
#tmp$edon <- tmp$munn <- NULL
#tmp <- merge(x = se.map@data, y = tmp, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
## table(is.na(tmp$disloc))             # verify missing
## tmp$SECCION[is.na(tmp$disloc)==TRUE] # verify missing
#se.map$disloc <- tmp$disloc#; se.map$cabloc <- tmp$cabloc
#
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
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = tmp3, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
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
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = pres, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
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
tmp <- merge(x = tmp, y = pres, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
#
se.map@data <- cbind(se.map@data, tmp)
colnames(se.map@data)
rm(tmp, tmp1, tmp2, tmp3, tmpv)

# add district ptot, rri proj at each election
load(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/votPobDis0018.RData")
tmp <- votPobDis0018$pob.distMap2006
tmp <- tmp[tmp$edon==edon,]
tmp1 <- di.map@data
tmp1$ord <- 1:nrow(tmp1)
tmp1 <- merge(x=tmp1, y=tmp, by.x="DISTRITO", by.y="disn")
tmp1[, grep("rri", colnames(tmp1))] <- round(tmp1[, grep("rri", colnames(tmp1))],2)
tmp1 <- tmp1[order(tmp1$ord), grep("ptot|rri", colnames(tmp1))]
di.map@data <- cbind(di.map@data, tmp1)
di.map$disrri06 <- paste(di.map$DISTRITO, " (", di.map$rris2006, ")", sep="")
di.map$disrri15 <- paste(di.map$DISTRITO, " (", di.map$rris2015, ")", sep="")
                         
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


# grafica distritos 1 por 1
# (use 1984 long/lat for this map when mercator projection was chosen)
p84 <- function(x = NA){
    x <- x
    x <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
}
portray <- se.map$bastion  # elegir qué reportará el mapa 2
portray2 <- se.map$ncascol # elegir qué reportará el mapa 3
dn <- 1                  # elegir un distrito
## for (dn in 1:45){
##     print(paste("disn =", dn))
## # plot state map with highlighted district
#png(file = paste(md2, edo, dn, "-1.png", sep = ""))
par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$mex), col = "white", axes = TRUE, main = "Estado de México (mapa local 2018)")#, bg = "lightblue")
#plot(p84(ed.map$df), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$pue), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$hgo), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$mor), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$gue), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$mic), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$que), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$gua), col = "white", add = TRUE, lty = 3)
# 
plot(p84(dl.map), add = TRUE, border = "gray")
plot(p84(dl.map[dl.map$disloc==dn,]), add = TRUE, border = "gray", col = "gray")
# thick state border
plot(p84(ed.map$mex), add = TRUE, lwd = 3)
plot(p84(ed.map$mex), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
text(coordinates(p84(dl.map)), labels=dl.map$disloc, cex=.85)
#
# add neighboring states
text( x = -99.15, y = 19.3, labels = "CDMX", col = "darkgray", cex = .9 )
text( x = -99, y = 20.2, labels = "HIDALGO", col = "darkgray", cex = .9 )
text( x = -98.58, y = 19.52, labels = "TLAX.", col = "darkgray", cex = .9, srt = -25)
text( x = -98.55, y = 18.6, labels = "PUEBLA", col = "darkgray", cex = .9, srt = -90)
text( x = -99.1, y = 18.75, labels = "MORELOS", col = "darkgray", cex = .9 )
text( x = -99.8, y = 18.4, labels = "GUERRERO", col = "darkgray", cex = .9 )
text( x = -100.5, y = 19.5, labels = "MICHOACAN", col = "darkgray", cex = .9 )
text( x = -100.55, y = 20.25, labels = "GUANAJUATO", col = "darkgray", cex = .9 )
text( x = -100.1, y = 20.3, labels = "QUERETARO", col = "darkgray", cex = .9 )
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
#png(file = paste(md2, edo, dn, "-2.png", sep = ""))
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
tmp <-  dl.map$cab[which(dl.map$disloc==dn)]
tmp2 <- dl.map$dsi[which(dl.map$disloc==dn)]
plot(dl.map[dl.map$disloc==dn,], axes = TRUE, main = paste("México ", dn, " - ", tmp, " (DSI = ", tmp2, ")", sep = ""))
plot(bg, add = TRUE)
#plot(dl.map[dl.map$disloc==dn,], lwd = 5, add = TRUE) # drop
plot(ed.map$mex, add = TRUE)
library(scales) # has function alpha()
plot(se.map, add = TRUE, border = "darkgray", col = alpha(portray, .25)) # color nwin
# plot(se.map[se.map$disn==dn,], add = TRUE, border = "darkgray", col = portray[se.map$disn==dn]) # color nwin -- se.map$disn is disfed
#plot(ffcc, add = TRUE, lwd = .75)
#
#
#plot(ed.map$nay, add = TRUE, lty = 1, col = rgb(1,1,1, alpha = .5)) # blurs colors inside state
plot(se.map[se.map$disloc2018==dn,], add = TRUE, border = "darkgray", col = alpha(portray[se.map$disloc2018==dn], .5)) # color nwin
# add casillas
points(cas.map, pch = 20, col = "white" , cex = .3)
#points(cas.map[cas.map$disloc2017==dn,], pch = 20, col = rgb(1,1,1,.33), cex = .3)
#
#
plot(ed.map$que, add = TRUE, lty = 1)
plot(ed.map$hgo, add = TRUE, lty = 1)
plot(ed.map$tla, add = TRUE, lty = 1)
plot(ed.map$pue, add = TRUE, lty = 1)
plot(ed.map$gue, add = TRUE, lty = 1)
plot(ed.map$mor, add = TRUE, lty = 1)
plot(ed.map$mic, add = TRUE, lty = 1)
#
sel <- which(dl1996.map$disloc==dl.map$father[dl.map$disloc==dn])
plot(dl1996.map[sel,], add = TRUE, lwd = 6, border = "red")
#
plot(dl.map[dl.map$disloc==dn,], add = TRUE, lwd = 4)
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
plot(ed.map$tla, add = TRUE, lwd = 3)
plot(ed.map$tla, add = TRUE, border = "red", lty = 3, lwd = 2)
## points(coordinates(cab), pch = 19, col = "white", cex = .5)
## points(coordinates(cab), pch = 1, col = "green", cex = .75)
text(coordinates(mu.map), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(mu.map), labels=mu.map$mun, cex=.5)
lp <- c("bottomright", #1 
        "bottomright", #2 
        "bottomleft",  #3 
        "bottomright", #4 
        "bottomright", #5 
        "bottomright", #6 
        "bottomright", #7 
        "bottomright",  #8 
        "bottomright",  #9 
        "bottomleft",  #10
        "bottomright",  #11
        "bottomright", #12
        "bottomright", #13
        "bottomleft",  #14
        "bottomleft",     #15
        "bottomleft",     #16
        "bottomleft",     #17
        "bottomleft",     #18
        "bottomright",     #19
        "topleft",  #20
        "bottomleft",  #21
        "bottomright", #22
        "bottomright", #23
        "bottomleft",  #24
        "bottomleft",     #25
        "bottomleft",     #26
        "bottomleft",     #27
        "bottomleft",     #28
        "bottomleft",     #29
        "bottomleft",  #30
        "bottomleft",  #31
        "bottomright", #32
        "bottomright", #33
        "bottomleft",  #34
        "bottomleft",     #35
        "bottomleft",     #36
        "bottomleft",     #37
        "bottomright",     #38
        "bottomright",     #39
        "bottomleft",  #40
        "bottomleft",  #41
        "bottomright", #42
        "topleft", #43
        "bottomleft",  #44
        "bottomright")     #45
legend(x=lp[dn], bg = "white", legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","black","black","gray"), lty = c(1,1,1,1,1), pch = c(NA,NA,NA,NA,19), lwd = c(6,6,2,2,0), bty="o", cex=.75)
legend(x=lp[dn], bg = NULL,    legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","red","green","white"),  lty = c(1,1,3,3,1), pch = c(NA,NA,NA,NA,20), lwd = c(2,2,2,2,0), bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = ifelse(lp[dn]=="topright", "topleft", "topright"), scale=.75)
addscalebar(style = "ticks", pos = ifelse(lp[dn]=="bottomright", "bottomleft", "bottomright"))
#dev.off()
#}

# plot same distrito's nCasillas
loc <- c("topright","bottomleft","topleft","bottomleft","bottomleft",
         "topright","bottomleft","bottomright","topright","topleft",
         "topleft","topleft","bottomright","topright","topleft",
         "topleft","topleft","bottomleft","bottomright","topright",
         "topright","topright","topleft","topright","topright",
         "bottomright","topright")
## pdf(file = paste(md2, edo, dn, "-3.pdf", sep = ""))
par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
tmp <- cabDisNames$cab[which(cabDisNames$edon == 9 & cabDisNames$disn==dn)]
plot(di.map[di.map$DISTRITO==dn,], axes = TRUE, main = paste("Distrito Federal", dn, "-", tmp))
plot(ed.map$df, add = TRUE)
plot(se.map[se.map$disn==dn,], add = TRUE, border = "darkgray", col = portray2[se.map$disn==dn]) # color nwin
#plot(ffcc, add = TRUE, lwd = .75)
#
plot(ed.map$mor, add = TRUE, lty = 1)
#
plot(di.map[di.map$DISTRITO==dn,], add = TRUE, lwd = 4)
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
plot(ed.map$df, add = TRUE, lwd = 3)
plot(ed.map$df, add = TRUE, border = "red", lty = 3, lwd = 2)
points(coordinates(cab), pch = 19, col = "white", cex = .5)
points(coordinates(cab), pch = 1, col = "green", cex = .75)
text(coordinates(mu.map), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(mu.map), labels=mu.map$mun, cex=.5)
legend(x=loc[dn], legend=c("21+","11-20","6-10","3-5","2","1"), fill=rev(purples), bty="o", bg = "white", cex=.85, title = "N casillas")
## dev.off()
## }


# export for google earth
#v1
## library(maptools)
## gd <- paste(md2, "kml/", sep = ""); setwd(gd)
## kmlPolygons(obj = se.map, kmlfile = "mexseccion.kml", border = "white", col = sub("#", "#FF", se.map$bastion))
#v2
library(plotKML)
setwd(paste(md2, "kml/", sep = ""))
#plotKML(se.map, filename = "mexseccion.kml", fill = col2kml(se.map$bastion), open.kml = FALSE)

kml_open(file.name = paste(edo, ".kml", sep = ""), folder.name = edo, kml_open = FALSE, kml_visibility = FALSE)
#
# Bastiones
tmp <- subset(se.map, bastion2 == "swing")
kml_layer.SpatialPolygons(tmp, colour = "gray", subfolder.name = "swingSec", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pan4")
kml_layer.SpatialPolygons(tmp, colour = "royalblue2", subfolder.name = "corePan4", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pan5")
kml_layer.SpatialPolygons(tmp, colour = "royalblue3", subfolder.name = "corePan5", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pan6")
kml_layer.SpatialPolygons(tmp, colour = "royalblue4", subfolder.name = "corePan6", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pri4")
kml_layer.SpatialPolygons(tmp, colour = "red1", subfolder.name = "corePri4", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pri5")
kml_layer.SpatialPolygons(tmp, colour = "red2", subfolder.name = "corePri5", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pri6")
kml_layer.SpatialPolygons(tmp, colour = "red3", subfolder.name = "corePri6", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "izq4")
kml_layer.SpatialPolygons(tmp, colour = "gold1", subfolder.name = "coreLeft4", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "izq5")
kml_layer.SpatialPolygons(tmp, colour = "gold2", subfolder.name = "coreLeft5", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "izq6")
kml_layer.SpatialPolygons(tmp, colour = "gold3", subfolder.name = "coreLeft6", altitudeMode = "clampToGround", alpha = .7)
#
# Estado
tmp <- as(ed.map$df, "SpatialLinesDataFrame") # coerce polygons to lines 
kml_layer.SpatialLines(tmp, colour = "hotpink", width = 2.5, subfolder.name = "stateLimit", altitudeMode = "clampToGround")
#
# Distritos federales 2006
tmp <- as(di.map, "SpatialLinesDataFrame") # coerce polygons to lines 
kml_layer.SpatialLines(tmp, colour = "white", width = 2, subfolder.name = "fedDist2006map", altitudeMode = "clampToGround")
tmp <- SpatialPointsDataFrame(coords=coordinates(di.map), data = di.map@data, proj4string = osm())
kml_layer.SpatialPoints(tmp, LabelScale = .75, extrude = TRUE, labels = disrri06, subfolder.name = "fedDistRRI@2006", altitudeMode = "clampToGround")
kml_layer.SpatialPoints(tmp, LabelScale = .75, extrude = TRUE, labels = disrri15, subfolder.name = "fedDistRRI@2015", altitudeMode = "clampToGround")
#
# Municipios
tmp <- as(mu.map, "SpatialLinesDataFrame") # coerce polygons to lines
kml_layer.SpatialLines(tmp, colour = "springgreen", width = 1, subfolder.name = "municLimit", altitudeMode = "clampToGround")
tmp <- SpatialPointsDataFrame(coords=coordinates(mu.map), data = mu.map@data, proj4string = osm())
kml_layer.SpatialPoints(tmp, shape = "", labels = mun, subfolder.name = "municNames", altitudeMode = "clampToGround", LabelScale = .5)
#
# Lista nominal 2015
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
tmp <- SpatialPointsDataFrame(coords=coordinates(se.map), data = se.map@data, proj4string = osm())
tmp$ln15k <- round(tmp$ln15/1000, 1) # lista nominal in thousands for labels
tmp$sqrtln15 <- sqrt(tmp$ln15) # lista nominal sq root
#kml_layer.SpatialPoints(tmp, shape = shape, labels = ln15k, colour = ln15, colour_scale = SAGA_pal$SG_COLORS_WHITE_GREEN, extrude = TRUE, altitude = ln15, altitudeMode = "relativeToGround", subfolder.name = "barras") 
kml_layer.SpatialPoints(tmp, shape = shape, labels = "", colour = "green", size = sqrtln15, alpha = .33, altitudeMode = "clampToGround", subfolder.name = "listaNominal@2015") 
kml_layer.SpatialPoints(tmp, shape = "", labels = ln15k, colour = "white", altitudeMode = "clampToGround", subfolder.name = "listaNominal@2015x1000") 
#
# añadir fch, amlo, epn
#
kml_close(file.name = paste(edo, ".kml", sep = ""))

# display.pal(SAGA_pal[1:5]) # <- useful to pick colors

# compress kml files to kmz
md2 <- c("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/maps/")
library(plotKML)
setwd(paste(md2, "kml/", sep = ""))
kml_compress("df.kml")

# print the result:
library(XML)
xmlRoot(xmlTreeParse("tmp.kml"))[["Document"]][1:100]


