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

# choose state number
edon <- 15
edo <- c("ags","bc","bcs","cam","coa","col","cps","cua","df","dgo","gua","gue","hgo","jal","mex","mic","mor","nay","nl","oax","pue","que","qui","san","sin","son","tab","tam","tla","ver","yuc","zac")
edo <- edo[edon]

# working directory and data/map directories
path <- "~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/"
setwd(paste(path, "maps/0code/", sep = ""))
dd <- "~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/" # data directory
md <- paste(path, "shp/disfed2018/", sep = "")                 # main map directory (shapefile repo)
md2 <- paste(path, "maps/", sep = "")                          # will save maps here

###########################
# get mun names and votes #
###########################
path <- paste(dd, "aymu1977-present.csv", sep = "")
munvot <- read.csv(file = path, stringsAsFactors=FALSE)
#
###############################################
# seccion-level dip fed votes for 6 elections #
###############################################
path <- c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/data/")
load(file=paste(path, "elDatForMaps.RData", sep = "")) 

###################
# geospatial data #
###################
library(spdep); library(maptools)
# used to determine what datum map is in 
library(rgdal)
#gpclibPermit()
#################
# secciones map #
#################
path <- paste(md, edo, sep = "") # archivo con mapas ine
se.map <- readOGR(dsn = path, layer = 'SECCION')
summary(se.map)
# projects to a different datum with long and lat
se.map <- spTransform(se.map, osm()) # project to osm native Mercator

#################################################################
# read state borders 1-by-1: uncomment bordering states for map #
#################################################################
ed.map <- list()
## tmp <- paste(md, "ags", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ags <- tmp
## #
## tmp <- paste(md, "bc", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$bc <- tmp
## #
## tmp <- paste(md, "bcs", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$bcs <- tmp
## #
## tmp <- paste(md, "cam", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cam <- tmp
## #
## tmp <- paste(md, "coa", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$coa <- tmp
## #
## tmp <- paste(md, "col", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$col <- tmp
## #
## tmp <- paste(md, "cps", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cps <- tmp
## #
## tmp <- paste(md, "cua", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cua <- tmp
#
tmp <- paste(md, "df", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$df <- tmp
#
## tmp <- paste(md, "dgo", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$dgo <- tmp
## #
tmp <- paste(md, "gua", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$gua <- tmp
#
tmp <- paste(md, "gue", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$gue <- tmp
#
tmp <- paste(md, "hgo", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$hgo <- tmp
#
## tmp <- paste(md, "jal", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$jal <- tmp
#
tmp <- paste(md, "mex", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$mex <- tmp
#
tmp <- paste(md, "mic", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$mic <- tmp
#
tmp <- paste(md, "mor", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$mor <- tmp
#
## tmp <- paste(md, "nay", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nay <- tmp
## #
## tmp <- paste(md, "nl", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nl <- tmp
## #
## tmp <- paste(md, "oax", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$oax <- tmp
#
tmp <- paste(md, "pue", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$pue <- tmp
#
tmp <- paste(md, "que", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$que <- tmp
#
## tmp <- paste(md, "qui", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$qui <- tmp
## #
## tmp <- paste(md, "san", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$san <- tmp
## #
## tmp <- paste(md, "sin", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$sin <- tmp
## #
## tmp <- paste(md, "son", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$son <- tmp
## #
## tmp <- paste(md, "tab", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tab <- tmp
## #
## tmp <- paste(md, "tam", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tam <- tmp
## #
tmp <- paste(md, "tla", sep = "") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$tla <- tmp
#
## tmp <- paste(md, "ver", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ver <- tmp
## #
## tmp <- paste(md, "yuc", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$yuc <- tmp
## #
## tmp <- paste(md, "zac", sep = "") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$zac <- tmp

# read municipios
path <- paste(md, edo, sep = "") # archivo con mapas ine
mu.map <- readOGR(dsn = path, layer = 'MUNICIPIO')
summary(mu.map)
mu.map$mun <- mu.map$NOMBRE # mun names
mu.map$mun <- gsub(pattern = "[0-9]+", replacement = "", mu.map$mun) # some names have numbers, drop them
# may need to shorten mun names...
# projects to a different datum with long and lat
mu.map <- spTransform(mu.map, osm())

#######################################
# read shapefiles distritos federales #
#######################################
path <- paste(md, edo, sep = "") 
df.map <- readOGR(dsn = path, layer = 'DISTRITO')
# rename columns
head(df.map)
sel <- which(colnames(df.map@data)=="distrito")
colnames(df.map@data)[sel] <- "disfed"
sel <- which(colnames(df.map@data)=="entidad")
colnames(df.map@data)[sel] <- "edon"
# projects to a different ./datum with long and lat
df.map <- spTransform(df.map, osm()) # project to osm native Mercator
###################
# read disfed2006 #
###################
path <- paste(md, "../disfed2006/", edo, sep = "") 
df2006.map <- readOGR(dsn = path, layer = 'DISTRITO')
# rename columns
head(df2006.map)
sel <- which(colnames(df2006.map@data)=="DISTRITO")
colnames(df2006.map@data)[sel] <- "disfed"
sel <- which(colnames(df2006.map@data)=="ENTIDAD")
colnames(df2006.map@data)[sel] <- "edon"
# projects to a different datum with long and lat
df2006.map <- spTransform(df2006.map, osm()) # project to osm native Mercator

## # add father/son info and dsi of mapLoc < ESTO NO LO TENGO PARA FEDERALES, HABRIA QUE PREPARARLO DESDE LA SECCIONES
## dsi <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/simIndex/dist_mex.csv"
## dsi <- read.csv(file = dsi, stringsAsFactors = FALSE)
## #
## #df.map$ord <- 1:nrow(df.map@data)
## df.map@data <- merge(x = df.map@data, y = dsi, by.x = "disloc", by.y = "disloc2018", all.x = TRUE, all.y = FALSE)
## rm(dsi)

## #### DROP????
## # read comparative district maps
## # a. from seccion2dis map, in order to export into se.map for sub-setting
## #sec2dis <- read.csv("/home/eric/Dropbox/data/mapas/reseccionamiento/equivSecc/tablaEquivalenciasSeccionales1994-2010.2013.csv", stringsAsFactors = FALSE)
## #sec2dis <- sec2dis[sec2dis$edon == 18,]
## sec2dis <- read.csv("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/mexLoc.csv", stringsAsFactors = FALSE)
## # send to seccion map
## colnames(se.map)
## tmp <- data.frame(SECCION = se.map$SECCION)
## tmp$orden <- 1:nrow(tmp)
## tmp <- merge(x = tmp, y = sec2dis, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
## tmp <- tmp[order(tmp$orden), grep("^dis.+$", colnames(tmp))]
## #tmp <- tmp[order(tmp$orden), grep("SECCION|^dis.+$", colnames(tmp))]
## se.map@data <- cbind(se.map@data, tmp)
## rm(tmp)
## #

## # read cabeceras distritales (via vocal ejecutivo)
## tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
## cabDis <- readOGR(dsn = tmp, layer = 'VOCAL_EJECUTIVO_DISTRITAL')
## # projects to a different datum with long and lat
## cabDis <- spTransform(cabDis, osm())
## #
## cabDisNames <- read.csv(paste(wd2, "cabeceras2006.csv", sep = ""), stringsAsFactors = FALSE)

########################
# add casillas in 2012 #
########################
path <- paste(md, "../disfed2006/", edo, sep = "") # archivo con mapas rojano
cas.map <- readOGR(dsn = path, layer = 'CASILLA')
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
tmp <- merge(x = tmp, y = ncasillas[ncasillas$edon==edon, c("seccion","e12")], by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
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
tmp <- merge(x = tmp, y = nwin[nwin$edon==edon,], by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
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


# grafica distritos locales 1 por 1
# (use 1984 long/lat for this map when mercator projection was chosen)
p84 <- function(x = NA){
    x <- x
    x <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
}
portray <- se.map$bastion  # elegir qué reportará el mapa 2
portray2 <- se.map$ncascol # elegir qué reportará el mapa 3
dn <- 29                  # elegir un distrito
## for (dn in 30:45){
##     print(paste("disn =", dn))
## # plot state map with highlighted district
#png(file = paste(md2, edo, dn, "-1.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
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
plot(p84(df.map), add = TRUE, border = "gray")
plot(p84(df.map[df.map$disloc==dn,]), add = TRUE, border = "gray", col = "hotpink")
# thick state border
plot(p84(ed.map$mex), add = TRUE, lwd = 3)
plot(p84(ed.map$mex), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
text(coordinates(p84(df.map)), labels=df.map$disloc, cex=.85)
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
m <- p84(df.map[df.map$disloc==dn,])  # subsetted map
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
tmp <-  df.map$cab[which(df.map$disloc==dn)]
tmp2 <- df.map$dsi[which(df.map$disloc==dn)]
plot(df.map[df.map$disloc==dn,], axes = TRUE, main = paste("México ", dn, " - ", tmp, " (DSI = ", tmp2, ")", sep = ""))
plot(bg, add = TRUE)
#plot(df.map[df.map$disloc==dn,], lwd = 5, add = TRUE) # drop
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
sel <- which(dl1996.map$disloc==df.map$father[df.map$disloc==dn])
plot(dl1996.map[sel,], add = TRUE, lwd = 6, border = "red")
#
plot(df.map[df.map$disloc==dn,], add = TRUE, lwd = 4)
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
        "bottomleft",  #26
        "bottomleft",  #27
        "bottomleft",  #28
        "bottomleft",  #29
        "bottomleft",  #30
        "bottomleft",  #31
        "bottomright", #32
        "bottomright", #33
        "bottomleft",  #34
        "bottomleft",  #35
        "bottomleft",  #36
        "bottomleft",  #37
        "bottomright", #38
        "bottomright", #39
        "bottomleft",  #40
        "bottomleft",  #41
        "bottomright", #42
        "topleft",     #43
        "bottomleft",  #44
        "bottomright") #45
legend(x=lp[dn], bg = "white", legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","black","black","gray"), lty = c(1,1,1,1,1), pch = c(NA,NA,NA,NA,19), lwd = c(6,6,2,2,0), bty="o", cex=.75)
legend(x=lp[dn], bg = NULL,    legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","red","green","white"),  lty = c(1,1,3,3,1), pch = c(NA,NA,NA,NA,20), lwd = c(2,2,2,2,0), bty="o", cex=.75)
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
M <- nrow(mu.map@data)    # number of municipalities
munn <- 1                  # elegir un municipio
## for (munn in 1:M){
##     print(paste("munn =", munn))
## # plot state map with highlighted district
#png(file = paste(md3, edo, munn, "-1.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$mex), col = "white", axes = TRUE, main = "Estado de México (municipios)")#, bg = "lightblue")
#plot(p84(ed.map$df), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$pue), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$hgo), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$mor), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$gue), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$mic), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$que), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$gua), col = "white", add = TRUE, lty = 3)
# 
plot(p84(mu.map), add = TRUE, border = "gray")
plot(p84(mu.map[mu.map$municipio==munn,]), add = TRUE, border = "black", col = "hotpink")
# thick state border
plot(p84(ed.map$mex), add = TRUE, lwd = 3)
plot(p84(ed.map$mex), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
text(coordinates(p84(mu.map)), labels=mu.map$municipio, cex=.85)
#
# add neighboring states
text( x = -99.15,  y = 19.3,  labels = "CDMX",       col = "darkgray", cex = .9 )
text( x = -99,     y = 20.2,  labels = "HIDALGO",    col = "darkgray", cex = .9 )
text( x = -98.58,  y = 19.52, labels = "TLAX.",      col = "darkgray", cex = .9, srt = -25)
text( x = -98.55,  y = 18.6,  labels = "PUEBLA",     col = "darkgray", cex = .9, srt = -90)
text( x = -99.1,   y = 18.75, labels = "MORELOS",    col = "darkgray", cex = .9 )
text( x = -99.8,   y = 18.4,  labels = "GUERRERO",   col = "darkgray", cex = .9 )
text( x = -100.5,  y = 19.5,  labels = "MICHOACAN",  col = "darkgray", cex = .9 )
text( x = -100.55, y = 20.25, labels = "GUANAJUATO", col = "darkgray", cex = .9 )
text( x = -100.1,  y = 20.3,  labels = "QUERETARO",  col = "darkgray", cex = .9 )
#dev.off()

# plot same municipio only
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
plot(mu.map[mu.map$municipio==munn,], axes = TRUE, main = paste("México ", munn, " - ", tmp, sep = ""))
plot(bg, add = TRUE)
#plot(df.map[df.map$disloc==munn,], lwd = 5, add = TRUE) # drop
plot(ed.map$mex, add = TRUE)
library(scales) # has function alpha()
plot(se.map, add = TRUE, border = "darkgray", col = alpha(portray, .25)) # color nwin
# plot(se.map[se.map$disn==munn,], add = TRUE, border = "darkgray", col = portray[se.map$disn==munn]) # color nwin -- se.map$disn is disfed
#plot(ffcc, add = TRUE, lwd = .75)
#
#
#plot(ed.map$nay, add = TRUE, lty = 1, col = rgb(1,1,1, alpha = .5)) # blurs colors inside state
plot(se.map[se.map$MUNICIPIO==munn,], add = TRUE, border = "darkgray", col = alpha(portray[se.map$MUNICIPIO==munn], .5)) # color nwin
# add casillas
points(cas.map, pch = 20, col = "white" , cex = .3)
#points(cas.map[cas.map$disloc2017==munn,], pch = 20, col = rgb(1,1,1,.33), cex = .3)
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
plot(df.map, add = TRUE, lwd = 4)
#
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
#
plot(ed.map$mex, add = TRUE, lwd = 3)
plot(ed.map$mex, add = TRUE, border = "red", lty = 3, lwd = 2)
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
        "bottomright", #97
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
        "bottomright", #118
        "bottomright", #119
        "bottomright", #120
        "bottomleft",  #121
        "bottomright", #122
        "bottomright", #123
        "bottomright", #124
        "bottomright") #125
legend(x=lp[munn], bg = "white", legend=c("lím. munic.", "dist. local","lím. edo.","casilla"), col=c("black","black","black","gray"), lty = c(1,1,1,1), pch = c(NA,NA,NA,19), lwd = c(2,6,2,0), bty="o", cex=.75)
legend(x=lp[munn], bg = NULL,    legend=c("lím. munic.","dist. local","lím. edo.","casilla"), col=c("green","black","red","white"),  lty = c(3,1,3,1,1), pch = c(NA,NA,NA,20), lwd = c(2,2,2,0), bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = ifelse(lp[munn]=="topright", "topleft", "topright"), scale=.75)
addscalebar(style = "ticks", pos = ifelse(lp[munn]=="bottomright", "bottomleft", "bottomright"))
#dev.off()
#}


