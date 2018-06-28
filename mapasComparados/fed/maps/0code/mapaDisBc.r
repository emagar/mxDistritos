# script is adapted from one in /data/elecs/MXelsCalendGovt/atlasDis/code/

#source(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/code/mapPrep.r") # sólo correr si hubiera cambios en los datos

# # OJO: when using spTranform in script, use line below for google earth, or next line for OSM/google maps
#x.map <- spTransform(x.map, CRS("+proj=longlat +datum=WGS84"))
#x.map <- spTransform(x.map, osm()) # project to osm native Mercator

# to use osm backgrounds
library(rJava)
library(OpenStreetMap)
library(rgdal)

rm(list = ls())

# choose state number
edon <- 2
edo <- c("ags","bc","bcs","cam","coa","col","cps","cua","df","dgo","gua","gue","hgo","jal","mex","mic","mor","nay","nl","oax","pue","que","qui","san","sin","son","tab","tam","tla","ver","yuc","zac")
edo <- edo[edon]

# working directory and data/map directories
ruta <- "~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados"
setwd(ruta)
mapdir <- "fed/shp/disfed2018"                                    # main map directory (shapefile repo)
mapdir2 <- "fed/maps"                                             # will save maps here

## ###########################
## # get mun names and votes #
## ###########################
## ruta <- "~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/aymu1977-present.csv"
## munvot <- read.csv(file = ruta, stringsAsFactors=FALSE)

###############################################
# seccion-level dip fed votes for 6 elections #
###############################################
load(file="fed/data/elDatForMaps.RData") # "~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/data" 
# merge win and nwin
nwin <- cbind(nwin, win)
head(nwin)
#rm(win)

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
ruta <- file.path(mapdir, edo) # archivo con mapas ine
se.map <- readOGR(dsn = ruta, layer = 'SECCION')
summary(se.map)
# projects to a different datum with long and lat
se.map <- spTransform(se.map, osm()) # project to osm native Mercator

#################################################################
# read state borders 1-by-1: uncomment bordering states for map #
#################################################################
ed.map <- list()
#
## tmp <- file.path(mapdir, "ags") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ags <- tmp
## #
tmp <- file.path(mapdir, "bc") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$bc <- tmp
#
tmp <- file.path(mapdir, "bcs") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$bcs <- tmp
#
## tmp <- file.path(mapdir, "cam") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cam <- tmp
## #
## tmp <- file.path(mapdir, "coa") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$coa <- tmp
## #
## tmp <- file.path(mapdir, "col") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$col <- tmp
## #
## tmp <- file.path(mapdir, "cps") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cps <- tmp
## #
## tmp <- file.path(mapdir, "cua") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$cua <- tmp
## #
## tmp <- file.path(mapdir, "df") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$df <- tmp
## #
## tmp <- file.path(mapdir, "dgo") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$dgo <- tmp
## #
## tmp <- file.path(mapdir, "gua") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$gua <- tmp
## #
## tmp <- file.path(mapdir, "gue") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$gue <- tmp
## #
## tmp <- file.path(mapdir, "hgo") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$hgo <- tmp
## #
## tmp <- file.path(mapdir, "jal") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$jal <- tmp
## #
## tmp <- file.path(mapdir, "mex") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$mex <- tmp
## #
## tmp <- file.path(mapdir, "mic") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$mic <- tmp
## #
## tmp <- file.path(mapdir, "mor") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$mor <- tmp
## #
## tmp <- file.path(mapdir, "nay") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nay <- tmp
## #
## tmp <- file.path(mapdir, "nl") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$nl <- tmp
## #
## tmp <- file.path(mapdir, "oax") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$oax <- tmp
## #
## tmp <- file.path(mapdir, "pue") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$pue <- tmp
## #
## tmp <- file.path(mapdir, "que") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$que <- tmp
## #
## tmp <- file.path(mapdir, "qui") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$qui <- tmp
## #
## tmp <- file.path(mapdir, "san") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$san <- tmp
## #
## tmp <- file.path(mapdir, "sin") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$sin <- tmp
## #
tmp <- file.path(mapdir, "son") # archivo con mapas ine
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, osm())
ed.map$son <- tmp
#
## tmp <- file.path(mapdir, "tab") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tab <- tmp
## #
## tmp <- file.path(mapdir, "tam") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tam <- tmp
## #
## tmp <- file.path(mapdir, "tla") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$tla <- tmp
## #
## tmp <- file.path(mapdir, "ver") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$ver <- tmp
## #
## tmp <- file.path(mapdir, "yuc") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$yuc <- tmp
## #
## tmp <- file.path(mapdir, "zac") # archivo con mapas ine
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, osm())
## ed.map$zac <- tmp

# read municipios
ruta <- file.path(mapdir, edo) # archivo con mapas ine
mu.map <- readOGR(dsn = ruta, layer = 'MUNICIPIO')
summary(mu.map)
mu.map$mun <- mu.map$nombre # mun names
mu.map$mun <- gsub(pattern = "[0-9]+", replacement = "", mu.map$mun) # some names have numbers, drop them
# may need to shorten mun names...
# projects to a different datum with long and lat
mu.map <- spTransform(mu.map, osm())

#######################################
# read shapefiles distritos federales #
#######################################
ruta <- file.path(mapdir, edo)
df.map <- readOGR(dsn = ruta, layer = 'DISTRITO')
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
ruta <- file.path("fed/shp/disfed2006", edo) 
df2006.map <- readOGR(dsn = ruta, layer = 'DISTRITO')
# rename columns
head(df2006.map)
sel <- which(colnames(df2006.map@data)=="DISTRITO")
colnames(df2006.map@data)[sel] <- "disfed"
sel <- which(colnames(df2006.map@data)=="ENTIDAD")
colnames(df2006.map@data)[sel] <- "edon"
# projects to a different datum with long and lat
df2006.map <- spTransform(df2006.map, osm()) # project to osm native Mercator

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
## tmp <- file.path(mapdir, edo, # archivo con mapas rojano
## cabDis <- readOGR(dsn = tmp, layer = 'VOCAL_EJECUTIVO_DISTRITAL')
## # projects to a different datum with long and lat
## cabDis <- spTransform(cabDis, osm())
## #
## cabDisNames <- read.csv(file.path(wd2, "cabeceras2006.csv"), stringsAsFactors = FALSE)

# read cabeceras distritales 2018
ruta <- "~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/data/dfdfcabeceras2018.csv"
tmp <- read.csv(file = ruta, stringsAsFactors = FALSE)
tmp <- tmp[,c("edon","disn","mun")]
colnames(tmp) <- c("edon","disfed","cab")
tmp2 <- df.map@data
tmp2$ord <- 1:nrow(tmp2)
tmp2 <- merge(x = tmp2, y = tmp, by = c("edon","disfed"), all.x = TRUE, all.y = FALSE)
tmp2 <- tmp2[order(tmp2$ord),]
tmp2$ord <- NULL
df.map@data <- tmp2

#############################################################
# add distritos locales: change layer year to match state's #
#############################################################
ruta <- file.path("loc/shp", edo)
dl.map <- readOGR(dsn = ruta, layer = 'disloc2016')
# projects to a different datum with long and lat
dl.map <- spTransform(dl.map, osm()) # project to osm native Mercator

########################
# add casillas in 2012 #
########################
ruta <- file.path("fed/shp/disfed2006", edo) # archivo con mapas rojano
cas.map <- readOGR(dsn = ruta, layer = 'CASILLA')
# projects to a different datum with long and lat
cas.map <- spTransform(cas.map, osm()) # project to osm native Mercator

# add federal districts for subsetting
tmp <- cas.map@data; tmp$ord <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = se.map[,c("seccion","distrito")], by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$ord),]
cas.map@data <- tmp

# drop casillas from missing secciones to avoid indeterminate subsetting
sel <- which(is.na(cas.map$distrito)==TRUE)
if (length(sel)>0) cas.map <- cas.map[-sel,] # drop missing cases
rm(sel)

# add ncasillas in 2012 to seccion map
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = ncasillas[ncasillas$edon==edon, c("seccion","e12")], by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("seccion","e12")]; 
se.map$ncasillas <- tmp$e12

# add nwin to seccion map
tmp <- data.frame(seccion = se.map$seccion)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = nwin[nwin$edon==edon,], by = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("seccion","pan","pri","prd","e15")]
se.map$nwinpan <- tmp$pan
se.map$nwinpri <- tmp$pri
se.map$nwinprd <- tmp$prd
# add 2015 winner to seccion map
se.map$win15 <- tmp$e15
rm(tmp)
#
###########################################################
# make colors                                             #
# need to change left to exclude morena victories in 2015 # <- OJO
###########################################################
library(RColorBrewer)
nclr <- 7                                    #CATEGORÍAS DE COLOR (MIN=3 MAX=9)
blues <- brewer.pal(nclr,"Blues")            #GENERA CODIGOS DE COLOR QUE CRECEN CON GRADO
reds <- brewer.pal(nclr,"Reds")              
yellows <- brewer.pal(nclr,"YlOrBr")         
#
library(plyr)
se.map$pancol <- mapvalues ( se.map$nwinpan, from = 0:6, to = blues )
se.map$pricol <- mapvalues ( se.map$nwinpri, from = 0:6, to = reds )
se.map$prdcol <- mapvalues ( se.map$nwinprd, from = 0:6, to = yellows )
# bastiones = 4+ wins
se.map$bastion <- rgb(190,190,190, maxColorValue = 255) # "gray"
## se.map$bastion[se.map$nwinpan>=4] <- "blue"
## se.map$bastion[se.map$nwinpri>=4] <- "red"
## se.map$bastion[se.map$nwinprd>=4] <- "gold"
se.map$bastion[se.map$nwinpan==4] <- blues[3]
se.map$bastion[se.map$nwinpan==5] <- blues[5]
se.map$bastion[se.map$nwinpan==6] <- blues[7]
se.map$bastion[se.map$nwinpri==4] <- reds[3]
se.map$bastion[se.map$nwinpri==5] <- reds[5]
se.map$bastion[se.map$nwinpri==6] <- reds[7]
se.map$bastion[se.map$nwinprd==4] <- yellows[2]
se.map$bastion[se.map$nwinprd==5] <- yellows[4]
se.map$bastion[se.map$nwinprd==6] <- yellows[6]

# color 2015 winner
blue <- blues[7]
red <- reds[7]
yellow <- rgb(255,215,0, maxColorValue = 255) #"gold"
#brown <- rgb(160,82,45, maxColorValue = 255) #"siena"
brown <- rgb(139,69,19, maxColorValue = 255) #"saddlebrown"
gray <- rgb(190,190,190, maxColorValue = 255)
green <- rgb(0,100,0, maxColorValue = 255) #"darkgreen"
#
#table(win$e15)
tmp <- rep(gray, nrow(se.map@data))
tmp[se.map$win15=="pan"] <- blue
tmp[se.map$win15=="pri" | se.map$win15=="pric"] <- red
tmp[se.map$win15=="prd" | se.map$win15=="prdc"] <- yellow
tmp[se.map$win15=="pvem"] <- green
tmp[se.map$win15=="morena"] <- brown
#
se.map$win15 <- tmp


############################################
# add seccion volat 2012-2105 = max change #
# add lisnom2015                           #
############################################
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
## ##################################################################################
## # add seccion fch, epn, amlo                                                     #
## # handy function to sort one data frame by order of another, matching data frame #
## ##################################################################################
## sortBy <- function(target, By){
##     t <- target; b <- By;
##     do.call(rbind, lapply(seq_len(nrow(b)), 
##             function(i) as.character(unlist(t[i,])[order(unlist(-b[i,]))]))) # change to -b if decreasing wished
## }
## # 2006
## tmpv <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/elecReturns/datosBrutos/resultSecciones/prSeccion2006.csv", stringsAsFactors = FALSE)
## tmpv <- tmpv[tmpv$edon==edon,]
## tmpv$edon <- tmpv$disn <- tmpv$munn <- tmpv$id_elec <- tmpv$nr <- tmpv$nul <- tmpv$tot <- tmpv$lisnom <- NULL
## colnames(tmpv)[grep("pan$", colnames(tmpv))] <- "fch"
## colnames(tmpv)[grep("apm", colnames(tmpv))] <- "pri"
## colnames(tmpv)[grep("pbt", colnames(tmpv))] <- "amlo"
## tmpv2 <- tmpv[,-1] # votes only
## etiq <- data.frame(matrix(rep(colnames(tmpv2), nrow(tmpv2)), nrow=nrow(tmpv2), byrow = TRUE), stringsAsFactors = FALSE)
## etiq <- sortBy(target = etiq, By = tmpv2)
## tmpv2 <- t(apply(tmpv2, 1, function(x) sort(x, decreasing = TRUE)))
## #
## pres <- data.frame(seccion=tmpv$seccion, win06p=etiq[,1], mg06p=round( (tmpv2[,1] - tmpv2[,2])/rowSums(tmpv2), 3), fch=round(tmpv$fch/rowSums(tmpv2), 3), amlo06=round(tmpv$amlo/rowSums(tmpv2), 3))
## #
## tmp <- data.frame(seccion = se.map$seccion)
## tmp$orden <- 1:nrow(tmp)
## tmp <- merge(x = tmp, y = pres, by = "seccion", all.x = TRUE, all.y = FALSE)
## tmp <- tmp[order(tmp$orden),]
## #
## # 2012
## tmpv <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/elecReturns/datosBrutos/resultSecciones/prSeccion2012.csv", stringsAsFactors = FALSE)
## tmpv <- tmpv[tmpv$edon==edon,]
## tmpv$edon <- tmpv$disn <- tmpv$munn <- tmpv$urbrur <- tmpv$nr <- tmpv$nul <- tmpv$lisnom <- NULL
## tmpv$amlo <- tmpv$prd + tmpv$pt + tmpv$mc + tmpv$prdptmc + tmpv$prdpt + tmpv$prdmc + tmpv$ptmc
## tmpv$epn <- tmpv$pri + tmpv$pvem + tmpv$pripvem
## tmpv$prd <- tmpv$pt <- tmpv$mc <- tmpv$prdptmc <- tmpv$prdpt <- tmpv$prdmc <- tmpv$ptmc <- tmpv$pri <- tmpv$pvem <- tmpv$pripvem <- NULL
## tmpv2 <- tmpv[,-1] # votes only
## etiq <- data.frame(matrix(rep(colnames(tmpv2), nrow(tmpv2)), nrow=nrow(tmpv2), byrow = TRUE), stringsAsFactors = FALSE)
## etiq <- sortBy(target = etiq, By = tmpv2)
## tmpv2 <- t(apply(tmpv2, 1, function(x) sort(x, decreasing = TRUE)))
## #
## pres <- data.frame(seccion=tmpv$seccion, win12p=etiq[,1], mg12p=round( (tmpv2[,1] - tmpv2[,2])/rowSums(tmpv2), 3), epn=round(tmpv$epn/rowSums(tmpv2), 3), amlo12=round(tmpv$amlo/rowSums(tmpv2), 3))
## tmp <- merge(x = tmp, y = pres, by = "seccion", all.x = TRUE, all.y = FALSE)
## tmp <- tmp[order(tmp$orden),]
## #
## se.map@data <- cbind(se.map@data, tmp)
## colnames(se.map@data)
## rm(tmp, tmp1, tmp2, tmp3, tmpv)

## # add district ptot, rri proj at each election
## load(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/votPobDis0018.RData")
## tmp <- votPobDis0018$pob.distMap2006
## tmp <- tmp[tmp$edon==edon,]
## tmp1 <- df2006.map@data
## tmp1$ord <- 1:nrow(tmp1)
## tmp1 <- merge(x=tmp1, y=tmp, by.x="distrito", by.y="disn")
## tmp1[, grep("rri", colnames(tmp1))] <- round(tmp1[, grep("rri", colnames(tmp1))],2)
## tmp1 <- tmp1[order(tmp1$ord), grep("ptot|rri", colnames(tmp1))]
## df2006.map@data <- cbind(df2006.map@data, tmp1)
## df2006.map$disrri06 <- paste(df2006.map$DISTRITO, " (", df2006.map$rris2006, ")", sep="")
## df2006.map$disrri15 <- paste(df2006.map$DISTRITO, " (", df2006.map$rris2015, ")", sep="")
                         
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
## write.csv(tmp, file = paste(mapdir, edo, "/magar.csv", sep = "") )
## tmp <- c("\"Integer\"",    "\"Integer\"",    "\"Integer\"",      "\"Integer\"",       "\"Integer\"",        "\"String\"",          "\"Integer\"",                   "\"Real\"", "\"Real\"") #,        "\"Integer\"",      "\"String\"")
## write(tmp, file = paste(mapdir, edo, "/magar.csvt", sep = ""), ncolumns = length(tmp), sep = "," )

## prepare dsi, fathers, sons
## READ HISTORICAL MAP
d <- read.csv(file = "../equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
sel <- which(d$edon==edon)
d <- d[sel,] # subset to relevant state
d <- d[is.na(d$dis2018)==FALSE,] # drop missing secciones
d <- d[is.na(d$dis2015)==FALSE,] # drop missing secciones
sel <- which(d$dis2015==0 | d$dis2018==0)
if (length(sel)>0) d <- d[-sel,] # drop missing secciones
#
d2018 <- d
d2018$father <- 0
d2018$dsi <- 0
#
for (s in 1:max(d$dis2018)){
    #s <- 1 # debug
    sel <- which(d$dis2018==s)
    potentialFathers <- unique(d$dis2015[sel])
    father <- 0
    dsi <- 0
    for (f in potentialFathers) {
        #f <- 2 # debug
        sel.u <- which(d$dis2018==s | d$dis2015==f)
        sel.i <- which(d$dis2018==s & d$dis2015==f)
        if (length(sel.i)/length(sel.u) > dsi){
            father <- f
            dsi <- length(sel.i)/length(sel.u)
        } else {
            next
        }
    }
    d2018$father[sel] <- father
    d2018$dsi[sel] <- dsi
}
#
d2018 <- d2018[duplicated(d2018$dis2018)==FALSE, c("dis2018","father","dsi")]
d2018 <- d2018[order(d2018$dis2018),]
#
d2015 <- d
d2015$son <- 0
d2015$dsi <- 0
#
for (f in 1:max(d$dis2015)){
    # f <- 1 # debug
    sel <- which(d$dis2015==f)
    potentialSons <- unique(d$dis2018[sel])
    son <- 0
    dsi <- 0
    for (s in potentialSons) {
        #s <- 14 # debug
        sel.u <- which(d$dis2018==s | d$dis2015==f)
        sel.i <- which(d$dis2018==s & d$dis2015==f)
        if (length(sel.i)/length(sel.u) > dsi){
            son <- s
            dsi <- length(sel.i)/length(sel.u)
        } else {
            next
        }
    }
    d2015$son[sel] <- son
    d2015$dsi[sel] <- dsi
}
#
d2015 <- d2015[duplicated(d2015$dis2015)==FALSE, c("dis2015","son","dsi")]
d2015 <- d2015[order(d2015$dis2015),]
#
tmp <- df.map@data
tmp$order <- 1:nrow(tmp)
tmp <- merge(tmp, d2018, by.x = "disfed", by.y = "dis2018", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$order),]
# import father and dsi
df.map$father <- tmp$father
df.map$dsi <- tmp$dsi
#
tmp <- df2006.map@data
tmp$order <- 1:nrow(tmp)
tmp <- merge(tmp, d2015, by.x = "disfed", by.y = "dis2015", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$order),]
# import son and dsi
df2006.map$son <- tmp$son
df2006.map$dsi <- tmp$dsi
#
# clean
rm(d, sel, potentialFathers, potentialSons, father, son, dsi, s, f, sel.u, sel.i, d2015, d2018)

# shave map's bb to exclude pacific island 
shave <- function(m = NA, p = .5, s = 0, eastwest = TRUE){ # m is map to be shaved, p the rightmost part (share) to keep, -1<s<1 a shift rightward, eastwest=FALSE makes shift northsouth
    #m <- ed.map$col # duplicate
    m <- m; p <- p;
    dim <- ifelse(eastwest==TRUE, 1, 2) 
    b <- as.data.frame(m@bbox)
    b[dim,] <- b[dim,] - s*(b$max[dim] - b$min[dim])       # shift map rightward (bbox leftward)
    b$min[dim] <- b$max[dim] - p*(b$max[dim] - b$min[dim]) # keeps only 100*p% of horizontal length
    m@bbox <- as.matrix(b)
    #ed.map$col <- m
    return(m)
}

########################################################################
# grafica distritos 1 por 1                                            #
# (use 1984 long/lat for this map when mercator projection was chosen) #
########################################################################
p84 <- function(x = NA){
    x <- x
    x <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
}

dn <- 2 # debug

# wrap map 1 (state w all districts and selected in pink) in function
mapKeyPink <- function(dn = NULL){
    dn <- dn
    par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
    #par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
    plot(shave(p84(ed.map$bc),  p = .7), col = "white", axes = TRUE, main = "Baja California (distritos federales 2018)", bg = "lightblue")
    plot(p84(ed.map$bcs), col = "white", add = TRUE, lty = 3)
    plot(p84(ed.map$son), col = "white", add = TRUE, lty = 3)
    # 
    plot(p84(df.map), add = TRUE, border = "gray")
    plot(p84(df.map[df.map$disfed==dn,]), add = TRUE, border = "gray", col = "hotpink")
    # thick state border
    plot(p84(ed.map$bc), add = TRUE, lwd = 3)
    plot(p84(ed.map$bc), add = TRUE, border = "red", lty = 3, lwd = 2)
    ## points(cabDis, pch = 3) # cabeceras distritales
    ## points(cabDis)
    ## points(cabDis, pch = 19, cex = .75, col = "orange")
    text(coordinates(p84(df.map)), labels=df.map$disfed, cex=.85)
    #
    # add neighboring states
    text( x = -117, y = 29.5, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = -60 )
    text( x = -113.2, y = 30, labels = "M A R   D E   C O R T E Z", cex = .9, col = "deepskyblue", srt = -55 )
    text( x = -113.4, y = 27.9, labels = "B.C.S.", col = "darkgray", cex = .9 )
    text( x = -113.5, y = 31.8, labels = "SONORA", col = "darkgray", cex = .9, srt = -20 )
    text( x = -116, y = 32.75, labels = "EE.UU.", col = "darkgray", cex = .9, srt = 6 )
}

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

# wrap map 2 or 3 (district with 5-cycle history or with 2015 winner)
mapOneDistrito <- function(dn = NULL, what2plot = NULL){
    if (what2plot!=1 & what2plot!=2) stop("portray must = 1 (bastiones) or 2 (2015 winner)")
    if (what2plot==1){
        portray <- se.map$bastion
        message("Plotting core secciones (bastiones)")
    } else {
        portray <- se.map$win15
        message("Plotting sección winner in 2015")
    }
    # plot same distrito only
    # need to merge disn info into mun and sec object, in order to select just those belonging to dis
    # get openstreetmap basemap for background
    m <- p84(df.map[df.map$disfed==dn,])  # subsetted map
    b <- as.data.frame(m@bbox)
    b <- sqbbox(b)
    # gets xx% more than bbox
    xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
    # checks if basemap (type os, saved as R data) is in disk
    bmps <- dir(path=paste(mapdir2, "/basemaps/", sep = ""))
    if (paste(edo, dn, "-os.RData", sep = "") %in% bmps) {
        load(file = paste(mapdir2, "/basemaps/", edo, dn, "-os.RData", sep = "")) # gets bg.os
        bg <- bg.os
    } else {
        # choose one of four background picture types
        #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
        #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
                                        #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
        bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
        save(bg.os, file = paste(mapdir2, "/basemaps/", edo, dn, "-os.RData", sep = "")) # save a copy of the basemap for future use
        bg <- bg.os
    }
    #
    #png(file = paste(mapdir2, "/", edo, dn, "-2.png", sep = ""), width=15, height=15, units="cm", res=144) 
    par(fig = c(0,1,0,1)) # sets primary plot size (to include smaller plot inside below)
    par(mar = c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
    tmp <-  df.map$cab[which(df.map$disfed==dn)]
    tmp2 <- df.map$dsi[which(df.map$disfed==dn)]
    plot(df.map[df.map$disfed==dn,], axes = TRUE, main = paste("CdMx ", dn, " - ", tmp, sep = ""))
    plot(bg, add = TRUE)
    #plot(df.map[df.map$disloc==dn,], lwd = 5, add = TRUE) # drop
    plot(ed.map$ver, add = TRUE)
    library(scales) # has function alpha()
    plot(se.map, add = TRUE, border = "darkgray", col = alpha(portray, .25)) # color nwin
    # plot(se.map[se.map$disn==dn,], add = TRUE, border = "darkgray", col = portray[se.map$disn==dn]) # color nwin -- se.map$disn is disfed
    #plot(ffcc, add = TRUE, lwd = .75)
    #
    #
    #plot(ed.map$nay, add = TRUE, lty = 1, col = rgb(1,1,1, alpha = .5)) # blurs colors inside state
    plot(se.map[se.map$distrito==dn,], add = TRUE, border = "darkgray", col = alpha(portray[se.map$distrito==dn], .5)) # color nwin
    # add casillas
    points(cas.map, pch = 20, col = "white" , cex = .3)
    #points(cas.map[cas.map$disloc2017==dn,], pch = 20, col = rgb(1,1,1,.33), cex = .3)
    #
    #
    plot(ed.map$tam, add = TRUE, lty = 1)
    plot(ed.map$hgo, add = TRUE, lty = 1)
    plot(ed.map$pue, add = TRUE, lty = 1)
    plot(ed.map$mex, add = TRUE, lty = 1)
    plot(ed.map$mor, add = TRUE, lty = 1)
    #plot(ed.map$gue, add = TRUE, lty = 1)
    plot(ed.map$oax, add = TRUE, lty = 1)
    plot(ed.map$cps, add = TRUE, lty = 1)
    plot(ed.map$tab, add = TRUE, lty = 1)
    #
    # dibuja distrito padre
    sel <- which(df2006.map$disfed==df.map$father[df.map$disfed==dn])
    plot(df2006.map[sel,], add = TRUE, lwd = 6, border = "red")
    #
    plot(df.map[df.map$disfed==dn,], add = TRUE, lwd = 4)
    plot(mu.map, add = TRUE, border = "green", lwd = 1)
    plot(mu.map, add = TRUE, lwd = 1, lty = 3)
    plot(ed.map$ver, add = TRUE, lwd = 3)
    plot(ed.map$ver, add = TRUE, border = "red", lty = 3, lwd = 2)
    ## points(coordinates(cab), pch = 19, col = "white", cex = .5)
    ## points(coordinates(cab), pch = 1, col = "green", cex = .75)
    text(coordinates(mu.map), labels=mu.map$mun, cex=.51, col = "green")
    text(coordinates(mu.map), labels=mu.map$mun, cex=.5)
    # 1st item in pair is border legend placement, 2nd is color code (1=ne 2=se 3=sw 4=nw)
    lp <- list(c(4,3), #1 
               c(3,2), #2 
               c(1,3), #3 
               c(1,2), #4 
               c(4,3), #5 
               c(4,2), #6 
               c(4,3), #7 
               c(1,3), #8 
               c(4,3), #9 
               c(1,2), #10
               c(1,2), #11
               c(4,3), #12
               c(4,2), #13
               c(1,2), #14
               c(4,3), #15
               c(4,2), #16
               c(4,2), #17
               c(2,3), #18
               c(1,2), #19
               c(1,2), #20
               c(1,3), #21
               c(4,2), #22
               c(4,3), #23
               c(1,3)) #24
    lp <- lp[[dn]] # pick relevant pair only
    lp <- c(lp, setdiff(1:4,lp)) # add free corners for north arrow and scale
    lp <- mapvalues( lp, from = 1:4, to = c("topright","bottomright","bottomleft","topleft"), warn_missing = FALSE )
    legend(x=lp[1], bg = "white", legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","black","black","gray"), lty = c(1,1,1,1,1), pch = c(NA,NA,NA,NA,19), lwd = c(6,6,2,2,0), bty="o", cex=.75)
    legend(x=lp[1], bg = NULL,    legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","red","green","white"),  lty = c(1,1,3,3,1), pch = c(NA,NA,NA,NA,20), lwd = c(2,2,2,2,0), bty="o", cex=.75)
    library(prettymapr)
    addnortharrow(pos = lp[3], scale=.75)
    addscalebar(style = "ticks", pos = lp[4])
    #
    if (what2plot==1){
        # size reduction for secondary plot (serves as legend) conditional on placement
        if (lp[2]=="topright")    par(fig = c( .8,  1, .8,  1), new = TRUE) 
        if (lp[2]=="topleft")     par(fig = c(  0, .2, .8,  1), new = TRUE)
        if (lp[2]=="bottomleft")  par(fig = c(  0, .2,  0, .2), new = TRUE) 
        if (lp[2]=="bottomright") par(fig = c( .8,  1,  0, .2), new = TRUE) 
        ##################################################################################################################
        # bastion legend for three parties
        clr <- data.frame(pan = blues[c(3,5,7)], pri = reds[c(3,5,7)], prd = yellows[c(2,4,6)], stringsAsFactors = FALSE)
        sz <- .75
        par(mar=c(0,0,1,0)) ## SETS B L U R MARGIN SIZES
#        par(bg = "white")
        plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE, main = "Ganó 2000-15", cex.main = .75)
        polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
        polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
        for (r in 1:3){
            for (c in 1:3){
                polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .65), border = "white", lwd = 4)
            }
        }
        for (c in 1:3){
            polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .65), border = "white", lwd = 4)
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
    }
    #
    if (what2plot==2){
        legend(x=lp[2], bg = "white", legend=c("pan","pri","prd","morena","pvem","otro"), fill=alpha(c(blue,red,yellow,brown,green,gray),.67), bty="o", title = "Ganó 2015", cex=.75)
    }
    #dev.off()
}

mapKeyPink(3)
mapOneDistrito(dn = 12, what2plot = 1)

########################################################################
# grafica distritos 1 por 1                                            #
# (use 1984 long/lat for this map when mercator projection was chosen) #
########################################################################
for (dn in 1:24){
    #dn <- 10 # debug
    print(paste("disn =", dn))
    #############################################
    ## plot state map with highlighted district #
    #############################################
    png(file = paste(mapdir2, "/", edo, dn, "-1.png", sep = ""), width=15, height=15, units="cm", res=144) 
    mapKeyPink(dn)
    dev.off()
    #######################################
    ## plot district map with 2015 winner #
    #######################################
    png(file = paste(mapdir2, "/", edo, dn, "-3.png", sep = ""), width=15, height=15, units="cm", res=144) 
    mapOneDistrito(dn = dn, what2plot = 2)
    dev.off()
    ############################################
    ## plot district map with mid-term history #
    ############################################
    png(file = paste(mapdir2, "/", edo, dn, "-2.png", sep = ""), width=15, height=15, units="cm", res=144) 
    mapOneDistrito(dn = dn, what2plot = 1)
    dev.off()
}
