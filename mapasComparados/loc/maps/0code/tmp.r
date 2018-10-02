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
# gets xx degrees more than bbox (decimal defines share of max range)
xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
    # checks if basemap (type os, saved as R data) is in disk
    bmps <- dir(path=paste(md2, "basemaps/", sep = ""))
    if (paste(edo, "-os.RData", sep = "") %in% bmps) {
        load(file = paste(md2, "basemaps/", edo, dn, "-os.RData", sep = "")) # gets bg.os
        bg <- bg.os
    } else {
        # choose one of four background picture types
        #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
        #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
                                        #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
        bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))#, minNumTiles = 9)
        save(bg.os, file = paste(md2, "basemaps/", edo, "-os.RData", sep = "")) # save a copy of the basemap for future use
        bg <- bg.os
    }
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

# distritos c indígenas
disIndig <- c(4, 5, 7, 8, 9, 11, 20, 21, 22)

# número efectivo d lenguas
#pdf(file = paste(md2, edo, "-nel.pdf", sep = ""))
#png(file = paste(md2, edo, "-nel.png", sep = ""), width=10, height=10, units="cm", res=144) 
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Pluralidad lingüística de secciones y municipios indígenas")#, bg = "lightblue")
#plot(bg, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
library(scales) # has function alpha()
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray3, .95))
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
legend(x="bottomright", bg = "white", legend=c("1","1.2","1.4","1.6","1.8","2"), fill=redgreen, title = "nMolinar lenguas", bty="o", cex=.75)
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
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray3, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray3[sel1s], .67))
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
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray3, .33))
plot(p84(se.map[sel1s,]), add = TRUE, border = "gray", col = alpha(portray3[sel1s], .67))
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




