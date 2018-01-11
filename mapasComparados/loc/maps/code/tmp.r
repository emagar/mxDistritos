################################
################################
## grafica municipios 1 por 1 ##
################################
################################
md3 <- "../../ay/maps"
# (use 1984 long/lat for this map when mercator projection was chosen)
p84 <- function(x = NA){
    x <- x
    x <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
}
portray <- se.map$bastion  # elegir qué reportará el mapa 2
M <- nrow(mu.map@data)    # number of municipalities
munn <- 1                  # elegir un municipio
## for (mun in 1:M){
##     print(paste("disn =", mun))
## # plot state map with highlighted district
#png(file = paste(md3, edo, dn, "-1.png", sep = ""), width=10, height=10, units="cm", res=144) 
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
#plot(dl.map[dl.map$disloc==munn,], lwd = 5, add = TRUE) # drop
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
plot(dl.map, add = TRUE, lwd = 4)
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
        "bottomright", #45
        "bottomleft",  #46
        "bottomleft",  #47
        "bottomright", #48
        "bottomright", #49
        "bottomleft",  #50
        "bottomleft",  #51
        "bottomright", #52
        "topleft",     #53
        "bottomleft",  #54
        "bottomright", #55
        "bottomleft",  #56
        "bottomleft",  #57
        "bottomright", #58
        "bottomright", #59
        "bottomleft",  #60
        "bottomleft",  #61
        "bottomright", #62
        "topleft",     #63
        "bottomleft",  #64
        "bottomright", #65
        "bottomleft",  #66
        "bottomleft",  #67
        "bottomright", #68
        "bottomright", #69
        "bottomleft",  #70
        "bottomleft",  #71
        "bottomright", #72
        "topleft",     #73
        "bottomleft",  #74
        "bottomright", #75
        "bottomleft",  #76
        "bottomleft",  #77
        "bottomright", #78
        "bottomright", #79
        "bottomleft",  #80
        "bottomleft",  #81
        "bottomright", #82
        "topleft",     #83
        "bottomleft",  #84
        "bottomright", #85
        "bottomleft",  #86
        "bottomleft",  #87
        "bottomright", #88
        "bottomright", #89
        "bottomleft",  #90
        "bottomleft",  #91
        "bottomright", #92
        "topleft",     #93
        "bottomleft",  #94
        "bottomright", #95
        "bottomleft",  #96
        "bottomleft",  #97
        "bottomright", #98
        "bottomright", #99
        "bottomleft",  #100
        "bottomleft",  #101
        "bottomright", #102
        "topleft",     #103
        "bottomleft",  #104
        "bottomright", #105
        "bottomleft",  #106
        "bottomleft",  #107
        "bottomright", #108
        "bottomright", #109
        "bottomleft",  #110
        "bottomleft",  #111
        "bottomright", #112
        "topleft",     #113
        "bottomleft",  #114
        "bottomright", #115
        "bottomleft",  #116
        "bottomleft",  #117
        "bottomright", #118
        "bottomright", #119
        "bottomleft",  #120
        "bottomleft",  #121
        "bottomright", #122
        "topleft",     #123
        "bottomleft",  #124
        "bottomright") #125
legend(x=lp[munn], bg = "white", legend=c("lím. munic.", "dist. local","lím. edo.","casilla"), col=c("black","black","black","gray"), lty = c(1,1,1,1), pch = c(NA,NA,NA,19), lwd = c(2,6,2,0), bty="o", cex=.75)
legend(x=lp[munn], bg = NULL,    legend=c("lím. munic.","dist. local","lím. edo.","casilla"), col=c("green","black","red","white"),  lty = c(3,1,3,1,1), pch = c(NA,NA,NA,20), lwd = c(2,2,2,0), bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = ifelse(lp[munn]=="topright", "topleft", "topright"), scale=.75)
addscalebar(style = "ticks", pos = ifelse(lp[munn]=="bottomright", "bottomleft", "bottomright"))
#dev.off()
#}
