# all state's secciones, colored by %  in single map
m <- p84(ed.map$cps)  # subsetted map
b <- as.data.frame(m@bbox)
# gets xx degrees more than bbox (decimal defines share of max range)
xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))
bg <- bg.bi
#png(file = paste(md3, edo, "-p5licat.png", sep = ""))
par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
#par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(p84(ed.map$cps), col = "white", axes = TRUE, main = "Chiapas (secciones y distritos electorales)", bg = "lightblue")
plot(bg)#, add = TRUE)
plot(p84(ed.map$oax), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$ver), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$tab), col = "white", add = TRUE, lty = 3)
plot(p84(ed.map$cam), col = "white", add = TRUE, lty = 3)
#
plot(p84(se.map), add = TRUE, border = "gray", col = alpha(portray2, .95))
#plot(p84(se.map), add = TRUE, border = "gray", col = portray2)
# 
plot(p84(dl.map), add = TRUE, border = "black")
# thick state border
#plot(p84(ed.map$cps), add = TRUE, lwd = 3)
#plot(p84(ed.map$cps), add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
text(coordinates(p84(dl.map)), labels=dl.map$disloc, cex=.89, col = "white")
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
#
legend(x="bottomright", bg = "white", legend=c("0-20","20-40","40-60","60-80","80-100"), fill=mauve, title = "% indígena", bty="o", cex=.75)
library(prettymapr)
addnortharrow(pos = ifelse(lp[dn]=="topright", "topleft", "topright"), scale=.75)
addscalebar(style = "ticks", pos = ifelse(lp[dn]=="bottomright", "bottomleft", "bottomright"))
#dev.off()

# distritos c indígenas
c(4, 5, 7, 8, 9, 11, 20, 21, 22)


