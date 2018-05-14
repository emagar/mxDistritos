# wrap map 1 (state w all districts and selected in pink) in function
mapKeyPink <- function(dn = NULL){
    dn <- dn
    par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
    #par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
    plot(p84(ed.map$mex), col = "white", axes = TRUE, main = "Estado de México (distritos federales 2018)")#, bg = "lightblue")
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
    plot(p84(df.map[df.map$disfed==dn,]), add = TRUE, border = "gray", col = "hotpink")
                                        # thick state border
    plot(p84(ed.map$mex), add = TRUE, lwd = 3)
    plot(p84(ed.map$mex), add = TRUE, border = "red", lty = 3, lwd = 2)
    ## points(cabDis, pch = 3) # cabeceras distritales
    ## points(cabDis)
    ## points(cabDis, pch = 19, cex = .75, col = "orange")
    text(coordinates(p84(df.map)), labels=df.map$disfed, cex=.85)
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
}



# wrap map 2 or 3 (district with 5-cycle history or with 2015 winner)
mapOneDistrito <- function(dn = NULL, portray = NULL){
    # plot same distrito only
    # need to merge disn info into mun and sec object, in order to select just those belonging to dis
    # get openstreetmap background
    m <- p84(df.map[df.map$disfed==dn,])  # subsetted map
    b <- as.data.frame(m@bbox)
    # gets xx degrees more than bbox (decimal defines share of max range)
    xx <- .12*max(b$max[2] - b$min[2], b$max[1] - b$min[1])
    # choose one of four background picture types
    #bg.tn <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("stamen-toner"))
    #bg.bi <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("bing"))
    #bg.to <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("maptoolkit-topo"))
    bg.os <- openmap(c(b$max[2]+xx,b$min[1]-xx), c(b$min[2]-xx,b$max[1]+xx), type=c("osm"))
    bg <- bg.os
    #
    #png(file = paste(mapdir2, "/", edo, dn, "-2.png", sep = ""), width=15, height=15, units="cm", res=144) 
    par(mar=c(0,0,2,0)) ## SETS B L U R MARGIN SIZES
    tmp <-  df.map$cab[which(df.map$disfed==dn)]
    tmp2 <- df.map$dsi[which(df.map$disfed==dn)]
    plot(df.map[df.map$disfed==dn,], axes = TRUE, main = paste("México ", dn, " - ", tmp, sep = ""))
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
    plot(se.map[se.map$disfed==dn,], add = TRUE, border = "darkgray", col = alpha(portray[se.map$disfed==dn], .5)) # color nwin
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
    ## sel <- which(df2006.map$disfed==df.map$father[df.map$disloc==dn]) # <- OJO: falta determinar father
    ## plot(df2006.map[sel,], add = TRUE, lwd = 6, border = "red")
    #
    plot(df.map[df.map$disfed==dn,], add = TRUE, lwd = 4)
    plot(mu.map, add = TRUE, border = "green", lwd = 1)
    plot(mu.map, add = TRUE, lwd = 1, lty = 3)
    plot(ed.map$mex, add = TRUE, lwd = 3)
    plot(ed.map$mex, add = TRUE, border = "red", lty = 3, lwd = 2)
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
            "bottomright", #10
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
            "bottomleft")  #41
    legend(x=lp[dn], bg = "white", legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","black","black","gray"), lty = c(1,1,1,1,1), pch = c(NA,NA,NA,NA,19), lwd = c(6,6,2,2,0), bty="o", cex=.75)
    legend(x=lp[dn], bg = NULL,    legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","red","green","white"),  lty = c(1,1,3,3,1), pch = c(NA,NA,NA,NA,20), lwd = c(2,2,2,2,0), bty="o", cex=.75)
    library(prettymapr)
    addnortharrow(pos = ifelse(lp[dn]=="topright", "topleft", "topright"), scale=.75)
    addscalebar(style = "ticks", pos = ifelse(lp[dn]=="bottomright", "bottomleft", "bottomright"))
    #dev.off()
}
    

########################################################################
# grafica distritos 1 por 1                                            #
# (use 1984 long/lat for this map when mercator projection was chosen) #
########################################################################
p84 <- function(x = NA){
    x <- x
    x <- spTransform(x, CRS("+proj=longlat +datum=WGS84"))
}


## for (dn in 1:41){
##     print(paste("disn =", dn))

###############################################
## # plot state map with highlighted district #
###############################################
#png(file = paste(mapdir2, "/", edo, dn, "-1.png", sep = ""), width=10, height=10, units="cm", res=144) 
mapKeyPink(dn = 15)
#dev.off()

##############################################
## # plot district map with mid-term history #
##############################################
#png(file = paste(mapdir2, "/", edo, dn, "-2.png", sep = ""), width=15, height=15, units="cm", res=144) 
mapOneDistrito(dn = 15, portray = se.map$bastion)
#dev.off()

#########################################
## # plot district map with 2015 winner #
#########################################
#png(file = paste(mapdir2, "/", edo, dn, "-3.png", sep = ""), width=15, height=15, units="cm", res=144) 
mapOneDistrito(dn = 15, portray = se.map$win15)
#dev.off()

# }


