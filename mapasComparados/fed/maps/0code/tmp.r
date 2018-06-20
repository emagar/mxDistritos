## prepare dsi, fathers, sons
## READ HISTORICAL MAP
d <- read.csv(file = "../equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
sel <- which(d$edon==edon)
d <- d[sel,] # subset to relevant state
#
d2018 <- d
d2018 <- d[-which(d2018$dis2018==0),] # drop missing secciones
d <- d[is.na(d2018$dis2018)==FALSE,] # drop missing secciones
d2018$father <- 0
d2018$dsi <- 0
#
for (s in 1:max(d$dis2018)){
    # s <- 1 # debug
    sel <- which(d$dis2018==s)
    potentialFathers <- unique(d$dis2015[sel])
    father <- 0
    dsi <- 0
    for (f in potentialFathers) {
        #f <- 1 # debug
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
d2015 <- d[-which(d2015$dis2015==0),] # drop missing secciones
d <- d[is.na(d2015$dis2015)==FALSE,] # drop missing secciones
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
        #s <- 1 # debug
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
tmp$order <- 1:41
tmp <- merge(tmp, d2018, by.x = "disfed", by.y = "dis2018", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$order),]
# import father and dsi
df.map$father <- tmp$father
df.map$dsi <- tmp$dsi
#
tmp <- df2006.map@data
tmp$order <- 1:40
tmp <- merge(tmp, d2015, by.x = "disfed", by.y = "dis2015", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$order),]
# import son and dsi
df2006.map$son <- tmp$son
df2006.map$dsi <- tmp$dsi
#
# clean
rm(d, sel, potentialFathers, potentialSons, father, son, dsi, s, f sel.u, sel.i, d2015, d2018)

