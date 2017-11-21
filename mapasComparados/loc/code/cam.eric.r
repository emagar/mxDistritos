rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

## # Estos son los archivos originales
## cam1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/camLoc.csv", stringsAsFactors = FALSE)

## # campeche
## head(cam1)
## cam1 <- cam1[,c("edon","seccion","munn","escenario3")]
## colnames(cam1) <- c("edon","seccion","munn","disn2018")

## # añade mapa dibujado en 2010, a mano
## cam2 <- read.csv("fuenteAlumnos/camMap2010.csv", stringsAsFactors = FALSE)
## colnames(cam2) <- c("seccion","disn2012")
## head(cam2)

## # fusiona siguientes dos columnas
## cam <- merge(x= cam1, y = cam2, by = "seccion", all = TRUE)

## dim(cam)
## dim(cam1)
## dim(cam2)

## head(cam)

## write.csv(cam, file = "camLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz




## # cam
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/camLoc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2012", new="disloc2012", what=d)
## d <- rename.col(old="disn2018", new="disloc2018", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "camLoc.csv", row.names = FALSE)



## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "camLoc.csv", stringsAsFactors = FALSE)
## head(d)
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## son    <- d$disloc2018
## father <- d$disloc2012
## N <- max(son, na.rm = TRUE)
## d$father <- NA
## d$dsi <- 0
## for (i in 1:N){
##     #i <- 1 # debug
##     sel.n <- which(son==i)                  # secciones in new district
##     tmp <- table(father[sel.n])
##     target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # takes first instance in case of tie (dual fathers) 
##     d$father[sel.n] <- target
##     sel.f <- which(father==target) # secciones in father district
##     sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
##     d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
## }
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2018","father","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]

## write.csv(dsi, file = "simIndex/dist_cam.csv", row.names = FALSE)



## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))

d <- read.csv(file = "camLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=4)
pob10 <- get2010(edon=4)

head(pob05)
head(pob10)
head(d)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disn2018
father <- d$disn2012
N <- max(son, na.rm = TRUE)
d$father <- NA
d$dsi <- 0
for (i in 1:N){
    #i <- 1 # debug
    sel.n <- which(son==i)                  # secciones in new district
    tmp <- table(father[sel.n])
    target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # takes first instance in case of tie (dual fathers) 
    d$father[sel.n] <- target
    sel.f <- which(father==target) # secciones in father district
    sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
    d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
# add 2005 pop
d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all = TRUE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all = TRUE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

# export districts object
dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- NULL
dsi$disn2012 <- NULL

dsi <- dsi[order(dsi$dsi),]

head(dsi)

write.csv(dsi, file = "simIndex/dist_cam.csv", row.names = FALSE)

summary(dsi$dsi)


