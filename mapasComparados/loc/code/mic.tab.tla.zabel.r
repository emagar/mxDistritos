rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

## tmp <- dir()
## sel <- grep(pattern = "(mic|tab|tla)Loc.csv", x = tmp)
## if (length(sel)>0) {
##     mic <- read.csv(file = tmp[sel[1]], stringsAsFactors = FALSE);
##     tab <- read.csv(file = tmp[sel[2]], stringsAsFactors = FALSE);
##     tla <- read.csv(file = tmp[sel[3]], stringsAsFactors = FALSE);
## }

# Estos son los archivos que circuló Zabel
mic2 <- read.csv("fuenteAlumnos/michoacan.redis.csv", stringsAsFactors = FALSE)
tab2 <- read.csv("fuenteAlumnos/tabasco.redis.csv", stringsAsFactors = FALSE)
tla2 <- read.csv("fuenteAlumnos/tlaxcala.redis.csv", stringsAsFactors = FALSE)
# y julio
tla3 <- read.csv("fuenteAlumnos/julio.tlax15loc.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
mic1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/micLoc.csv", stringsAsFactors = FALSE)
tab1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/tabLoc.csv", stringsAsFactors = FALSE)
tla19 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/tla19Loc.csv", stringsAsFactors = FALSE)
tla15 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/tla15Loc.csv", stringsAsFactors = FALSE)
## 
## head(mic2)
## dim(mic2)
## dim(mic1)
 
## # michoacan
## mic1 <- mic1[,c("edon","seccion","munn","escenario3")]
## colnames(mic1) <- c("edon","seccion","munn","disn2018")
## 
## # verifica integridad de las primeras dos columnas de los datos de zabel
## mic21 <- mic2[,c("seccion17","dist17")]
## tmp <- merge(x= mic1, y = mic21, by.x = "seccion", by.y = "seccion17", all = TRUE)
## nrow(tmp)==nrow(mic21) # must be TRUE
## rm(mic21)
## 
## # fusiona siguientes dos columnas
## mic22 <- mic2[,c("seccion12","distrito12")]
## colnames(mic22) <- c("seccion12","disn2012")
## mic22 <- mic22[is.na(mic22$seccion12)==FALSE,] # quita NAs
## 
## mic <- merge(x= mic1, y = mic22, by.x = "seccion", by.y = "seccion12", all = TRUE)
## 
## dim(mic)
## dim(mic1)
## dim(mic22)
## 
## head(mic)
## 
## write.csv(mic, file = "micLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

## #tabasco
## colnames(tab1)
## tab1 <- tab1[,c("edon","seccion","munn","escenario3")]
## colnames(tab1) <- c("edon","seccion","munn","disn2018")
## 
## # verifica integridad de las primeras dos columnas de los datos de zabel
## colnames(tab2)
## tab21 <- tab2[,c("seccion17","distrito17")]
## tmp <- merge(x= tab1, y = tab21, by.x = "seccion", by.y = "seccion17", all = TRUE)
## nrow(tmp)==nrow(tab21) # must be TRUE
## rm(tab21)
## 
## # fusiona siguientes dos columnas
## tab22 <- tab2[,c("seccion12","distrito12")]
## colnames(tab22) <- c("seccion12","disn2012")
## tab22 <- tab22[is.na(tab22$seccion12)==FALSE,] # quita NAs
## 
## tab <- merge(x= tab1, y = tab22, by.x = "seccion", by.y = "seccion12", all = TRUE)
## ## # missing secciones ... unfinished
## ## tmp <- read.csv("../../equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
## ## tmp <- tmp[which(tmp$edon==27),]
## ## sel <- which(is.na(tab$munn)==TRUE)
## ## tab$seccion[sel]
## ## sel.tmp <- which(tmp$seccion %in% tab$seccion[sel])
## ## tmp[sel.tmp,]
## 
## dim(tab)
## dim(tab1)
## dim(tab22)
## 
## head(tab)
## 
## write.csv(tab, file = "tabLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

## # tlaxcala
## colnames(tla19)
## tla19 <- tla19[,c("edon","seccion","munn","escenario3")]
## colnames(tla19) <- c("edon","seccion","munn","disn2016") # OJO: MAPA NUEVO ES disn2016
## colnames(tla15)
## tla15 <- tla15[,c("edon","seccion","munn","escenario3","criterio8","cab")]
## colnames(tla15) <- c("edon","seccion","munn","disn2016pre","disn2016","cab") # Se usó criterio 8

## # verifica integridad de las primeras dos columnas de los datos de zabel
## colnames(tla2)
## max(tla2$distrito12) # problemo!!
## tla21 <- tla2[,c("seccion17","distrito17")]
## tmp <- merge(x= tla15, y = tla21, by.x = "seccion", by.y = "seccion17", all = TRUE)
## nrow(tmp)==nrow(tla21) # must be TRUE
## rm(tla21)

## # fusiona siguientes dos columnas
## tla22 <- tla2[,c("seccion12","distrito12")]
## colnames(tla22) <- c("seccion12","disn2012")
## tla22 <- tla22[is.na(tla22$seccion12)==FALSE,] # quita NAs

## # version 19 distritos
## tla <- merge(x= tla19, y = tla22, by.x = "seccion", by.y = "seccion12", all = TRUE)

## dim(tla)
## dim(tla19)
## dim(tla22)

## # preguntar a julio qué son sus columnas disloc2012 y disloc2015
## dim(tla3)
## colnames(tla3)
## tla3$disn2013 <- tla3$disloc2015 # map used in 2013
## tla3 <- tla3[,c("seccion","disn2013")]
## tla <- merge(x = tla, y = tla3, all = TRUE)
## head(tla)

## table(tla$disn2013, tla$disn2016)
## table(tla$disn2012) # wrong: drop
## tla$disn2012 <- NULL
## head(tla)

## write.csv(tla, file = "fuenteAlumnos/tla19Loc.csv", row.names = FALSE)

## # version 15 distritos
## tla <- merge(x= tla15, y = tla22, by.x = "seccion", by.y = "seccion12", all = TRUE)

## dim(tla)
## dim(tla19)
## dim(tla22)

## head(tla)
## table(tla$disn2016pre, tla$disn2016) # OJO: SE USO CRITERIO 8
## tla$disn2016pre <- NULL # conserva el disn de http://cartografia.ife.org.mx//descargas/distritacion2017/local/29/D29.pdf (que debería ser el del criterio 8)

## tla <- merge(x = tla, y = tla3, by = "seccion", all = TRUE)
## tla$disn2012 <- NULL
## colnames(tla)

## write.csv(tla, file = "fuenteAlumnos/tla15Loc.csv", row.names = FALSE)


## ## tla 15
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/tla15Loc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed

## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2013", new="disloc2013", what=d)
## d <- rename.col(old="disn2016", new="disloc2016", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #

## write.csv(eq, file = "tla15Loc.csv", row.names = FALSE)



## # tla d19
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/tla19Loc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed

## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2013", new="disloc2013", what=d)
## d <- rename.col(old="disn2016", new="disloc2016", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #

## max(eq$disloc2016)
## write.csv(eq, file = "tla19Loc.csv", row.names = FALSE)



# mic
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/micLoc.csv", stringsAsFactors = FALSE)
head(d) 
# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2012", new="disloc2012", what=d)
d <- rename.col(old="disn2018", new="disloc2018", what=d)
#
# ---> NOTE:                                                                         <--- #
# ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
# ---> will generate new eq object with full map (incl. state and federal districts) <--- #

write.csv(eq, file = "micLoc.csv", row.names = FALSE)



## # tab
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/tabLoc.csv", stringsAsFactors = FALSE)
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
## write.csv(eq, file = "tabLoc.csv", row.names = FALSE)




## # DSI
## d <- read.csv(file = "tla15Loc.csv", stringsAsFactors = FALSE)
## head(d)
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## son    <- d$disloc2016
## father <- d$disloc2013
## N <- max(son)
## d$father <- NA
## d$dsi <- 0
## for (i in 1:N){
##     #i <- 3 # debug
##     sel.n <- which(son==i)                  # secciones in new district
##     tmp <- table(father[sel.n])
##     target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
##     d$father[sel.n] <- target
##     sel.f <- which(father==target) # secciones in father district
##     sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
##     d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
## }
## # export districts object
## dsi <- d[duplicated(son)==FALSE,]
## head(dsi)
## dsi <- dsi[,c("edon","disloc2016","father","dsi")]
## dsi <- dsi[order(dsi$dsi),]
## write.csv(dsi, file = "simIndex/dist_tla15.csv", row.names = FALSE)

## # d19
## d <- read.csv(file = "tla19Loc.csv", stringsAsFactors = FALSE)
## head(d)
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## son    <- d$disloc2016
## father <- d$disloc2013
## N <- max(son)
## d$father <- NA
## d$dsi <- 0
## for (i in 1:N){
##     #i <- 3 # debug
##     sel.n <- which(son==i)                  # secciones in new district
##     tmp <- table(father[sel.n])
##     target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
##     d$father[sel.n] <- target
##     sel.f <- which(father==target) # secciones in father district
##     sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
##     d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
## }
## # export districts object
## dsi <- d[duplicated(son)==FALSE,]
## head(dsi)
## dsi <- dsi[,c("edon","disloc2016","father","dsi")]
## dsi <- dsi[order(dsi$dsi),]
## write.csv(dsi, file = "simIndex/dist_tla19.csv", row.names = FALSE)


## prepare dsi
## READ HISTORICAL MAP
d <- read.csv(file = "micLoc.csv", stringsAsFactors = FALSE)
head(d)
# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disloc2018
father <- d$disloc2012
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
dsi <- d[duplicated(son)==FALSE,]
dsi <- dsi[,c("edon","disloc2018","father","dsi")]
head(dsi)
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_mic.csv", row.names = FALSE)


## prepare dsi
## READ HISTORICAL MAP
d <- read.csv(file = "tabLoc.csv", stringsAsFactors = FALSE)
head(d)
# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disloc2018
father <- d$disloc2012
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
dsi <- d[duplicated(son)==FALSE,]
dsi <- dsi[,c("edon","disloc2018","father","dsi")]
head(dsi)
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_tab.csv", row.names = FALSE)


