rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
setwd(dd)


############
#  11 gua  #
############
## # Estos son los archivos que circuló víctor
## gua2 <- read.csv("fuenteAlumnos/victor.guanajuato.csv", stringsAsFactors = FALSE)
## 
## # Estos son los archivos originales
## gua1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/guaLoc.csv", stringsAsFactors = FALSE)
##
## # gua
## colnames(gua1)
## gua1 <- gua1[,c("edon","seccion","munn","escenario3")]
## colnames(gua1) <- c("edon","seccion","munn","disn2018pre")
##
## # cambia nombres en datos de víctor
## colnames(gua2)
## colnames(gua2) <- c("seccion","munn","mun","disn2015","disn2018")
## gua2$munn <- NULL
##
## # fusiona siguientes dos columnas
## gua <- merge(x = gua1, y = gua2, by = "seccion", all = TRUE)
## gua$disn2018pre <- NULL # conserva disn de víctor
##
## dim(gua)
## dim(gua1)
## dim(gua2)
## head(gua)
##
## write.csv(gua, file = "fuenteAlumnos/guaLoc.csv", row.names = FALSE)


## # gua
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/guaLoc.csv", stringsAsFactors = FALSE)
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
## write.csv(eq, file = "guaLoc.csv", row.names = FALSE)


## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "guaLoc.csv", stringsAsFactors = FALSE)
##
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## head(d)
## table(d$disloc2018)
##
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
## #dsi <- d # debug
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2018","father","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]
##
## write.csv(dsi, file = "simIndex/dist_gua.csv", row.names = FALSE)




############
#  12 gue  #
############
##
###########################################################################
###########################################################################
# no debe usarse más, he borrado los archivos fuente para proteger gueLoc #
###########################################################################
###########################################################################
## # Estos son los archivos que circuló víctor
## gue2 <- read.csv("fuenteAlumnos/victor.guerrero.csv", stringsAsFactors = FALSE)
##
## # Estos son los archivos originales
## gue1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/gueLoc.csv", stringsAsFactors = FALSE)
##
## # gue
## colnames(gue1)
## gue1 <- gue1[,c("edon","seccion","munn","escenario3_c8")]
## colnames(gue1) <- c("edon","seccion","munn","disn2018pre")
##
## # cambia nombres en datos de víctor
## colnames(gue2)
## colnames(gue2) <- c("seccion","munn","mun","disn2015","disn2018")
## gue2$munn <- NULL
##
## # fusiona siguientes dos columnas
## gue <- merge(x = gue1, y = gue2, by = "seccion", all = TRUE)
## gue$disn2018pre <- NULL # conserva disn de víctor
##
## dim(gue)
## dim(gue1)
## dim(gue2)
## head(gue)
##
## write.csv(gue, file = "fuenteAlumnos/gueLoc.csv", row.names = FALSE)

###########################################################################
###########################################################################
# no debe usarse más, he borrado los archivos fuente para proteger gueLoc #
###########################################################################
###########################################################################
#
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/gueLoc.csv", stringsAsFactors = FALSE)
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
## write.csv(eq, file = "gueLoc.csv", row.names = FALSE)

## prepare dsi
## READ HISTORICAL MAP
d <- read.csv(file = "gueLoc.csv", stringsAsFactors = FALSE)
######################################################################################
##                                OJO                                               ##
## CUANDO SE DIBUJO EL MAPA LOCAL QUE SE USO EN 2015 EN GUE??? VER COMENTARIO ABAJO ##
######################################################################################
# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
head(d) # OJO!! disloc2015 lo extraje de gue2005dlca y disloc2018 de cartografia.ife.org.mx/descargas/distritacion2017/local/12/D12.pdf; éste se produjo en 2016, pero los dos mapas son PRACTICAMENTE IGUALES!!!
son    <- d$disloc2018
father <- d$disloc2015 # ES ESTRICTO SENTIDO, EL FATHER ES EL DE 2015...
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
#dsi <- d # debug
dsi <- d[duplicated(son)==FALSE,]
dsi <- dsi[,c("edon","disloc2018","father","dsi")]
head(dsi)
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_gue.csv", row.names = FALSE)


############
#  19  nl  #
############
## # Estos son los archivos que circuló víctor
## nl2 <- read.csv("fuenteAlumnos/victor.nvoleon.csv", stringsAsFactors = FALSE)
## 
## # Estos son los archivos originales
## nl1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/nlLoc.csv", stringsAsFactors = FALSE)
## 
## # nl
## colnames(nl1)
## nl1 <- nl1[,c("edon","seccion","munn","escenario3")]
## colnames(nl1) <- c("edon","seccion","munn","disn2018pre")
## 
## # cambia nombres en datos de daniel
## colnames(nl2)
## colnames(nl2) <- c("seccion","munn","mun","disn2015","disn2018")
## 
## # fusiona siguientes dos columnas
## nl <- merge(x = nl1, y = nl2, by = "seccion", all = TRUE)
## nl$disn2018pre <- NULL # conserva disn de víctor
## 
## dim(nl)
## dim(nl1)
## dim(nl2)
## head(nl)
## 
## write.csv(nl, file = "fuenteAlumnos/nlLoc.csv", row.names = FALSE)

## # nl
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/nlLoc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2015", new="disloc2015", what=d)
## d <- rename.col(old="disn2018", new="disloc2018", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "nlLoc.csv", row.names = FALSE)


## ## prepare dsi
## ## nl
## ## READ HISTORICAL MAP
## d <- read.csv(file = "nlLoc.csv", stringsAsFactors = FALSE)
## 
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## head(d)
## son    <- d$disloc2018
## father <- d$disloc2015
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
## #dsi <- d # debug
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2018","father","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]
## 
## write.csv(dsi, file = "simIndex/dist_nl.csv", row.names = FALSE)



############
#  24 san  #
############
## # Estos son los archivos que circuló daniel
## san2 <- read.csv("fuenteAlumnos/dani.san.csv", stringsAsFactors = FALSE)
## 
## # Estos son los archivos originales
## san1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/sanLoc.csv", stringsAsFactors = FALSE)
## 
## head(san1)
## head(san2)
## 
## # san
## colnames(san1)
## san1 <- san1[,c("edon","seccion","munn","escenario3")]
## colnames(san1) <- c("edon","seccion","munn","disn2018pre")
## 
## # cambia nombres en datos de daniel
## colnames(san2)
## san2 <- san2[,c("seccionn","dist_new","dist_old")]
## colnames(san2) <- c("seccion","disn2018","disn2015")
## 
## # fusiona siguientes dos columnas
## san <- merge(x = san1, y = san2, by = "seccion", all = TRUE)
## san$disn2018pre <- NULL # conserva disn de daniel
## 
## dim(san)
## dim(san1)
## dim(san2)
## head(san)
## 
## write.csv(san, file = "fuenteAlumnos/sanLoc.csv", row.names = FALSE)


## # san
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/sanLoc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2015", new="disloc2015", what=d)
## d <- rename.col(old="disn2018", new="disloc2018", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "sanLoc.csv", row.names = FALSE)


## ## prepare dsi
## ## san
## ## READ HISTORICAL MAP
## d <- read.csv(file = "sanLoc.csv", stringsAsFactors = FALSE)
## 
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## head(d)
## son    <- d$disloc2018
## father <- d$disloc2015
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
## #dsi <- d # debug
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2018","father","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]
## 
## write.csv(dsi, file = "simIndex/dist_san.csv", row.names = FALSE)



