rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló omar
col2 <- read.csv("fuenteAlumnos/omarDistCol.csv", stringsAsFactors = FALSE)
cua2 <- read.csv("fuenteAlumnos/omarDistChih.csv", stringsAsFactors = FALSE)
mor2 <- read.csv("fuenteAlumnos/omarDistMor.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
col1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/colLoc.csv", stringsAsFactors = FALSE)
cua1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cuaLoc.csv", stringsAsFactors = FALSE)
mor1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/mor12Loc.csv", stringsAsFactors = FALSE)
mor18 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/mor18Loc.csv", stringsAsFactors = FALSE) # también incorporaré el mapa con 18 distritos



## # chihuahua
## colnames(cua1)
## cua1 <- cua1[,c("edon","seccion","munn","escenario3")]
## colnames(cua1) <- c("edon","seccion","munn","disn2016")
## 
## # cambia nombres en datos de Omar
## head(cua2)
## cua2$merge <- NULL
## colnames(cua2) <- c("disn2015","seccion","disn2013") # omar: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 
## 
## # fusiona
## cua <- merge(x = cua1, y = cua2, by = "seccion", all = TRUE)
## 
## dim(cua)
## dim(cua1)
## dim(cua2)
## 
## head(cua)
## table(cua$disn2015, cua$disn2016) # disn mismatch, me quedo con el núm de omar
## cua$disn2016 <- cua$disn2015; cua$disn2015 <- NULL
## 
## write.csv(cua, file = "fuenteAlumnos/cuaLoc.csv", row.names = FALSE) 




## # colima
## colnames(col1)
## col1 <- col1[,c("edon","seccion","munn","escenario3")]
## colnames(col1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Omar
## head(col2)
## table(col2$X_merge)
## col2$X_merge <- NULL
## colnames(col2) <- c("disn2016","seccion","disn2015") # investigar año del mapa abandonado
## 
## # fusiona
## col <- merge(x = col1, y = col2, by = "seccion", all = TRUE)
## 
## dim(col)
## dim(col1)
## dim(col2)
## 
## head(col)
## 
## table(col$disn2016, col$disn2018) # disn mismatch, me quedo con el núm de omar
## col$disn2018 <- col$disn2016; col$disn2016 <- NULL
## 
## write.csv(col, file = "fuenteAlumnos/colLoc.csv", row.names = FALSE)




## # morelos 12
## colnames(mor1)
## mor1 <- mor1[,c("edon","seccion","munn","escenario3")]
## colnames(mor1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Omar
## head(mor2)
## colnames(mor2) <- c("disn2017","seccion","disn2012","cab")
## 
## # fusiona
## mor <- merge(x = mor1, y = mor2, by = "seccion", all = TRUE)
## 
## dim(mor)
## dim(mor1)
## dim(mor2)
## 
## head(mor)
## 
## table(mor$disn2018, mor$disn2017) # disn mismatch, me quedo con el núm de omar
## mor$seccion[which(mor$disn2018==5 & mor$disn2017==4)] # verifiqué que la sección 901 sí aparece en disn=4
## mor$disn2018 <- mor$disn2017; mor$disn2017 <- NULL
## 
## write.csv(mor, file = "fuenteAlumnos/mor12Loc.csv", row.names = FALSE)


## # morelos 18
## colnames(mor18)
## mor18 <- mor18[,c("edon","seccion","munn","escenario3")]
## colnames(mor18) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Omar
## head(mor2)
## colnames(mor2) <- c("disn2017","seccion","disn2012","cab")
## 
## # fusiona
## mor <- merge(x = mor18, y = mor2, by = "seccion", all = TRUE)
## mor$disn2017 <- NULL
## 
## dim(mor)
## dim(mor18)
## dim(mor2)
## 
## head(mor)
## 
## write.csv(mor, file = "fuenteAlumnos/mor18Loc.csv", row.names = FALSE)




## # cua
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/cuaLoc.csv", stringsAsFactors = FALSE)
## sel <- which(d$seccion==3218) # cambió de municipio 19=chihuahua a 2=meoqui
## d$munn[sel] <- 19             # para compatibilizar con pasado
## rm(sel)
## head(d)
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
## 
## write.csv(eq, file = "cuaLoc.csv", row.names = FALSE)


## # col
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/colLoc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed
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
## write.csv(eq, file = "colLoc.csv", row.names = FALSE)


## # mor 12
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/mor12Loc.csv", stringsAsFactors = FALSE)
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
## write.csv(eq, file = "mor12Loc.csv", row.names = FALSE)



## # mor 18
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/mor18Loc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2018", new="disloc2018", what=d)
## d <- rename.col(old="disn2012", new="disloc2012", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "mor18Loc.csv", row.names = FALSE)


## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "cuaLoc.csv", stringsAsFactors = FALSE)
## head(d)
## which(d$disloc2013==0)
## which(d$disloc2016==0)
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## son    <- d$disloc2016
## father <- d$disloc2013
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
## dsi <- d # debug
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2016","father","dsi")]
## dsi <- dsi[order(dsi$dsi),]
## head(dsi)
## 
## write.csv(dsi, file = "simIndex/dist_cua.csv", row.names = FALSE)



## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "colLoc.csv", stringsAsFactors = FALSE)
## head(d)
## #which(d$disloc2015==0)
## #which(d$disloc2018==0)
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
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
## dsi <- dsi[,c("edon","disloc2016","father","dsi")]
## dsi <- dsi[order(dsi$dsi),]
## head(dsi)
## 
## write.csv(dsi, file = "simIndex/dist_col.csv", row.names = FALSE)



## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "mor12Loc.csv", stringsAsFactors = FALSE)
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
## dsi <- dsi[order(dsi$dsi),]
## head(dsi)
## 
## write.csv(dsi, file = "simIndex/dist_mor12.csv", row.names = FALSE)



## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "mor18Loc.csv", stringsAsFactors = FALSE)
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
## dsi <- d # debug
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2018","father","dsi")]
## dsi <- dsi[order(dsi$dsi),]
## head(dsi)

## write.csv(dsi, file = "simIndex/dist_mor18.csv", row.names = FALSE)

