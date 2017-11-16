rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

## # si los encuentra, lee datos de disco 
## tmp <- dir()
## sel <- grep(pattern = "(que|san|zac)Loc.csv", x = tmp)
## if (length(sel)>0) {
##     mic <- read.csv(file = tmp[sel[1]], stringsAsFactors = FALSE);
##     zac <- read.csv(file = tmp[sel[2]], stringsAsFactors = FALSE);
## #    san <- read.csv(file = tmp[sel[3]], stringsAsFactors = FALSE);
## }

# Estos son los archivos que circuló
que2 <- read.csv("fuenteAlumnos/veronica.QRO.csv", stringsAsFactors = FALSE)
zac2 <- read.csv("fuenteAlumnos/veronica.ZAC.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
que1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/queLoc.csv", stringsAsFactors = FALSE)
zac1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/zacLoc.csv", stringsAsFactors = FALSE)

head(que2)
dim(que2)
dim(que1)

## # queretaro
## colnames(que1)
## que1 <- que1[,c("edon","seccion","munn","escenario3")]
## colnames(que1) <- c("edon","seccion","munn","disn2018")
## 
## # verifica integridad de las primeras dos columnas de los datos de vero
## que21 <- que2[,c("Sección_17","Dtto_17")]
## tmp <- merge(x= que1, y = que21, by.x = "seccion", by.y = "Sección_17", all = TRUE)
## nrow(tmp)==nrow(que21) # must be TRUE
## rm(que21)
## 
## # fusiona siguientes dos columnas
## que22 <- que2[,c("Sección_12","Dtto_12")]
## colnames(que22) <- c("seccion","disn2012")
## que22 <- que22[is.na(que22$seccion)==FALSE,] # quita NAs
## 
## que <- merge(x= que1, y = que22, by = "seccion", all = TRUE)
## 
## dim(que)
## dim(que1)
## dim(que22)
## 
## que$disn2012 <- as.numeric(as.roman(que$disn2012)) # quita numero romano
## 
## head(que)
## 
## write.csv(que, file = "fuenteAlumnos/queLoc.csv", row.names = FALSE) 

## # zacatecas
## colnames(zac1)
## zac1 <- zac1[,c("edon","seccion","munn","escenario3")]
## colnames(zac1) <- c("edon","seccion","munn","disn2018")
## 
## # verifica integridad de las primeras dos columnas de los datos de zabel
## colnames(zac2)
## zac21 <- zac2[,c("Sección_17","Dtto_17")]
## tmp <- merge(x= zac1, y = zac21, by.x = "seccion", by.y = "Sección_17", all = TRUE)
## nrow(tmp)==nrow(zac21) # must be TRUE
## rm(zac21)
## 
## # fusiona siguientes dos columnas
## zac22 <- zac2[,c("Sección_13","Dtto_13")]
## colnames(zac22) <- c("seccion","disn2013")
## zac22 <- zac22[is.na(zac22$seccion)==FALSE,] # quita NAs
## 
## zac <- merge(x= zac1, y = zac22, by = "seccion", all = TRUE)
## 
## dim(zac)
## dim(zac1)
## dim(zac22)
## 
## zac$disn2013 <- as.numeric(as.roman(zac$disn2013)) # quita numero romano
## 
## head(zac)
## 
## write.csv(zac, file = "fuenteAlumnos/zacLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz



## # que
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/queLoc.csv", stringsAsFactors = FALSE)
## head(d) 
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
## write.csv(eq, file = "queLoc.csv", row.names = FALSE)


## # zac
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/zacLoc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2013", new="disloc2013", what=d)
## d <- rename.col(old="disn2018", new="disloc2018", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "zacLoc.csv", row.names = FALSE)



## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "queLoc.csv", stringsAsFactors = FALSE)
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
## 
## write.csv(dsi, file = "simIndex/dist_que.csv", row.names = FALSE)



## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "zacLoc.csv", stringsAsFactors = FALSE)
## head(d)
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## son    <- d$disloc2018
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
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2018","father","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]
## 
## write.csv(dsi, file = "simIndex/dist_zac.csv", row.names = FALSE)

