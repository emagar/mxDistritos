rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló Santi
bc2 <- read.csv("fuenteAlumnos/MapaBajaCaliforniaCorregido.csv", stringsAsFactors = FALSE)
qui2 <- read.csv("fuenteAlumnos/quiasco.redis.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
bc1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/bcLoc.csv", stringsAsFactors = FALSE)
qui1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/quiLoc.csv", stringsAsFactors = FALSE)

head(bc2)
dim(bc2)
dim(bc1)

## # bc
## colnames(bc1)
## bc1 <- bc1[,c("edon","seccion","munn","escenario1")]
## colnames(bc1) <- c("edon","seccion","munn","disn2016pre")
## 
## # verifica integridad de las primeras dos columnas de los datos de Santi
## colnames(bc2)
## colnames(bc2) <- c("edon", "seccion","disn2016","disn2013","munn","cab")
## bc2$edon <- NULL; bc2$munn <- NULL
## bc <- merge(x= bc1, y = bc2, by = "seccion", all = TRUE)
## 
## dim(bc)
## dim(bc1)
## dim(bc2)
## 
## head(bc)
## table(bc$disn2016pre, bc$disn2016) # números de Santi son los que reporta ine
## bc$disn2016pre <- NULL
## 
## write.csv(bc, file = "fuenteAlumnos/bcLoc.csv", row.names = FALSE) 



# qui
colnames(qui1)
qui1 <- qui1[,c("edon","seccion","munn","escenario3")]
colnames(qui1) <- c("edon","seccion","munn","disn2016pre") # se usó criterio 8

# verifica integridad de las primeras dos columnas de los datos de Santi
colnames(qui2)
qui2$edon <- NULL
colnames(qui2) <- c("seccion","disn2016","disn2013")
qui <- merge(x= qui1, y = qui2, by = "seccion", all = TRUE)

dim(qui)
dim(qui1)
dim(qui2)

head(qui)

write.csv(qui, file = "fuenteAlumnos/quiLoc.csv", row.names = FALSE) 



## # bc
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/bcLoc.csv", stringsAsFactors = FALSE)
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
## write.csv(eq, file = "bcLoc.csv", row.names = FALSE)




## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "bcLoc.csv", stringsAsFactors = FALSE)
## head(d)
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
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2016","father","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]
## 
## write.csv(dsi, file = "simIndex/dist_bc.csv", row.names = FALSE)



