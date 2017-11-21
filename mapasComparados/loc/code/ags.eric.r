rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló humberto
ags2 <- read.csv("fuenteAlumnos/agsLoc.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
ags1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/agsLoc.csv", stringsAsFactors = FALSE)


## # ags
## colnames(ags1)
## ags1 <- ags1[,c("edon","seccion","munn","escenario3")]
## colnames(ags1) <- c("edon","seccion","munn","disn2016")
## 
## # cambia nombres en datos de humberto
## head(ags2)
## ags2$s <- NULL
## ags2$edon <- ags2$munn <- NULL
## colnames(ags2) <- c("edon","seccion","munn","disn2016b","disn2013") # necesito año de disnold
## 
## # fusiona
## ags <- merge(x = ags1, y = ags2, by = "seccion", all = TRUE)
## 
## dim(ags)
## dim(ags1)
## dim(ags2)
## 
## head(ags)
## table(ags$disn2016b==ags$disn2016)
## ags$disn2016b <- NULL
## 
## write.csv(ags, file = "fuenteAlumnos/agsLoc.csv", row.names = FALSE) 





## # ags
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/agsLoc.csv", stringsAsFactors = FALSE)
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
## write.csv(eq, file = "agsLoc.csv", row.names = FALSE)


## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "agsLoc.csv", stringsAsFactors = FALSE)
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
## dsi <- dsi[order(dsi$dsi),]
## head(dsi)
## 
## write.csv(dsi, file = "simIndex/dist_ags.csv", row.names = FALSE)


