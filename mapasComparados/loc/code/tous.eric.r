rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
setwd(dd)

############
#   1 ags  #
############
# Estos son los archivos que circuló humberto
#ags2 <- read.csv("fuenteAlumnos/agsLoc.csv", stringsAsFactors = FALSE)
#
# Estos son los archivos originales
#ags1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/agsLoc.csv", stringsAsFactors = FALSE)

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







############
#   4 cam  #
############
## # Estos son los archivos originales
## cam1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/camLoc.csv", stringsAsFactors = FALSE)
#
## # campeche
## head(cam1)
## cam1 <- cam1[,c("edon","seccion","munn","escenario3")]
## colnames(cam1) <- c("edon","seccion","munn","disn2018")
#
## # añade mapa dibujado en 2010, a mano
## cam2 <- read.csv("fuenteAlumnos/camMap2010.csv", stringsAsFactors = FALSE)
## colnames(cam2) <- c("seccion","disn2012")
## head(cam2)
#
## # fusiona siguientes dos columnas
## cam <- merge(x= cam1, y = cam2, by = "seccion", all = TRUE)
#
## dim(cam)
## dim(cam1)
## dim(cam2)
#
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
#
## write.csv(dsi, file = "simIndex/dist_cam.csv", row.names = FALSE)




############
#   5 coa  #
############
## PREPARES HISTORICAL MAP
## u <- url("http://ericmagar.com/data/redistrict/subnat/coa/coaDisn14toDisn17.csv")
## d <- read.csv(file = u, stringsAsFactors = FALSE)
## d <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/coaLoc.csv", stringsAsFactors = FALSE)
## d <- d[,c("edon","seccion","munn","escenario3","criterio8","cab")]
## table(d$escenario3, d$criterio8) # OJO: SE USO CRITERIO 8; distritos idénticos a json, disn mismatch. ¿Quizás sólo cambiaron las cabeceras? ¿Por qué renumerías via criterio 8 sin cambio de límites?
## d$escenario3 <- NULL # conservo disn que obtuve en http://cartografia.ife.org.mx//descargas/distritacion2017/local/05/D05.pdf
## colnames(d) <- c("edon","seccion","munn","disn2017","cab")
## head(d)
## 
## # this has final disn and 2014 map
## d <- read.csv(file = "fuenteAlumnos/eric.coaLoc.csv", stringsAsFactors = FALSE)
## colnames(d) <- c("seccion","munn","mun","disn2014","disn2017")
## 
## # add 2008 and 2011 maps
## d08 <- read.csv(file = "fuenteAlumnos/coa2008dlca.csv", stringsAsFactors = FALSE)
## ## d11 <- read.csv(file = "fuenteAlumnos/coa2011dlca.csv", stringsAsFactors = FALSE)
## d08 <- d08[, c("mun","disn","seccion","lisnom")]
## ## d11 <- d11[, c("mun","disn","seccion")]
## colnames(d08) <- c("mun","disn2008","seccion","lisnom08")
## ## colnames(d11) <- c("mun","disn2011","seccion")
## d08 <- d08[duplicated(d08$seccion)==FALSE,]
## ## d11 <- d11[duplicated(d11$seccion)==FALSE,]
## d <- merge(x = d, y = d08, by = "seccion", all = TRUE)
## ## d <- merge(x = d, y = d11, by = "seccion", all = TRUE)
## ##    
## head(d)
## # fill missing mun and munn
## sel <- which(is.na(d$munn))
## d$mun.x[sel] <- d$mun.y[sel]
## sel <- which(is.na(d$mun.y))
## d$mun.y[sel] <- d$mun.x[sel]
## # fix missin munn by hand
## sel <- which(is.na(d$munn))
## d.ss <- d[sel,] # subset
## d.ss$munn[grep("Acuña", d.ss$mun.x)] <- 2
## d.ss$munn[grep("Castaños", d.ss$mun.x)] <- 6
## d.ss$munn[grep("Cuatrociénegas", d.ss$mun.x)] <- 7
## d.ss$munn[grep("Nava", d.ss$mun.x)] <- 22
## d.ss$munn[grep("Ocampo", d.ss$mun.x)] <- 23
## d.ss$munn[grep("Piedras Negras", d.ss$mun.x)] <- 25
## d.ss$munn[grep("Saltillo", d.ss$mun.x)] <- 30
## d.ss$munn[grep("Sierra Mojada", d.ss$mun.x)] <- 34
## d.ss$munn[grep("Torreón", d.ss$mun.x)] <- 35
## d[sel,] <- d.ss   # return subset
## # drop redundant cols
## d$mun.x <- d$mun.y; d$mun.y <- NULL
## colnames(d)[which(colnames(d)=="mun.x")] <- "mun"
## # rename maps
## colnames(d)[which(colnames(d)=="disn2014")] <- "disn2011" # districts created in 2011
## colnames(d)[which(colnames(d)=="disn2008")] <- "disn2005" # districts used in 2005, when created unknown
## d <- d[, c("seccion","munn","mun","disn2005","disn2011","disn2017","lisnom08")] # re-order columns
## head(d)
## # fix secciones missing in some map but not others
## ## 2011 map not needed, will not read it bc is same as 2014
## #which(is.na(d$munn)==TRUE) # none
## for (i in 1:max(d$munn)){
##     #i <- 2 # debug
##     sel <- which(d$munn==i)
##     d.ss <- d[sel,] # subset
##     sel.ss <- which(is.na(d.ss$disn2005)==TRUE | is.na(d.ss$disn2011)==TRUE | is.na(d.ss$disn2017)==TRUE) # secciones missing in one+ maps
##     if (length(sel.ss)==0) next # skip if no NAs
##     # 2005
##     tmp <- unique(d.ss$disn2005[!is.na(d.ss$disn2005)]) # municipios in district other than NAs
##     if (length(tmp)==1) {d.ss$disn2005 <- tmp} # skip if municipio split in several districts else replace NAs with single value
##     # 2011
##     tmp <- unique(d.ss$disn2011[!is.na(d.ss$disn2011)]) # municipios in district other than NAs
##     if (length(tmp)==1) {d.ss$disn2011 <- tmp} # skip if municipio split in several districts else replace NAs with single value
##     # 2017
##     tmp <- unique(d.ss$disn2017[!is.na(d.ss$disn2017)]) # municipios in district other than NAs
##     if (length(tmp)==1) {d.ss$disn2017 <- tmp} # skip if municipio split in several districts else replace NAs with single value
##     #
##     d[sel,] <- d.ss # return subset
## }
## rm(sel.ss, d.ss, tmp)
## #
## write.csv(d, file = "fuenteAlumnos/coaLoc.csv", row.names = FALSE)


## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/coaLoc.csv", stringsAsFactors = FALSE)
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2005", new="disloc2005", what=d)
## d <- rename.col(old="disn2011", new="disloc2011", what=d)
## d <- rename.col(old="disn2017", new="disloc2017", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "coaLoc.csv", row.names = FALSE)


## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))
pob05 <- get2005(edon=5)
pob10 <- get2010(edon=5)
head(pob05)
head(pob10)
head(d)
# add 2005 pop
d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## ## READ HISTORICAL MAPS
## d <- read.csv(file = "coaLoc.csv", stringsAsFactors = FALSE)
## head(d)
## son    <- d$disloc2017
## father <- d$disloc2011
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
## dsi <- dsi[,c("edon","disloc2017","father","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$disloc2017),]
## dsi$cab2017 <- c("Acuña", "Piedras Negras", "Sabinas", "San Pedro", "Monclova", "Frontera", "Matamoros", "Torreón", "Torreón", "Torreón", "Torreón", "Ramos Arizpe", "Saltillo", "Saltillo", "Saltillo", "Saltillo")
## dsi <- dsi[order(dsi$dsi),]
## write.csv(dsi, file = "simIndex/dist_coa.csv", row.names = FALSE)
#
## # dsi seen from parent perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## d$son17 <- NA
## d$dsi <- 0
## for (i in 1:16){
##     #i <- 16 # debug
##     sel.o <- which(d$disn14==i)                  # secciones in original district
##     tmp <- table(d$disn17[sel.o])
##     target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
##     d$son2017[sel.o] <- target
##     sel.s <- which(d$disn17==target) # secciones in son district
##     sel.c <- intersect(sel.o, sel.s) # secciones common to original and son districts
##     d$dsi[sel.o] <- round( length(sel.c) / (length(sel.o) + length(sel.s) - length(sel.c)) , 3 )
## }
## dsi <- d[duplicated(d$disn14)==FALSE, c("disn14","son2017","dsi")]
## dsi <- dsi[order(dsi$disn14),]
## dsi$cab14 <- c("Saltillo", "Saltillo", "Saltillo", "Saltillo", "Ramos Arizpe", "Torreón", "Torreón", "Torreón", "Torreón", "San Pedro", "Frontera", "Monclova", "Múzquiz", "Sabinas", "Acuña", "Piedras Negras")
## dsi <- dsi[order(dsi$dsi),]
## summary(dsi$dsi)






############
#   9  df  #
############
# Estos son los archivos que circuló chema
## df2 <- read.csv("fuenteAlumnos/chema_cdmxLoc.csv", stringsAsFactors = FALSE)
## df3 <- df2 # duplica
## 
## # Estos son los archivos originales
## df1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/df33Loc.csv", stringsAsFactors = FALSE)
## df40 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/df40Loc.csv", stringsAsFactors = FALSE)

## # when full municipio assigned to a district, this transfers info to secciones
## library(crayon)
## # function to fill secciones with a municipality's info
## munReplica <- function(dat = dat){
##     #sel.col <- grep("dis", colnames(dat))
##     sel.col <- setdiff(1:ncol(dat), grep("edon|seccion|munn", colnames(dat))) # version for colnames identifying actor 
##     sel.row <- which(dat$seccion==0)
##     for (r in sel.row){
##         for (c in sel.col){
##             #r <- 2; c <- 12 # debug
##             if (is.na(dat[r, c])==TRUE) next
##             dis <- dat[r, c] # copy district number for municipality's secciones
##             sel <- which(dat$munn==dat$munn[r] & is.na(dat[,c])==TRUE) # select rows matching municipality with no district info in the column
##             dat[sel, c] <- dis
##         }
##     }
##     dat <- dat[-which(dat$seccion==0), ] # drop seccion=0 rows
##     return(dat)
## }
## #
## 
## # manipulate objects here
## head(df1)
## head(df40)
## tmp <- df40[,c("seccion","munn")]
## table(df1$munn)
## dat <- merge(x = df1, y = tmp, by = c("seccion","munn"), all = TRUE) # df1 has full municipios in everyone's districts, need to add missing secciones (present in df40) 
## dim(tmp)
## dim(dat)
## head(dat)
## table(dat$munn) # solved
## table(dat$escenario3_c8[dat$munn==8], useNA = "always")
## 
## dat <- dat[order(dat$seccion, dat$munn),] # sort
## if (length(which(dat$seccion==0))==0) {
##     message(green("Municipalities aren't building block here. Move on to write.csv"))
## } else {
##     message(red("Execute munReplica()"))
## }
## 
## dat <- munReplica(dat)
## 
## head(dat)
## table(dat$escenario3_c8)
## 
## write.csv(dat, file = "df33Loc.csv", row.names = FALSE)

## head(df2)
## dim(df2)
## dim(df1)
## dim(df40)
## 
## # ciudad de mex 33 distritos
## OJO: SE USO CRITERIO 8, REGISTRADO 
## colnames(df1)
## df1 <- df1[,c("edon","seccion","munn","escenario3_c8")]
## colnames(df1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Chema
## head(df2)
## df2$father15 <- df2$dsi <- df2$X <- NULL
## colnames(df2) <- c("seccion","munn","disn2015","disn2017") # chema: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo)
## df2$munn <- NULL
## table(df2$disn2015==df2$disn2017)
## 
## # fusiona
## df <- merge(x = df1, y = df2, by = "seccion", all = TRUE)
## 
## dim(df)
## dim(df1)
## dim(df2)
## 
## head(df)
## # retoma 
## table(df$disn2018, df$disn2017) # <-- Hay una correspondencia 1-a-1 entre el mapa de Json y los que obtuvo Chema (disn2017), pero los disn no corresponden. Esto se repite en otros estados distritados más tarde en el proceso de redistritación 2015-2016. Preguntar a cartografía qué expliquen.
## df$disn2018 <- df$disn2017; df$disn2017 <- NULL # Me quedo con los números de Chema
## 
## write.csv(df, file = "fuenteAlumnos/df33Loc.csv", row.names = FALSE)
## 
## 
## # mapa con 40 distritos
## colnames(df40)
## df40 <- df40[,c("edon","seccion","munn","escenario3")]
## colnames(df40) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Chema
## head(df2)
## df2$disn2017 <- NULL
## 
## # fusiona
## df <- merge(x = df40, y = df2, by = "seccion", all = TRUE)
## 
## dim(df)
## dim(df40)
## dim(df2)
## head(df)
## 
## write.csv(df, file = "fuenteAlumnos/df40Loc.csv", row.names = FALSE) 

## # df33
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/df33Loc.csv", stringsAsFactors = FALSE)
## head(d) 
## colnames(d) <- c("seccion","edon","munn","disn2018","disn2012") # correct map years
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
## write.csv(eq, file = "df33Loc.csv", row.names = FALSE)
## 
## 
## # df40
## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/df40Loc.csv", stringsAsFactors = FALSE)
## head(d) # dist_old year needed
## colnames(d) <- c("seccion","edon","munn","disn2018","disn2012") # correct map years
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
## write.csv(eq, file = "df40Loc.csv", row.names = FALSE)


## ## ADD 2005 and 2010 seccion-level population
## ## get functions to include population
## source(paste(dd, "code/getPop.r", sep = ""))
## pob05 <- get2005(edon=9)
## pob10 <- get2010(edon=9)
## head(pob05)
## head(pob10)
## #
## ## READ HISTORICAL MAP --- 2018 with 33 districts
## d <- read.csv(file = "df33Loc.csv", stringsAsFactors = FALSE)
## # add 2005 pop
## d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
## ## d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
## ## d$ptot <- NULL
## colnames(d)[which(colnames(d)=="ptot")] <- "pob05"
## # add 2010 pop
## d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
## ## d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
## ## d$ptot <- NULL
## colnames(d)[which(colnames(d)=="ptot")] <- "pob10"
## head(d)
## write.csv(d, file = "df33Loc.csv", row.names = FALSE)
## #
## ## READ HISTORICAL MAP --- 2018 with 40 districts
## d <- read.csv(file = "df40Loc.csv", stringsAsFactors = FALSE)
## # add 2005 pop
## d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
## ## d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
## ## d$ptot <- NULL
## colnames(d)[which(colnames(d)=="ptot")] <- "pob05"
## # add 2010 pop
## d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
## ## d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
## ## d$ptot <- NULL
## colnames(d)[which(colnames(d)=="ptot")] <- "pob10"
## head(d)
## write.csv(d, file = "df40Loc.csv", row.names = FALSE)

## ## prepare rris
## ## READ HISTORICAL MAP --- 2018 with 33 districts
## d <- read.csv(file = "df33Loc.csv", stringsAsFactors = FALSE)
## d$pob05[is.na(d$pob05)] <- 0 # remove NAs from secciones with no pop figure
## d$pob10[is.na(d$pob10)] <- 0
## sel <- which(d$disloc2018==0); d <- d[-sel,] # drop unassigned secciones
## d$pob05 <- ave(d$pob05, as.factor(d$disloc2018), FUN = sum, na.rm = TRUE)
## d$pob10 <- ave(d$pob10, as.factor(d$disloc2018), FUN = sum, na.rm = TRUE)
## d <- d[duplicated(d$disloc2018)==FALSE, c("edon","disloc2018","pob05","pob10")]
## projPob <- function(p1,p2,y3){
##     b <- (p2-p1)/(2010-2005)
##     a <- p2 - b*2010
##     result <- a + b*y3
##     return(result)
## }
## d$pob15 <- projPob(d$pob05,d$pob10,2015)
## d <- d[order(d$disloc2018),]
## d$rri <- sum(d$pob15)/(33*d$pob15)
## head(d)
## write.csv(d, file = "rris/df33_disloc2018.csv", row.names = FALSE)
## #
## ## READ HISTORICAL MAP --- 2018 with 40 districts
## d <- read.csv(file = "df40Loc.csv", stringsAsFactors = FALSE)
## d$pob05[is.na(d$pob05)] <- 0 # remove NAs from secciones with no pop figure
## d$pob10[is.na(d$pob10)] <- 0
## sel <- which(d$disloc2018==0); d <- d[-sel,] # drop unassigned secciones
## d$pob05 <- ave(d$pob05, as.factor(d$disloc2018), FUN = sum, na.rm = TRUE)
## d$pob10 <- ave(d$pob10, as.factor(d$disloc2018), FUN = sum, na.rm = TRUE)
## d <- d[duplicated(d$disloc2018)==FALSE, c("edon","disloc2018","pob05","pob10")]
## d$pob15 <- projPob(d$pob05,d$pob10,2015)
## d <- d[order(d$disloc2018),]
## d$rri <- sum(d$pob15)/(40*d$pob15)
## head(d)
## write.csv(d, file = "rris/df40_disloc2018.csv", row.names = FALSE)
## #
## ## READ HISTORICAL MAP --- 2012 
## d <- read.csv(file = "df40Loc.csv", stringsAsFactors = FALSE)
## d$pob05[is.na(d$pob05)] <- 0 # remove NAs from secciones with no pop figure
## d$pob10[is.na(d$pob10)] <- 0
## sel <- which(d$disloc2012==0); d <- d[-sel,] # drop unassigned secciones
## d$pob05 <- ave(d$pob05, as.factor(d$disloc2012), FUN = sum, na.rm = TRUE)
## d$pob10 <- ave(d$pob10, as.factor(d$disloc2012), FUN = sum, na.rm = TRUE)
## d <- d[duplicated(d$disloc2012)==FALSE, c("edon","disloc2012","pob05","pob10")]
## d$pob15 <- projPob(d$pob05,d$pob10,2015)
## d <- d[order(d$disloc2012),]
## d$rri <- sum(d$pob15)/(40*d$pob15)
## head(d)
## write.csv(d, file = "rris/df_disloc2012.csv", row.names = FALSE)

## ## prepare dsi (with common secciones) 
## ## READ HISTORICAL MAP
## d <- read.csv(file = "df33Loc.csv", stringsAsFactors = FALSE)
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
## #
## write.csv(dsi, file = "simIndex/dist_df33.csv", row.names = FALSE)
## #
## #
## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "df40Loc.csv", stringsAsFactors = FALSE)
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
## #
## write.csv(dsi, file = "simIndex/dist_df40.csv", row.names = FALSE)






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
#  18 nay  #
############
## # El archivo histórico
## nay <- read.csv("fuenteAlumnos/nayLoc.eric.csv", stringsAsFactors = FALSE)
## head(nay)
## nay3 <- read.csv("../../../../elecReturns/datosBrutos/nay2005dlca.csv", stringsAsFactors = FALSE)
## head(nay3)
## nay3 <- nay3[duplicated(nay3$casilla)==FALSE, c("casilla","disn")]
## colnames(nay3) <- c("seccion","disn2005")
## nay4 <- read.csv("../../../../elecReturns/datosBrutos/nay2008aycasilla.regidDemarcacion.csv", stringsAsFactors = FALSE)
## head(nay4)
## nay4 <- nay4[duplicated(nay4$secn)==FALSE, c("secn","demarcacion")]
## colnames(nay4) <- c("seccion","demarc2008")
## nay4$demarc2008 <- as.numeric(sub("DEM.([0-9]+)", "\\1", nay4$demarc2008))
## nay <- merge(nay, nay3, "seccion", all = TRUE)
## nay <- merge(nay, nay4, "seccion", all = TRUE)
## rm(nay3, nay4)
## head(nay)
## nay$demarc2008 <- nay$munn+nay$demarc2008/100
## table(nay$demarc2008==nay$demarc2014, useNA = "ifany")
## 
## write.csv(nay, file = "fuenteAlumnos/nayLoc.csv", row.names = FALSE)

## nay
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/nayLoc.csv", stringsAsFactors = FALSE)
## head(d) 
## colnames(d) <- c("seccion","disn2017","cab","mun","munn","demarc2014","disn2005","demarc2008") # dist_old year needed
## d <- d[, c("seccion","cab","mun","munn","disn2005","disn2017","demarc2008","demarc2014")]

## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2005", new="disloc2005", what=d)
## d <- rename.col(old="disn2017", new="disloc2017", what=d)
## #

## ###################################################################################
## ## NOTE:                                                                         ##
## ## open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   ##
## ## will generate new eq object with full map (incl. state and federal districts) ##
## ###################################################################################

## table(eq$demarc2008==eq$demarc2014, useNA = "ifany") # no change in demarcaciones since 2008
## eq$demarc2014 <- NULL

## write.csv(eq, file = "nayLoc.csv", row.names = FALSE)



## # prepara/exporta dsi
## d <- read.csv(file = "nayLoc.csv", stringsAsFactors = FALSE)
## colnames(d)
## son    <- d$disloc2017
## father <- d$disloc2005
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
## dsi <- dsi[, c("disloc2017","father","cab","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]

## write.csv(dsi, file = "simIndex/dist_nay.csv", row.names = FALSE)





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



