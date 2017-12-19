rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
setwd(dd)


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



