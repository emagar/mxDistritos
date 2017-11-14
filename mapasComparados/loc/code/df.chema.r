rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

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



## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "fuenteAlumnos/df33Loc.csv", stringsAsFactors = FALSE)
## 
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## son    <- d$disn2018
## father <- d$disn2015
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
## 
## dsi <- d[duplicated(son)==FALSE,]
## dsi$seccion <- dsi$munn <- NULL
## dsi$disn2015 <- NULL
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]
## 
## write.csv(dsi, file = "simIndex/dist_df33.csv", row.names = FALSE)
## 
## 
## ## prepare dsi
## ## READ HISTORICAL MAP
## d <- read.csv(file = "fuenteAlumnos/df40Loc.csv", stringsAsFactors = FALSE)
## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## son    <- d$disn2018
## father <- d$disn2015
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
## 
## dsi <- d[duplicated(son)==FALSE,]
## dsi$seccion <- dsi$munn <- NULL
## dsi$disn2015 <- NULL
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]
## 
## write.csv(dsi, file = "simIndex/dist_df40.csv", row.names = FALSE)



# df33
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/df33Loc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2015", new="disloc2015", what=d)
d <- rename.col(old="disn2018", new="disloc2018", what=d)
#
# ---> NOTE:                                                                         <--- #
# ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
# ---> will generate new eq object with full map (incl. state and federal districts) <--- #

write.csv(eq, file = "df33Loc.csv", row.names = FALSE)


# df40
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/df40Loc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2015", new="disloc2015", what=d)
d <- rename.col(old="disn2018", new="disloc2018", what=d)
#
# ---> NOTE:                                                                         <--- #
# ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
# ---> will generate new eq object with full map (incl. state and federal districts) <--- #

write.csv(eq, file = "df40Loc.csv", row.names = FALSE)







## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))
pob05 <- get2005(edon=9)
pob10 <- get2010(edon=9)

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













