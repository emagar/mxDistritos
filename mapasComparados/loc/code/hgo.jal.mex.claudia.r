rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló claudia
mex2 <- read.csv("fuenteAlumnos/Edomex12y17.csv", stringsAsFactors = FALSE)
hgo2 <- read.csv("fuenteAlumnos/Hidalgo13y17.csv", stringsAsFactors = FALSE)
jal2 <- read.csv("fuenteAlumnos/Jalisco12y17.csv", stringsAsFactors = FALSE)
cps2 <- read.csv("fuenteAlumnos/claudia.cps12.csv", stringsAsFactors = FALSE)
oax2 <- read.csv("fuenteAlumnos/claudia.oax12.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
mex1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/mexLoc.csv", stringsAsFactors = FALSE)
hgo1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/hgoLoc.csv", stringsAsFactors = FALSE)
jal1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/jalLoc.csv", stringsAsFactors = FALSE)
cps1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cpsLoc.csv", stringsAsFactors = FALSE)
oax1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/oaxLoc.csv", stringsAsFactors = FALSE)

head(oax2)
dim(oax2)
dim(oax1)

## # mexico
## colnames(mex1)
## mex1 <- mex1[,c("edon","seccion","munn","escenario3")]
## colnames(mex1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Claudia
## colnames(mex2)
## mex2 <- mex2[,c("seccion","munn","Distrito12")]
## mex2$munn <- NULL
## colnames(mex2) <- c("seccion","disn2012") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correctamente
## 
## # fusiona siguientes dos columnas
## mex <- merge(x = mex1, y = mex2, by = "seccion", all = TRUE)
## 
## dim(mex)
## dim(mex1)
## dim(mex2)
## 
## head(mex)
## 
## write.csv(mex, file = "fuenteAlumnos/mexLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz
## 
## # hidalgo
## colnames(hgo1)
## hgo1 <- hgo1[,c("edon","seccion","munn","escenario3_cg")]
## colnames(hgo1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Claudia
## colnames(hgo2)
## hgo2 <- hgo2[,c("seccion","munn","Distrito13")]
## colnames(hgo2) <- c("seccion","munn","disn2013") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correcta mente
## hgo2$munn <- NULL
## 
## # fusiona 
## hgo <- merge(x = hgo1, y = hgo2, by = "seccion", all = TRUE)
## 
## dim(hgo)
## dim(hgo1)
## dim(hgo2)
## 
## head(hgo)
## 
## write.csv(hgo, file = "fuenteAlumnos/hgoLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz
## 
## 
## # jalisco
## colnames(jal1)
## jal1 <- jal1[,c("edon","seccion","munn","escenario3")]
## colnames(jal1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Claudia
## colnames(jal2)
## jal2 <- jal2[,c("seccion","munn","Distrito12")]
## colnames(jal2) <- c("seccion","munn","disn2012") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correcta mente
## jal2$munn <- NULL
## 
## # fusiona 
## jal <- merge(x = jal1, y = jal2, by = "seccion", all = TRUE)
## 
## dim(jal)
## dim(jal1)
## dim(jal2)
## 
## head(jal)
## 
## write.csv(jal, file = "fuenteAlumnos/jalLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz
## 
## 
## # chiapas
## colnames(cps1)
## cps1 <- cps1[,c("edon","seccion","munn","escenario3")]
## colnames(cps1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Claudia
## colnames(cps2)
## colnames(cps2) <- c("disn2012","seccion") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correcta mente
## 
## # fusiona 
## cps <- merge(x = cps1, y = cps2, by = "seccion", all = TRUE)
## 
## dim(cps)
## dim(cps1)
## dim(cps2)
## 
## head(cps)
## 
## write.csv(cps, file = "fuenteAlumnos/cpsLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz
## 
## 
## # oaxaca
## colnames(oax1)
## oax1 <- oax1[,c("edon","seccion","munn","escenario3_tribunal")]
## colnames(oax1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Claudia
## colnames(oax2)
## colnames(oax2) <- c("disn2012","seccion") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correcta mente
## 
## # fusiona 
## oax <- merge(x = oax1, y = oax2, by = "seccion", all = TRUE)
## 
## dim(oax)
## dim(oax1)
## dim(oax2)
## 
## head(oax)
## 
## write.csv(oax, file = "fuenteAlumnos/oaxLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz



# hgo
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/hgoLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2013", new="disloc2013", what=d)
d <- rename.col(old="disn2018", new="disloc2018", what=d)
#
# ---> NOTE:                                                                         <--- #
# ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
# ---> will generate new eq object with full map (incl. state and federal districts) <--- #

write.csv(eq, file = "hgoLoc.csv", row.names = FALSE)


# jal
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/jalLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

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

write.csv(eq, file = "jalLoc.csv", row.names = FALSE)


# mex
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/mexLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

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

write.csv(eq, file = "mexLoc.csv", row.names = FALSE)


# cps
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/cpsLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

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

write.csv(eq, file = "cpsLoc.csv", row.names = FALSE)


# oax
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/oaxLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

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

write.csv(eq, file = "oaxLoc.csv", row.names = FALSE)


## prepare dsi
## hgo
## READ HISTORICAL MAP
d <- read.csv(file = "hgoLoc.csv", stringsAsFactors = FALSE)
head(d)
# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disloc2018
father <- d$disloc2013
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

write.csv(dsi, file = "simIndex/dist_hgo.csv", row.names = FALSE)


## prepare dsi
## jal
## READ HISTORICAL MAP
d <- read.csv(file = "jalLoc.csv", stringsAsFactors = FALSE)
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

write.csv(dsi, file = "simIndex/dist_jal.csv", row.names = FALSE)


## prepare dsi
## mex
## READ HISTORICAL MAP
d <- read.csv(file = "mexLoc.csv", stringsAsFactors = FALSE)
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

write.csv(dsi, file = "simIndex/dist_mex.csv", row.names = FALSE)


## prepare dsi
## cps
## READ HISTORICAL MAP
d <- read.csv(file = "cpsLoc.csv", stringsAsFactors = FALSE)
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

write.csv(dsi, file = "simIndex/dist_cps.csv", row.names = FALSE)


## prepare dsi
## oax
## READ HISTORICAL MAP
d <- read.csv(file = "oaxLoc.csv", stringsAsFactors = FALSE)
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

write.csv(dsi, file = "simIndex/dist_oax.csv", row.names = FALSE)

