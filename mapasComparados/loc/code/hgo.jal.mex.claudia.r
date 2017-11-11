rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló claudia
mex2 <- read.csv("fuenteAlumnos/Edomex12y17.csv", stringsAsFactors = FALSE)
hgo2 <- read.csv("fuenteAlumnos/Hidalgo13y17.csv", stringsAsFactors = FALSE)
jal2 <- read.csv("fuenteAlumnos/Jalisco12y17.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
mex1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/mexLoc.csv", stringsAsFactors = FALSE)
hgo1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/hgoLoc.csv", stringsAsFactors = FALSE)
jal1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/jalLoc.csv", stringsAsFactors = FALSE)

head(mex2)
dim(mex2)
dim(mex1)

# mexico
colnames(mex1)
mex1 <- mex1[,c("edon","seccion","munn","escenario3")]
colnames(mex1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Claudia
colnames(mex2)
mex2 <- mex2[,c("seccion","munn","Distrito12")]
mex2$munn <- NULL
colnames(mex2) <- c("seccion","disn2012") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correctamente

# fusiona siguientes dos columnas
mex <- merge(x = mex1, y = mex2, by = "seccion", all = TRUE)

dim(mex)
dim(mex1)
dim(mex2)

head(mex)

write.csv(mex, file = "mexLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz

# hidalgo
colnames(hgo1)
hgo1 <- hgo1[,c("edon","seccion","munn","escenario3_cg")]
colnames(hgo1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Claudia
colnames(hgo2)
hgo2 <- hgo2[,c("seccion","munn","Distrito13")]
colnames(hgo2) <- c("seccion","munn","disn2013") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correcta mente
hgo2$munn <- NULL

# fusiona 
hgo <- merge(x = hgo1, y = hgo2, by = "seccion", all = TRUE)

dim(hgo)
dim(hgo1)
dim(hgo2)

head(hgo)

write.csv(hgo, file = "hgoLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz


# jalisco
colnames(jal1)
jal1 <- jal1[,c("edon","seccion","munn","escenario3")]
colnames(jal1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Claudia
colnames(jal2)
jal2 <- jal2[,c("seccion","munn","Distrito12")]
colnames(jal2) <- c("seccion","munn","disn2012") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correcta mente
jal2$munn <- NULL

# fusiona 
jal <- merge(x = jal1, y = jal2, by = "seccion", all = TRUE)

dim(jal)
dim(jal1)
dim(jal2)

head(jal)

write.csv(jal, file = "jalLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz


rm(hgo1,hgo2,jal1,jal2,mex1,mex2) # limpieza
ls()


# calcula dsi
## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))
# hidalgo
d <- read.csv(file = "hgoLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=5)
pob10 <- get2010(edon=5)

head(pob05)
head(pob10)
head(d)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disn2018
father <- d$disn2013
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
# add 2005 pop
d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- dsi$disn2013 <- NULL
head(dsi)
dsi <- dsi[order(dsi$disn2018),]
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_hgo.csv", row.names = FALSE)


# jalisco
d <- read.csv(file = "jalLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=5)
pob10 <- get2010(edon=5)

head(pob05)
head(pob10)
head(d)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disn2018
father <- d$disn2012
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
# add 2005 pop
d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- dsi$disn2012 <- NULL
head(dsi)
dsi <- dsi[order(dsi$disn2018),]
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_jal.csv", row.names = FALSE)


# mexico
d <- read.csv(file = "mexLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=5)
pob10 <- get2010(edon=5)

head(pob05)
head(pob10)
head(d)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disn2018
father <- d$disn2012
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
# add 2005 pop
d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- dsi$disn2012 <- NULL
head(dsi)
dsi <- dsi[order(dsi$disn2018),]
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_mex.csv", row.names = FALSE)





# claudia:
# 1. pareciera que usaste el reporte de eleccion de 2012 o 2013 para reconstruir el mapa seccion-distrito anterior. Busca por favor un resultado de 2015 para mex y jal y repite la fusión de tus datos. Estoy seguro de que caerá mucho el número de secciones faltantes. 
# 2. En el caso de Hidalgo, verifica si hubo elección posterior a 2013 con map antiguo. Quizás puedas hacer lo mismo que en 1.
# 3. Prepara el código para obtener el índice s de cox y katz. 
# *Estoy por hacerlo*

