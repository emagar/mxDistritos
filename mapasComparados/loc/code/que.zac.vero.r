rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# si los encuentra, lee datos de disco 
tmp <- dir()
sel <- grep(pattern = "(que|san|zac)Loc.csv", x = tmp)
if (length(sel)>0) {
    mic <- read.csv(file = tmp[sel[1]], stringsAsFactors = FALSE);
    zac <- read.csv(file = tmp[sel[2]], stringsAsFactors = FALSE);
#    san <- read.csv(file = tmp[sel[3]], stringsAsFactors = FALSE);
}

# Estos son los archivos que circuló
que2 <- read.csv("fuenteAlumnos/veronica.QRO.csv", stringsAsFactors = FALSE)
zac2 <- read.csv("fuenteAlumnos/veronica.ZAC.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
que1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/queLoc.csv", stringsAsFactors = FALSE)
zac1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/zacLoc.csv", stringsAsFactors = FALSE)

head(que2)
dim(que2)
dim(que1)

# queretaro
que1 <- que1[,c("edon","seccion","munn","escenario3")]
colnames(que1) <- c("edon","seccion","munn","disn2018")

# verifica integridad de las primeras dos columnas de los datos de zabel
que21 <- que2[,c("Sección_17","Dtto_17")]
tmp <- merge(x= que1, y = que21, by.x = "seccion", by.y = "Sección_17", all = TRUE)
nrow(tmp)==nrow(que21) # must be TRUE
rm(que21)

# fusiona siguientes dos columnas
que22 <- que2[,c("Sección_12","Dtto_12")]
colnames(que22) <- c("seccion","disn2012")
que22 <- que22[is.na(que22$seccion)==FALSE,] # quita NAs

que <- merge(x= que1, y = que22, by = "seccion", all = TRUE)

dim(que)
dim(que1)
dim(que22)

que$disn2012 <- as.numeric(as.roman(que$disn2012)) # quita numero romano

head(que)

#write.csv(que, file = "queLoc.csv", row.names = FALSE) # Veronica: usa éste para sacar el insice s de cox y katz

# zacatecas
colnames(zac1)
zac1 <- zac1[,c("edon","seccion","munn","escenario3")]
colnames(zac1) <- c("edon","seccion","munn","disn2018")

# verifica integridad de las primeras dos columnas de los datos de zabel
colnames(zac2)
zac21 <- zac2[,c("Sección_17","Dtto_17")]
tmp <- merge(x= zac1, y = zac21, by.x = "seccion", by.y = "Sección_17", all = TRUE)
nrow(tmp)==nrow(zac21) # must be TRUE
rm(zac21)

# fusiona siguientes dos columnas
zac22 <- zac2[,c("Sección_13","Dtto_13")]
colnames(zac22) <- c("seccion","disn2013")
zac22 <- zac22[is.na(zac22$seccion)==FALSE,] # quita NAs

zac <- merge(x= zac1, y = zac22, by = "seccion", all = TRUE)

dim(zac)
dim(zac1)
dim(zac22)

zac$disn2013 <- as.numeric(as.roman(zac$disn2013)) # quita numero romano

head(zac)

write.csv(zac, file = "zacLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

rm(que1,que2,que22,zac1,zac2,zac22,tmp) # limpieza
ls()




## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))

## READ HISTORICAL MAPS
## zac
d <- read.csv(file = "zacLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=32)
pob10 <- get2010(edon=32)

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
    #i <- 3 # debug
    sel.n <- which(son==i)                  # secciones in new district
    tmp <- table(father[sel.n])
    target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
    d$father[sel.n] <- target
    sel.f <- which(father==target) # secciones in father district
    sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
    d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
# add 2005 pop
d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all = TRUE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all = TRUE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

# export districts object
dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- NULL
dsi$disn2013 <- NULL

dsi <- dsi[order(dsi$dsi),]

head(dsi)

write.csv(dsi, file = "simIndex/dist_zac.csv", row.names = FALSE)

summary(dsi$dsi)



## READ HISTORICAL MAPS
## que
d <- read.csv(file = "queLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=22)
pob10 <- get2010(edon=22)

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
    #i <- i+1; i # debug
    sel.n <- which(son==i)                  # secciones in new district
    tmp <- table(father[sel.n])
    target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # first match in case of tie 
    d$father[sel.n] <- target
    sel.f <- which(father==target) # secciones in father district
    sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
    d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
# add 2005 pop
d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all = TRUE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all = TRUE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

# export districts object
dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- NULL
dsi$disn2012 <- NULL

dsi <- dsi[order(dsi$dsi),]

head(dsi)

write.csv(dsi, file = "simIndex/dist_que.csv", row.names = FALSE)

summary(dsi$dsi)







