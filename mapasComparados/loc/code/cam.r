rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

## # Estos son los archivos originales
## cam1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/camLoc.csv", stringsAsFactors = FALSE)

## # campeche
## head(cam1)
## cam1 <- cam1[,c("edon","seccion","munn","escenario3")]
## colnames(cam1) <- c("edon","seccion","munn","disn2018")

## # añade mapa dibujado en 2010, a mano
## cam2 <- read.csv("fuenteAlumnos/camMap2010.csv", stringsAsFactors = FALSE)
## colnames(cam2) <- c("seccion","disn2012")
## head(cam2)

## # fusiona siguientes dos columnas
## cam <- merge(x= cam1, y = cam2, by = "seccion", all = TRUE)

## dim(cam)
## dim(cam1)
## dim(cam2)

## head(cam)

## write.csv(cam, file = "camLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz


## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))

## READ HISTORICAL MAPS
d <- read.csv(file = "tla15Loc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=29)
pob10 <- get2010(edon=29)

head(d)
head(pob05)
head(pob10)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disn2018
father <- d$disn2012
N <- max(son)
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
dsi$disn2012 <- NULL

dsi <- dsi[order(dsi$dsi),]

head(dsi)

write.csv(dsi, file = "simIndex/dist_tla15.csv", row.names = FALSE)

summary(dsi$dsi)


# d19
d <- read.csv(file = "tla19Loc.csv", stringsAsFactors = FALSE)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disn2018
father <- d$disn2012
N <- max(son)
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
dsi$disn2012 <- NULL

dsi <- dsi[order(dsi$dsi),]

head(dsi)

write.csv(dsi, file = "simIndex/dist_tla19.csv", row.names = FALSE)

summary(dsi$dsi)


## tla15 <- read.csv(file = file, stringsAsFactors = FALSE)
## #
## tla15 <- tla15[, c("seccion","munn","escenario3")]
## colnames(tla15) <- c("seccion","munn","disn2018.15")
## tla15$munn <- NULL
## #
## tla <- merge(x= tla, y = tla15, by = "seccion", all = TRUE)
## colnames(tla) <- c("seccion", "edon", "munn", "disn2018.19", "distrito12", "disn2018")
## head(tla)
## #
## write.csv(tla, file = "tlaLoc.csv", row.names = FALSE)


## tabasco
d <- read.csv(file = "tabLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=27)
pob10 <- get2010(edon=27)

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

write.csv(dsi, file = "simIndex/dist_tab.csv", row.names = FALSE)

summary(dsi$dsi)



## camhoacan
d <- read.csv(file = "camLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=16)
pob10 <- get2010(edon=16)

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
d$pob05 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

# export districts object
dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- NULL
dsi$disn2012 <- NULL

dsi <- dsi[order(dsi$dsi),]

head(dsi)

write.csv(dsi, file = "simIndex/dist_cam.csv", row.names = FALSE)

summary(dsi$dsi)
