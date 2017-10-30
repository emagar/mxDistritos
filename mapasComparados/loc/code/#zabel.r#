rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

## tmp <- dir()
## sel <- grep(pattern = "(mic|tab|tla)Loc.csv", x = tmp)
## if (length(sel)>0) {
##     mic <- read.csv(file = tmp[sel[1]], stringsAsFactors = FALSE);
##     tab <- read.csv(file = tmp[sel[2]], stringsAsFactors = FALSE);
##     tla <- read.csv(file = tmp[sel[3]], stringsAsFactors = FALSE);
## }

## # Estos son los archivos que circuló Zabel
## mic2 <- read.csv("fuenteAlumnos/michoacan.redis.csv", stringsAsFactors = FALSE)
## tab2 <- read.csv("fuenteAlumnos/tabasco.redis.csv", stringsAsFactors = FALSE)
## tla2 <- read.csv("fuenteAlumnos/tlaxcala.redis.csv", stringsAsFactors = FALSE)

## # Estos son los archivos originales
## mic1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/micLoc.csv", stringsAsFactors = FALSE)
## tab1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/tabLoc.csv", stringsAsFactors = FALSE)
## tla19 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/tla19Loc.csv", stringsAsFactors = FALSE)
## tla15 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/tla15Loc.csv", stringsAsFactors = FALSE)

## head(mic2)
## dim(mic2)
## dim(mic1)

## # michoacan
## mic1 <- mic1[,c("edon","seccion","munn","escenario3")]
## colnames(mic1) <- c("edon","seccion","munn","disn2018")

## # verifica integridad de las primeras dos columnas de los datos de zabel
## mic21 <- mic2[,c("seccion17","dist17")]
## tmp <- merge(x= mic1, y = mic21, by.x = "seccion", by.y = "seccion17", all = TRUE)
## nrow(tmp)==nrow(mic21) # must be TRUE
## rm(mic21)

## # fusiona siguientes dos columnas
## mic22 <- mic2[,c("seccion12","distrito12")]
## colnames(mic22) <- c("seccion12","disn2012")
## mic22 <- mic22[is.na(mic22$seccion12)==FALSE,] # quita NAs

## mic <- merge(x= mic1, y = mic22, by.x = "seccion", by.y = "seccion12", all = TRUE)

## dim(mic)
## dim(mic1)
## dim(mic22)

## head(mic)

## write.csv(mic, file = "micLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

## #tabasco
## colnames(tab1)
## tab1 <- tab1[,c("edon","seccion","munn","escenario3")]
## colnames(tab1) <- c("edon","seccion","munn","disn2018")

## # verifica integridad de las primeras dos columnas de los datos de zabel
## colnames(tab2)
## tab21 <- tab2[,c("seccion17","distrito17")]
## tmp <- merge(x= tab1, y = tab21, by.x = "seccion", by.y = "seccion17", all = TRUE)
## nrow(tmp)==nrow(tab21) # must be TRUE
## rm(tab21)

## # fusiona siguientes dos columnas
## tab22 <- tab2[,c("seccion12","distrito12")]
## colnames(tab22) <- c("seccion12","disn2012")
## tab22 <- tab22[is.na(tab22$seccion12)==FALSE,] # quita NAs

## tab <- merge(x= tab1, y = tab22, by.x = "seccion", by.y = "seccion12", all = TRUE)
## ## # missing secciones ... unfinished
## ## tmp <- read.csv("../../equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
## ## tmp <- tmp[which(tmp$edon==27),]
## ## sel <- which(is.na(tab$munn)==TRUE)
## ## tab$seccion[sel]
## ## sel.tmp <- which(tmp$seccion %in% tab$seccion[sel])
## ## tmp[sel.tmp,]

## dim(tab)
## dim(tab1)
## dim(tab22)

## head(tab)

## write.csv(tab, file = "tabLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

## # tlaxcala
## colnames(tla19)
## tla19 <- tla19[,c("edon","seccion","munn","escenario3")]
## colnames(tla19) <- c("edon","seccion","munn","disn2018")
## colnames(tla15)
## tla15 <- tla15[,c("edon","seccion","munn","escenario3")]
## colnames(tla15) <- c("edon","seccion","munn","disn2018")

## # verifica integridad de las primeras dos columnas de los datos de zabel
## colnames(tla2)
## max(tla2$distrito17)
## tla21 <- tla2[,c("seccion17","distrito17")]
## tmp <- merge(x= tla15, y = tla21, by.x = "seccion", by.y = "seccion17", all = TRUE)
## nrow(tmp)==nrow(tla21) # must be TRUE
## rm(tla21)

## # fusiona siguientes dos columnas
## tla22 <- tla2[,c("seccion12","distrito12")]
## colnames(tla22) <- c("seccion12","disn2012")
## tla22 <- tla22[is.na(tla22$seccion12)==FALSE,] # quita NAs

## # version 19 distritos
## tla <- merge(x= tla19, y = tla22, by.x = "seccion", by.y = "seccion12", all = TRUE)

## dim(tla)
## dim(tla19)
## dim(tla22)

## head(tla)

## write.csv(tla, file = "tla19Loc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

## # version 15 distritos
## tla <- merge(x= tla15, y = tla22, by.x = "seccion", by.y = "seccion12", all = TRUE)

## dim(tla)
## dim(tla19)
## dim(tla22)

## head(tla)

## write.csv(tla, file = "tla15Loc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz



## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))

## READ HISTORICAL MAPS
## tlaxcala
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



## michoacan
d <- read.csv(file = "micLoc.csv", stringsAsFactors = FALSE)

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

write.csv(dsi, file = "simIndex/dist_mic.csv", row.names = FALSE)

summary(dsi$dsi)
