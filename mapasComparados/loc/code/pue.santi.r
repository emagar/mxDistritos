rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

## # Estos son los archivos que circuló santiago
## pue2 <- read.csv("fuenteAlumnos/santiago.puebla2015y2017.csv", stringsAsFactors = FALSE)
## 
## # Estos son los archivos originales
## pue1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/pueLoc.csv", stringsAsFactors = FALSE)
## 
## # Esto lo que bajé para 2013
## pue13 <- read.csv("fuenteAlumnos/pue2013Loc.csv", stringsAsFactors = FALSE)
## pue13 <- pue13[duplicated(pue13$seccion)==FALSE,]
## pue13$cas <- NULL;
## colnames(pue13) <- c("disn2013","mun","seccion","lisnom13")
## 
## head(pue13)
## head(pue2)
## dim(pue13)
## dim(pue2)
## dim(pue1)
## 
## # puebla
## colnames(pue1)
## pue1 <- pue1[,c("edon","seccion","munn","escenario3")]
## colnames(pue1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Santi
## colnames(pue2)
## pue2$seccion <- pue2$seccion - 210000
## table(pue2$Dtto_17==pue2$Dtto_15, useNA = "ifany")
## pue2$Dtto_17 <- NULL
## colnames(pue2) <- c("seccion","disn2015") # santi: investiga el año electoral inaugural del mapa abandonado para nombrarlo correctamente
## 
## # fusiona siguientes dos columnas
## pue <- merge(x = pue1, y = pue2, by = "seccion", all = TRUE)
## 
## dim(pue)
## 
## # 2013
## pue <- merge(x = pue, y = pue13, by = "seccion", all = TRUE)
## 
## dim(pue)
## dim(pue1)
## dim(pue13)
## head(pue)
## 
## table(pue$disn2015, pue$disn2018, useNA = "ifany") # disn mismatch
## pue$disn2018 <- pue$disn2015; pue$disn2015 <- NULL # me quedo con el núm de Santi
## 
## sel <- which(is.na(pue$disn2018)==TRUE)
## pue[sel,]
## pue$edon[sel] <- 21
## pue$disn2018[sel] <- 0
## pue$munn[pue$seccion==1250] <- 115
## pue$munn[pue$seccion==1407] <- 115
## pue$munn[pue$seccion==1664] <- 120
## 
## write.csv(pue, file = "fuenteAlumnos/pueLoc.csv", row.names = FALSE) # Santi: usa éste para sacar el insice s de cox y katz



## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/pueLoc.csv", stringsAsFactors = FALSE)
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## head(d)
## d <- rename.col(old="disn2013", new="disloc2013", what=d)
## d <- rename.col(old="disn2018", new="disloc2018", what=d)
## 
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "pueLoc.csv", row.names = FALSE)



# compute pue's dsi
d <- read.csv(file = "pueLoc.csv", stringsAsFactors = FALSE)
head(d)
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
summary(d$dsi)

dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$mun <- dsi$munn <- dsi$disfed1994 <- dsi$disfed1997 <- dsi$disfed2006 <- dsi$disfed2018 <- NULL
dsi$disloc2013 <- NULL
head(dsi)
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_pue.csv", row.names = FALSE)





# add 2005 pop

d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- dsi$disn2007 <- NULL
head(dsi)
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_pue.csv", row.names = FALSE)

