rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

## # Estos son los archivos que circuló daniel
## bcs2 <- read.csv("fuenteAlumnos/dani.bcs_dsi.csv", stringsAsFactors = FALSE)
## dgo2 <- read.csv("fuenteAlumnos/dani.dgo_dsi.csv", stringsAsFactors = FALSE)
## ver2 <- read.csv("fuenteAlumnos/dani.ver.csv", stringsAsFactors = FALSE)
## 
## # duplica para exportar dsi
## bcs3 <- bcs2
## dgo3 <- dgo2
## ver3 <- ver2
## 
## # Estos son los archivos originales
## bcs1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/bcsLoc.csv", stringsAsFactors = FALSE)
## dgo1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/dgoLoc.csv", stringsAsFactors = FALSE)
## ver1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/verLoc.csv", stringsAsFactors = FALSE)
## 
## head(bcs2)
## dim(bcs2)
## dim(bcs1)
## 
## # bcs
## colnames(bcs1)
## bcs1 <- bcs1[,c("edon","seccion","munn","escenario3")]
## colnames(bcs1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Daniel
## colnames(bcs2)
## bcs2$father <- bcs2$dsi <- NULL
## #colnames(bcs2) <- c("seccion","disn2012") # daniel: investiga el año electoral inaugural del mapa abandonado para nombrarlo correctamente
## #bcs2 <- bcs2[,c("seccion","disn2012")]
## 
## # fusiona
## bcs <- merge(x = bcs1, y = bcs2, by = "seccion", all = TRUE)
## # verifica integridad del merge de Daniel
## table(bcs$disn2018==bcs$dist_new)
## bcs$dist_new <- NULL
## 
## dim(bcs)
## dim(bcs1)
## dim(bcs2)
## 
## head(bcs)
## 
## #write.csv(bcs, file = "bcsLoc.csv", row.names = FALSE) # Daniel: usa éste para sacar el insice s de cox y katz
## 
## # dgo
## colnames(dgo1)
## dgo1 <- dgo1[,c("edon","seccion","munn","escenario3")]
## colnames(dgo1) <- c("edon","seccion","munn","disn2018")
## 
## # cambia nombres en datos de Daniel
## colnames(dgo2)
## dgo2$father <- dgo2$dsi <- NULL
## #colnames(dgo2) <- c("seccion","disn2012") # daniel: investiga el año electoral inaugural del mapa abandonado para nombrarlo correctamente
## #dgo2 <- dgo2[,c("seccion","disn2012")]
## 
## # fusiona
## dgo <- merge(x = dgo1, y = dgo2, by = "seccion", all = TRUE)
## # verifica integridad del merge de Daniel
## table(dgo$disn2018==dgo$dist_new)
## sel <- which(dgo$disn2018!=dgo$dist_new); dgo[sel,] # hay un dato perdido... lo tomaré de dgo1
## dgo$dist_new <- NULL
## 
## dim(dgo)
## dim(dgo1)
## dim(dgo2)
## 
## head(dgo)
## 
## #write.csv(dgo, file = "dgoLoc.csv", row.names = FALSE) # Daniel: usa éste para sacar el insice s de cox y katz
## 
## 
## 
## # ver
## colnames(ver1)
## ver1 <- ver1[,c("edon","seccion","munn","escenario3")]
## colnames(ver1) <- c("edon","seccion","munn","disn2018")
## ver1 <- ver1[order(ver1$seccion),]
## 
## # cambia nombres en datos de Daniel
## colnames(ver2)
## head(ver1)
## head(ver2)
## colnames(ver2) <- c("dist_new","seccion","disn2007")
## ver2 <- ver2[order(ver2$seccion),]
## 
## # fusiona
## ver <- merge(x = ver1, y = ver2, by = "seccion", all = TRUE)
## # verifica integridad del merge de Daniel
## table(ver$disn2018==ver$dist_new, useNA = "ifany")
## ###############################
## # daniel: parece que ya quedó #
## ###############################
## ver$dist_new <- NULL
## 
## 
## dim(ver)
## dim(ver1)
## dim(ver2)
## 
## head(ver)
## 
## write.csv(ver, file = "verLoc.csv", row.names = FALSE) # Daniel: usa éste para sacar el insice s de cox y katz
## 
## 
## # prepara/exporta dsi
## bcs3 <- bcs3[duplicated(bcs3$dist_new)==FALSE, c("dist_new", "father", "dsi")]
## bcs3 <- bcs3[order(bcs3$dsi),]
## colnames(bcs3) <- c("disn2018","father","dsi")
## dgo3 <- dgo3[-is.na(dgo3$father),] # drops one NA
## dgo3 <- dgo3[duplicated(dgo3$dist_new)==FALSE, c("dist_new", "father", "dsi")]
## dgo3 <- dgo3[order(dgo3$dsi),]
## colnames(dgo3) <- c("disn2018","father","dsi")
## 
## write.csv(bcs3, file = "simIndex/dist_bcs.csv", row.names = FALSE)
## write.csv(dgo3, file = "simIndex/dist_dgo.csv", row.names = FALSE)
## 
## # compute ver's dsi
## d <- read.csv(file = "verLoc.csv", stringsAsFactors = FALSE)
## son    <- d$disn2018
## father <- d$disn2007
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
## # add 2005 pop
## d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
## d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
## d$ptot <- NULL
## # add 2010 pop
## d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
## d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
## d$ptot <- NULL
## 
## dsi <- d[duplicated(son)==FALSE,]
## dsi$seccion <- dsi$munn <- dsi$disn2007 <- NULL
## head(dsi)
## dsi <- dsi[order(dsi$dsi),]
## 
## write.csv(dsi, file = "simIndex/dist_ver.csv", row.names = FALSE)

## bcs
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/bcsLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2005", new="disloc2005", what=d)
d <- rename.col(old="disn2011", new="disloc2011", what=d)
d <- rename.col(old="disn2018", new="disloc2017", what=d)
#

###################################################################################
## NOTE:                                                                         ##
## open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   ##
## will generate new eq object with full map (incl. state and federal districts) ##
###################################################################################

write.csv(eq, file = "bcsLoc.csv", row.names = FALSE)

# dgo
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/dgoLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2005", new="disloc2005", what=d)
d <- rename.col(old="disn2011", new="disloc2011", what=d)
d <- rename.col(old="disn2017", new="disloc2017", what=d)
#

###################################################################################
## NOTE:                                                                         ##
## open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   ##
## will generate new eq object with full map (incl. state and federal districts) ##
###################################################################################

write.csv(eq, file = "dgoLoc.csv", row.names = FALSE)

## ver
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/verLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2007", new="disloc2007", what=d)
d <- rename.col(old="disn2018", new="disloc2018", what=d)
#

###################################################################################
## NOTE:                                                                         ##
## open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   ##
## will generate new eq object with full map (incl. state and federal districts) ##
###################################################################################

write.csv(eq, file = "verLoc.csv", row.names = FALSE)





