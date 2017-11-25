rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

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
d <- read.csv(file = "fuenteAlumnos/nayLoc.csv", stringsAsFactors = FALSE)
head(d) 
colnames(d) <- c("seccion","disn2017","cab","mun","munn","demarc2014","disn2005","demarc2008") # dist_old year needed
d <- d[, c("seccion","cab","mun","munn","disn2005","disn2017","demarc2008","demarc2014")]

# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2005", new="disloc2005", what=d)
d <- rename.col(old="disn2017", new="disloc2017", what=d)
#

###################################################################################
## NOTE:                                                                         ##
## open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   ##
## will generate new eq object with full map (incl. state and federal districts) ##
###################################################################################

table(eq$demarc2008==eq$demarc2014, useNA = "ifany") # no change in demarcaciones since 2008
eq$demarc2014 <- NULL

write.csv(eq, file = "nayLoc.csv", row.names = FALSE)



# prepara/exporta dsi
d <- read.csv(file = "nayLoc.csv", stringsAsFactors = FALSE)
colnames(d)
son    <- d$disloc2017
father <- d$disloc2005
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
dsi <- dsi[, c("disloc2017","father","cab","dsi")]
head(dsi)
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_nay.csv", row.names = FALSE)

