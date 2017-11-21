rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
setwd(dd)

## PREPARES HISTORICAL MAP
## u <- url("http://ericmagar.com/data/redistrict/subnat/coa/coaDisn14toDisn17.csv")
## d <- read.csv(file = u, stringsAsFactors = FALSE)
## d <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/coaLoc.csv", stringsAsFactors = FALSE)
## d <- d[,c("edon","seccion","munn","escenario3","criterio8","cab")]
## table(d$escenario3, d$criterio8) # OJO: SE USO CRITERIO 8; distritos idénticos a json, disn mismatch. ¿Quizás sólo cambiaron las cabeceras? ¿Por qué renumerías via criterio 8 sin cambio de límites?
## d$escenario3 <- NULL # conservo disn que obtuve en http://cartografia.ife.org.mx//descargas/distritacion2017/local/05/D05.pdf
## colnames(d) <- c("edon","seccion","munn","disn2017","cab")
## head(d)
## 
## # this has final disn and 2014 map
## d <- read.csv(file = "fuenteAlumnos/eric.coaLoc.csv", stringsAsFactors = FALSE)
## colnames(d) <- c("seccion","munn","mun","disn2014","disn2017")
## 
## # add 2008 and 2011 maps
## d08 <- read.csv(file = "fuenteAlumnos/coa2008dlca.csv", stringsAsFactors = FALSE)
## ## d11 <- read.csv(file = "fuenteAlumnos/coa2011dlca.csv", stringsAsFactors = FALSE)
## d08 <- d08[, c("mun","disn","seccion","lisnom")]
## ## d11 <- d11[, c("mun","disn","seccion")]
## colnames(d08) <- c("mun","disn2008","seccion","lisnom08")
## ## colnames(d11) <- c("mun","disn2011","seccion")
## d08 <- d08[duplicated(d08$seccion)==FALSE,]
## ## d11 <- d11[duplicated(d11$seccion)==FALSE,]
## d <- merge(x = d, y = d08, by = "seccion", all = TRUE)
## ## d <- merge(x = d, y = d11, by = "seccion", all = TRUE)
## ##    
## head(d)
## # fill missing mun and munn
## sel <- which(is.na(d$munn))
## d$mun.x[sel] <- d$mun.y[sel]
## sel <- which(is.na(d$mun.y))
## d$mun.y[sel] <- d$mun.x[sel]
## # fix missin munn by hand
## sel <- which(is.na(d$munn))
## d.ss <- d[sel,] # subset
## d.ss$munn[grep("Acuña", d.ss$mun.x)] <- 2
## d.ss$munn[grep("Castaños", d.ss$mun.x)] <- 6
## d.ss$munn[grep("Cuatrociénegas", d.ss$mun.x)] <- 7
## d.ss$munn[grep("Nava", d.ss$mun.x)] <- 22
## d.ss$munn[grep("Ocampo", d.ss$mun.x)] <- 23
## d.ss$munn[grep("Piedras Negras", d.ss$mun.x)] <- 25
## d.ss$munn[grep("Saltillo", d.ss$mun.x)] <- 30
## d.ss$munn[grep("Sierra Mojada", d.ss$mun.x)] <- 34
## d.ss$munn[grep("Torreón", d.ss$mun.x)] <- 35
## d[sel,] <- d.ss   # return subset
## # drop redundant cols
## d$mun.x <- d$mun.y; d$mun.y <- NULL
## colnames(d)[which(colnames(d)=="mun.x")] <- "mun"
## # rename maps
## colnames(d)[which(colnames(d)=="disn2014")] <- "disn2011" # districts created in 2011
## colnames(d)[which(colnames(d)=="disn2008")] <- "disn2005" # districts used in 2005, when created unknown
## d <- d[, c("seccion","munn","mun","disn2005","disn2011","disn2017","lisnom08")] # re-order columns
## head(d)
## # fix secciones missing in some map but not others
## ## 2011 map not needed, will not read it bc is same as 2014
## #which(is.na(d$munn)==TRUE) # none
## for (i in 1:max(d$munn)){
##     #i <- 2 # debug
##     sel <- which(d$munn==i)
##     d.ss <- d[sel,] # subset
##     sel.ss <- which(is.na(d.ss$disn2005)==TRUE | is.na(d.ss$disn2011)==TRUE | is.na(d.ss$disn2017)==TRUE) # secciones missing in one+ maps
##     if (length(sel.ss)==0) next # skip if no NAs
##     # 2005
##     tmp <- unique(d.ss$disn2005[!is.na(d.ss$disn2005)]) # municipios in district other than NAs
##     if (length(tmp)==1) {d.ss$disn2005 <- tmp} # skip if municipio split in several districts else replace NAs with single value
##     # 2011
##     tmp <- unique(d.ss$disn2011[!is.na(d.ss$disn2011)]) # municipios in district other than NAs
##     if (length(tmp)==1) {d.ss$disn2011 <- tmp} # skip if municipio split in several districts else replace NAs with single value
##     # 2017
##     tmp <- unique(d.ss$disn2017[!is.na(d.ss$disn2017)]) # municipios in district other than NAs
##     if (length(tmp)==1) {d.ss$disn2017 <- tmp} # skip if municipio split in several districts else replace NAs with single value
##     #
##     d[sel,] <- d.ss # return subset
## }
## rm(sel.ss, d.ss, tmp)
## #
## write.csv(d, file = "fuenteAlumnos/coaLoc.csv", row.names = FALSE)




## ## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
## d <- read.csv(file = "fuenteAlumnos/coaLoc.csv", stringsAsFactors = FALSE)
## 
## # handy function to rename one data.frame's column
## rename.col <- function(old=NA, new=NA, what=NA){
##     old <- old; new <- new; what <- what;
##     colnames(what)[which(colnames(what)==old)] <- new
##     return(what)
## }
## d <- rename.col(old="disn2005", new="disloc2005", what=d)
## d <- rename.col(old="disn2011", new="disloc2011", what=d)
## d <- rename.col(old="disn2017", new="disloc2017", what=d)
## #
## # ---> NOTE:                                                                         <--- #
## # ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
## # ---> will generate new eq object with full map (incl. state and federal districts) <--- #
## 
## write.csv(eq, file = "coaLoc.csv", row.names = FALSE)



## READ HISTORICAL MAPS
d <- read.csv(file = "coaLoc.csv", stringsAsFactors = FALSE)
head(d)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disloc2017
father <- d$disloc2011
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
dsi <- dsi[,c("edon","disloc2017","father","dsi")]
head(dsi)
dsi <- dsi[order(dsi$disloc2017),]
dsi$cab2017 <- c("Acuña", "Piedras Negras", "Sabinas", "San Pedro", "Monclova", "Frontera", "Matamoros", "Torreón", "Torreón", "Torreón", "Torreón", "Ramos Arizpe", "Saltillo", "Saltillo", "Saltillo", "Saltillo")
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_coa.csv", row.names = FALSE)

# dsi seen from parent perspective
# new district's "father" and district similarity index, cf. Cox & Katz
d$son17 <- NA
d$dsi <- 0
for (i in 1:16){
    #i <- 16 # debug
    sel.o <- which(d$disn14==i)                  # secciones in original district
    tmp <- table(d$disn17[sel.o])
    target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
    d$son2017[sel.o] <- target
    sel.s <- which(d$disn17==target) # secciones in son district
    sel.c <- intersect(sel.o, sel.s) # secciones common to original and son districts
    d$dsi[sel.o] <- round( length(sel.c) / (length(sel.o) + length(sel.s) - length(sel.c)) , 3 )
}

dsi <- d[duplicated(d$disn14)==FALSE, c("disn14","son2017","dsi")]
dsi <- dsi[order(dsi$disn14),]
dsi$cab14 <- c("Saltillo", "Saltillo", "Saltillo", "Saltillo", "Ramos Arizpe", "Torreón", "Torreón", "Torreón", "Torreón", "San Pedro", "Frontera", "Monclova", "Múzquiz", "Sabinas", "Acuña", "Piedras Negras")
dsi <- dsi[order(dsi$dsi),]

summary(dsi$dsi)



# add 2005 pop
d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL





## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))
pob05 <- get2005(edon=5)
pob10 <- get2010(edon=5)

head(pob05)
head(pob10)
head(d)





# añade población
dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/"
load(paste(dd, "votPobDis0018.RData", sep = ""))
ls()
names(votPobDis0018$pob.distMap2015p3)
