############
#  20 oax  #
############
# 2010 map eric re-did
d <- read.csv(file = "fuenteAlumnos/oaxLoc2010.csv", stringsAsFactors = FALSE)
sel <- which(d$lisnom==""|d$lisnom=="-")
d$lisnom[sel] <- 0; rm(sel)
d$lisnom <- as.numeric(d$lisnom)
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
d$lisnom <- ave(d$lisnom, as.factor(d$seccion), FUN=sum, na.rm=TRUE)
d <- d[duplicated(d$seccion)==FALSE,] # drop redundant  obs
d$ord <- NULL; d$loc <- NULL; d$casilla <- NULL
d$cab2010 <- d$cab; d$cab <- NULL
d$ife <- 20000 + d$ife # as in aymu
dim(d)
d10 <- d; rm(d) #rename
#
# read what claudia prepared
d <- read.csv(file = "fuenteAlumnos/oaxLoc.csv", stringsAsFactors = FALSE)
dim(d)
d <- merge(x = d, y = d10, by = "seccion", all = TRUE)
d <- d[,c("seccion","munn","ife","mun","edon","lisnom","disfed1979","disfed1997","disfed2006","disfed2018","disloc2010","cab2010","disloc2012","disloc2018","nota")]
write.csv(d, file = "oaxLoc.csv", row.names = FALSE)

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


## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))
pob05 <- get2005(edon=5)
pob10 <- get2010(edon=5)
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

## # dsi seen from offspring perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## ## READ HISTORICAL MAPS
## d <- read.csv(file = "coaLoc.csv", stringsAsFactors = FALSE)
## head(d)
## son    <- d$disloc2017
## father <- d$disloc2011
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
## dsi <- d[duplicated(son)==FALSE,]
## dsi <- dsi[,c("edon","disloc2017","father","dsi")]
## head(dsi)
## dsi <- dsi[order(dsi$disloc2017),]
## dsi$cab2017 <- c("Acuña", "Piedras Negras", "Sabinas", "San Pedro", "Monclova", "Frontera", "Matamoros", "Torreón", "Torreón", "Torreón", "Torreón", "Ramos Arizpe", "Saltillo", "Saltillo", "Saltillo", "Saltillo")
## dsi <- dsi[order(dsi$dsi),]
## write.csv(dsi, file = "simIndex/dist_coa.csv", row.names = FALSE)
#
## # dsi seen from parent perspective
## # new district's "father" and district similarity index, cf. Cox & Katz
## d$son17 <- NA
## d$dsi <- 0
## for (i in 1:16){
##     #i <- 16 # debug
##     sel.o <- which(d$disn14==i)                  # secciones in original district
##     tmp <- table(d$disn17[sel.o])
##     target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
##     d$son2017[sel.o] <- target
##     sel.s <- which(d$disn17==target) # secciones in son district
##     sel.c <- intersect(sel.o, sel.s) # secciones common to original and son districts
##     d$dsi[sel.o] <- round( length(sel.c) / (length(sel.o) + length(sel.s) - length(sel.c)) , 3 )
## }
## dsi <- d[duplicated(d$disn14)==FALSE, c("disn14","son2017","dsi")]
## dsi <- dsi[order(dsi$disn14),]
## dsi$cab14 <- c("Saltillo", "Saltillo", "Saltillo", "Saltillo", "Ramos Arizpe", "Torreón", "Torreón", "Torreón", "Torreón", "San Pedro", "Frontera", "Monclova", "Múzquiz", "Sabinas", "Acuña", "Piedras Negras")
## dsi <- dsi[order(dsi$dsi),]
## summary(dsi$dsi)
