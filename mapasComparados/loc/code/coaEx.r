rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
setwd(dd)

## ## PREPARES HISTORICAL MAP
## ## u <- url("http://ericmagar.com/data/redistrict/subnat/coa/coaDisn14toDisn17.csv")
## ## d <- read.csv(file = u, stringsAsFactors = FALSE)
## d <- read.csv(file = "fuenteAlumnos/eric.coaLoc.csv", stringsAsFactors = FALSE)
## colnames(d) <- c("seccion","munn","mun","disn2014","disn2017")
## head(d)
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
## write.csv(d, file = "coaLoc.csv", row.names = FALSE)

## get functions to include population
source(paste(dd, "code/getPop.r", sep = ""))

## READ HISTORICAL MAP
d <- read.csv(file = "coaLoc.csv", stringsAsFactors = FALSE)

pob05 <- get2005(edon=5)
pob10 <- get2010(edon=5)

head(d)
head(pob05)
head(pob10)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
d$father14 <- NA
d$dsi <- 0
for (i in 1:16){
    #i <- 16 # debug
    sel.n <- which(d$disn17==i)                  # secciones in new district
    tmp <- table(d$disn14[sel.n])
    target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
    d$father14[sel.n] <- target
    sel.f <- which(d$disn14==target) # secciones in father district
    sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
    d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}

dsi <- d[duplicated(d$disn17)==FALSE, c("disn17","father14","dsi")]
dsi <- dsi[order(dsi$disn17),]
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




# añade población
dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/"
load(paste(dd, "votPobDis0018.RData", sep = ""))
ls()
names(votPobDis0018$pob.distMap2015p3)
