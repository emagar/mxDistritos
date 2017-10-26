## PREPARES HISTORICAL MAP
## u <- url("http://ericmagar.com/data/redistrict/subnat/coa/coaDisn14toDisn17.csv")
## d <- read.csv(file = u, stringsAsFactors = FALSE)
d <- read.csv(file = "fuenteAlumnos/eric.coaLoc.csv", stringsAsFactors = FALSE)
colnames(d) <- c("seccion","munn","mun","disn2014","disn2017")
head(d)
# add 2008 and 2011 maps
d08 <- read.csv(file = "fuenteAlumnos/coa2008dlca.csv", stringsAsFactors = FALSE)
## d11 <- read.csv(file = "fuenteAlumnos/coa2011dlca.csv", stringsAsFactors = FALSE)
d08 <- d08[, c("mun","disn","seccion","lisnom")]
## d11 <- d11[, c("mun","disn","seccion")]
colnames(d08) <- c("mun","disn2008","seccion","lisnom08")
## colnames(d11) <- c("mun","disn2011","seccion")
d08 <- d08[duplicated(d08$seccion)==FALSE,]
## d11 <- d11[duplicated(d11$seccion)==FALSE,]
d <- merge(x = d, y = d08, by = "seccion", all = TRUE)
## d <- merge(x = d, y = d11, by = "seccion", all = TRUE)
##    
head(d)
# fill missing mun and munn
sel <- which(is.na(d$munn))
d$mun.x[sel] <- d$mun.y[sel]
sel <- which(is.na(d$mun.y))
d$mun.y[sel] <- d$mun.x[sel]
# fix missin munn by hand
sel <- which(is.na(d$munn))
d.ss <- d[sel,] # subset
d.ss$munn[grep("Acuña", d.ss$mun.x)] <- 2
d.ss$munn[grep("Castaños", d.ss$mun.x)] <- 6
d.ss$munn[grep("Cuatrociénegas", d.ss$mun.x)] <- 7
d.ss$munn[grep("Nava", d.ss$mun.x)] <- 22
d.ss$munn[grep("Ocampo", d.ss$mun.x)] <- 23
d.ss$munn[grep("Piedras Negras", d.ss$mun.x)] <- 25
d.ss$munn[grep("Saltillo", d.ss$mun.x)] <- 30
d.ss$munn[grep("Sierra Mojada", d.ss$mun.x)] <- 34
d.ss$munn[grep("Torreón", d.ss$mun.x)] <- 35
d[sel,] <- d.ss   # return subset
# drop redundant cols
d$mun.x <- d$mun.y; d$mun.y <- NULL
colnames(d)[which(colnames(d)=="mun.x")] <- "mun"
# rename maps
colnames(d)[which(colnames(d)=="disn2014")] <- "disn2011" # districts created in 2011
colnames(d)[which(colnames(d)=="disn2008")] <- "disn2005" # districts used in 2005, when created unknown
d <- d[, c("seccion","munn","mun","disn2005","disn2011","disn2017","lisnom08")] # re-order columns
head(d)
# fix secciones missing in some map but not others
## 2011 map not needed, will not read it bc is same as 2014
#which(is.na(d$munn)==TRUE) # none
for (i in 1:max(d$munn)){
    #i <- 2 # debug
    sel <- which(d$munn==i)
    d.ss <- d[sel,] # subset
    sel.ss <- which(is.na(d.ss$disn2005)==TRUE | is.na(d.ss$disn2011)==TRUE | is.na(d.ss$disn2017)==TRUE) # secciones missing in one+ maps
    if (length(sel.ss)==0) next # skip if no NAs
    # 2005
    tmp <- unique(d.ss$disn2005[!is.na(d.ss$disn2005)]) # municipios in district other than NAs
    if (length(tmp)==1) {d.ss$disn2005 <- tmp} # skip if municipio split in several districts else replace NAs with single value
    # 2011
    tmp <- unique(d.ss$disn2011[!is.na(d.ss$disn2011)]) # municipios in district other than NAs
    if (length(tmp)==1) {d.ss$disn2011 <- tmp} # skip if municipio split in several districts else replace NAs with single value
    # 2017
    tmp <- unique(d.ss$disn2017[!is.na(d.ss$disn2017)]) # municipios in district other than NAs
    if (length(tmp)==1) {d.ss$disn2017 <- tmp} # skip if municipio split in several districts else replace NAs with single value
    #
    d[sel,] <- d.ss # return subset
}
rm(sel.ss, d.ss, tmp)
#
write.csv(d, file = "coaLoc.csv", row.names = FALSE)
