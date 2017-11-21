######################################################################################
## Script duplicates eqSecc master file to generate seccion-to-federalDistrict maps ##
## (1979, 1997, 2006, 2018) that will be manipulated to re-asign missing secciones. ##
##                                                                                  ##
## Prepared by Eric Magar                                                           ##
## 11/11/2017                                                                       ##
## email emagar at gmail dot com                                                    ##
######################################################################################
dd1 <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/"
dd2 <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/"

eqS <- read.csv(paste(dd1, "tablaEquivalenciasSeccionalesDesde1994.csv", sep = ""), stringsAsFactors = FALSE)
mpf <- read.csv(paste(dd2, "eumFed.csv", sep = ""), stringsAsFactors = FALSE)                  # old version
mg <- merge(x = eqS, y = mpf, by = c("edon","seccion"), all = TRUE)

# inspect
dim(eqS)
dim(mpf)
dim(mg)
head(mg)
table(is.na(eqS$munn), useNA = "ifany")
table(mg$munn.x==mg$munn.y, useNA = "ifany")
table(mg$dis2018.x==mg$dis2018.y, useNA = "ifany")

# Develop bloc here to fill missing secciones as in eqPrep.r (or mainScript.r in replication code)

# Subset and export new object
sel <- c("ord", "edon", "edo", "seccion", "munn", "dis1994", "dis2003", "dis2015", "dis2018") # select late versions of each map to reflect seccion changes
mpf2 <- eqS[, sel]
colnames(mpf2) <- c("ord", "edon", "edo", "seccion", "munn", "dis1979", "dis1997", "dis2006", "dis2018") # rename to actial map names
write.csv(mpf2, file = paste(dd2, "eumFed.csv", sep = ""), row.names = FALSE)



