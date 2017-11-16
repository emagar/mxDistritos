rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló humberto
ags2 <- read.csv("fuenteAlumnos/agsLoc.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
ags1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/agsLoc.csv", stringsAsFactors = FALSE)

head(ags2)
dim(ags2)
dim(ags1)



# ags
colnames(ags1)
ags1 <- ags1[,c("edon","seccion","munn","escenario3")]
colnames(ags1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de humberto
head(ags2)
ags2$s <- NULL
colnames(ags2) <- c("edon","seccion","munn","disn2016","disn2013") # necesito año de disnold

# fusiona
ags <- merge(x = ags1, y = ags2, by = "seccion", all = TRUE)

dim(ags)
dim(ags1)
dim(ags2)

head(ags)
table(ags$disn2015, ags$disn2018) # disn mismatch, me quedo con el núm de omar
ags$disn2018 <- ags$disn2015; ags$disn2015 <- NULL

write.csv(ags, file = "fuenteAlumnos/agsLoc.csv", row.names = FALSE) 





# ags
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/agsLoc.csv", stringsAsFactors = FALSE)
head(d) 
# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2013", new="disloc2013", what=d)
d <- rename.col(old="disn2018", new="disloc2018", what=d)
#
# ---> NOTE:                                                                         <--- #
# ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
# ---> will generate new eq object with full map (incl. state and federal districts) <--- #

write.csv(eq, file = "agsLoc.csv", row.names = FALSE)


