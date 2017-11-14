## READ HISTORICAL MAPS (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/bcsLoc.csv", stringsAsFactors = FALSE)
d <- read.csv(file = "fuenteAlumnos/dgoLoc.csv", stringsAsFactors = FALSE)
d <- read.csv(file = "fuenteAlumnos/verLoc.csv", stringsAsFactors = FALSE)

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

# open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors
# will generate new eq object with full map (incl. state and federal districts)
# export
write.csv(eq, file = "bcsLoc.csv", row.names = FALSE)
write.csv(eq, file = "dgoLoc.csv", row.names = FALSE)
write.csv(eq, file = "verLoc.csv", row.names = FALSE)
