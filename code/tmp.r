colnames(tmp)
tmp$seccion
is.na(tmp$orig.dest)
sel <- which(eq$edon==21 & eq$seccion==2659)
is.na(eq$orig.dest[sel])

# cam
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/camLoc.csv", stringsAsFactors = FALSE)
head(d) # dist_old year needed

# handy function to rename one data.frame's column
rename.col <- function(old=NA, new=NA, what=NA){
    old <- old; new <- new; what <- what;
    colnames(what)[which(colnames(what)==old)] <- new
    return(what)
}
d <- rename.col(old="disn2012", new="disloc2012", what=d)
d <- rename.col(old="disn2018", new="disloc2018", what=d)
#
# ---> NOTE:                                                                         <--- #
# ---> open useEqPrep2fillMissSeccionesLocalMaps.r and run manually to spot errors   <--- #
# ---> will generate new eq object with full map (incl. state and federal districts) <--- #

write.csv(eq, file = "camLoc.csv", row.names = FALSE)
