# df
## READ HISTORICAL MAP (MISSING SECCIONES POSSIBLE)
d <- read.csv(file = "fuenteAlumnos/dfLoc.csv", stringsAsFactors = FALSE)
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

write.csv(eq, file = "dfLoc.csv", row.names = FALSE)




## prepare dsi
## READ HISTORICAL MAP
d <- read.csv(file = "df33Loc.csv", stringsAsFactors = FALSE)
head(d)
# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
son    <- d$disloc2018
father <- d$disloc2015
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
dsi <- dsi[,c("edon","disloc2018","father","dsi")]
head(dsi)
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_df33.csv", row.names = FALSE)
