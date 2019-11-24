rm(list = ls())
                    
path <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ifeRedist2013/"

# get master secciones file to pour new data in
eq <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv")
sel <- which(colnames(eq) %in% c("ord","edo","mun","edosecn","dis1994","dis1997","dis2000","dis2003","dis2006","dis2009","dis2012","dis2015","dis2018","OBSERVACIONES","action","fr.to","orig.dest","when","color","coment"))
eq <- eq[,-sel]
eq[1,]
colnames(eq)[which(colnames(eq)=="dis2013")] <- "e3"
#
# rename
dat <- eq


#################
## escenario 1 ##
#################
e1 <- data.frame() # will receive data
#
for (e in 1:32){
    #e <- 1
    edon <- e # 1:32
    edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
    dd1   <- paste(path, "1erEscenario2013/", edo, sep = "")
    dd1pp <- paste(path, "1erEscenarioComentariosPartidos/", edo, sep = "")
    dd2   <- paste(path, "2oEscenario/", edo, sep = "")
    dd2pp <- paste(path, "2oEscenarioComentariosPartidos/", edo, sep = "")
    dd3   <- paste(path, "3erEscenario/",      sep = "")
    #
    setwd(dd1)
    #
    tmp <- read.csv(file = "escenario.dat.ife", sep = "\t", header = FALSE)
    colnames(tmp) <- c("edon","e1","munn","seccion")
    #
    e1 <- rbind(e1, tmp)
}
#
e1  <- within(e1, ife <- edon*1000 + munn) # add ife municipio code
#
# handle seccion!=0 cases
sel <- which(e1$seccion==0) # select obs where district is reported for munn only
dat <- merge(x = dat, y = e1[-sel,c("edon","seccion","e1")], by = c("edon","seccion"), all = TRUE)
# handle seccion==0 cases, ie. full municipios 
for (s in 1:length(sel)){
    #s <- 1 # debug
    sel.m <- which(dat$ife==e1$ife[sel[s]]) # indices with same municipio number as seccion==0 cases
    dat$e1[sel.m] <- e1$e1[sel[s]]          # plug district number to those indices
}

#################################
## escenario 1 party proposals ##
#################################
e1 <- data.frame() # will receive data
#
for (e in 1:32){
    #e <- 1
    edon <- e # 1:32
    edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
    dd1   <- paste(path, "1erEscenario2013/", edo, sep = "")
    dd1pp <- paste(path, "1erEscenarioComentariosPartidos/", edo, "/Escenarios/", sep = "")
    dd2   <- paste(path, "2oEscenario/", edo, sep = "")
    dd2pp <- paste(path, "2oEscenarioComentariosPartidos/", edo, sep = "")
    dd3   <- paste(path, "3erEscenario/",      sep = "")
    #
    setwd(dd1pp)
    #
    # explore workdir
    files.tmp <- list.files()
    files.tmp <- files.tmp[grep(paste(edo, "[Fed|Loc].+zip", sep = ""), files.tmp)]     # keep only zipped in listing
    tmp <- grep("19", files.tmp)                    # locate first scenario...
    files.tmp <- files.tmp[-tmp]                    # and drop it (already added)
    files.tmp
    #
    i <- 1
    ## # file contents
    ## unzip(tmp, list = TRUE)
    unzip(files.tmp[i])
    name.tmp <- unzip(files.tmp[i], list = TRUE)$Name # pick unzipped file's name for dataframe's column

    tmp <- read.csv(file = "escenario.dat.ife", sep = "\t", header = FALSE)
    colnames(tmp) <- c("edon","e1","munn","seccion")
    #
    e1 <- rbind(e1, tmp)
}
#
e1  <- within(e1, ife <- edon*1000 + munn) # add ife municipio code
#
# handle seccion!=0 cases
sel <- which(e1$seccion==0) # select obs where district is reported for munn only
dat <- merge(x = dat, y = e1[-sel,c("edon","seccion","e1")], by = c("edon","seccion"), all = TRUE)
# handle seccion==0 cases, ie. full municipios 
for (s in 1:length(sel)){
    #s <- 1 # debug
    sel.m <- which(dat$ife==e1$ife[sel[s]]) # indices with same municipio number as seccion==0 cases
    dat$e1[sel.m] <- e1$e1[sel[s]]          # plug district number to those indices
}




dat[1:12,]
dim(dat)


# handle full municipios 
sel <- which(output$seccion==0) # select obs where district is reported for munn only
    for (s in 1:length(sel)){
        #s <- 2 # debug
        sel.m <- which(output$munn==output$munn[s])
        output$dis62.20.157[sel.m] <- output$dis62.20.157[s]
        output$dis62.20.37 [sel.m] <- output$dis62.20.37 [s]
        output$dis62.20.39 [sel.m] <- output$dis62.20.39 [s]
        output$dis62.20.38 [sel.m] <- output$dis62.20.38 [s]
        output$dis62.20.32 [sel.m] <- output$dis62.20.32 [s]
        output$dis62.20.41 [sel.m] <- output$dis62.20.41 [s]
        output$dis62.20.33 [sel.m] <- output$dis62.20.33 [s]
        output$dis62.20.35 [sel.m] <- output$dis62.20.35 [s]
        output$dis62.20.36 [sel.m] <- output$dis62.20.36 [s]
        output$dis71.20.157[sel.m] <- output$dis71.20.157[s]
    }
}



dir()
# explore workdir
files.tmp <- list.files()
files.tmp <- files.tmp[grep(paste(edo, "[Fed|Loc].+zip", sep = ""), files.tmp)]     # keep only zipped in listing
files.tmp <- files.tmp[-grep("geojson", files.tmp)] # drop geojson
tmp <- grep("100|157", files.tmp)                   # locate first/second scenario... 
files.tmp <- c(files.tmp[tmp], files.tmp[-tmp])     # and place it at start of loop
files.tmp

i <- 1
## # file contents
## unzip(tmp, list = TRUE)
unzip(files.tmp[i])
name.tmp <- unzip(files.tmp[i], list = TRUE)$Name # pick unzipped file's name for dataframe's column
# extract secciones
dat.tmp <- getSec(edon = edon, jsonFile = name.tmp)
# simplify name
name.tmp <- sub(pattern = "SICED--D[FL]--(.+)--.+--.+", replacement = "\\1", name.tmp)
name.tmp <- gsub(pattern = "[-]", replacement = ".", name.tmp)
colnames(dat.tmp)[which(colnames(dat.tmp)=="dis")] <- paste("dis", name.tmp, sep = "") # change name accordingly
#
output <- dat.tmp
#
for (i in 2:length(files.tmp)){
    #i <- 2 # debug
    unzip(files.tmp[i])
    name.tmp <- unzip(files.tmp[i], list = TRUE)$Name # pick unzipped file's name
    # extract secciones
    dat.tmp <- getSec(edon = edon, jsonFile = name.tmp)
    # simplify name
    name.tmp <- sub(pattern = "SICED--D[FL]--(.+)--.+--.+", replacement = "\\1", name.tmp)
    name.tmp <- gsub(pattern = "[-]", replacement = ".", name.tmp)
    colnames(dat.tmp)[which(colnames(dat.tmp)=="dis")] <- paste("dis", name.tmp, sep = "") # change name accordingly
    #
    output <- merge(x = output, y = dat.tmp, by = c("edon", "seccion", "munn"), all = TRUE) # merge data
}
head(output)

e1 <- output

# 2do escenario
setwd(dd2) 
# explore workdir
files.tmp <- list.files()
files.tmp <- files.tmp[grep(paste(edo, "[Fed|Loc].+zip", sep = ""), files.tmp)]     # keep only zipped in listing
files.tmp <- files.tmp[-grep("geojson", files.tmp)] # drop geojson
tmp <- grep("100|157", files.tmp)                   # locate first/second scenario... 
files.tmp <- c(files.tmp[tmp], files.tmp[-tmp])     # and place it at start of loop
files.tmp

i <- 1
## # file contents
## unzip(tmp, list = TRUE)
unzip(files.tmp[i])
name.tmp <- unzip(files.tmp[i], list = TRUE)$Name # pick unzipped file's name for dataframe's column
# extract secciones
dat.tmp <- getSec(edon = edon, jsonFile = name.tmp)
# simplify name
name.tmp <- sub(pattern = "SICED--D[FL]--(.+)--.+--.+", replacement = "\\1", name.tmp)
name.tmp <- gsub(pattern = "[-]", replacement = ".", name.tmp)
colnames(dat.tmp)[which(colnames(dat.tmp)=="dis")] <- paste("dis", name.tmp, sep = "") # change name accordingly
#
output <- dat.tmp
#
for (i in 2:length(files.tmp)){
    #i <- 2 # debug
    unzip(files.tmp[i])
    name.tmp <- unzip(files.tmp[i], list = TRUE)$Name # pick unzipped file's name
    # extract secciones
    dat.tmp <- getSec(edon = edon, jsonFile = name.tmp)
    # simplify name
    name.tmp <- sub(pattern = "SICED--D[FL]--(.+)--.+--.+", replacement = "\\1", name.tmp)
    name.tmp <- gsub(pattern = "[-]", replacement = ".", name.tmp)
    colnames(dat.tmp)[which(colnames(dat.tmp)=="dis")] <- paste("dis", name.tmp, sep = "") # change name accordingly
    #
    output <- merge(x = output, y = dat.tmp, by = c("edon", "seccion", "munn"), all = TRUE) # merge data
}
head(output)

output <- merge(x = e1, y = output, by = c("edon","seccion","munn"), all = TRUE)
e1 <- output

# 3er escenario
setwd(dd3) 
# explore workdir
files.tmp <- list.files()
files.tmp <- files.tmp[grep(paste(edo, "[Fed|Loc].+zip", sep = ""), files.tmp)]     # keep only zipped in listing
files.tmp <- files.tmp[-grep("geojson", files.tmp)] # drop geojson
tmp <- grep("100|157", files.tmp)                   # locate first/second scenario... 
files.tmp <- c(files.tmp[tmp], files.tmp[-tmp])     # and place it at start of loop
files.tmp

i <- 1
## # file contents
## unzip(tmp, list = TRUE)
unzip(files.tmp[i])
name.tmp <- unzip(files.tmp[i], list = TRUE)$Name # pick unzipped file's name for dataframe's column
# extract secciones
dat.tmp <- getSec(edon = edon, jsonFile = name.tmp)
# simplify name
name.tmp <- sub(pattern = "SICED--D[FL]--(.+)--.+--.+", replacement = "\\1", name.tmp)
name.tmp <- gsub(pattern = "[-]", replacement = ".", name.tmp)
colnames(dat.tmp)[which(colnames(dat.tmp)=="dis")] <- paste("dis", name.tmp, sep = "") # change name accordingly
#
output <- dat.tmp
#
## for (i in 2:length(files.tmp)){
##     #i <- 2 # debug
##     unzip(files.tmp[i])
##     name.tmp <- unzip(files.tmp[i], list = TRUE)$Name # pick unzipped file's name
##     # extract secciones
##     dat.tmp <- getSec(edon = edon, jsonFile = name.tmp)
##     # simplify name
##     name.tmp <- sub(pattern = "SICED--D[FL]--(.+)--.+--.+", replacement = "\\1", name.tmp)
##     name.tmp <- gsub(pattern = "[-]", replacement = ".", name.tmp)
##     colnames(dat.tmp)[which(colnames(dat.tmp)=="dis")] <- paste("dis", name.tmp, sep = "") # change name accordingly
##     #
##     output <- merge(x = output, y = dat.tmp, by = c("edon", "seccion", "munn"), all = TRUE) # merge data
## }
head(output)
#colnames(output) <- c("edon","seccion","munn","dis71.20.157cg","dis71.20.157")

output <- merge(x = e1, y = output, by = c("edon","seccion","munn"), all = TRUE)


write.csv(output, file = paste(path, edo, "Fed.csv", sep = ""), row.names = FALSE); rm(e1)



# function extracting secciones and corresponding districts in given json file
getSec <- function(edo=NA, edon=NA, jsonFile=NA){
    # check that input is correctly specified
    output <- data.frame() # will receive secciones to plug into eqMp
    edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
    if (is.na(edo)==TRUE & is.na(edon)==TRUE){
        stop("Must provide edo or edon.")
    }
    if (is.na(edo)==FALSE & (is.character(edo)==FALSE | length(intersect(edo, edos))==0)){
        stop("edo must be one of valid abbreviations: ags, bc, bcs, cam, coa, col, cps, cua, df, dgo, gua, gue, hgo, jal, mex, mic, mor, nay, nl, oax, pue, que, qui, san, sin, son, tab, tam, tla, ver, yuc, zac")
    }
    if (is.na(edon)==FALSE & (is.numeric(edon)==FALSE | edon<1 | edon>32)){
        stop("edon must be numeric in 1:32.")
    }
    if (is.na(edo)==FALSE & is.na(edon)==FALSE){
        if (edos[edon]!=edo) stop("edo--edon mismatch.")
    }
    if (is.na(edo)==TRUE){
        edo <- edos[edon]
    }
    if (is.na(edon)==TRUE){
        edon <- which(edos==edo)
    }
    # read file
    txt <- readLines(con = jsonFile, warn = FALSE)
    #txt <- file.path(dd, "OaxacaLocal.json") 
    dat <- fromJSON(txt)#, flatten=TRUE)
    D <- max(dat$distritos$d) # number of districts in state
    for (dis in 1:D){
        #dis <- 1 # debug
        tmp.m <- dat$distritos$des[[dis]]$mu
        M <- length(tmp.m)
        if (M==1){
            tmp.s <- dat$distritos$des[[dis]]$s[[1]]
            S <- length(tmp.s)
            tmp.e <- data.frame(edon=rep(edon,S), seccion=tmp.s, munn=rep(tmp.m,S), dis=rep(dis,S))
        } else {
            tmp.e <- data.frame()
            for(munn in 1:M){
                #munn <- 2 # debug
                tmp.s <- dat$distritos$des[[dis]]$s[[munn]]
                S <- length(tmp.s)
                tmp.e <- rbind(tmp.e, data.frame(edon=rep(edon,S), seccion=tmp.s, munn=rep(tmp.m[munn],S), dis=rep(dis,S)))
            }
        }
        output <- rbind(output, tmp.e)
        }
    return(output)
}





