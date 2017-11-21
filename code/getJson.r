rm(list = ls())
                    
path <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineJsonFuente2017/"

#library(rjson)
library(jsonlite)

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

########################################
# aqui empieza la manipulacion Federal #
########################################
edon <- 17 # 1:32
edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
#dd <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ineJson/pruebas"
dd1 <- paste(path, "fed/e1/", edo, sep = "")
dd2 <- paste(path, "fed/e2/", edo, sep = "")
dd3 <- paste(path, "fed/e3/",      sep = "")

# 1er escenario
setwd(dd1) 
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



######################################
# aqui empieza la manipulacion Local #
######################################
edon <- 9 # 1:32
edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
#dd <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ineJson/pruebas"
dd4 <- paste(path, "loc/"   , edo, sep = "")
setwd(dd4) 
#
# explore workdir
files.tmp <- list.files()
#files.tmp <- files.tmp[grep(paste(edo, "12[Fed|Loc].+zip", sep = ""), files.tmp)]     # keep only zipped in listing
files.tmp <- files.tmp[grep(paste(edo, "33Loc.+zip", sep = ""), files.tmp)]     # keep only zipped in listing
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
    #i <- 10 # debug
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
dim(output)

#write.csv(output, file = paste(path, edo, "33Loc.csv", sep = ""), row.names = FALSE)
write.csv(output, file = paste(path, edo, "Loc.csv", sep = ""), row.names = FALSE)

