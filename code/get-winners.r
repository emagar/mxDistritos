##########################################
## Code invoked from elec-data-for-maps ##
## to determine municipal and district  ##
## winners in each year                 ##
##########################################

#############################
## unit winners and margin ##
#############################
if (agg=="m") {
    winner <- v94[,c("edon","inegi","ife")] # will receive data
    rownames(winner) <- NULL
}
if (agg=="s") {
    winner <- v94[,c("edon","seccion")] # will receive data
    rownames(winner) <- NULL
}
##########
## 1991 ##
##########
if (agg=="m") { # secc data not ready yet
    sel.c <- c("pan","pri","parm","pdm","pfcrn","pps","prd","pt","pem","prt")
    v <- v91[,sel.c]
    v <- v / rowSums(v)
    l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
    l <- sortBy(target = l, By = v)
    v <- sortBy(target = v, By = v)
    winner$w91  <- l[,1]
    winner$mg91 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
    winner$w91[is.na(winner$mg91)] <- NA
    message("1991 done")
}
##########
## 1994 ##
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem")
v <- v94[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w94  <- l[,1]
winner$mg94 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("1994 done")
##########
## 1997 ##
##########
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm")
v <- v97[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w97  <- l[,1]
winner$mg97 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("1997 done")
##########
## 2000 ##
##########
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn")
v <- v00[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w00  <- l[,1]
winner$mg00 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("2000 done")
##########
## 2003 ##
##########
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc")
v <- v03[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w03  <- l[,1]
winner$mg03 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("2003 done")
##########
## 2006 ##
##########
sel.c <- c("pan","pric","prdc","pna","asdc")
v <- v06[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w06  <- l[,1]
winner$mg06 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("2006 done")
##########
## 2009 ##
##########
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd")
v <- v09[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w09  <- l[,1]
winner$mg09 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("2009 done")
##########
## 2012 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc")
v <- v12[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w12  <- l[,1]
winner$mg12 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("2012 done")
##########
## 2015 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2")
v <- v15[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w15  <- l[,1]
winner$mg15 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("2015 done")
##########
## 2018 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2")
v <- v18[,sel.c]
v <- v / rowSums(v)
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w18  <- l[,1]
winner$mg18 <- round(as.numeric(v[,1]) - as.numeric(v[,2]), 3)
message("2018 done")
#
# clean
rm(v,l)

# n win
#pan
tmp <- winner[,grep("^w[0-9]", colnames(winner))]
tmp <- ifelse(tmp=="pan" | tmp=="panc", 1, 0)
winner$nwin.pan <- rowSums(tmp, na.rm=TRUE)
#pri
tmp <- winner[,grep("^w[0-9]", colnames(winner))]
tmp <- ifelse(tmp=="pri" | tmp=="pric", 1, 0)
winner$nwin.pri <- rowSums(tmp, na.rm=TRUE)
#left
tmp <- winner[,grep("^w[0-9]", colnames(winner))]
sel.r <- which(tmp[,"w18"]=="prd")
tmp[sel.r,"w18"] <- "panc" # rename prd in 2018 to not count it as left's here
tmp <- ifelse(tmp=="prd" | tmp=="prdc" | tmp=="morena" | tmp=="morenac", 1, 0)
winner$nwin.left <- rowSums(tmp, na.rm=TRUE)
#oth
tmp <- winner[,grep("^w[0-9]", colnames(winner))]
winner <- within(winner, nwin.oth <- ncol(tmp) - nwin.pan - nwin.pri - nwin.left) # pbm: assigns NAs as oth victory

## # rename output
## if (agg=="m") {
##     winmun <- winner;
##     rm(winner)
## }
## if (agg=="s") {
##     winsec <- winner;
##     rm(winner)
## }
