##########################################
## Code invoked from elec-data-for-maps ##
## to determine municipal and district  ##
## winners in each year                 ##
##########################################

# unit winners and margin
if (agg=="m") {
    winner <- v94[,c("edon","inegi","ife")] # will receive data
}
if (agg=="s") {
    winner <- v94[,c("edon","seccion")] # will receive data
}
# 1994
v <- data.frame(pan    = pan   [,1],
                pri    = pri   [,1],
                morena = morena[,1],
                oth    = oth   [,1])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w94  <- l[,1]
winner$mg94 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("1994 done")
# 1997
v <- data.frame(pan    = pan   [,2],
                pri    = pri   [,2],
                morena = morena[,2],
                oth    = oth   [,2])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w97  <- l[,1]
winner$mg97 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("1997 done")
# 2000
v <- data.frame(pan    = pan   [,3],
                pri    = pri   [,3],
                morena = morena[,3],
                oth    = oth   [,3])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w00  <- l[,1]
winner$mg00 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("2000 done")
# 2003
v <- data.frame(pan    = pan   [,4],
                pri    = pri   [,4],
                morena = morena[,4],
                oth    = oth   [,4])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w03  <- l[,1]
winner$mg03 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("2003 done")
# 2006
v <- data.frame(pan    = pan   [,5],
                pri    = pri   [,5],
                morena = morena[,5],
                oth    = oth   [,5])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w06  <- l[,1]
winner$mg06 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("2006 done")
# 2009
v <- data.frame(pan    = pan   [,6],
                pri    = pri   [,6],
                morena = morena[,6],
                oth    = oth   [,6])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w09  <- l[,1]
winner$mg09 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("2009 done")
# 2012
v <- data.frame(pan    = pan   [,7],
                pri    = pri   [,7],
                morena = morena[,7],
                oth    = oth   [,7])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w12  <- l[,1]
winner$mg12 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("2012 done")
# 2015
v <- data.frame(pan    = pan   [,8],
                pri    = pri   [,8],
                morena = morena[,8],
                oth    = oth   [,8])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w15  <- l[,1]
winner$mg15 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("2015 done")
# 2018
v <- data.frame(pan    = pan   [,9],
                pri    = pri   [,9],
                morena = morena[,9],
                oth    = oth   [,9])
l <- as.data.frame(matrix(rep(colnames(v), nrow(v)), nrow = nrow(v), byrow = TRUE), stringsAsFactors = FALSE)
l <- sortBy(target = l, By = v)
v <- sortBy(target = v, By = v)
winner$w18  <- l[,1]
winner$mg18 <- as.numeric(v[,1]) - as.numeric(v[,2])
message("2018 done")
#
# clean
rm(v,l)
rownames(winner) <- NULL

# n win
#pan
tmp <- winner[,grep("^w[0-9]", colnames(winner))]
tmp <- ifelse(tmp=="pan", 1, 0)
winner$nwin.pan <- rowSums(tmp)
#pri
tmp <- winner[,grep("^w[0-9]", colnames(winner))]
tmp <- ifelse(winner=="pri", 1, 0)
winner$nwin.pri <- rowSums(tmp)
#morena
tmp <- winner[,grep("^w[0-9]", colnames(winner))]
tmp <- ifelse(winner=="morena", 1, 0)
winner$nwin.morena <- rowSums(tmp)
#oth
tmp <- winner[,grep("^w[0-9]", colnames(winner))]
tmp <- ifelse(winner=="oth", 1, 0)
winner$nwin.oth <- rowSums(tmp)

## # rename output
## if (agg=="m") {
##     winmun <- winner;
##     rm(winner)
## }
## if (agg=="s") {
##     winsec <- winner;
##     rm(winner)
## }
