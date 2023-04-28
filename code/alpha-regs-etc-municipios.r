####################################################################
## Script for autoregressive vote estimates and alpha regressions ##
## invoked from code/elec-data-for-maps.r                         ##
##                                                                ##
## Author: Eric Magar                                             ##
## emagar at itam dot mx                                          ##
## Date: 17apr2023                                                ##
## Last modified: 18apr2023                                       ##
####################################################################

###################################################################
## Note: Search function estim_dis below, that wraps estimation. ##
## Script above that preps time-series data.                     ##
## Script below that saves output and cleans.                    ##
###################################################################

############################################################################
## # backwards estimation, unit = districts                               ##
## 1988d <- 1991d   1994d   1997d79 2000d79 2003d79                       ##
## 1991d <- 1994d   1997d79 2000d79 2003d79 2006d79                       ##
## 1994d <- 1997d79 2000d79 2003d79 2006d79 2009d79                       ##
## 1997d <- 2000d   2003d   2006d97 2009d97 2012d97                       ##
## 2000d <- 2003d   2006d97 2009d97 2012d97 2015d97                       ##
## 2003d <- 2006d97 2009d97 2012d97 2015d97 2018d97                       ##
## 2006d <- 2009d   2012d   2015d   2018d06 2021d06                       ##
## # forward estimation, unit = districts                                 ##
##          2009d18 2012d18 2015d18 2018d   2021d   -> 2024d18            ##
##          2006d18 2009d18 2012d18 2015d18 2018d   -> 2021d              ##
##          2003d18 2006d18 2009d18 2012d18 2015d18 -> 2018d              ##
##          2000d06 2003d06 2006d   2009d   2012d   -> 2015d              ##
##          1997d06 2000d06 2003d06 2006d   2009d   -> 2012d              ##
##          1994d06 1997d06 2000d06 2003d06 2006d   -> 2009d              ##
##          1991d06 1994d06 1997d06 2000d06 2003d06 -> 2006d # no 1991d06 ##
############################################################################

#################################
## aggregate municipio returns ##
#################################
##########################################################################
## Function to square d by adding municipios absent frim that election  ##
##########################################################################
add.miss.mun <- function(d){
    ## get vector of all municipios in eq 
    all.ife <- eq[,grep("^ife", colnames(eq))]
    all.ife <- as.vector(unlist(all.ife))
    all.ife <- unique(all.ife)
    all.ife <- all.ife[order(all.ife)]
    tail(all.ife)
    ## any municipios missing?
    mis.ife <- all.ife[which(all.ife %notin% d$ife)]
    if (length(mis.ife)>0){
        tmp <- d[1:length(mis.ife),]
        tmp[] <- NA
        tmp$ife <- mis.ife
        tmp$edon <- as.integer(tmp$ife/1000)
        ## if so, add empty rows to d
        d <- rbind(d, tmp)
    }
    return(d)
}

##########
## 1991 ## OJO: 1991 seccion identifiers are wrong, can aggregate with ife, but no info for counterfactuals
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec","lisnom","dextra")
# actual municipalities
d <- v91s; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
#d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
#d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
#d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; mun after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v91m <- d                                      # rename object  

##########
## 1994 ##
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom","dextra")
# actual 1994 municipalities
d <- v94s; d[is.na(d)] <- 0
#d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m <- d                                      # rename object  
# 1997 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m97 <- d                                    # rename object  
# 2000 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m00 <- d                                    # rename object  
# 2003 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m03 <- d                                    # rename object  
# 2006 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m06 <- d                                    # rename object  
# 2009 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m09 <- d                                    # rename object  
# 2012 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m12 <- d                                    # rename object  
# 2015 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m15 <- d                                    # rename object  
# 2018 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m18 <- d                                    # rename object  
# 2021 municipalities
d <- v94s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v94m21 <- d                                    # rename object  

##########
## 1997 ##
##########
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","lisnom","dextra")
# 1994 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m94 <- d                                    # rename object  
# actual 1997 municipalities
d <- v97s; d[is.na(d)] <- 0
#d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m <- d                                      # rename object  
# 2000 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m00 <- d                                    # rename object  
# 2003 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m03 <- d                                    # rename object  
# 2006 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m06 <- d                                    # rename object  
# 2009 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m09 <- d                                    # rename object  
# 2012 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m12 <- d                                    # rename object  
# 2015 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m15 <- d                                    # rename object  
# 2018 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m18 <- d                                    # rename object  
# 2021 municipalities
d <- v97s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v97m21 <- d                                    # rename object  

##########
## 2000 ##
##########
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","lisnom","dpanc","dprdc","dextra")
# 1994 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m94 <- d                                    # rename object  
# 1997 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m97 <- d                                    # rename object  
# actual 2000 municipalities
d <- v00s; d[is.na(d)] <- 0
#d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m <- d                                      # rename object  
# 2003 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m03 <- d                                    # rename object  
# 2006 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m06 <- d                                    # rename object  
# 2009 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m09 <- d                                    # rename object  
# 2012 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m12 <- d                                    # rename object  
# 2015 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m15 <- d                                    # rename object  
# 2018 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m18 <- d                                    # rename object  
# 2021 municipalities
d <- v00s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v00m21 <- d                                    # rename object  

##########
## 2003 ##
##########
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","lisnom","dpric","dextra")
# 1994 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m94 <- d                                    # rename object  
# 1997 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m97 <- d                                    # rename object  
# 2000 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m00 <- d                                    # rename object  
# 2003 actual municipalities
d <- v03s; d[is.na(d)] <- 0
#d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m <- d                                      # rename object  
# 2006 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m06 <- d                                    # rename object  
# 2009 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m09 <- d                                    # rename object  
# 2012 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m12 <- d                                    # rename object  
# 2015 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m15 <- d                                    # rename object  
# 2018 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m18 <- d                                    # rename object  
# 2021 municipalities
d <- v03s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v03m21 <- d                                    # rename object  

##########
## 2006 ##
##########
sel.c <- c("pan","pric","prdc","pna","asdc","efec","lisnom","dpric","dprdc","dextra")
# 1994 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m94 <- d                                    # rename object  
# 1997 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m97 <- d                                    # rename object  
# 2000 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m00 <- d                                    # rename object  
# 2003 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m03 <- d                                    # rename object  
# 2006 actual municipalities
d <- v06s; d[is.na(d)] <- 0
#d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m <- d                                      # rename object  
# 2009 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m09 <- d                                    # rename object  
# 2012 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m12 <- d                                    # rename object  
# 2015 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m15 <- d                                    # rename object  
# 2018 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m18 <- d                                    # rename object  
# 2021 municipalities
d <- v06s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v06m21 <- d                                    # rename object  

##########
## 2009 ##
##########
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc","dextra")
# 1994 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m94 <- d                                    # rename object  
# 1997 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m97 <- d                                    # rename object  
# 2000 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m00 <- d                                    # rename object  
# 2003 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m03 <- d                                    # rename object  
# 2006 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m06 <- d                                    # rename object  
# 2009 actual municipalities
d <- v09s; d[is.na(d)] <- 0
#d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m <- d                                      # rename object  
# 2012 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m12 <- d                                    # rename object  
# 2015 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m15 <- d                                    # rename object  
# 2018 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m18 <- d                                    # rename object  
# 2021 municipalities
d <- v09s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v09m21 <- d                                    # rename object  

##########
## 2012 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc","dextra")
# 1994 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m94 <- d                                    # rename object  
# 1997 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m97 <- d                                    # rename object  
# 2000 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m00 <- d                                    # rename object  
# 2003 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m03 <- d                                    # rename object  
# 2006 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m06 <- d                                    # rename object  
# 2009 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m09 <- d                                    # rename object  
# 2012 municipalities
d <- v12s; d[is.na(d)] <- 0
#d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m <- d                                      # rename object  
# 2015 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m15 <- d                                    # rename object  
# 2018 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m18 <- d                                    # rename object  
# 2021 municipalities
d <- v12s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v12m21 <- d                                    # rename object  

##########
## 2015 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc","dextra")
# 1994 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m94 <- d                                    # rename object  
# 1997 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m97 <- d                                    # rename object  
# 2000 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m00 <- d                                    # rename object  
# 2003 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m03 <- d                                    # rename object  
# 2006 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m06 <- d                                    # rename object  
# 2009 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m09 <- d                                    # rename object  
# 2012 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m12 <- d                                    # rename object  
# 2015 actual municipalities
d <- v15s; d[is.na(d)] <- 0
#d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m <- d                                      # rename object  
# 2018 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m18 <- d                                    # rename object  
# 2021 municipalities
d <- v15s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v15m21 <- d                                    # rename object  

##########
## 2018 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# 1994 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m94 <- d                                    # rename object  
# 1997 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m97 <- d                                    # rename object  
# 2000 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m00 <- d                                    # rename object  
# 2003 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m03 <- d                                    # rename object  
# 2006 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m06 <- d                                    # rename object  
# 2009 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m09 <- d                                    # rename object  
# 2012 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m12 <- d                                    # rename object  
# 2015 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m15 <- d                                    # rename object  
# 2018 municipalities
d <- v18s; d[is.na(d)] <- 0
#d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m <- d                                      # rename object  
# 2021 municipalities
d <- v18s; d[is.na(d)] <- 0
d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v18m21 <- d                                    # rename object  

##########
## 2021 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# 1994 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife1994                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m94 <- d                                    # rename object
# 1997 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife1997                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m97 <- d                                    # rename object  
# 2000 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2000                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m00 <- d                                    # rename object  
# 2003 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2003                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m03 <- d                                    # rename object  
# 2006 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2006                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m06 <- d                                    # rename object  
# 2009 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2009                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m09 <- d                                    # rename object  
# 2012 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2012                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m12 <- d                                    # rename object  
# 2015 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2015                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m15 <- d                                    # rename object  
# 2018 municipalities
d <- v21s; d[is.na(d)] <- 0
d$ife <- d$ife2018                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m18 <- d                                    # rename object  
# 2021 actual municipalities
d <- v21s; d[is.na(d)] <- 0
#d$ife <- d$ife2021                             # municipio ids from the historic map
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop historic ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
d <- add.miss.mun(d)                           # add missing municipios, if any
d <- d[order(d$ife),]                          # sort
v21m <- d                                      # rename object  
#
rm(sel.drop,sel.c,add.miss.mun)

# verify nrow==2469
table(c(
    nrow(v91m), nrow(v94m),
    nrow(v97m), nrow(v00m), nrow(v03m),
    nrow(v06m), nrow(v09m), nrow(v12m), nrow(v15m),
    nrow(v18m), nrow(v21m),
##
##
    nrow(v97m94), nrow(v00m94), nrow(v03m94),
    nrow(v06m94), nrow(v09m94), nrow(v12m94), nrow(v15m94),
    nrow(v18m94), nrow(v21m94),
##
    nrow(v94m97),
                  nrow(v00m97), nrow(v03m97),
    nrow(v06m97), nrow(v09m97), nrow(v12m97), nrow(v15m97),
    nrow(v18m97), nrow(v21m97),
##
    nrow(v94m00),
    nrow(v97m00),               nrow(v03m00),
    nrow(v06m00), nrow(v09m00), nrow(v12m00), nrow(v15m00),
    nrow(v18m00), nrow(v21m00),
##
    nrow(v94m03),
    nrow(v97m03), nrow(v00m03), 
    nrow(v06m03), nrow(v09m03), nrow(v12m03), nrow(v15m03),
    nrow(v18m03), nrow(v21m03),
##
    nrow(v94m06),
    nrow(v97m06), nrow(v00m06), nrow(v03m06),
                  nrow(v09m06), nrow(v12m06), nrow(v15m06),
    nrow(v18m06), nrow(v21m06),
##
    nrow(v94m09),
    nrow(v97m09), nrow(v00m09), nrow(v03m09),
    nrow(v06m09),               nrow(v12m09), nrow(v15m09),
    nrow(v18m09), nrow(v21m09),
##
    nrow(v94m12),
    nrow(v97m12), nrow(v00m12), nrow(v03m12),
    nrow(v06m12), nrow(v09m12),               nrow(v15m12),
    nrow(v18m12), nrow(v21m12),
##
    nrow(v94m15),
    nrow(v97m15), nrow(v00m15), nrow(v03m15),
    nrow(v06m15), nrow(v09m15), nrow(v12m15), 
    nrow(v18m15), nrow(v21m15),
##
    nrow(v94m18),
    nrow(v97m18), nrow(v00m18), nrow(v03m18),
    nrow(v06m18), nrow(v09m18), nrow(v12m18), nrow(v15m18),
                  nrow(v21m18),
    ##
    nrow(v94m21),
    nrow(v97m21), nrow(v00m21), nrow(v03m21),
    nrow(v06m21), nrow(v09m21), nrow(v12m21), nrow(v15m21),
    nrow(v18m21) 
))
##
nmun <- nrow(v00m)  ## n municipalities in square data


###############################################################################
## Prepare manipulated party objects for time-series and alpha regressions   ##
## After 2024 election, uncheck/add lines                                    ##
##                *** One object per municipio map ***                       ##
## *** Map changes almost every year, so one per federal election  ***       ##
###############################################################################
#
# version 1: extend partial coalitions across the board
# shares
panm21 <- data.frame(
    #v91 =  with(v91m21, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m21, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m21, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m21, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m21, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m21, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m21, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m21, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m21, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m21, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m,   ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm18 <- data.frame(
    #v91 =  with(v91m18, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m18, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m18, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m18, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m18, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m18, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m18, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m18, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m18, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m,   ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m18, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm15 <- data.frame(
    #v91 =  with(v91m15, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m15, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m15, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m15, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m15, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m15, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m15, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m15, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m,   ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m15, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m15, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm12 <- data.frame(
    #v91 =  with(v91m12, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m12, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m12, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m12, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m12, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m12, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m12, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m,   ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m12, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m12, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m12, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm09 <- data.frame(
    #v91 =  with(v91m09, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m09, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m09, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m09, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m09, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m09, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m,   ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m09, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m09, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m09, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m09, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm06 <- data.frame(
    #v91 =  with(v91m06, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m06, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m06, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m06, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m06, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m,   ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m06, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m06, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m06, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m06, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m06, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm03 <- data.frame(
    #v91 =  with(v91m03, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m03, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m03, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m03, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m,   ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m03, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m03, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m03, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m03, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m03, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m03, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm00 <- data.frame(
    #v91 =  with(v91m00, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m00, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m00, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m,   ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m00, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m00, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m00, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m00, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m00, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m00, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m00, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm97 <- data.frame(
    #v91 =  with(v91m97, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m97, ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m,   ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m97, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m97, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m97, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m97, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m97, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m97, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m97, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m97, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
panm94 <- data.frame(
    #v91 =  with(v91m94, ifelse(efec==0, NA,  pan               / efec)), 
    v94 =  with(v94m,   ifelse(efec==0, NA,  pan               / efec)),
    v97 =  with(v97m94, ifelse(efec==0, NA,  pan               / efec)),
    v00 =  with(v00m94, ifelse(efec==0, NA,  panc              / efec)),
    v03 =  with(v03m94, ifelse(efec==0, NA,  pan               / efec)),
    v06 =  with(v06m94, ifelse(efec==0, NA,  pan               / efec)),
    v09 =  with(v09m94, ifelse(efec==0, NA,  pan               / efec)),
    v12 =  with(v12m94, ifelse(efec==0, NA,  pan               / efec)),
    v15 =  with(v15m94, ifelse(efec==0, NA,  pan               / efec)),
    v18 =  with(v18m94, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
    v21 =  with(v21m94, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
)
## panm91 <- data.frame(
##     v91 =  with(v91m,   ifelse(efec==0, NA,  pan               / efec)), 
##     v94 =  with(v94m91, ifelse(efec==0, NA,  pan               / efec)),
##     v97 =  with(v97m91, ifelse(efec==0, NA,  pan               / efec)),
##     v00 =  with(v00m91, ifelse(efec==0, NA,  panc              / efec)),
##     v03 =  with(v03m91, ifelse(efec==0, NA,  pan               / efec)),
##     v06 =  with(v06m91, ifelse(efec==0, NA,  pan               / efec)),
##     v09 =  with(v09m91, ifelse(efec==0, NA,  pan               / efec)),
##     v12 =  with(v12m91, ifelse(efec==0, NA,  pan               / efec)),
##     v15 =  with(v15m91, ifelse(efec==0, NA,  pan               / efec)),
##     v18 =  with(v18m91, ifelse(efec==0, NA, (pan + panc + prd) / efec)),  # dropped mc
##     v21 =  with(v21m91, ifelse(efec==0, NA, (pan + panc + prd) / efec))   # drop prd?
## )
#
panm21 <- round(panm21, 3)
panm18 <- round(panm18, 3)
panm15 <- round(panm15, 3)
panm12 <- round(panm12, 3)
panm09 <- round(panm09, 3)
panm06 <- round(panm06, 3)
panm03 <- round(panm03, 3)
panm00 <- round(panm00, 3)
panm97 <- round(panm97, 3)
panm94 <- round(panm94, 3)
## panm91 <- round(panm91, 3)
#
prim21 <- data.frame(
    #v91 =  with(v91m21, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m21, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m21, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m21, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m21, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m21, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m21, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m21, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m21, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m21, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m,   ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim18 <- data.frame(
    #v91 =  with(v91m18, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m18, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m18, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m18, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m18, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m,   ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m18, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim15 <- data.frame(
    #v91 =  with(v91m15, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m15, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m15, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m15, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m15, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m15, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m15, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m15, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m15, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m15, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim12 <- data.frame(
    #v91 =  with(v91m12, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m12, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m12, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m12, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m12, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m12, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m12, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m12, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m12, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m12, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim09 <- data.frame(
    #v91 =  with(v91m09, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m09, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m09, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m09, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m09, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m09, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m09, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m09, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m09, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m09, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim06 <- data.frame(
    #v91 =  with(v91m06, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m06, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m06, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m06, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m,   ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m06, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m06, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim03 <- data.frame(
    #v91 =  with(v91m03, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m03, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m03, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m03, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m03, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m03, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m03, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m03, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m03, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m03, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim00 <- data.frame(
    #v91 =  with(v91m00, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m00, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m00, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m,   ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m00, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m00, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m00, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m00, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m00, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m00, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m00, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim97 <- data.frame(
    #v91 =  with(v91m97, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m97, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m,   ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m97, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m97, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m97, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m97, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prim94 <- data.frame(
    #v91 =  with(v91m94, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94m,   ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97m94, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00m94, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03m94, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06m94, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09m94, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12m94, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15m94, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18m94, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21m94, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
## prim91 <- data.frame(
##     v91 =  with(v91m,   ifelse(efec==0, NA,  pri                      / efec)),
##     v94 =  with(v94m91, ifelse(efec==0, NA,  pri                      / efec)),
##     v97 =  with(v97m91, ifelse(efec==0, NA,  pri                      / efec)),
##     v00 =  with(v00m91, ifelse(efec==0, NA,  pri                      / efec)),
##     v03 =  with(v03m91, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
##     v06 =  with(v06m91, ifelse(efec==0, NA,  pric                     / efec)),
##     v09 =  with(v09m91, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
##     v12 =  with(v12m91, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
##     v15 =  with(v15m91, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
##     v18 =  with(v18m91, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
##     v21 =  with(v21m91, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
## )
#
prim21 <- round(prim21, 3)
prim18 <- round(prim18, 3)
prim15 <- round(prim15, 3)
prim12 <- round(prim12, 3)
prim09 <- round(prim09, 3)
prim06 <- round(prim06, 3)
prim03 <- round(prim03, 3)
prim00 <- round(prim00, 3)
prim97 <- round(prim97, 3)
prim94 <- round(prim94, 3)
## prim91 <- round(prim91, 3)
#
leftm21 <- data.frame(
    #v91 = with(v91m21, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m21, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m21, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m21, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m21, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m21, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m21, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m21, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m21, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m21, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m,   ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm18 <- data.frame(
    #v91 = with(v91m18, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m18, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m18, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m18, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m18, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m18, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m18, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m18, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m18, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m,   ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m18, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm15 <- data.frame(
    #v91 = with(v91m15, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m15, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m15, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m15, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m15, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m15, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m15, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m15, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m,   ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m15, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m15, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm12 <- data.frame(
    #v91 = with(v91m12, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m12, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m12, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m12, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m12, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m12, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m12, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m,   ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m12, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m12, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m12, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm09 <- data.frame(
    #v91 = with(v91m09, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m09, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m09, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m09, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m09, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m09, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m,   ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m09, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m09, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m09, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m09, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm06 <- data.frame(
    #v91 = with(v91m06, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m06, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m06, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m06, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m06, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m,   ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m06, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m06, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m06, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m06, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m06, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm03 <- data.frame(
    #v91 = with(v91m03, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m03, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m03, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m03, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m,   ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m03, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m03, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m03, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m03, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m03, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m03, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm00 <- data.frame(
    #v91 = with(v91m00, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m00, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m00, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m,   ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m00, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m00, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m00, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m00, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m00, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m00, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m00, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm97 <- data.frame(
    #v91 = with(v91m97, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m97, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m,   ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m97, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m97, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m97, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m97, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m97, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m97, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m97, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m97, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftm94 <- data.frame(
    #v91 = with(v91m94, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94m,   ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97m94, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00m94, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03m94, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06m94, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09m94, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12m94, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15m94, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18m94, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21m94, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
## leftm91 <- data.frame(
##     v91 = with(v91m,   ifelse(efec==0, NA,  prd                             / efec)),
##     v94 = with(v94m91, ifelse(efec==0, NA,  prd                             / efec)),
##     v97 = with(v97m91, ifelse(efec==0, NA,  prd                             / efec)),
##     v00 = with(v00m91, ifelse(efec==0, NA,  prdc                            / efec)),
##     v03 = with(v03m91, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
##     v06 = with(v06m91, ifelse(efec==0, NA,  prdc                            / efec)),
##     v09 = with(v09m91, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
##     v12 = with(v12m91, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
##     v15 = with(v15m91, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
##     v18 = with(v18m91, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
##     v21 = with(v21m91, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
## )
#
leftm21 <- round(leftm21, 3)
leftm18 <- round(leftm18, 3)
leftm15 <- round(leftm15, 3)
leftm12 <- round(leftm12, 3)
leftm09 <- round(leftm09, 3)
leftm06 <- round(leftm06, 3)
leftm03 <- round(leftm03, 3)
leftm00 <- round(leftm00, 3)
leftm97 <- round(leftm97, 3)
leftm94 <- round(leftm94, 3)
## leftm91 <- round(leftm91, 3)
#
othm21 <- data.frame(
    #v91 =  with(v91m21, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m21, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m21, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m21, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m21, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m21, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m21, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m21, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m21, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m21, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m,   ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm18 <- data.frame(
    #v91 =  with(v91m18, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m18, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m18, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m18, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m18, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m18, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m18, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m18, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m18, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m,   ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m18, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm15 <- data.frame(
    #v91 =  with(v91m15, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m15, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m15, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m15, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m15, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m15, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m15, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m15, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m,   ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m15, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m15, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm12 <- data.frame(
    #v91 =  with(v91m12, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m12, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m12, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m12, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m12, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m12, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m12, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m,   ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m12, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m12, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m12, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm09 <- data.frame(
    #v91 =  with(v91m09, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m09, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m09, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m09, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m09, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m09, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m,   ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m09, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m09, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m09, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m09, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm06 <- data.frame(
    #v91 =  with(v91m06, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m06, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m06, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m06, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m06, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m,   ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m06, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m06, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m06, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m06, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m06, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm03 <- data.frame(
    #v91 =  with(v91m03, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m03, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m03, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m03, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m,   ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m03, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m03, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m03, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m03, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m03, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m03, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm00 <- data.frame(
    #v91 =  with(v91m00, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m00, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m00, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m,   ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m00, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m00, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m00, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m00, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m00, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m00, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m00, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm97 <- data.frame(
    #v91 =  with(v91m97, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m97, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m,   ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m97, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m97, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m97, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m97, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m97, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m97, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m97, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m97, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othm94 <- data.frame(
    #v91 =  with(v91m94, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94m,   ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97m94, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00m94, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03m94, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06m94, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09m94, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12m94, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15m94, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18m94, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
    v21 =  with(v21m94, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
## othm91 <- data.frame(
##     v91 =  with(v91m,   ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
##     v94 =  with(v94m91, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
##     v97 =  with(v97m91, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
##     v00 =  with(v00m91, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
##     v03 =  with(v03m91, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
##     v06 =  with(v06m91, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
##     v09 =  with(v09m91, ifelse(efec==0, NA, (pna + psd)                                / efec)),
##     v12 =  with(v12m91, ifelse(efec==0, NA,  pna                                       / efec)),
##     v15 =  with(v15m91, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
##     v18 =  with(v18m91, ifelse(efec==0, NA, (mc + indep1 + indep2)                     / efec)),
##     v21 =  with(v21m91, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
## )
#
othm21 <- round(othm21, 3)
othm18 <- round(othm18, 3)
othm15 <- round(othm15, 3)
othm12 <- round(othm12, 3)
othm09 <- round(othm09, 3)
othm06 <- round(othm06, 3)
othm03 <- round(othm03, 3)
othm00 <- round(othm00, 3)
othm97 <- round(othm97, 3)
othm94 <- round(othm94, 3)
## othm91 <- round(othm91, 3)
#
efecm21 <- data.frame(
    #v91 = v91m21$efec,
    v94 = v94m21$efec,
    v97 = v97m21$efec,
    v00 = v00m21$efec,
    v03 = v03m21$efec,
    v06 = v06m21$efec,
    v09 = v09m21$efec,
    v12 = v12m21$efec,
    v15 = v15m21$efec,
    v18 = v18m21$efec,
    v21 = v21m  $efec
)
efecm18 <- data.frame(
    #v91 = v91m18$efec,
    v94 = v94m18$efec,
    v97 = v97m18$efec,
    v00 = v00m18$efec,
    v03 = v03m18$efec,
    v06 = v06m18$efec,
    v09 = v09m18$efec,
    v12 = v12m18$efec,
    v15 = v15m18$efec,
    v18 = v18m  $efec,
    v21 = v21m18$efec
)
efecm15 <- data.frame(
    #v91 = v91m15$efec,
    v94 = v94m15$efec,
    v97 = v97m15$efec,
    v00 = v00m15$efec,
    v03 = v03m15$efec,
    v06 = v06m15$efec,
    v09 = v09m15$efec,
    v12 = v12m15$efec,
    v15 = v15m  $efec,
    v18 = v18m15$efec,
    v21 = v21m15$efec
)
efecm12 <- data.frame(
    #v91 = v91m12$efec,
    v94 = v94m12$efec,
    v97 = v97m12$efec,
    v00 = v00m12$efec,
    v03 = v03m12$efec,
    v06 = v06m12$efec,
    v09 = v09m12$efec,
    v12 = v12m  $efec,
    v15 = v15m12$efec,
    v18 = v18m12$efec,
    v21 = v21m12$efec
)
efecm09 <- data.frame(
    #v91 = v91m09$efec,
    v94 = v94m09$efec,
    v97 = v97m09$efec,
    v00 = v00m09$efec,
    v03 = v03m09$efec,
    v06 = v06m09$efec,
    v09 = v09m  $efec,
    v12 = v12m09$efec,
    v15 = v15m09$efec,
    v18 = v18m09$efec,
    v21 = v21m09$efec
)
efecm06 <- data.frame(
    #v91 = v91m06$efec,
    v94 = v94m06$efec,
    v97 = v97m06$efec,
    v00 = v00m06$efec,
    v03 = v03m06$efec,
    v06 = v06m  $efec,
    v09 = v09m06$efec,
    v12 = v12m06$efec,
    v15 = v15m06$efec,
    v18 = v18m06$efec,
    v21 = v21m06$efec
)
efecm03 <- data.frame(
    #v91 = v91m03$efec,
    v94 = v94m03$efec,
    v97 = v97m03$efec,
    v00 = v00m03$efec,
    v03 = v03m  $efec,
    v06 = v06m03$efec,
    v09 = v09m03$efec,
    v12 = v12m03$efec,
    v15 = v15m03$efec,
    v18 = v18m03$efec,
    v21 = v21m03$efec
)
efecm00 <- data.frame(
    #v91 = v91m00$efec,
    v94 = v94m00$efec,
    v97 = v97m00$efec,
    v00 = v00m  $efec,
    v03 = v03m00$efec,
    v06 = v06m00$efec,
    v09 = v09m00$efec,
    v12 = v12m00$efec,
    v15 = v15m00$efec,
    v18 = v18m00$efec,
    v21 = v21m00$efec
)
efecm97 <- data.frame(
    #v91 = v91m97$efec,
    v94 = v94m97$efec,
    v97 = v97m  $efec,
    v00 = v00m97$efec,
    v03 = v03m97$efec,
    v06 = v06m97$efec,
    v09 = v09m97$efec,
    v12 = v12m97$efec,
    v15 = v15m97$efec,
    v18 = v18m97$efec,
    v21 = v21m97$efec
)
efecm94 <- data.frame(
    #v91 = v91m94$efec,
    v94 = v94m  $efec,
    v97 = v97m94$efec,
    v00 = v00m94$efec,
    v03 = v03m94$efec,
    v06 = v06m94$efec,
    v09 = v09m94$efec,
    v12 = v12m94$efec,
    v15 = v15m94$efec,
    v18 = v18m94$efec,
    v21 = v21m94$efec
)
## efecm91 <- data.frame(
##     v91 = v91m  $efec,
##     v94 = v94m91$efec,
##     v97 = v97m91$efec,
##     v00 = v00m91$efec,
##     v03 = v03m91$efec,
##     v06 = v06m91$efec,
##     v09 = v09m91$efec,
##     v12 = v12m91$efec,
##     v15 = v15m91$efec,
##     v18 = v18m91$efec,
##     v21 = v21m91$efec
## )
#
lisnomm21 <- data.frame(
    #v91 = v91m21$lisnom,
    v94 = v94m21$lisnom,
    v97 = v97m21$lisnom,
    v00 = v00m21$lisnom,
    v03 = v03m21$lisnom,
    v06 = v06m21$lisnom,
    v09 = v09m21$lisnom,
    v12 = v12m21$lisnom,
    v15 = v15m21$lisnom,
    v18 = v18m21$lisnom,
    v21 = v21m  $lisnom
)
lisnomm18 <- data.frame(
    #v91 = v91m18$lisnom,
    v94 = v94m18$lisnom,
    v97 = v97m18$lisnom,
    v00 = v00m18$lisnom,
    v03 = v03m18$lisnom,
    v06 = v06m18$lisnom,
    v09 = v09m18$lisnom,
    v12 = v12m18$lisnom,
    v15 = v15m18$lisnom,
    v18 = v18m  $lisnom,
    v21 = v21m18$lisnom
)
lisnomm15 <- data.frame(
    #v91 = v91m15$lisnom,
    v94 = v94m15$lisnom,
    v97 = v97m15$lisnom,
    v00 = v00m15$lisnom,
    v03 = v03m15$lisnom,
    v06 = v06m15$lisnom,
    v09 = v09m15$lisnom,
    v12 = v12m15$lisnom,
    v15 = v15m  $lisnom,
    v18 = v18m15$lisnom,
    v21 = v21m15$lisnom
)
lisnomm12 <- data.frame(
    #v91 = v91m12$lisnom,
    v94 = v94m12$lisnom,
    v97 = v97m12$lisnom,
    v00 = v00m12$lisnom,
    v03 = v03m12$lisnom,
    v06 = v06m12$lisnom,
    v09 = v09m12$lisnom,
    v12 = v12m  $lisnom,
    v15 = v15m12$lisnom,
    v18 = v18m12$lisnom,
    v21 = v21m12$lisnom
)
lisnomm09 <- data.frame(
    #v91 = v91m09$lisnom,
    v94 = v94m09$lisnom,
    v97 = v97m09$lisnom,
    v00 = v00m09$lisnom,
    v03 = v03m09$lisnom,
    v06 = v06m09$lisnom,
    v09 = v09m  $lisnom,
    v12 = v12m09$lisnom,
    v15 = v15m09$lisnom,
    v18 = v18m09$lisnom,
    v21 = v21m09$lisnom
)
lisnomm06 <- data.frame(
    #v91 = v91m06$lisnom,
    v94 = v94m06$lisnom,
    v97 = v97m06$lisnom,
    v00 = v00m06$lisnom,
    v03 = v03m06$lisnom,
    v06 = v06m  $lisnom,
    v09 = v09m06$lisnom,
    v12 = v12m06$lisnom,
    v15 = v15m06$lisnom,
    v18 = v18m06$lisnom,
    v21 = v21m06$lisnom
)
lisnomm03 <- data.frame(
    #v91 = v91m03$lisnom,
    v94 = v94m03$lisnom,
    v97 = v97m03$lisnom,
    v00 = v00m03$lisnom,
    v03 = v03m  $lisnom,
    v06 = v06m03$lisnom,
    v09 = v09m03$lisnom,
    v12 = v12m03$lisnom,
    v15 = v15m03$lisnom,
    v18 = v18m03$lisnom,
    v21 = v21m03$lisnom
)
lisnomm00 <- data.frame(
    #v91 = v91m00$lisnom,
    v94 = v94m00$lisnom,
    v97 = v97m00$lisnom,
    v00 = v00m  $lisnom,
    v03 = v03m00$lisnom,
    v06 = v06m00$lisnom,
    v09 = v09m00$lisnom,
    v12 = v12m00$lisnom,
    v15 = v15m00$lisnom,
    v18 = v18m00$lisnom,
    v21 = v21m00$lisnom
)
lisnomm97 <- data.frame(
    #v91 = v91m97$lisnom,
    v94 = v94m97$lisnom,
    v97 = v97m  $lisnom,
    v00 = v00m97$lisnom,
    v03 = v03m97$lisnom,
    v06 = v06m97$lisnom,
    v09 = v09m97$lisnom,
    v12 = v12m97$lisnom,
    v15 = v15m97$lisnom,
    v18 = v18m97$lisnom,
    v21 = v21m97$lisnom
)
lisnomm94 <- data.frame(
    #v91 = v91m94$lisnom,
    v94 = v94m  $lisnom,
    v97 = v97m94$lisnom,
    v00 = v00m94$lisnom,
    v03 = v03m94$lisnom,
    v06 = v06m94$lisnom,
    v09 = v09m94$lisnom,
    v12 = v12m94$lisnom,
    v15 = v15m94$lisnom,
    v18 = v18m94$lisnom,
    v21 = v21m94$lisnom
)
## lisnomm91 <- data.frame(
##     v91 = v91m  $lisnom,
##     v94 = v94m91$lisnom,
##     v97 = v97m91$lisnom,
##     v00 = v00m91$lisnom,
##     v03 = v03m91$lisnom,
##     v06 = v06m91$lisnom,
##     v09 = v09m91$lisnom,
##     v12 = v12m91$lisnom,
##     v15 = v15m91$lisnom,
##     v18 = v18m91$lisnom,
##     v21 = v21m91$lisnom
## )
#

# transpose to plug columns (units) into new data.frames
panm21    <- t(panm21)
panm18    <- t(panm18)
panm15    <- t(panm15)
panm12    <- t(panm12)
panm09    <- t(panm09)
panm06    <- t(panm06)
panm03    <- t(panm03)
panm00    <- t(panm00)
panm97    <- t(panm97)
panm94    <- t(panm94)
## panm91    <- t(panm91)
#
prim21    <- t(prim21)
prim18    <- t(prim18)
prim15    <- t(prim15)
prim12    <- t(prim12)
prim09    <- t(prim09)
prim06    <- t(prim06)
prim03    <- t(prim03)
prim00    <- t(prim00)
prim97    <- t(prim97)
prim94    <- t(prim94)
## prim91    <- t(prim91)
#
leftm21    <- t(leftm21)
leftm18    <- t(leftm18)
leftm15    <- t(leftm15)
leftm12    <- t(leftm12)
leftm09    <- t(leftm09)
leftm06    <- t(leftm06)
leftm03    <- t(leftm03)
leftm00    <- t(leftm00)
leftm97    <- t(leftm97)
leftm94    <- t(leftm94)
## leftm91    <- t(leftm91)
#
othm21    <- t(othm21)
othm18    <- t(othm18)
othm15    <- t(othm15)
othm12    <- t(othm12)
othm09    <- t(othm09)
othm06    <- t(othm06)
othm03    <- t(othm03)
othm00    <- t(othm00)
othm97    <- t(othm97)
othm94    <- t(othm94)
## othm91    <- t(othm91)
#
efecm21    <- t(efecm21)
efecm18    <- t(efecm18)
efecm15    <- t(efecm15)
efecm12    <- t(efecm12)
efecm09    <- t(efecm09)
efecm06    <- t(efecm06)
efecm03    <- t(efecm03)
efecm00    <- t(efecm00)
efecm97    <- t(efecm97)
efecm94    <- t(efecm94)
## efecm91    <- t(efecm91)
#
lisnomm21    <- t(lisnomm21)
lisnomm18    <- t(lisnomm18)
lisnomm15    <- t(lisnomm15)
lisnomm12    <- t(lisnomm12)
lisnomm09    <- t(lisnomm09)
lisnomm06    <- t(lisnomm06)
lisnomm03    <- t(lisnomm03)
lisnomm00    <- t(lisnomm00)
lisnomm97    <- t(lisnomm97)
lisnomm94    <- t(lisnomm94)
## lisnomm91    <- t(lisnomm91)
#

#################################################################
## extendCoal.. will receive data for regressions, one per map ##
#################################################################
tmp <-     as.list(rep(NA, nmun)) # empty list will receive one data.frame per unit
names(tmp) <- v00m$ife
#
extendCoalm21 <- extendCoalm18 <- 
extendCoalm15 <- extendCoalm12 <- extendCoalm09 <- extendCoalm06 <- 
extendCoalm03 <- extendCoalm00 <- extendCoalm97 <- 
extendCoalm94 <-
##extendCoalm91 <- 
    tmp
rm(tmp)

## if (agg=="s"){
##     extendCoal <- as.list(rep(NA, nrow(v00s))) # empty list will receive one data.frame per unit
##     names(extendCoal) <- v00s$edon*10000 + v00s$seccion # untested
## }

# loop over municipios
for (i in 1:nmun){
    #i <- 81 # debug
    message(sprintf("loop %s of %s", i, nmun))
    #########################
    ## votes with 2021 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm21[,i],
                      pri    = prim21[,i],
                      left   = leftm21[,i],
                      oth    = othm21[,i],
                      efec   = efecm21[,i],
                      lisnom = lisnomm21[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm21[[i]] <- tmp
    # name list object
    names(extendCoalm21)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 2018 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm18[,i],
                      pri    = prim18[,i],
                      left   = leftm18[,i],
                      oth    = othm18[,i],
                      efec   = efecm18[,i],
                      lisnom = lisnomm18[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm18[[i]] <- tmp
    # name list object
    names(extendCoalm18)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 2015 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm15[,i],
                      pri    = prim15[,i],
                      left   = leftm15[,i],
                      oth    = othm15[,i],
                      efec   = efecm15[,i],
                      lisnom = lisnomm15[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm15[[i]] <- tmp
    # name list object
    names(extendCoalm15)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 2012 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm12[,i],
                      pri    = prim12[,i],
                      left   = leftm12[,i],
                      oth    = othm12[,i],
                      efec   = efecm12[,i],
                      lisnom = lisnomm12[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm12[[i]] <- tmp
    # name list object
    names(extendCoalm12)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 2009 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm09[,i],
                      pri    = prim09[,i],
                      left   = leftm09[,i],
                      oth    = othm09[,i],
                      efec   = efecm09[,i],
                      lisnom = lisnomm09[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm09[[i]] <- tmp
    # name list object
    names(extendCoalm09)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 2006 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm06[,i],
                      pri    = prim06[,i],
                      left   = leftm06[,i],
                      oth    = othm06[,i],
                      efec   = efecm06[,i],
                      lisnom = lisnomm06[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm06[[i]] <- tmp
    # name list object
    names(extendCoalm06)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 2003 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm03[,i],
                      pri    = prim03[,i],
                      left   = leftm03[,i],
                      oth    = othm03[,i],
                      efec   = efecm03[,i],
                      lisnom = lisnomm03[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm03[[i]] <- tmp
    # name list object
    names(extendCoalm03)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 2000 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm00[,i],
                      pri    = prim00[,i],
                      left   = leftm00[,i],
                      oth    = othm00[,i],
                      efec   = efecm00[,i],
                      lisnom = lisnomm00[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm00[[i]] <- tmp
    # name list object
    names(extendCoalm00)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 1997 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm97[,i],
                      pri    = prim97[,i],
                      left   = leftm97[,i],
                      oth    = othm97[,i],
                      efec   = efecm97[,i],
                      lisnom = lisnomm97[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm97[[i]] <- tmp
    # name list object
    names(extendCoalm97)[i] <- tmp$ife[1]
    #
    #########################
    ## votes with 1994 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = panm94[,i],
                      pri    = prim94[,i],
                      left   = leftm94[,i],
                      oth    = othm94[,i],
                      efec   = efecm94[,i],
                      lisnom = lisnomm94[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$ife    <- v00m$ife[i]
    # fill info to new list
    extendCoalm94[[i]] <- tmp
    # name list object
    names(extendCoalm94)[i] <- tmp$ife[1]
    #
    ## #########################
    ## ## votes with map 1991 ##
    ## #########################
    ## tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
    ##                   pan    = panm91[,i],
    ##                   pri    = prim91[,i],
    ##                   left   = leftm91[,i],
    ##                   oth    = othm91[,i],
    ##                   efec   = efecm91[,i],
    ##                   lisnom = lisnomm91[,i])
    ## # replace NAs with period's mean
    ## if (length(tmp[is.na(tmp)])>0){
    ##     per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
    ##     tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
    ##     tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
    ##     tmp$left  [is.na(tmp$left)] <- per.means["left"];
    ##     tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
    ##     tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
    ##     tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    ## }
    ## # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    ## if (length(tmp[tmp==0])>0){
    ##     tmp[tmp==0] <- 0.001;
    ## }
    ## # re-compute shares to add to 1
    ## tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    ## # add id
    ## tmp$ife    <- v00m$ife[i]
    ## # fill info to new list
    ## extendCoalm91[[i]] <- tmp
    ## # name list object
    ## names(extendCoalm91)[i] <- tmp$ife[1]
}
    
##################################
## datos para regresión de alfa ##
##################################
#
#############################################################################################################
## Nota 16jul2021: al añadir datos 2021 cambiarán todas las alfas y betas! Una solución fácil (que usaré)  ##
## y otra que requiere más coco. La fácil es reestimar con 2021 e identificar el commit que reportaba la   ##
## versión hasta 2018. La otra: definir una ventana temporal (como cinco elecciones) para producir alfas   ##
## y betas cada elección: alfa.2006, alfa.2009, etc. Debería poder investigar cómo usan esto en el Capital ##
## Asset Pricing Model...                                                                                  ##
#############################################################################################################
yr.means <- data.frame(yr = seq(1991,2021,3), # 11 election-years
                       pan    = rep(NA,11),
                       pri    = rep(NA,11),
                       left   = rep(NA,11),
                       oth    = rep(NA,11))
# function to sum numeric columns
if (agg=="s"){
    cs <- function(x) colSums(x[x$dunbaja==0,], na.rm=TRUE) # drops secciones that received aggregates upon splitting
} else {
    cs <- function(x){
        sel.nums <- unlist(lapply(x, is.numeric), use.names = FALSE) # selects only numeric columns in data frame
        res <- colSums(x[,sel.nums], na.rm=TRUE)
        return(res)
    }
}
#
# compute national mean vote
yr.means$pan   [1] <-  cs(v91s)["pan"]                                                         / cs(v91s)["efec"]
yr.means$pri   [1] <-  cs(v91s)["pri"]                                                         / cs(v91s)["efec"]
yr.means$left  [1] <-  cs(v91s)["prd"]                                                         / cs(v91s)["efec"]
yr.means$oth   [1] <- (cs(v91s)["efec"] - cs(v91s)["pan"] - cs(v91s)["pri"] - cs(v91s)["prd"]) / cs(v91s)["efec"]
#
yr.means$pan   [2] <-  cs(v94s)["pan"]                                                         / cs(v94s)["efec"]
yr.means$pri   [2] <-  cs(v94s)["pri"]                                                         / cs(v94s)["efec"]
yr.means$left  [2] <-  cs(v94s)["prd"]                                                         / cs(v94s)["efec"]
yr.means$oth   [2] <- (cs(v94s)["efec"] - cs(v94s)["pan"] - cs(v94s)["pri"] - cs(v94s)["prd"]) / cs(v94s)["efec"]
#                
yr.means$pan   [3] <-  cs(v97s)["pan"]                                                         / cs(v97s)["efec"]
yr.means$pri   [3] <-  cs(v97s)["pri"]                                                         / cs(v97s)["efec"]
yr.means$left  [3] <-  cs(v97s)["prd"]                                                         / cs(v97s)["efec"]
yr.means$oth   [3] <- (cs(v97s)["efec"] - cs(v97s)["pan"] - cs(v97s)["pri"] - cs(v97s)["prd"]) / cs(v97s)["efec"]
#                
yr.means$pan   [4] <-  cs(v00s)["panc"]                    / cs(v00s)["efec"]
yr.means$pri   [4] <-  cs(v00s)["pri"]                     / cs(v00s)["efec"]
yr.means$left  [4] <-  cs(v00s)["prdc"]                    / cs(v00s)["efec"]
yr.means$oth   [4] <- (cs(v00s)["pcd"] + cs(v00s)["parm"]) / cs(v00s)["efec"]
#                
yr.means$pan   [5] <-  cs(v03s)["pan"]                                                                         / cs(v03s)["efec"]
yr.means$pri   [5] <- (cs(v03s)["pri"] + cs(v03s)["pric"] + cs(v03s)["pvem"])                                  / cs(v03s)["efec"]
yr.means$left  [5] <- (cs(v03s)["prd"] + cs(v03s)["pt"]   + cs(v03s)["conve"])                                 / cs(v03s)["efec"]
yr.means$oth   [5] <- (cs(v03s)["psn"] + cs(v03s)["pas"]  + cs(v03s)["mp"] + cs(v03s)["plm"] + cs(v03s)["fc"]) / cs(v03s)["efec"]
#                
yr.means$pan   [6] <-  cs(v06s)["pan"]                     / cs(v06s)["efec"]
yr.means$pri   [6] <-  cs(v06s)["pric"]                    / cs(v06s)["efec"]
yr.means$left  [6] <-  cs(v06s)["prdc"]                    / cs(v06s)["efec"]
yr.means$oth   [6] <- (cs(v06s)["pna"] + cs(v06s)["asdc"]) / cs(v06s)["efec"]
#                
yr.means$pan   [7] <-  cs(v09s)["pan"]                                                           / cs(v09s)["efec"]
yr.means$pri   [7] <- (cs(v09s)["pri"] + cs(v09s)["pric"] + cs(v09s)["pvem"])                    / cs(v09s)["efec"]
yr.means$left  [7] <- (cs(v09s)["prd"] + cs(v09s)["pt"]   + cs(v09s)["ptc"] + cs(v09s)["conve"]) / cs(v09s)["efec"]
yr.means$oth   [7] <- (cs(v09s)["pna"] + cs(v09s)["psd"])                                        / cs(v09s)["efec"]
#                
yr.means$pan   [8] <-  cs(v12s)["pan"]                                                       / cs(v12s)["efec"]
yr.means$pri   [8] <- (cs(v12s)["pri"] + cs(v12s)["pric"] + cs(v12s)["pvem"])                / cs(v12s)["efec"]
yr.means$left  [8] <- (cs(v12s)["prd"] + cs(v12s)["prdc"] + cs(v12s)["pt"] + cs(v12s)["mc"]) / cs(v12s)["efec"]
yr.means$oth   [8] <-  cs(v12s)["pna"]                                                       / cs(v12s)["efec"]
#                
yr.means$pan   [9] <-  cs(v15s)["pan"]                                                                                / cs(v15s)["efec"]
yr.means$pri   [9] <- (cs(v15s)["pri"] + cs(v15s)["pric"] + cs(v15s)["pvem"])                                         / cs(v15s)["efec"]
yr.means$left  [9] <- (cs(v15s)["prd"] + cs(v15s)["prdc"] + cs(v15s)["pt"] + cs(v15s)["morena"] + cs(v15s)["pes"])    / cs(v15s)["efec"]
yr.means$oth   [9] <- (cs(v15s)["mc"]  + cs(v15s)["pna"]  + cs(v15s)["ph"] + cs(v15s)["indep1"] + cs(v15s)["indep2"]) / cs(v15s)["efec"]
#
yr.means$pan   [10] <- (cs(v18s)["pan"]    + cs(v18s)["panc"]    + cs(v18s)["prd"]  + cs(v18s)["mc"])  / cs(v18s)["efec"]
yr.means$pri   [10] <- (cs(v18s)["pri"]    + cs(v18s)["pric"]    + cs(v18s)["pvem"] + cs(v18s)["pna"]) / cs(v18s)["efec"]
yr.means$left  [10] <- (cs(v18s)["morena"] + cs(v18s)["morenac"] + cs(v18s)["pt"]   + cs(v18s)["pes"]) / cs(v18s)["efec"]
yr.means$oth   [10] <- (cs(v18s)["indep1"] + cs(v18s)["indep2"])                                       / cs(v18s)["efec"]
#
yr.means$pan   [11] <- (cs(v21s)["pan"]    + cs(v21s)["panc"]    + cs(v21s)["prd"])                                       / cs(v21s)["efec"]
yr.means$pri   [11] <-  cs(v21s)["pri"]                                                                                   / cs(v21s)["efec"] # dropped cs(v21s)["pric"]
yr.means$left  [11] <- (cs(v21s)["morena"] + cs(v21s)["morenac"] + cs(v21s)["pt"]  + cs(v21s)["pvem"])                    / cs(v21s)["efec"]
yr.means$oth   [11] <- (cs(v21s)["mc"]     + cs(v21s)["pes"]     + cs(v21s)["rsp"] + cs(v21s)["fxm"] + cs(v21s)["indep"]) / cs(v21s)["efec"]
#
yr.means <- within(yr.means, mean.rpan    <- pan  / pri)
yr.means <- within(yr.means, mean.rleft   <- left / pri)
yr.means <- within(yr.means, mean.roth    <- oth  / pri)
#
yr.means[,2:8] <- round(yr.means[,2:8], 3)
#
# plug into data
for (i in 1:nrow(v00d)){
    #i <- 2 # debug
    extendCoald18[[i]] <- cbind(extendCoald18[[i]], yr.means[,6:8])
    extendCoald06[[i]] <- cbind(extendCoald06[[i]], yr.means[,6:8])
    extendCoald97[[i]] <- cbind(extendCoald97[[i]], yr.means[,6:8])
    extendCoald79[[i]] <- cbind(extendCoald79[[i]], yr.means[,6:8])
}


#################################################################################################
## - should also try jags estimation to get post-sample of vhats and alphas                    ##
## - report mg effect of unit change in bar(v) at year's level instead of betahat (cf. Linzer) ##
#################################################################################################
#
###############################
## código de las regresiones ##
###############################
vhat.2024 <-                 # <--- OJO 19abr21: assumes no redistricting in 2024 change when 2024 map available
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
#vhat.2003 <- vhat.2000 <- vhat.1997 <- vhat.1994 <-
vhat.1991 <- vhat.1988 <-
    data.frame(pan  = rep(NA, nrow(v00d)),
               pri  = rep(NA, nrow(v00d)),
               left = rep(NA, nrow(v00d))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00d)),
                       pri    = rep(NA, nrow(v00d)),
                       left   = rep(NA, nrow(v00d))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00d)),
                      left   = rep(NA, nrow(v00d)),
                      oth    = rep(NA, nrow(v00d))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00d))) # empty list will receive one time-series
                                   # regression per municipio, each used to
                                   # predict votes in 2006:2021
# add names to m and s (to d must be done yearly basis due to redistricting)
if (agg=="m") names(tmp) <- v00d$ife
if (agg=="s") names(tmp) <- v00d$edon*10000 + v00d$seccion # untested
if (agg=="d"){
    tmp18 <- tmp06 <- tmp97 <- tmp79 <- tmp # each map will receive district names
    names(tmp79) <- v94d$disn
    names(tmp97) <- v97d$disn
    names(tmp06) <- v06d$disn
    names(tmp18) <- v18d$disn
}
#
if (agg=="d"){
    regs.1988 <- regs.1991 <-
        list(pan    = tmp79,
             left   = tmp79,
             oth    = tmp79,
             readme = "No pri regs because DVs are pri-ratios")
    regs.2006 <- regs.2009 <- regs.2012 <- regs.2015 <- 
        list(pan    = tmp06,
             left   = tmp06,
             oth    = tmp06,
             readme = "No pri regs because DVs are pri-ratios")
    regs.2018 <- regs.2021 <- regs.2024 <-  
        list(pan    = tmp18,
             left   = tmp18,
             oth    = tmp18,
             readme = "No pri regs because DVs are pri-ratios")
    # for district, one mean.reg per map
    mean.regs.d79 <-
        list(pan    = tmp79,
             left   = tmp79,
             oth    = tmp79,
             readme = "No pri regs bec DVs are pri-ratios")
    mean.regs.d97 <-
        list(pan    = tmp97,
             left   = tmp97,
             oth    = tmp97,
             readme = "No pri regs bec DVs are pri-ratios")
    mean.regs.d06 <-
        list(pan    = tmp06,
             left   = tmp06,
             oth    = tmp06,
             readme = "No pri regs bec DVs are pri-ratios")
    mean.regs.d18 <-
        list(pan    = tmp18,
             left   = tmp18,
             oth    = tmp18,
             readme = "No pri regs bec DVs are pri-ratios")
} else {
    regs.2006 <- regs.2009 <- regs.2012 <- regs.2015 <- regs.2018 <- regs.2021 <- 
        list(pan    = tmp,
             left   = tmp,
             oth    = tmp,
             readme = "No pri regs because DVs are pri-ratios");
    mean.regs <-
        list(pan    = tmp,
             left   = tmp,
             oth    = tmp,
             readme = "No pri regs bec DVs are pri-ratios")
}
rm(tmp,tmp79,tmp97,tmp06,tmp18)

#
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoald06, sum)
non.nas <- unlist(non.nas)
table(is.na(non.nas))
non.nas                     # debug
extendCoald18[[206]]           # debug: 20jul2021 NA due to unreported sole sección in cps municipio
which(is.na(non.nas)==TRUE) # debug
non.nas <- which(is.na(non.nas)==FALSE)
length(non.nas)
#    

###############################################################
## District 5-yr estimates that can be computed before 2024  ##
## |      | map    |        |        |       |               ##
## | vhat | 1979   | 1997   | 2006   | 2018  |               ##
## |------+--------+--------+--------+-------|               ##
## | 1988 | *back* |   XX   |   XX   |   XX  |               ##
## | 1991 | *back* |  back  |  back  |  back |               ##
## | 1994 | *back* |  back  |  back  |  back |               ##
## | 1997 |  back  | *back* |  back  |  back |               ##
## | 2000 |  back  | *back* |  back  |  back |               ##
## | 2003 |  BOTH  | *back* |  back  |  back |               ##
## | 2006 |  BOTH  |  back  | *back* |  back |               ##
## | 2009 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2012 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2015 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2018 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## | 2021 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## | 2024 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## Notes:                                                    ##
## - Starred are needed                                      ##
## - get 2006 estimates backwards                            ##
## - use 1979 BOTH to compare 2003 and 2006 fwd/backwd vhats ##
###############################################################

wrap district estimations in a function.
It takes the map to use as argument, choosing which election-years to estimate by default.
Setting dbackward=1 performs reverse estimation with future, not past, elections.
Tweaking object yrs.to.estimate should allow to specify non-default estimation years.

# district estimates are contingent on chosen map
sel.map <- c(1979,1997,2006,2018)[2]
#
#######################################################################
## read 1988dfdf from external file to add votes when using 1979 map ##
#######################################################################
if (sel.map==1979){
    tmp <- read.csv(file="../dfdf1979-on.csv")
    tmp <- tmp[tmp$yr==1988,]
    tmp.v <- tmp[, grep("^v[0-9]{2}$", colnames(tmp))]
    tmp.l <- tmp[, grep("^l[0-9]{2}$", colnames(tmp))]
    tmp$efec <- rowSums(tmp.v, na.rm=TRUE)               # recompute efec
    tmp.v <- round(tmp.v / tmp$efec, 3)                  # shares
    sel.c <- apply(tmp.l, 1, function(x) which(x=="pan"))
    tmp$pan <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
    sel.c <- apply(tmp.l, 1, function(x) which(x=="pri"))
    tmp$pri <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
    tmp$left <- NA # left will sum fdn (not prt)
    sel.c <- apply(tmp.l, 1, function(x) which(x=="pdm"))
    tmp$oth <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
    sel.c <- apply(tmp.l, 1, function(x) which(x=="prt"))
    tmp$oth2 <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
    tmp$oth <- tmp$oth+tmp$oth2; tmp$oth2 <- NULL 
    tmp <- within(tmp, left <- 1 - pan - pri - oth)
    tmp <- tmp[,c("disn","pan","pri","left","oth","efec","lisnom")]
    tmp.1988 <- tmp
    rm(tmp,sel.c,tmp.v,tmp.l)
    #
    # if map is 1979 will estimate 1988 backwards, add row in data frames
    add1988 <- function(x) rbind(v88=c(1988,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), x)
    tmp <- extendCoald79 # duplicate for manipulation
    ## OJO: should replace NAs above with 1988 district returns in file
    tmp <- lapply(extendCoald79, add1988) # add row for 1988 to each data frame in list
    # add corresponding 1988 shares
    table(as.numeric(names(tmp))==tmp.1988$disn) # check items in same order 
    data.frame(ext=as.numeric(names(tmp)), v88=tmp.1988$disn) # check items in same order 
    for (i in 1:300){
        tmp[[i]][1,c("pan","pri","left","oth","efec","lisnom","disn")] <-
            tmp.1988[i,c("pan","pri","left","oth","efec","lisnom","disn")] # fill 1st row w ith obs
    }
    extendCoald79 <- tmp
    rm(add1988,tmp,tmp.1988)
}

for (i in non.nas){
    #i <- 81 # debug
    #i <- 44508 # debug
    message(sprintf("loop %s of %s", i, max(non.nas)))
    # subset data to single unit
if (sel.map==1979) data.tmp <- extendCoald79[[i]]
if (sel.map==1997) data.tmp <- extendCoald97[[i]]
if (sel.map==2006) data.tmp <- extendCoald06[[i]]
if (sel.map==2018) data.tmp <- extendCoald18[[i]]
    #data.tmp <- extendCoal[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA, data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA, data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA, data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ############################################
    ## backwards-predict 1988 with next 5 els ##
    ############################################
    if (sel.map==1979){
    year <- 1988
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.1988[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1988$pan [[i]]   <- reg.pan
    regs.1988$left[[i]]   <- reg.left
    regs.1988$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ############################################
    ## backwards-predict 1991 with next 5 els ##
    ############################################
    if (sel.map==1979){
    year <- 1991
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.1991[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1991$pan [[i]]   <- reg.pan
    regs.1991$left[[i]]   <- reg.left
    regs.1991$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ## ############################################
    ## ## backwards-predict 1994 with next 5 els ##
    ## ############################################
    ## if (sel.map==1979){
    ## year <- 1994
    ## reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## #
    ## new.d <- data.frame(yr = year)
    ## rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    ## rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    ## rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    ## vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    ## bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    ## #
    ## ## plug into results objects ##
    ## vhat.1994[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    ## regs.1994$pan [[i]]   <- reg.pan
    ## regs.1994$left[[i]]   <- reg.left
    ## regs.1994$oth [[i]]   <- reg.oth
    ## #
    ## if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    ## if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    ## if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    ## if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    ## if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    ## #
    ## data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    ## data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    ## data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    ## data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    ## data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    ## }
    ## #
    ## ############################################
    ## ## backwards-predict 1997 with next 5 els ##
    ## ############################################
    ## if (sel.map==1997){
    ## year <- 1997
    ## reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## #
    ## new.d <- data.frame(yr = year)
    ## rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    ## rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    ## rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    ## vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    ## bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    ## #
    ## ## plug into results objects ##
    ## vhat.1997[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    ## regs.1997$pan [[i]]   <- reg.pan
    ## regs.1997$left[[i]]   <- reg.left
    ## regs.1997$oth [[i]]   <- reg.oth
    ## #
    ## if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    ## if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    ## if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    ## if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    ## if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    ## #
    ## data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    ## data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    ## data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    ## data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    ## data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    ## }
    ## #
    ## ############################################
    ## ## backwards-predict 2000 with next 5 els ##
    ## ############################################
    ## if (sel.map==1997){
    ## year <- 2000
    ## reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## #
    ## new.d <- data.frame(yr = year)
    ## rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    ## rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    ## rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    ## vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    ## bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    ## #
    ## ## plug into results objects ##
    ## vhat.2000[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    ## regs.2000$pan [[i]]   <- reg.pan
    ## regs.2000$left[[i]]   <- reg.left
    ## regs.2000$oth [[i]]   <- reg.oth
    ## #
    ## if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    ## if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    ## if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    ## if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    ## if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    ## #
    ## data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    ## data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    ## data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    ## data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    ## data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    ## }
    ## #
    ## ############################################
    ## ## backwards-predict 2003 with next 5 els ##
    ## ############################################
    ## if (sel.map==1997){
    ## year <- 2003
    ## reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    ## #
    ## new.d <- data.frame(yr = year)
    ## rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    ## rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    ## rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    ## vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    ## bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    ## bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    ## #
    ## ## plug into results objects ##
    ## vhat.2003[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    ## regs.2003$pan [[i]]   <- reg.pan
    ## regs.2003$left[[i]]   <- reg.left
    ## regs.2003$oth [[i]]   <- reg.oth
    ## #
    ## if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    ## if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    ## if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    ## if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    ## if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    ## #
    ## data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    ## data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    ## data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    ## data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    ## data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    ## }
    #
    ############################################
    ## backwards-predict 2006 with next 5 els ##
    ############################################
    if (sel.map==2006){
    year <- 2006
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2006[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006$pan [[i]]   <- reg.pan
    regs.2006$left[[i]]   <- reg.left
    regs.2006$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    if (sel.map==2006){
    year <- 2009
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2009[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2009$pan [[i]]   <- reg.pan
    regs.2009$left[[i]]   <- reg.left
    regs.2009$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    if (sel.map==2006){
    year <- 2012
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2012[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2012$pan [[i]]   <- reg.pan
    regs.2012$left[[i]]   <- reg.left
    regs.2012$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    if (sel.map==2006){
    year <- 2015
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2015[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2015$pan [[i]]   <- reg.pan
    regs.2015$left[[i]]   <- reg.left
    regs.2015$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    if (sel.map==2018){
    year <- 2018
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2018[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2018$pan [[i]]   <- reg.pan
    regs.2018$left[[i]]   <- reg.left
    regs.2018$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    if (sel.map==2018){
    year <- 2021
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2021[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2021$pan [[i]]   <- reg.pan
    regs.2021$left[[i]]   <- reg.left
    regs.2021$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2024 with last 5 els ##
    ##################################
    if (sel.map==2018){
    year <- 2024
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2024[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2024$pan [[i]]   <- reg.pan
    regs.2024$left[[i]]   <- reg.left
    regs.2024$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan   <-  lm(formula = log(pan /pri)  ~  mean.rpan,  data = data.tmp)
    reg.left  <-  lm(formula = log(left/pri)  ~  mean.rleft, data = data.tmp)
    reg.oth   <-  lm(formula = log(oth /pri)  ~  mean.roth,  data = data.tmp)
    #
    # point prediction alpha with mean at zero 
    new.d <- data.frame(mean.rpan = 0)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    new.d <- data.frame(mean.rleft   = 0)
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    new.d <- data.frame(mean.roth = 0)
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    #
    #c(vhat.pan, vhat.pri, vhat.left, 1-vhat.pan-vhat.pri-vhat.left)
    alphahat[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    betahat[i,1] <- coef(reg.pan)   [2]
    betahat[i,2] <- coef(reg.left  )[2]
    betahat[i,3] <- coef(reg.oth)   [2]
    #
    if (sel.map==1979){
       mean.regs.d79$pan   [[i]] <- reg.pan
       mean.regs.d79$left  [[i]] <- reg.left  
       mean.regs.d79$oth   [[i]] <- reg.oth
    }
    if (sel.map==1997){
       mean.regs.d97$pan   [[i]] <- reg.pan
       mean.regs.d97$left  [[i]] <- reg.left  
       mean.regs.d97$oth   [[i]] <- reg.oth
    }
    if (sel.map==2006){
       mean.regs.d06$pan   [[i]] <- reg.pan
       mean.regs.d06$left  [[i]] <- reg.left  
       mean.regs.d06$oth   [[i]] <- reg.oth
    }
    if (sel.map==2018){
       mean.regs.d18$pan   [[i]] <- reg.pan
       mean.regs.d18$left  [[i]] <- reg.left  
       mean.regs.d18$oth   [[i]] <- reg.oth
    }
    #mean.regs$pan   [[i]] <- reg.pan
    #mean.regs$left  [[i]] <- reg.left  
    #mean.regs$oth   [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left  <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan   <- alphahat$pan [i]
    data.tmp$alphahat.pri   <- alphahat$pri [i]
    data.tmp$alphahat.left  <- alphahat$left[i]
    data.tmp$betahat.pan    <- betahat$pan  [i]
    data.tmp$betahat.left   <- betahat$left [i]
    data.tmp$betahat.oth    <- betahat$oth  [i]
    data.tmp <- round(data.tmp,3)
    #
    ########################################################
    ## optional: plug vhats alphas betas back into data   ##
    ########################################################
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        oth <- NULL; # drop compositional vote complement
        betahat.oth <- NULL; # drop this beta
        #betahat.pan <- betahat.left   <- betahat.oth <- NULL; # drop betas
    })
    # return estimates to data object
if (sel.map==1979) extendCoald79[[i]] <- data.tmp
if (sel.map==1997) extendCoald97[[i]] <- data.tmp
if (sel.map==2006) extendCoald06[[i]] <- data.tmp
if (sel.map==2018) extendCoald18[[i]] <- data.tmp
    #extendCoal[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################

# clean, all this is saved in extendCoal, mean.regs, regs.2006, regs.2009, regs.2012, regs.2015, regs.2018
rm(alphahat, betahat, bhat.left, bhat.pan, reg.left, reg.oth, reg.pan, rhat.left, rhat.oth, rhat.pan, vhat.2006, vhat.2009, vhat.2012, vhat.2015, vhat.2018, vhat.2021, vhat.2024, vhat.left, vhat.pan, vhat.pri)


##################################################################
## ESTIMATE MANIPULATED MUNICIPAL REGRESSIONS (NEW MUN FIX)     ##
## AND MANIPULATE regs AND extendCoal OBJECTS WHERE APPROPRIATE ##
##################################################################
if (agg=="m"){
    source("code/code-to-run-counterfactual-mun-regs.r")
}

# restore
non.nas <- non.nas.orig; rm(non.nas.orig)

# tmp for debugging
#save.image("data/too-big-4-github/tmp4.RData")

rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(wd)
load("data/too-big-4-github/tmp4.RData")
ls()


# clean
rm(v91manip, v94manip, v97manip, v00manip, v03manip, v06manip, v09manip, v12manip, v15manip, v18manip,
   regs.2006manip, regs.2009manip, regs.2012manip, regs.2015manip, regs.2018manip, mean.regsmanip,
   regs.2006manip2, regs.2009manip2, regs.2012manip2, regs.2015manip2, regs.2018manip2, mean.regsmanip2,
   extendCoalmanip, extendCoalmanip2)
rm(v91,v94,v97,v00,v03,v06,v09,v12,v15,v18)
rm(pan,pri,left,oth,efec)
rm(sel,sel1,sel2,sel.to,sel.c,target.ife,i,tmp)

# adds manipulation indicator to all data frames in list
if (agg=="m") {
    extendCoal <- sapply(extendCoal, simplify = FALSE, function(x) {
        x$munchg <- 0;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg1994)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2006);
        x$munchg[sel] <- 1994;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg1997)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2006:2009);
        x$munchg[sel] <- 1997;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg2000)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2006:2012);
        x$munchg[sel] <- 2000;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg2003)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2006:2015);
        x$munchg[sel] <- 2003;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg2006)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2006:2018);
        x$munchg[sel] <- 2006;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg2009)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2009:2021);
        x$munchg[sel] <- 2009;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg2012)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2012:2024);
        x$munchg[sel] <- 2012;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg2015)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2015:2027);
        x$munchg[sel] <- 2015;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg2018)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2018:2030);
        x$munchg[sel] <- 2018;
        return(x)
    })
    sel2 <- which(names(extendCoal) %in% chg2021)
    extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
        sel <- which(x$yr %in% 2021:2033);
        x$munchg[sel] <- 2021;
        return(x)
    })
    ## sel2 <- which(names(extendCoal) %in% chg2024)
    ## extendCoal[sel2] <- sapply(extendCoal[sel2], simplify = FALSE, function(x) {
    ##     sel <- which(x$yr %in% 2024:2036);
    ##     x$munchg[sel] <- 2024;
    ##     return(x)
    ## })
}
if (agg=="s") { # will be manipulated below
    extendCoal <- sapply(extendCoal, simplify = FALSE, function(x) {
        x$new <- x$split <- 0;
        return(x)
    })
}

######################################################################
## manipulate split secciones (adding aggregate regression results) ##
######################################################################
if (agg=="s") {
    sel.split <- which(eq$action=="split")
    info <- eq[sel.split, c("edon","seccion","orig.dest","when")]
    info$edosecn <- info$edon*10000+info$seccion
    tmp <- v00s$edon*10000+v00s$seccion
    sel.from <- which(tmp %in% info$edosecn)
    #
    for (i in 1:length(sel.from)){
        #i <- 66 # debug
        message(sprintf("loop %s of %s", i, length(sel.from)))
        reg.from <- extendCoal[[sel.from[i]]] # counterfactual seccion with vhat alpha for aggregate post-split
        #
        # locate split's new secciones
        sel.to <- as.numeric(unlist(strsplit(info$orig.dest[i], ":")))
        sel.to <- seq(from = sel.to[1], to = sel.to[2], by = 1)
        sel.to <- v00s$edon[sel.from[i]] * 10000 + sel.to
        sel.to <- which(tmp %in% sel.to)
        #
        for (j in sel.to){ # loop over new secciones
            #j <- sel.to[5] # debug
            reg.to <- extendCoal[[j]]  # regression to manipulate
            year <- info$when[i]               # year reseccionamiento
            sel.na <- which(reg.to$yr <= year) # elections before reseccionamiento 
            reg.to[sel.na,] <- within(reg.to[sel.na,], {
                pan <- pri <- left <- d.pan <- d.pri <- d.left <- NA; # drop mean vote used, use NAs
            })
            # columns to manipulate
            sel.col <- c("vhat.pan", "vhat.pri", "vhat.left", "bhat.pan", "bhat.left",  "alphahat.pan", "alphahat.pri", "alphahat.left", "betahat.pan", "betahat.left")
            reg.to[,sel.col] <- reg.from[,sel.col] # from -> to
            # indicate manipulation
            reg.to$new[-sel.na] <- year
            # return manipulated data
            extendCoal[[j]] <- reg.to
        }
        # indicate manipulation
        reg.from$split[-sel.na] <- year
        # return manipulated data
        extendCoal[[sel.from[i]]] <- reg.from
    }
}

##########################################################################
## generate data frame with one year's predictions/estimates for export ##
##########################################################################
tmp.func <- function(year) {
    #year <- 2009         # debug
    #X <- extendCoal[[1]] # debug
    sel <- which(extendCoal[[1]]$yr==year) # which row reports year (symmetric in all other objects in list)
    # generate list with selected row only in every municipio
    tmp <- lapply(extendCoal, FUN = function(X) {
        prune <- X[sel,]
        return(prune)
    })
    # spot NAs in list
    tmp.sel <- setdiff(1:length(extendCoal), non.nas)
    # fill with same-dim NA data.frame
    tmp.manip <- tmp[[non.nas[1]]]
    tmp.manip[,-1] <- NA # all but 1st col (yr) to NA
    if (length(tmp.sel)>0) tmp[tmp.sel] <- lapply(tmp[tmp.sel], function(x) tmp.manip)
    # turn into one dataframe
    # table(summary(tmp)) # debug
    tmp <- do.call("rbind", tmp)
    rownames(tmp) <- NULL
    ## # next block seems redundant 2sep2020
    ## if (agg=="m") sel.col <- c("edon","ife","inegi")       # cols to merge when using municipios
    ## if (agg=="s") sel.col <- c("edon","seccion","edosecn","ife","inegi") # when using secciones
    ## tmp <- cbind(tmp, v00[,sel.col])
    ## rm(sel.col)
    return(tmp)
}

extendCoal.2006 <- tmp.func(year=2006)
extendCoal.2009 <- tmp.func(year=2009)
extendCoal.2012 <- tmp.func(year=2012)
extendCoal.2015 <- tmp.func(year=2015)
extendCoal.2018 <- tmp.func(year=2018)
extendCoal.2021 <- tmp.func(year=2021)
extendCoal.2024 <- tmp.func(year=2024)
#rm(extendCoal.2015) # clean memory

# plug inegi into data for export
tmp <- v21m[,c("ife","inegi")]
#dim(tmp); dim(extendCoal.2006) # debug
extendCoal.2006 <- merge(x = extendCoal.2006, y = tmp, by = "ife", all = TRUE)
extendCoal.2009 <- merge(x = extendCoal.2009, y = tmp, by = "ife", all = TRUE)
extendCoal.2012 <- merge(x = extendCoal.2012, y = tmp, by = "ife", all = TRUE)
extendCoal.2015 <- merge(x = extendCoal.2015, y = tmp, by = "ife", all = TRUE)
extendCoal.2018 <- merge(x = extendCoal.2018, y = tmp, by = "ife", all = TRUE)
extendCoal.2021 <- merge(x = extendCoal.2021, y = tmp, by = "ife", all = TRUE)
extendCoal.2024 <- merge(x = extendCoal.2024, y = tmp, by = "ife", all = TRUE)

# if missing ife code, that wrongly adds a rown with NAs, drop 
sel <- which(is.na(extendCoal.2006$ife))
if (length(sel)>0){
    extendCoal.2006 <- extendCoal.2006[-sel,];
    extendCoal.2009 <- extendCoal.2009[-sel,];
    extendCoal.2012 <- extendCoal.2012[-sel,];
    extendCoal.2015 <- extendCoal.2015[-sel,];
    extendCoal.2018 <- extendCoal.2018[-sel,];
    extendCoal.2021 <- extendCoal.2021[-sel,];
    extendCoal.2024 <- extendCoal.2024[-sel,];
}

# drop some columns
extendCoal.2006 <- within(extendCoal.2006, yr <- edosecn <- NULL)
extendCoal.2009 <- within(extendCoal.2009, yr <- edosecn <- NULL)
extendCoal.2012 <- within(extendCoal.2012, yr <- edosecn <- NULL)
extendCoal.2015 <- within(extendCoal.2015, yr <- edosecn <- NULL)
extendCoal.2018 <- within(extendCoal.2018, yr <- edosecn <- NULL)
extendCoal.2021 <- within(extendCoal.2021, yr <- edosecn <- NULL)
extendCoal.2024 <- within(extendCoal.2024, yr <- edosecn <- NULL)

# more cleaning
rm(add.split,cs,sel.split)
rm(info,new.d,non.nas,per.means,year)
rm(yr.means)
rm(tmp,data.tmp)


##################
## save to disk ##
##################
if (agg=="m") {
    write.csv(extendCoal.2006,
              file = paste(wd, "data/dipfed-municipio-vhat-2006.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2009,
              file = paste(wd, "data/dipfed-municipio-vhat-2009.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2012,
              file = paste(wd, "data/dipfed-municipio-vhat-2012.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2015,
              file = paste(wd, "data/dipfed-municipio-vhat-2015.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2018,
              file = paste(wd, "data/dipfed-municipio-vhat-2018.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2021,
              file = paste(wd, "data/dipfed-municipio-vhat-2021.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2024,
              file = paste(wd, "data/dipfed-municipio-vhat-2024.csv", sep = ""), row.names = FALSE)
}
if (agg=="s") {
    write.csv(extendCoal.2009,
              file = paste(wd, "data/dipfed-seccion-vhat-2009.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2012,
              file = paste(wd, "data/dipfed-seccion-vhat-2012.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2015,
              file = paste(wd, "data/dipfed-seccion-vhat-2015.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2018,
              file = paste(wd, "data/dipfed-seccion-vhat-2018.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2021,
              file = paste(wd, "data/dipfed-seccion-vhat-2021.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2024,
              file = paste(wd, "data/dipfed-seccion-vhat-2024.csv", sep = ""), row.names = FALSE)
}

# save municipal regression objects
save(mean.regs, file = paste(wd, "data/dipfed-municipio-mean-regs.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(regs.2006, file = paste(wd, "data/dipfed-municipio-regs-2006.RData", sep = ""), compress = "gzip")
save(regs.2009, file = paste(wd, "data/dipfed-municipio-regs-2009.RData", sep = ""), compress = "gzip")
save(regs.2012, file = paste(wd, "data/dipfed-municipio-regs-2012.RData", sep = ""), compress = "gzip")
save(regs.2015, file = paste(wd, "data/dipfed-municipio-regs-2015.RData", sep = ""), compress = "gzip")
save(regs.2018, file = paste(wd, "data/dipfed-municipio-regs-2018.RData", sep = ""), compress = "gzip")
save(regs.2021, file = paste(wd, "data/dipfed-municipio-regs-2021.RData", sep = ""), compress = "gzip")
save(regs.2024, file = paste(wd, "data/dipfed-municipio-regs-2024.RData", sep = ""), compress = "gzip")

# save sección regression objects
save(mean.regs, file = paste(wd, "data/too-big-4-github/dipfed-seccion-mean-regs.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(regs.2009, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2009.RData", sep = ""), compress = "gzip")
save(regs.2012, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2012.RData", sep = ""), compress = "gzip")
save(regs.2015, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2015.RData", sep = ""), compress = "gzip")
save(regs.2018, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2018.RData", sep = ""), compress = "gzip")
save(regs.2021, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2021.RData", sep = ""), compress = "gzip")

# load regression object
load(file = paste(wd, "data/dipfed-municipio-regs-2015.RData", sep = ""))
ls()
summary(regs.2015)
summary.lm(regs.2015$oth[[1]])$coef[2,1]



# version 2: coalitions only in districts where they happened
## leftRealm <- data.frame(v00 = v00m$prdc / v00m$efec,
##                         v03 = v03m$prd / v03m$efec,
##                         v06 = v06m$prdc / v06m$efec,
##                         v09 = v09m$prd / v09m$efec,
##                         v12 = v12m$prdc / v12m$efec,
##                         v15 = (v15m$prd * (1 - v15m$dprdc) + v15m$prdc * v15m$dprdc + v15m$morena) / v15m$efec,
##                         v18 = (v18m$morena * (1 - v18m$dmorenac) + v18m$morenac * v18m$dmorenac) / v18m$efec)
## esto no jala, parece que morena!=0 cuando dmorenac==1
## morenaRealm[1,]
# version 3: party's own vote plus proportional part of coal
## morenaBrkm
