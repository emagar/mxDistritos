##########
## 1994 ##
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom","dextra")
# actual districts
d <- v94; d[is.na(d)] <- 0
### DROP DIS==0 HERE IN EVERY BLOCK
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v94d <- d                                      # rename object  
# 2006 counterfactual districts
d <- v94; d[is.na(d)] <- 0
### DROP DIS2006==0 HERE IN EVERY BLOCK
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v94d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v94; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- within(d, disn <- dis1979 <- dis1997 <- dis2006 <- dis2013 <- NULL) # drop unneeded district ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; dis2018 after edon")] # order columns
v94d18 <- d                                    # rename object  
# actual municipalities
d <- v94; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
#d[1,]
v94m <- d                                      # rename object  
