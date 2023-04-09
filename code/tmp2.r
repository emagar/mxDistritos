my_agg <- function(d=d, sel.c=sel.c, by=NA, y1991=FALSE){
    d <- d              # assign mem
    sel.c <- sel.c      # assign mem
    if (is.na(by)==TRUE){ # default is aggregate secciones when by==NA
        if (y1991==FALSE){
            by <- as.factor(d$edon*10000+d$seccion) # seccion indicator post 1991
        } else {
            by <- as.factor(d$ife*1000000+d$disn*10000+d$seccion) # seccion indicator 1991
        }
    } else { # to aggregate by something else (e.g. dis1979 or disn or ife)
        by <- d[,which(colnames(d) %in% by)] # duplicate column that will serve as aggregator
        by <- as.factor(by)
    }
    sel.c <- which(colnames(d) %in% sel.c); # extract indices of selected columns
    for (i in sel.c){
        #i <- sel.c[1] #debug
        d[,i] <- ave(d[,i], by, FUN=sum, na.rm=TRUE) # sum up
    }
    sel.r <- which(duplicated(by)==TRUE)         # drop duplicate obs
    d <- d[-sel.r,]
    return(d)
}
