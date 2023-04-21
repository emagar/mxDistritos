if (sel.map==1979){
    tmp <- extendCoald79                                                       # duplicate for manipulation
    add1988 <- function(x) rbind(v88=c(1988,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), x) # add empty row to each data frame in list
    tmp <- lapply(tmp, add1988)                                                # add row for 1988 to each data frame in list
    for (i in 1:300){
        #i <- 1 #debug
        sel.r <- which(tmp.1988$disn==as.numeric(names(tmp)[i])) # pick row matching disn
        tmp[[i]][1,c("pan","pri","left","oth","efec","lisnom","disn")] <-
            tmp.1988[sel.r,c("pan","pri","left","oth","efec","lisnom","disn")] # fill 1st row w sel.row cols
    }

    extendCoald79 <- tmp
    rm(add1988,tmp,tmp.1988)
}
