tmp.func <- function(year) {
    #year <- 2009         # debug
    #X <- extendCoal[[1]] # debug
    ## select relevant results object
    if (year %in% c(1985,1988,1991,1994)) tmp.dat <- extendCoald79
    if (year %in% c(1997,2000,2003))      tmp.dat <- extendCoald97
    if (year %in% c(2006,2009,2012,2015)) tmp.dat <- extendCoald06
    if (year %in% c(2018,2021,2024))      tmp.dat <- extendCoald18
    sel.row <- which(tmp.dat[[1]]$yr==year) # which row reports year sought (symmetric across objects in list)
    # generate list with selected row only in every district
    tmp.out <- lapply(tmp.dat, FUN = function(X) {
        prune <- X[sel.row,]
        return(prune)
    })
    ## # spot NAs in list
    ## tmp.sel <- setdiff(1:length(extendCoal), non.nas)
    ## # fill with same-dim NA data.frame
    ## tmp.manip <- tmp.out[[non.nas[1]]]
    ## tmp.manip[,-1] <- NA # all but 1st col (yr) to NA
    ## if (length(tmp.sel)>0) tmp.out[tmp.sel] <- lapply(tmp.out[tmp.sel], function(x) tmp.manip)
    # turn into one dataframe
    # table(summary(tmp)) # debug
    tmp.out <- do.call("rbind", tmp.out)
    rownames(tmp.out) <- NULL
    return(tmp.out)
}
