######################################################################
## Add missing columns to unmanipulated list items (for squareness) ##
######################################################################
## yes.nas.2009 <- setdiff(1:nmun, non.nas(2009))
## yes.nas.2012 <- setdiff(1:nmun, non.nas(2012))
## yes.nas.2015 <- setdiff(1:nmun, non.nas(2015))
## yes.nas.2018 <- setdiff(1:nmun, non.nas(2018))
## yes.nas.2021 <- setdiff(1:nmun, non.nas(2021))

yes.nas.2009 <- c(17,34,35,195,196,197,198,199,200,201,202,203,204,205,206,207,665,937,938,1825)
yes.nas.2012 <- c(17,34,35,195,196,197,198,199,200,201,202,203,204,205,206,207,937,938,1825)
yes.nas.2015 <- c(17,34,35,195,196,197,198,199,200,201,202,203,204,205,206,207,937,938,1825)
yes.nas.2018 <- c(17,34,35,202,203,204,205,206,207,937,938)
yes.nas.2021 <- c(17)


sel.map <- 2009
for (i in yes.nas.2009){  # loop over nas
    ## subset data to single unit
    if (sel.map==1994) data.tmp <- extendCoalm94[[i]]
    if (sel.map==1997) data.tmp <- extendCoalm97[[i]]
    if (sel.map==2000) data.tmp <- extendCoalm00[[i]]
    if (sel.map==2003) data.tmp <- extendCoalm03[[i]]
    if (sel.map==2006) data.tmp <- extendCoalm06[[i]]
    if (sel.map==2009) data.tmp <- extendCoalm09[[i]]
    if (sel.map==2012) data.tmp <- extendCoalm12[[i]]
    if (sel.map==2015) data.tmp <- extendCoalm15[[i]]
    if (sel.map==2018) data.tmp <- extendCoalm18[[i]]
    if (sel.map==2021) data.tmp <- extendCoalm21[[i]]
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        oth <- NULL
        betahat.left  <- NA;
        betahat.pan   <- NA;
        alphahat.left <- NA;
        alphahat.pri  <- NA;
        alphahat.pan  <- NA;
        dbackward     <- NA;
        bhat.left     <- NA;
        bhat.pan      <- NA;
        vhat.left     <- NA;
        vhat.pri      <- NA;
        vhat.pan      <- NA;
        d.left        <- NA;
        d.pri         <- NA;
        d.pan         <- NA;
        }
    )
    ## return estimates to data object
    if (sel.map==1994) extendCoalm94[[i]] <- data.tmp
    if (sel.map==1997) extendCoalm97[[i]] <- data.tmp
    if (sel.map==2000) extendCoalm00[[i]] <- data.tmp
    if (sel.map==2003) extendCoalm03[[i]] <- data.tmp
    if (sel.map==2006) extendCoalm06[[i]] <- data.tmp
    if (sel.map==2009) extendCoalm09[[i]] <- data.tmp
    if (sel.map==2012) extendCoalm12[[i]] <- data.tmp
    if (sel.map==2015) extendCoalm15[[i]] <- data.tmp
    if (sel.map==2018) extendCoalm18[[i]] <- data.tmp
    if (sel.map==2021) extendCoalm21[[i]] <- data.tmp
}

sel.map <- 2012
for (i in yes.nas.2012){  # loop over nas
    ## subset data to single unit
    if (sel.map==1994) data.tmp <- extendCoalm94[[i]]
    if (sel.map==1997) data.tmp <- extendCoalm97[[i]]
    if (sel.map==2000) data.tmp <- extendCoalm00[[i]]
    if (sel.map==2003) data.tmp <- extendCoalm03[[i]]
    if (sel.map==2006) data.tmp <- extendCoalm06[[i]]
    if (sel.map==2009) data.tmp <- extendCoalm09[[i]]
    if (sel.map==2012) data.tmp <- extendCoalm12[[i]]
    if (sel.map==2015) data.tmp <- extendCoalm15[[i]]
    if (sel.map==2018) data.tmp <- extendCoalm18[[i]]
    if (sel.map==2021) data.tmp <- extendCoalm21[[i]]
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        oth <- NULL
        betahat.left  <- NA;
        betahat.pan   <- NA;
        alphahat.left <- NA;
        alphahat.pri  <- NA;
        alphahat.pan  <- NA;
        dbackward     <- NA;
        bhat.left     <- NA;
        bhat.pan      <- NA;
        vhat.left     <- NA;
        vhat.pri      <- NA;
        vhat.pan      <- NA;
        d.left        <- NA;
        d.pri         <- NA;
        d.pan         <- NA;
        }
    )
    ## return estimates to data object
    if (sel.map==1994) extendCoalm94[[i]] <- data.tmp
    if (sel.map==1997) extendCoalm97[[i]] <- data.tmp
    if (sel.map==2000) extendCoalm00[[i]] <- data.tmp
    if (sel.map==2003) extendCoalm03[[i]] <- data.tmp
    if (sel.map==2006) extendCoalm06[[i]] <- data.tmp
    if (sel.map==2009) extendCoalm09[[i]] <- data.tmp
    if (sel.map==2012) extendCoalm12[[i]] <- data.tmp
    if (sel.map==2015) extendCoalm15[[i]] <- data.tmp
    if (sel.map==2018) extendCoalm18[[i]] <- data.tmp
    if (sel.map==2021) extendCoalm21[[i]] <- data.tmp
}

sel.map <- 2015
for (i in yes.nas.2015){  # loop over nas
    ## subset data to single unit
    if (sel.map==1994) data.tmp <- extendCoalm94[[i]]
    if (sel.map==1997) data.tmp <- extendCoalm97[[i]]
    if (sel.map==2000) data.tmp <- extendCoalm00[[i]]
    if (sel.map==2003) data.tmp <- extendCoalm03[[i]]
    if (sel.map==2006) data.tmp <- extendCoalm06[[i]]
    if (sel.map==2009) data.tmp <- extendCoalm09[[i]]
    if (sel.map==2012) data.tmp <- extendCoalm12[[i]]
    if (sel.map==2015) data.tmp <- extendCoalm15[[i]]
    if (sel.map==2018) data.tmp <- extendCoalm18[[i]]
    if (sel.map==2021) data.tmp <- extendCoalm21[[i]]
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        oth <- NULL
        betahat.left  <- NA;
        betahat.pan   <- NA;
        alphahat.left <- NA;
        alphahat.pri  <- NA;
        alphahat.pan  <- NA;
        dbackward     <- NA;
        bhat.left     <- NA;
        bhat.pan      <- NA;
        vhat.left     <- NA;
        vhat.pri      <- NA;
        vhat.pan      <- NA;
        d.left        <- NA;
        d.pri         <- NA;
        d.pan         <- NA;
        }
    )
    ## return estimates to data object
    if (sel.map==1994) extendCoalm94[[i]] <- data.tmp
    if (sel.map==1997) extendCoalm97[[i]] <- data.tmp
    if (sel.map==2000) extendCoalm00[[i]] <- data.tmp
    if (sel.map==2003) extendCoalm03[[i]] <- data.tmp
    if (sel.map==2006) extendCoalm06[[i]] <- data.tmp
    if (sel.map==2009) extendCoalm09[[i]] <- data.tmp
    if (sel.map==2012) extendCoalm12[[i]] <- data.tmp
    if (sel.map==2015) extendCoalm15[[i]] <- data.tmp
    if (sel.map==2018) extendCoalm18[[i]] <- data.tmp
    if (sel.map==2021) extendCoalm21[[i]] <- data.tmp
}

sel.map <- 2018
for (i in yes.nas.2018){  # loop over nas
    ## subset data to single unit
    if (sel.map==1994) data.tmp <- extendCoalm94[[i]]
    if (sel.map==1997) data.tmp <- extendCoalm97[[i]]
    if (sel.map==2000) data.tmp <- extendCoalm00[[i]]
    if (sel.map==2003) data.tmp <- extendCoalm03[[i]]
    if (sel.map==2006) data.tmp <- extendCoalm06[[i]]
    if (sel.map==2009) data.tmp <- extendCoalm09[[i]]
    if (sel.map==2012) data.tmp <- extendCoalm12[[i]]
    if (sel.map==2015) data.tmp <- extendCoalm15[[i]]
    if (sel.map==2018) data.tmp <- extendCoalm18[[i]]
    if (sel.map==2021) data.tmp <- extendCoalm21[[i]]
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        oth <- NULL
        betahat.left  <- NA;
        betahat.pan   <- NA;
        alphahat.left <- NA;
        alphahat.pri  <- NA;
        alphahat.pan  <- NA;
        dbackward     <- NA;
        bhat.left     <- NA;
        bhat.pan      <- NA;
        vhat.left     <- NA;
        vhat.pri      <- NA;
        vhat.pan      <- NA;
        d.left        <- NA;
        d.pri         <- NA;
        d.pan         <- NA;
        }
    )
    ## return estimates to data object
    if (sel.map==1994) extendCoalm94[[i]] <- data.tmp
    if (sel.map==1997) extendCoalm97[[i]] <- data.tmp
    if (sel.map==2000) extendCoalm00[[i]] <- data.tmp
    if (sel.map==2003) extendCoalm03[[i]] <- data.tmp
    if (sel.map==2006) extendCoalm06[[i]] <- data.tmp
    if (sel.map==2009) extendCoalm09[[i]] <- data.tmp
    if (sel.map==2012) extendCoalm12[[i]] <- data.tmp
    if (sel.map==2015) extendCoalm15[[i]] <- data.tmp
    if (sel.map==2018) extendCoalm18[[i]] <- data.tmp
    if (sel.map==2021) extendCoalm21[[i]] <- data.tmp
}

sel.map <- 2021
for (i in yes.nas.2021){  # loop over nas
    ## subset data to single unit
    if (sel.map==1994) data.tmp <- extendCoalm94[[i]]
    if (sel.map==1997) data.tmp <- extendCoalm97[[i]]
    if (sel.map==2000) data.tmp <- extendCoalm00[[i]]
    if (sel.map==2003) data.tmp <- extendCoalm03[[i]]
    if (sel.map==2006) data.tmp <- extendCoalm06[[i]]
    if (sel.map==2009) data.tmp <- extendCoalm09[[i]]
    if (sel.map==2012) data.tmp <- extendCoalm12[[i]]
    if (sel.map==2015) data.tmp <- extendCoalm15[[i]]
    if (sel.map==2018) data.tmp <- extendCoalm18[[i]]
    if (sel.map==2021) data.tmp <- extendCoalm21[[i]]
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        oth <- NULL
        betahat.left  <- NA;
        betahat.pan   <- NA;
        alphahat.left <- NA;
        alphahat.pri  <- NA;
        alphahat.pan  <- NA;
        dbackward     <- NA;
        bhat.left     <- NA;
        bhat.pan      <- NA;
        vhat.left     <- NA;
        vhat.pri      <- NA;
        vhat.pan      <- NA;
        d.left        <- NA;
        d.pri         <- NA;
        d.pan         <- NA;
        }
    )
    ## return estimates to data object
    if (sel.map==1994) extendCoalm94[[i]] <- data.tmp
    if (sel.map==1997) extendCoalm97[[i]] <- data.tmp
    if (sel.map==2000) extendCoalm00[[i]] <- data.tmp
    if (sel.map==2003) extendCoalm03[[i]] <- data.tmp
    if (sel.map==2006) extendCoalm06[[i]] <- data.tmp
    if (sel.map==2009) extendCoalm09[[i]] <- data.tmp
    if (sel.map==2012) extendCoalm12[[i]] <- data.tmp
    if (sel.map==2015) extendCoalm15[[i]] <- data.tmp
    if (sel.map==2018) extendCoalm18[[i]] <- data.tmp
    if (sel.map==2021) extendCoalm21[[i]] <- data.tmp
}

    
