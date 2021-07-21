sel <- which(names(extendCoal) %in% chg2003)
extendCoal[sel[1]]


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
