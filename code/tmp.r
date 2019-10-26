add.split <- function(year.var = NA) {
    #
    if (year.var==1991) d <- v91
    if (year.var==1994) d <- v94
    if (year.var==1997) d <- v97
    if (year.var==2000) d <- v00
    if (year.var==2003) d <- v03
    if (year.var==2006) d <- v06
    if (year.var==2009) d <- v09
    if (year.var==2012) d <- v12
    if (year.var==2015) d <- v15
    if (year.var==2018) d <- v18
    #
    sel.agg <- which(d$edon==info$edon & d$seccion %in% sel.to)
    sel.col <- setdiff(colnames(d), c("edon","disn","seccion","munn","dunbaja","edosecn",
                                      "d94","d97","d00","d03","d06","d09","d12","d15","d18")) # exclude ids (order-dependent)
    #
    # sum votes
    totals <- colSums(d[sel.agg, sel.col])
    # dummies
    if (!is.na(totals["dpanc"])    & totals["dpanc"]>0)    totals["panc"]     <- 1
    if (!is.na(totals["dpric"])    & totals["dpric"]>0)    totals["dpric"]    <- 1
    if (!is.na(totals["dprdc"])    & totals["dprdc"]>0)    totals["dprdc"]    <- 1
    if (!is.na(totals["dmorenac"]) & totals["dmorenac"]>0) totals["dmorenac"] <- 1
    # paste them into eliminated sección
    sel.target <- which(d$edon==info$edon & d$seccion==info$seccion)
    d[sel.target,sel.col] <- totals;
    d$dunbaja[sel.target] <- 1;  # indicates manipulation
    #
    # return manipulated data
    if (year.var==1991) v91 <- d
    if (year.var==1994) v94 <- d
    if (year.var==1997) v97 <- d
    if (year.var==2000) v00 <- d
    if (year.var==2003) v03 <- d
    if (year.var==2006) v06 <- d
    if (year.var==2009) v09 <- d
    if (year.var==2012) v12 <- d
    if (year.var==2015) v15 <- d
    if (year.var==2018) v18 <- d
}

# then run function

if (year < 1994) {
    add.split(1994);
    add.split(1997);
    add.split(2000);
    add.split(2003);
    add.split(2006);
    add.split(2009);
    add.split(2012);
    add.split(2015);
    add.split(2018);
}
if (year < 1997) {
    add.split(1997);
    add.split(2000);
    add.split(2003);
    add.split(2006);
    add.split(2009);
    add.split(2012);
    add.split(2015);
    add.split(2018);
}
if (year < 2000) {
    add.split(2000);
    add.split(2003);
    add.split(2006);
    add.split(2009);
    add.split(2012);
    add.split(2015);
    add.split(2018);
}
if (year < 2003) {
    add.split(2003);
    add.split(2006);
    add.split(2009);
    add.split(2012);
    add.split(2015);
    add.split(2018);
}
if (year < 2006) {
    add.split(2006);
    add.split(2009);
    add.split(2012);
    add.split(2015);
    add.split(2018);
}
if (year < 2009) {
    add.split(2009);
    add.split(2012);
    add.split(2015);
    add.split(2018);
}
if (year < 2012) {
    add.split(2012);
    add.split(2015);
    add.split(2018);
}
if (year < 2015) {
    add.split(2015);
    add.split(2018);
}
if (year < 2018) {
    add.split(2018);
}
    
