#############################################################
## ####################################################### ##
## ## code to re-construct some re-seccionamiento cases ## ##
## ## invoked from elec-data-for-maps                   ## ##
## ####################################################### ##
#############################################################

# handle split cases --- since sección "dada de baja", can easily reaggregate votes after 
# split's year w/o causing trouble, sección unused afterwards
# (adds-up new secciones to add votes to split seccion after it was dropped)
add.split <- function(year.var = NA) {
    #
    #year.var <- 2015 # debug
    if (year.var==1991) d <- v91s
    if (year.var==1994) d <- v94s
    if (year.var==1997) d <- v97s
    if (year.var==2000) d <- v00s
    if (year.var==2003) d <- v03s
    if (year.var==2006) d <- v06s
    if (year.var==2009) d <- v09s
    if (year.var==2012) d <- v12s
    if (year.var==2015) d <- v15s
    if (year.var==2018) d <- v18s
    #
    #d <- v15 # debug
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
    return(d)
}

# add dummies indicating a manipulation
v94s$dunbaja <- 0
v97s$dunbaja <- 0
v00s$dunbaja <- 0
v03s$dunbaja <- 0
v06s$dunbaja <- 0
v09s$dunbaja <- 0
v12s$dunbaja <- 0
v15s$dunbaja <- 0
v18s$dunbaja <- 0

#########################################
## manipulate seccines that were split ##
#########################################
sel.split <- which(eq$action=="split")
#table(eq$when[sel.split])
#
for (i in sel.split){
    #i <- sel.split[1] # debug
    # get basic info to re-aggregate post-split seccciones
    info <- eq[i, c("edon","seccion","orig.dest","when")]
    year <- info$when # reseccionamiento year (modify data in subsequent years only)
    # get target secciones (those that will be summed-up)
    sel.to <- as.numeric(unlist(strsplit(info$orig.dest, ":")))
    sel.to <- seq(from = sel.to[1], to = sel.to[2], by = 1)
    #
    # now run function
    if (year < 1994) {
        v94s <- add.split(1994);
        v97s <- add.split(1997);
        v00s <- add.split(2000);
        v03s <- add.split(2003);
        v06s <- add.split(2006);
        v09s <- add.split(2009);
        v12s <- add.split(2012);
        v15s <- add.split(2015);
        v18s <- add.split(2018);
    }
    if (year >= 1994 & year < 1997) {
        v97s <- add.split(1997);
        v00s <- add.split(2000);
        v03s <- add.split(2003);
        v06s <- add.split(2006);
        v09s <- add.split(2009);
        v12s <- add.split(2012);
        v15s <- add.split(2015);
        v18s <- add.split(2018);
    }
    if (year >= 1997 & year < 2000) {
        v00s <- add.split(2000);
        v03s <- add.split(2003);
        v06s <- add.split(2006);
        v09s <- add.split(2009);
        v12s <- add.split(2012);
        v15s <- add.split(2015);
        v18s <- add.split(2018);
    }
    if (year >= 2000 & year < 2003) {
        v03s <- add.split(2003);
        v06s <- add.split(2006);
        v09s <- add.split(2009);
        v12s <- add.split(2012);
        v15s <- add.split(2015);
        v18s <- add.split(2018);
    }
    if (year >= 2003 & year < 2006) {
        v06s <- add.split(2006);
        v09s <- add.split(2009);
        v12s <- add.split(2012);
        v15s <- add.split(2015);
        v18s <- add.split(2018);
    }
    if (year >= 2006 & year < 2009) {
        v09s <- add.split(2009);
        v12s <- add.split(2012);
        v15s <- add.split(2015);
        v18s <- add.split(2018);
    }
    if (year >= 2009 & year < 2012) {
        v12s <- add.split(2012);
        v15s <- add.split(2015);
        v18s <- add.split(2018);
    }
    if (year >= 2012 & year < 2015) {
        v15s <- add.split(2015);
        v18s <- add.split(2018);
    }
    if (year >= 2015 & year < 2018) {
        v18s <- add.split(2018);
    }
}

# subset secciones that were merged
## sel.merged <- which(eq$action=="merged")
## table(eq$when[sel.merged])
####################################################################################################
## will not manipulate secciones that were merged bec (a) must be very small to merit dissolution ##
## and therefore have minimal effects on host seccion and (b) will not appear in newer shapefiles ##
####################################################################################################

########################################
## rest of changes too hard to handle ##
########################################
## table(eq$action)

