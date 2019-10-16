##########
## 1991 ##
##########
d <- v91; d[is.na(d)] <- 0
sel.c <- c("pan","pri","parm","pdm","pfcrn","pps","prd","pt","pem","prt","efec")
d <- ag.mun(d,sel.c)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v91m <- d
##########
## 1994 ##
##########
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v94m <- d
##########
## 1997 ##
##########
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v97m <- d
