## Code is a replica, updated, of /home/eric/Downloads/Desktop/MXelsCalendGovt/atlasDis/code/mapPrep.r
## Prepares various seccion-level measures of party performance in federal diputado elections
## retrieves electoral info from data in another repository: github.com/emagar/elecRturns

## esto prepara los datos electorales para mapear los distritos de cada estado.
## el contenido queda guardado en un archivo de datos.
## sólo tiene que correrse en caso de cambios.
rm(list=ls())
options(width = 110)
#
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")

# cambiar por cartografía 2017 en /home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/shp/disfed2018
# también preparé cartografía 2020   <---  OJO
md <- c("/home/eric/Dropbox/data/mapas/cartografia28feb2013rojano/")
setwd(dd)

##################################################################
## function to aggregate casilla-level votes into higher levels ##
##################################################################
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
##########################################################################
## function to replace character/factor cells with numeric value or NA; ##
## then chg NAs w 0s                                                    ##
##########################################################################
to.num <- function(d = d, sel.c = sel.c){
    sel.c <- which(colnames(d) %in% sel.c); # extract indices
    tmp.d <- d[,sel.c];                     # subset data for manipulation
    # filter non-mumeric w chars then numeric: if not 0-9 will become NAs 
    tmp.d <- sapply(tmp.d, function(x){     
        if (is.numeric(x)) x else suppressWarnings(as.numeric(as.character(x)))
    });
    tmp.d[is.na(tmp.d)] <- 0; # change resulting NAs with zero
    d[,sel.c] <- tmp.d;       # return manipulated data
    return(d)
}
#
##################################################################################
## function splitting coalition vote in proportion to votes won by each in unit ##
##################################################################################
apportion_v <- function(dat=NA, members=NA, joint=NA){
    if (length(members)==0 | length(joint)) stop
    dummy <- paste0("d",joint) # dummy indicating coalition in unit
    sel.r <- which(dat[,dummy]==1 & dat[,joint]>0) # rows with a coalition and non-zero joint vote
    sel.c <- which(colnames(dat) %in% members)   # target columns
    tmp_v <- dat[sel.r,sel.c]              # subset individual contribs for manipulation
    tmp_j <- dat[sel.r,joint]              # subset joint votes for manipulation
    tmp_w <- tmp_v / rowSums(tmp_v)      # weights
    tmp_w[is.na(tmp_w)] <- 1/ncol(tmp_w) # indeterminates to even split (eg. pt=0 mc=0 pt.mc=10)
    tmp_m <- round(tmp_v + tmp_j * tmp_w, 1) # manipulated version
    #
    dat[sel.r,sel.c] <- tmp_m # return manipulation to member's columns
    dat[sel.r,joint] <- 0     # joint vote to zero
    return(dat)
}
#
###############################################
## function to fill sel.c column NAs to zero ##
###############################################
na2zero <- function(dat=NA, sel.c){
    tmp <- dat[,sel.c] # subset for manipulation
    tmp[is.na(tmp)] <- 0
    dat[,sel.c] <- tmp # return manipulation to data
    return(dat)
}
# handy function to sort one data frame by order of another, matching data frame
source("/home/eric/Dropbox/data/useful-functions/sortBy.r")
# 'not in' function
source("/home/eric/Dropbox/data/useful-functions/notin.r")

###################################
## ############################# ##
## ## read casilla-level data ## ##
## ############################# ##
###################################

##########
## 1991 ## OJO: 1991 seccion ids incomparable to later ones, yet possible to aggregate disn/ife (no counterfactuals, though)
##########
d <- read.csv("dip1991.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
# vote columns selector
sel.c <-            c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec","lisnom")
# clean data
#table(d$note)
#table(d$status)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0; d$dextra[grep("extraordinaria", d$note)] <- 1                       # special elections
sel.r <- which(d$status=="Anulada"); d[sel.r,sel.c] <- 0                           # voided casillas to zero
d <- within(d, efec <- pan + pri + pps + prd + pfcrn + parm + pdm + prt + pem + pt)# valid vote
d <- within(d, tot <- nul <- nr <- note <- status <- casilla <- NULL)              # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=TRUE) #ag.sec(d, sel.c)
## ## coalition dummies
## d <- within(d, dpanc <- dpric <- dprdc <- 0)
##############################################
## rename vote returns objects for analysis ##
##############################################
v91_agg <- v91_split <- d

##########
## 1994 ##
##########
d <- read.csv("dip1994.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
sel.c <-            c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom")
# clean data
#table(d$status)
#table(d$nota)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0; d$dextra[grep("extraordinaria", d$nota)] <- 1                       # special elections
sel.r <- which(d$status=="Anulada"); d[sel.r,sel.c] <- 0                           # voided casillas to zero
d <- within(d, efec <- pan + pri + pps + prd + pfcrn + parm + uno.pdm + pt + pvem) # valid vote
d <- within(d, tot <- nul <- nr <- nota <- status <- casilla <- NULL)              # economize columns
## aggregate seccion-level votes ##
d2 <- d # debug
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
## ## coalition dummies
## d <- within(d, dpanc <- dpric <- dprdc <- 0)
##############################################
## rename vote returns objects for analysis ##
##############################################
v94_agg <- v94_split <- d

##########
## 1997 ##
##########
d <- read.csv("dip1997.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
sel.c <-            c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","lisnom")
# clean
#table(d$status)
#table(d$note)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
d <- within(d, efec <- pan + pri + prd + pc + pt + pvem + pps + pdm)               # valid vote
sel.r <- which(d$status=="Anulada"); d[sel.r,sel.c] <- 0                           # voided casillas to zero
d <- within(d, tot <- nul <- nr <- status <- note <- casilla <- NULL)              # economize cols
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
## ## coalition dummies
## d <- within(d, dpanc <- dpric <- dprdc <- 0)
##############################################
## rename vote returns objects for analysis ##
##############################################
v97_agg <- v97_split <- d

##########
## 2000 ##
##########
d <- read.csv("dip2000.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pan.pvem")]             <- "panc"
colnames(d)[which(colnames(d)=="prd.pt.conve.pas.psn")] <- "prdc"
sel.c <-            c("panc","pri","prdc","pcd","parm","dsppn","efec","lisnom")
# clean data
#table(d$status)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- which(d$status=="Anulada"); d[sel.r,sel.c] <- 0                           # voided casillas to zero
d <- within(d, efec <- panc + pri + prdc + pcd + parm + dsppn)                     # valid vote
d <- within(d, tot <- nul <- nr <- status <- nota <- casilla <- NULL)              # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
## d$dpric <- 0
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpanc, useNA = "always")
table(d$dprdc, useNA = "always")
##############################################
## rename vote returns objects for analysis ##
##############################################
v00_agg <- d

##########
## 2003 ##
##########
d <- read.csv( "dip2003.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pri.pvem")] <- "pric" # "pri-pvem"
sel.c <-            c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric","efec","lisnom")
# clean
#table(d$status)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- as.numeric(d$nota=="eleccion extraordinaria 2004")                     # special elections
sel.r <- grep("Anulada|instalada|entregado", d$status); d[sel.r,sel.c] <- 0        # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pt + pvem + conve + psn + pas + mp + plm + fc + pric) # valid vote
d <- within(d, tot <- nul <- nr <- nota <- status <- casilla <- NULL)              # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
# district coalition dummies
## d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- 0
table(d$dpric, useNA = "always")
##############################################
## rename vote returns objects for analysis ##
##############################################
v03_agg <- d

##########
## 2006 ##
##########
d <- read.csv( "dip2006.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pri.pvem")]     <- "pric"
colnames(d)[which(colnames(d)=="prd.pt.conve")] <- "prdc"
sel.c <-            c("pan","pric","prdc","pna","asdc","efec","lisnom")
#
# clean
#table(d$status)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("Anulada|instalada", d$status); d[sel.r,sel.c] <- 0                  # voided casillas to zero
d <- within(d, efec <- pan + pric + prdc + pna + asdc)                             # valid vote
d <- within(d, tot <- nul <- nr <- status <- casilla <- NULL)                      # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
## coalition dummies
d <- within(d, {
##    dpanc <- 0;
    dpric <- dprdc <- 1
})
##############################################
## rename vote returns objects for analysis ##
##############################################
v06_agg <- d

##########
## 2009 ##
##########
d <- read.csv( "dip2009.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pri.pvem")] <- "pric"
colnames(d)[which(colnames(d)=="pt.conve")] <- "ptc"
sel.c <-            c("pan","pri","prd","pvem","pt","conve","pna","psd","pric","ptc","efec","lisnom")
# clean data
#table(d$status)
#table(d$tepjf)
d <- to.num(d,sel.c)                                                               # numericize
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("instalada|entregado|sin acta", d$status, ignore.case = TRUE)        # voided casillas to zero
sel.r <- c(sel.r, grep("Anulada",             d$tepjf, ignore.case = TRUE))        # voided casillas to zero
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + conve + pna + psd + pric + ptc) # valid vote
d <- within(d, tot <- nul <- nr <- tepjf <- status <- casilla <- NULL)             # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
# district coalition dummies
## d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- 0
d$dptc <- ave(d$ptc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dptc <- as.numeric(d$dptc>0)
table(d$dpric, useNA = "always")
table(d$dptc, useNA = "always")
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0;
})
sel.r <- which(d_agg$dptc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    ptc <- pt + conve + ptc;
    pt <- conve <- 0;
})
# ptc coal across the board
d_agg$pt <- d_agg$conve <- NULL
#############################################################
## split coalition vote according to contributions in unit ##
#############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pri","pvem"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("pt","conve"), joint="ptc")
##############################################
## rename vote returns objects for analysis ##
##############################################
v09_agg <- d_agg
v09_split <- d_split

##########
## 2012 ##
##########
d <- read.csv( "dip2012.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pri.pvem")]  <- "pric"
colnames(d)[which(colnames(d)=="prd.pt.mc")] <- "prdc"
sel.c <-            c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom")
# clean data
#table(d$status)
#table(d$tepjf)
#table(d$nota)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("Anulada", d$tepjf, ignore.case = TRUE)                              # voided casillas to zero
sel.r <- c(sel.r, grep("instalada|entregado", d$status, ignore.case = TRUE))       # voided casillas to zero
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + pric + prdc)       # valid vote
d <- within(d, tot <- nul <- nr <- status <- tepjf <- casilla <- nota <- NULL)     # economize columns
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
# coalition dummies
## d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpric, useNA = "always")
table(d$dprdc, useNA = "always")
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0;
})
sel.r <- which(d_agg$dprdc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    prdc <- prd + pt + mc + prdc;
    prd <- pt <- mc <- 0;
})
# prdc across the board
d_agg$prd <- d_agg$pt <- d_agg$mc <- NULL
#############################################################
## split coalition vote according to contributions in unit ##
#############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pri","pvem"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("prd", "pt","mc"), joint="prdc")
##############################################
## rename vote returns objects for analysis ##
##############################################
v12_agg <- d_agg
v12_split <- d_split

##########
## 2015 ##
##########
d <- read.csv( "dip2015.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pan.pna")]  <- "panc"
colnames(d)[which(colnames(d)=="pri.pvem")] <- "pric"
colnames(d)[which(colnames(d)=="prd.pt")]   <- "prdc"
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","panc","pric","prdc","indep1","indep2","efec","lisnom")
# clean data
#table(d$status)
#table(d$tepjf)
#table(d$nota)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0; d$dextra[grep("extraordinaria", d$nota)] <- 1                       # special elections
sel.r <- grep("suspensión|instalada|entregado", d$status, ignore.case = TRUE)      # voided casillas to zero
sel.r <- c(sel.r, grep("Anulada", d$tepjf, ignore.case = TRUE))                    # voided casillas to zero
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + morena + ph + pes + panc + pric + prdc + indep1 + indep2) # valid votes
d <- within(d, tot <- nul <- nr <- status <- tepjf <- nota <- casilla <- NULL)     # economize columns
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)
d$dpanc <- as.numeric(d$dpanc>0)
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
## d$dmorenac <- 0
table(d$dpanc, useNA = "always")
table(d$dpric, useNA = "always")
table(d$dprdc, useNA = "always")
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpanc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    panc <- pan + panc + pna;
    pan <- pna <- 0;
})
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0;
})
sel.r <- which(d_agg$dprdc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    prdc <- prd + pt + prdc;
    prd <- pt <- 0;
})
##############################################################
## split coalition votes according to contributions in unit ##
##############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pan","pna"), joint="panc")
d_split <- apportion_v(dat=d_split, members=c("pri","pvem"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("prd", "pt"), joint="prdc")
##############################################
## rename vote returns objects for analysis ##
##############################################
v15_agg <- d_agg
v15_split <- d_split

##########
## 2018 ##
##########
d <- read.csv( "dip2018.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# vote columns selector
colnames(d)[which(colnames(d)=="pan.prd.mc")]    <- "panc"
colnames(d)[which(colnames(d)=="pri.pvem.pna")]  <- "pric"
colnames(d)[which(colnames(d)=="pt.morena.pes")] <- "morenac"
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom")
# clean data
table(d$nota)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("suspensi.n|no instalada|no entregado", d$nota, ignore.case = TRUE)  # voided casillas to zero
#table(d$nota[sel.r])
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + morena + pes + panc + pric + morenac + indep1 + indep2) # valid vote
d <- within(d, nr <- nul <- tot <- nota <- casilla  <- NULL)                       # economize columns
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
#d$dprdc <- d$dpanc
d$dmorenac <- ave(d$morenac, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dmorenac <- as.numeric(d$dmorenac>0)
table(d$dpanc, useNA = "always")
table(d$dpric, useNA = "always")
#table(d$dprdc, useNA = "always")
table(d$dmorenac, useNA = "always")
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpanc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    panc <- pan + prd + mc + panc;        # pan-prd-etc vote to panc
    pan <- prd <- mc <- 0;
})
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem + pna;
    pri <- pvem <- pna <- 0;
})
sel.r <- which(d_agg$dmorenac==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    morenac <- morena + pt + pes + morenac;
    morena <- pt <- pes <- 0;
})
#############################################################
## split coalition vote according to contributions in unit ##
#############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pan","prd","mc"), joint="panc")
d_split <- apportion_v(dat=d_split, members=c("pri","pvem","pna"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("morena", "pt","pes"), joint="morenac")
##############################################
## rename vote returns objects for analysis ##
##############################################
v18_agg <- d_agg
v18_split <- d_split

##########
## 2021 ##
##########
d <- read.csv( "dip2021.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),] # sort
# drop voto extranjero
table(d$edon[which(d$seccion==0)])
d <- d[-which(d$seccion==0),]
# vote columns selector
colnames(d)[which(colnames(d)=="pan.pri.prd")]    <- "panc"
colnames(d)[which(colnames(d)=="pvem.pt.morena")] <- "morenac"
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","morenac","efec","lisnom")
# clean data
#table(d$observaciones)
#table(d$tepjf)
d <- to.num(d,sel.c)                                                               # numericize
d <- na2zero(dat=d)                                                                # NAs to zero in sel.c
d$dextra <- 0                                                                      # no special elections this year
sel.r <- grep("anulada", d$tepjf, ignore.case = TRUE)                              # voided casillas to zero
sel.r <- c(sel.r, grep("sin boletas|no recibido|suspensi.n|no instalada|no entregado", d$observaciones, ignore.case = TRUE)) # voided casillas to zero
d[sel.r,sel.c] <- 0                                                                # voided casillas to zero
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + morena + pes + rsp + fxm + indep + panc + morenac) # valid vote
d <- within(d, nr <- nul <- total <- observaciones <- tepjf <- casilla <- NULL)    # economize columns
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
d$dmorenac <- ave(d$morenac, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dmorenac <- as.numeric(d$dmorenac>0)
d$dpric <- d$dpanc
#d$dprdc <- d$dpanc
table(d$dpanc, useNA = "always")
table(d$dmorenac, useNA = "always")
## aggregate seccion-level votes ##
d <- my_agg(d=d, sel.c=sel.c, by=NA, y1991=FALSE) #ag.sec(d, sel.c)
#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpanc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    panc <- pan + pri + prd + panc;       # pan-pri-prd vote to panc
    pan <- pri <- prd <- 0;
})
sel.r <- which(d_agg$dmorenac==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    morenac <- morena + pvem + pt + morenac;
    morena <- pvem <- pt <- 0;
})
#############################################################
## split coalition vote according to contributions in unit ##
#############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pan","pri", "prd"), joint="panc")
d_split <- apportion_v(dat=d_split, members=c("morena", "pvem", "pt"), joint="morenac")
##############################################
## rename vote returns objects for analysis ##
##############################################
v21_agg <- d_agg
v21_split <- d_split

# clean
rm(d,d_agg,d_split,sel.c,sel.r,to.num,apportion_v,na2zero)

## commented 26mar2023, needs work in 2021 to allocate pan.pri.prd win to largest
## ################################
## ## district winners 1994-2021 ##
## ################################
## #v91d <- v91_agg;
## v94d <- v94_agg; v97d <- v97_agg; v00d <- v00_agg; v03d <- v03_agg; v06d <- v06_agg; v09d <- v09_agg; v12d <- v12_agg; v15d <- v15_agg; v18d <- v18_agg; v21d <- v21_agg
## v94d <- within(v94d, {
##     pan <-   ave(pan,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-   ave(pri,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pps <-   ave(pps,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-   ave(prd,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pfcrn <- ave(pfcrn,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     parm <-  ave(parm,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     uno.pdm<-ave(uno.pdm, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-    ave(pt,      as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-  ave(pvem,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v94d <- v94d[duplicated(v94d$edon*100 + v94d$disn)==FALSE,]
## v94d <- v94d[order(v94d$edon*100, v94d$disn),]
## #
## v97d <- within(v97d, {
##     pan <-   ave(pan,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-   ave(pri,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-   ave(prd,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pc <-    ave(pc,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-    ave(pt,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-  ave(pvem,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pps <-   ave(pps,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pdm <-   ave(pdm,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v97d <- v97d[duplicated(v97d$edon*100 + v97d$disn)==FALSE,]
## v97d <- v97d[order(v97d$edon*100, v97d$disn),]
## #
## v00d <- within(v00d, {
##     panc <-  ave(panc,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-   ave(pri,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prdc <-  ave(prdc,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pcd <-   ave(pcd,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     parm <-  ave(parm,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     dsppn <- ave(dsppn, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v00d <- v00d[duplicated(v00d$edon*100 + v00d$disn)==FALSE,]
## v00d <- v00d[order(v00d$edon*100, v00d$disn),]
## #
## v03d <- within(v03d, {
##     pan <-   ave(pan,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-   ave(pri,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-   ave(prd,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-    ave(pt,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-  ave(pvem,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     conve <- ave(conve, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     psn <-   ave(psn,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pas <-   ave(pas,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     mp <-    ave(mp,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     plm <-   ave(plm,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     fc <-    ave(fc,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <-  ave(pric,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v03d <- v03d[duplicated(v03d$edon*100 + v03d$disn)==FALSE,]
## v03d <- v03d[order(v03d$edon*100, v03d$disn),]
## #
## v06d <- within(v06d, {
##     pan   <- ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric  <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prdc  <- ave(prdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna   <- ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     asdc  <- ave(asdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v06d <- v06d[duplicated(v06d$edon*100 + v06d$disn)==FALSE,]
## v06d <- v06d[order(v06d$edon*100, v06d$disn),]
## #
## v09d <- within(v09d, {
##     pan  <- ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri  <- ave(pri,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd  <- ave(prd,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <- ave(pvem, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna  <- ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     psd  <- ave(psd,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     ptc  <- ave(ptc,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v09d <- v09d[duplicated(v09d$edon*100 + v09d$disn)==FALSE,]
## v09d <- v09d[order(v09d$edon*100, v09d$disn),]
## #
## v12d <- within(v12d, {
##     pan <-  ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-  ave(pri,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <- ave(pvem, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna <-  ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prdc <- ave(prdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v12d <- v12d[duplicated(v12d$edon*100 + v12d$disn)==FALSE,]
## v12d <- v12d[order(v12d$edon*100, v12d$disn),]
## #
## v15d <- within(v15d, {
##     pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna <-    ave(pna,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     ph <-     ave(ph,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     panc <-   ave(panc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prdc <-   ave(prdc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep1 <- ave(indep1, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep2 <- ave(indep2, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v15d <- v15d[duplicated(v15d$edon*100 + v15d$disn)==FALSE,]
## v15d <- v15d[order(v15d$edon*100, v15d$disn),]
## #
## v18d <- within(v18d, {
##     pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pna <-    ave(pna,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     panc <-   ave(panc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morenac<- ave(morenac,as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep1 <- ave(indep1, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep2 <- ave(indep2, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v18d <- v18d[duplicated(v18d$edon*100 + v18d$disn)==FALSE,]
## v18d <- v18d[order(v18d$edon*100, v18d$disn),]
## #
## v21d <- within(v21d, {
##     pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     rsp <-    ave(rsp,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     fxm <-    ave(fxm,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     indep <- ave(indep, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     panc <-   ave(panc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## #    pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     morenac<- ave(morenac,as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
##     efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
## })
## v21d <- v21d[duplicated(v21d$edon*100 + v21d$disn)==FALSE,]
## v21d <- v21d[order(v21d$edon*100, v21d$disn),]
## #
## windis <- v15d[,c("edon","disn")] # will receive data
## #
## ## # example
## ## v <- data.frame(c1=c(30,15,3), c2=c(10,25,2), c3=c(20,35,4))
## ## w <- data.frame(c1=c("thirty","fifteen","three"), c2=c("ten","twenty-five","two"), c3=c("twenty","thirty-five","four"))
## ## v.sorted <- t(apply(v, 1, function(x) sort(x, decreasing = TRUE))) # sort each row of df -- http://stackoverflow.com/questions/6063881/sorting-rows-alphabetically
## ## w.sorted <- sortBy(target = w, By = v)
## ## sortBy(target = v, By = v)
## #
## # 1994
## vot <- v94d[,c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem")]
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e94 <- etiq[,1] 
## #
## # 1997
## vot <- v97d[,c("pan","pri","prd","pc","pt","pvem","pps","pdm")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e97 <- etiq[,1] 
## #
## # 2000
## vot <- v00d[,c("panc","pri","prdc","pcd","parm","dsppn")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e00 <- etiq[,1] 
## #
## # 2003
## vot <- v03d[,c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e03 <- etiq[,1] 
## #
## # 2006
## vot <- v06d[,c("pan","pric","prdc","pna","asdc")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e06 <- etiq[,1] 
## ## runnerup$e06 <- etiq[,2]
## ## mg$e06 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e06 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2009
## vot <- v09d[,c("pan","pri","prd","pvem","pna","psd","pric","ptc")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e09 <- etiq[,1] 
## ## runnerup$e09 <- etiq[,2]
## ## mg$e09 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e09 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2012
## vot <- v12d[,c("pan","pri","pvem","pna","pric","prdc")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e12 <- etiq[,1] 
## ## runnerup$e12 <- etiq[,2]
## ## mg$e12 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e12 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2015
## vot <- v15d[,c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","panc","pric","prdc","indep1","indep2")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e15 <- etiq[,1]
## ## runnerup$e15 <- etiq[,2]
## ## mg$e15 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e15 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2018
## vot <- v18d[,c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e18 <- etiq[,1]
## ## runnerup$e15 <- etiq[,2]
## ## mg$e15 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e15 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## # 2021 -- uses v21dw instead of v21d
## vot <- v21d[,c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","morenac")]
## vot[is.na(vot)==TRUE] <- 0 # drop NAs
## # crea objeto de etiquetas
## etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
## colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
## #
## etiq <- sortBy(target = etiq, By = vot)
## vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
## #
## windis$e21 <- etiq[,1]
## #
## # assign coal win to bigger of pan or pri in district---PENDING, USE v21_split instead of w
## lab <- apply(tmp.w, 1, max)
## lab <- ifelse(tmp.w$pan==lab, "panc", "pric")
## windis$e21[which(windis$e21=="panc")] <- lab[which(windis$e21=="panc")]
## # assign coal win to bigger of morena or pvem in district
## lab <- apply(tmp.w2, 1, max)
## lab <- ifelse(tmp.w2$morena==lab, "morenac", "pvemc")
## windis$e21[which(windis$e21=="morenac")] <- lab[which(windis$e21=="morenac")]
## ## runnerup$e21 <- etiq[,2]
## ## mg$e21 <- (vot[,1] - vot[,2]) / rowSums(vot)
## ## enp$e21 <- 1/rowSums((vot/rowSums(vot))^2)
## #
## rm(vot,etiq)
## rm(v21dw,tmp.w,tmp.w2) # drop to avoid confusion
## #
## write.csv(windis, file = paste(dd, "dfdf2006-on-winners.csv", sep = ""))

###############################################################################
## keep split vote objects only for analysis                                 ##
## OJO: 2000, 2003, and 2006 have joint coalition vote only                  ##
## OJO: assumes pan=panc in 00, pri=pric in 03 and 06, prd=prdc in 00 and 06 ##
###############################################################################
v91 <- v91_split
v94 <- v94_split
v97 <- v97_split
v00 <- v00_agg
v03 <- v03_agg
v06 <- v06_agg
v09 <- v09_split
v12 <- v12_split
v15 <- v15_split
v18 <- v18_split
v21 <- v21_split
# free memory
rm(
    v91_split, v91_agg,
    v94_split, v94_agg,
    v97_split, v97_agg,
               v00_agg,
               v03_agg,
               v06_agg,
    v09_split, v09_agg,
    v12_split, v12_agg,
    v15_split, v15_agg,
    v18_split, v18_agg,
    v21_split, v21_agg
   )

##############################################################################
## OJO: Script code/manip-dis1979-dis2018-to-assign-dropped-secciones.r     ##
## ran independently 12apr2023 to infer historic federal districts when     ##
## reseccionamiento occurred. Output was incorportated into excel sheet     ##
## redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv. ##
## Run script again if changes to original file occurred                    ##
######  |  ###################################################################
     #  |  #
######  V  ########################
## get equivalencias seccionales ##
###################################
tmp <- paste(wd, "equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", sep = "")
eq <- read.csv(tmp, stringsAsFactors = FALSE)
# drop secciones whose numbers have never been used
sel <- which(eq$baja==1992); eq <- eq[-sel,]; rm(sel)
# check: any secciones missing from eq?
miss_secc <- function(dat=v21_split){ # defaults to 2021
    tmp <- deparse(substitute(dat)) # extract object's name
    sel <- which(as.factor(dat$edon+dat$seccion/10000) %notin% as.factor(eq$edon+eq$seccion/10000))
    if (length(sel)>0){
        dat$edon[sel]+dat$seccion[sel]/10000
#        print(c(tmp, ": ", dat$edon[sel]+dat$seccion[sel]/10000))
    } else {
        print(paste0(tmp, ": All v-secciones in eq object"))
    }
}
miss_secc(dat=v94)
miss_secc(dat=v97)
miss_secc(dat=v00)
miss_secc(dat=v03)
miss_secc(dat=v06)
miss_secc(dat=v09)
miss_secc(dat=v12)
miss_secc(dat=v15)
miss_secc(dat=v18)
miss_secc(dat=v21) # some 2021 secciones not in eq, all break sequence so must be temporary
#                  # OJO: will drop them to keep things square (still no clue about them)
sel.r <-          which(v21$edon==17 & v21$seccion>=5000)
sel.r <- c(sel.r, which(v21$edon==19 & v21$seccion>=9000))
v21 <- v21[-sel.r,]
rm(miss_secc,sel.r,tmp) # clean

######################################################################
## extract historic municipio and district info to merge into votes ##
######################################################################
mundis <- eq[, c("edon", "seccion", "ife", "inegi",
                 "ife1994", "ife1997", "ife2000", "ife2003", "ife2006", "ife2009",
                   "ife2012", "ife2015", "ife2018", "ife2021", 
                 "dis1979", "dis1997", "dis2006", "dis2013", "dis2018")]
#
# match yearly observations (secciones)
#dim(v06); dim(v09); dim(v12); dim(v15)
## v91 <- within(v91, {                                  # 1991 cannot be matched
##     edosecn <- ife*1000000 + disn*10000 + seccion
##     d91     <- 1;
## })
v94 <- within(v94, {
    edosecn <- edon*10000 + seccion;
    d94     <- 1;
})
v97 <- within(v97, {
    edosecn <- edon*10000 + seccion;
    d97     <- 1;
})
v00 <- within(v00, {
    edosecn <- edon*10000 + seccion;
    d00     <- 1;
})
v03 <- within(v03, {
    edosecn <- edon*10000 + seccion;
    d03     <- 1;
})
v06 <- within(v06, {
    edosecn <- edon*10000 + seccion;
    d06     <- 1;
})
v09 <- within(v09, {
    edosecn <- edon*10000 + seccion;
    d09     <- 1;
})
v12 <- within(v12, {
    edosecn <- edon*10000 + seccion;
    d12     <- 1;
})
v15 <- within(v15, {
    edosecn <- edon*10000 + seccion;
    d15     <- 1;
})
v18 <- within(v18, {
    edosecn <- edon*10000 + seccion;
    d18    <- 1;
})
v21 <- within(v21, {
    edosecn <- edon*10000 + seccion;
    d21    <- 1;
})
#
# dummies d91 d94 ... indicate secciones existing each year
## tmp <- merge(x=v91[,c("edosecn","d91")], y=v94[,c("edosecn","d94")], by = "edosecn", all = TRUE)
## tmp <- merge(x=tmp,                      y=v97[,c("edosecn","d97")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=v94[,c("edosecn","d94")], y=v97[,c("edosecn","d97")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,                      y=v00[,c("edosecn","d00")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,                      y=v03[,c("edosecn","d03")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,                      y=v06[,c("edosecn","d06")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,                      y=v09[,c("edosecn","d09")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,                      y=v12[,c("edosecn","d12")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,                      y=v15[,c("edosecn","d15")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,                      y=v18[,c("edosecn","d18")], by = "edosecn", all = TRUE)
tmp.all.sec <- merge(x=tmp.all.sec,                      y=v21[,c("edosecn","d21")], by = "edosecn", all = TRUE)
# fill 0s in year dummies
sel.c <- grep("^d[0-9]{2}$", colnames(tmp.all.sec))
tmp <- tmp.all.sec[,sel.c]
tmp[is.na(tmp)] <- 0
tmp.all.sec[,sel.c] <- tmp
#
## v91$d91 <-
v94$d94 <- v97$d97 <- v00$d00 <- v03$d03 <- v06$d06 <- v09$d09 <- v12$d12 <- v15$d15 <- v18$d18 <- v21$d21 <- NULL # clean
#
# adds any missing secciones to each object
#v91 <- merge(x=tmp.all.sec, y=v91, by = "edosecn", all = TRUE)
v94  <- merge(x=tmp.all.sec, y=v94, by = "edosecn", all = TRUE)
v97  <- merge(x=tmp.all.sec, y=v97, by = "edosecn", all = TRUE)
v00  <- merge(x=tmp.all.sec, y=v00, by = "edosecn", all = TRUE)
v03  <- merge(x=tmp.all.sec, y=v03, by = "edosecn", all = TRUE)
v06  <- merge(x=tmp.all.sec, y=v06, by = "edosecn", all = TRUE)
v09  <- merge(x=tmp.all.sec, y=v09, by = "edosecn", all = TRUE)
v12  <- merge(x=tmp.all.sec, y=v12, by = "edosecn", all = TRUE)
v15  <- merge(x=tmp.all.sec, y=v15, by = "edosecn", all = TRUE)
v18  <- merge(x=tmp.all.sec, y=v18, by = "edosecn", all = TRUE)
v21  <- merge(x=tmp.all.sec, y=v21, by = "edosecn", all = TRUE)
###########################
## verify dimensionality ##
###########################
dim(v21);
nrow(v94)==nrow(v97)
nrow(v97)==nrow(v00)
nrow(v00)==nrow(v03)
nrow(v03)==nrow(v06)
nrow(v06)==nrow(v06)
nrow(v06)==nrow(v12)
nrow(v12)==nrow(v15)
nrow(v15)==nrow(v18)
nrow(v18)==nrow(v21)
# fill in missing edon and seccion numbers
add.edon.secn <- function(x) {
    within(x, {
        edon    <- as.integer(edosecn/10000);
        seccion <- edosecn - edon*10000;
    })
}
#v91 <- add.edon.secn(v91)
v94 <- add.edon.secn(v94)
v97 <- add.edon.secn(v97)
v00 <- add.edon.secn(v00)
v03 <- add.edon.secn(v03)
v06 <- add.edon.secn(v06)
v09 <- add.edon.secn(v09)
v12 <- add.edon.secn(v12)
v15 <- add.edon.secn(v15)
v18 <- add.edon.secn(v18)
v21 <- add.edon.secn(v21)
# clean
rm(add.edon.secn,tmp.all.sec)

## #########################
## ## READ/PREP pop>18yrs ##
## #########################
## # 2005
## edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
## tmp18 <- data.frame()
## for (i in 1:9){
##     tmp2005 <- read.csv( paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/eceg_2005/", edos[i], "/0", i, "_", edos[i], "_pob.csv"), stringsAsFactors = FALSE)
##     tmp18 <- rbind(tmp18, tmp2005[, grep("ENTIDAD|SECCION|POB_TOT|EDQUI0[1-4]", colnames(tmp2005))])
## }
## for (i in 10:32){
##     tmp2005 <- read.csv( paste0("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/eceg_2005/", edos[i], "/", i, "_", edos[i], "_pob.csv"), stringsAsFactors = FALSE)
##     tmp18 <- rbind(tmp18, tmp2005[, grep("ENTIDAD|SECCION|POB_TOT|EDQUI0[1-4]", colnames(tmp2005))])
## }
## # drop seccion=0
## tmp18 <- tmp18[-which(tmp18$SECCION==0),]
## # EDQUI04 covers ages 15:19, drop two-fifths (assumes yearly cohorts equal size)
## tmp18$EDQUI04 <- as.integer(tmp18$EDQUI04 * .6)
## # p18
## tmp18$p18_2005 <- tmp18$POB_TOT - tmp18$EDQUI01 - tmp18$EDQUI02 - tmp18$EDQUI03 - tmp18$EDQUI04
## tmp18$ptot_2005 <- tmp18$POB_TOT
## #
## tmp18$edosecn <- tmp18$ENTIDAD*10000 + tmp18$SECCION
## tmp18 <- tmp18[, c("edosecn", "p18_2005", "ptot_2005")]
## # add missing secciones (tmp adds up all secciones reported in yearly federal returns)
## sel <- which(tmp18$edosecn %notin% tmp.all.sec$edosecn)
## tmp18$edosecn[sel]
## c(nrow(tmp.all.sec), nrow(tmp18))
## tmp18  <- merge(x=tmp.all.sec, y=tmp18,  by = "edosecn", all = TRUE)
## c(nrow(tmp.all.sec), nrow(tmp18))
## tmp18 <- tmp18[, -grep("d[0-9]", colnames(tmp18))]
## # add to pop object
## pob18 <- tmp18
## #
## # 2010
## tmp18 <- data.frame()
## for (i in 1:9){
##     tmp2010 <- read.csv( paste0("/home/eric/Desktop/MXelsCalendGovt/censos/secciones/eceg_2010/", edos[i], "/secciones_0", i, ".csv"), stringsAsFactors = FALSE)
##     tmp18 <- rbind(tmp18, tmp2010[, grep("ENTIDAD|CLAVEGEO|P_18YMAS$|POBTOT", colnames(tmp2010))])
## }
## for (i in 10:32){
##     tmp2010 <- read.csv( paste0("/home/eric/Desktop/MXelsCalendGovt/censos/secciones/eceg_2010/", edos[i], "/secciones_", i, ".csv"), stringsAsFactors = FALSE)
##     tmp18 <- rbind(tmp18, tmp2010[, grep("ENTIDAD|CLAVEGEO|P_18YMAS$|POBTOT", colnames(tmp2010))])
## }
## rm(tmp2010)
## # get seccion
## lastn <- function(x, n) substr(x, nchar(x)-n+1, nchar(x))
## tmp18$seccion <- as.numeric(lastn(tmp18$CLAVEGEO, 4))
## #
## tmp18$edosecn <- tmp18$ENTIDAD*10000 + tmp18$seccion
## tmp18$p18_2010 <- tmp18$P_18YMAS
## tmp18$ptot_2010 <- tmp18$POBTOT
## tmp18 <- tmp18[, c("edosecn", "p18_2010", "ptot_2010")]
## # add missing secciones
## tmp18  <- merge(x=tmp.all.sec, y=tmp18,  by = "edosecn", all = TRUE)
## tmp18 <- tmp18[, -grep("d[0-9]", colnames(tmp18))]
## # add to pop object
## pob18 <- merge(pob18, tmp18, by = "edosecn", all = TRUE)
## head(pob18)
## #
## # 2020
## tmp18 <- read.csv("/home/eric/Downloads/Desktop/MXelsCalendGovt/censos/secciones/eceg_2020/conjunto_de_datos/INE_SECCION_2020.csv", stringsAsFactors = FALSE)[, c(2,5,7,13)]
## tmp18$p18_2020 <- tmp18$POBTOT - tmp18$P_0A17
## tmp18$ptot_2020 <- tmp18$POBTOT
## tmp18$edosecn <- tmp18$ENTIDAD*10000 + tmp18$SECCION
## tmp18 <- tmp18[, c("edosecn", "p18_2020", "ptot_2020")]
## # add missing secciones
## tmp18  <- merge(x=tmp.all.sec, y=tmp18,  by = "edosecn", all = TRUE)
## tmp18 <- tmp18[, -grep("d[0-9]", colnames(tmp18))]
## # add to pop object
## pob18 <- merge(pob18, tmp18, by = "edosecn", all = TRUE)
## head(pob18)
## #
## # will need cleaning with reseccionamiento functions
## table(y2005=is.na(pob18$p18_2005), y2010=is.na(pob18$p18_2010))
## table(y2005=is.na(pob18$p18_2005), y2020=is.na(pob18$p18_2020))
## table(y2010=is.na(pob18$p18_2010), y2020=is.na(pob18$p18_2020))
## rm(lastn,tmp18,tmp.all.sec,add.edon.secn,sel,sel.r) # clean
## #
## # SEPARATE PTOT FROM P18
## pobtot <- pob18
## pob18  <- pob18 [, grep("edosecn|^p18",  colnames(pob18) )]
## pobtot <- pobtot[, grep("edosecn|^ptot", colnames(pobtot))]
## head(pobtot); head(pob18)

#############################################################
## drop casilla longitude/latitude from files that have it ##
#############################################################
d <- v06 # duplicate for manipulation
sel.c <- grep("longitude|latitude", colnames(d), ignore.case=TRUE)
if (length(d)>0) d <- d[,-sel.c]
v06 <- d # return to data
#
d <- v09 # duplicate for manipulation
sel.c <- grep("longitude|latitude", colnames(d), ignore.case=TRUE)
if (length(d)>0) d <- d[,-sel.c]
v09 <- d # return to data
#
## v12 has no long/lat
#
d <- v15 # duplicate for manipulation
sel.c <- grep("longitude|latitude", colnames(d), ignore.case=TRUE)
if (length(d)>0) d <- d[,-sel.c]
v15 <- d # return to data
#
d <- v18 # duplicate for manipulation
sel.c <- grep("longitude|latitude", colnames(d), ignore.case=TRUE)
if (length(d)>0) d <- d[,-sel.c]
v18 <- d # return to data

#############################################################
##   consolidate districts & municipios before secciones   ##
##   are manipulated to deal with reseccionamiento         ##
#############################################################
## eg. 1979 map federal election counterfactuals        ##
## to post-dict 1988 vote (cf. cantú, but longitudinal) ##
##########################################################
# function to sort df columns
source("/home/eric/Dropbox/data/useful-functions/moveme.r")
#
mundis$edosecn <- mundis$edon*10000 + mundis$seccion
# add counterfactual district and municipios to votes
sel.drop <- which(colnames(mundis) %in% c("edon","seccion","ife")) # do not merge these repeated columns
# v91 <- merge(x = v91, y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v91$munn <- NULL
v94 <- merge(x = v94,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v94$munn <- NULL
v97 <- merge(x = v97,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v97$munn <- NULL
v00 <- merge(x = v00,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v00$munn <- NULL
v03 <- merge(x = v03,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v03$munn <- NULL
v06 <- merge(x = v06,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v06$munn <- NULL
v09 <- merge(x = v09,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v09$munn <- NULL
v12 <- merge(x = v12,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v12$munn <- NULL
v15 <- merge(x = v15,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v15$munn <- NULL
v18 <- merge(x = v18,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v18$munn <- NULL
v21 <- merge(x = v21,  y = mundis[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v21$munn <- NULL
#
rm(mundis)

####################################################
## fill ife codes in secciones that changed munic ##
####################################################
ife.inegi <- eq[,c("inegi","ife")] # correct inegi where ife changed
ife.inegi <- ife.inegi[duplicated(ife.inegi$ife)==FALSE,]
library(plyr)
v94$ife <- v94$ife1994
v94$inegi <- mapvalues(v94$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v97$ife <- v97$ife1997
v97$inegi <- mapvalues(v97$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v00$ife <- v00$ife2000
v00$inegi <- mapvalues(v00$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v03$ife <- v03$ife2003
v03$inegi <- mapvalues(v03$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v06$ife <- v06$ife2006
v06$inegi <- mapvalues(v06$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v09$ife <- v09$ife2009
v09$inegi <- mapvalues(v09$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v12$ife <- v12$ife2012
v12$inegi <- mapvalues(v12$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v15$ife <- v15$ife2015
v15$inegi <- mapvalues(v15$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v18$ife <- v18$ife2018
v18$inegi <- mapvalues(v18$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
v21$ife <- v21$ife2021
v21$inegi <- mapvalues(v21$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)
rm(ife.inegi)

# save all to restore after manipulating district/munic aggregates
save.image("../../datosBrutos/not-in-git/tmp-restore.RData")

# load image
rm(list=ls())
options(width = 110)
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
setwd(dd)
load(file="../../datosBrutos/not-in-git/tmp-restore.RData")

##############################################
## aggregate district and municipio returns ##
##############################################
##########
## 1991 ## OJO: 1991 seccion identifiers are wrong, can aggregate with disn/ife, but no info for counterfactuals
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec","lisnom","dextra")
# actual districts
d <- v91; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=TRUE) # use aggregating function
#d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
#d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
#d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v91d <- d                                      # rename object  
# actual municipalities
d <- v91; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
#d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
#d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
#d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; mun after ife")] # order columns
v91m <- d                                      # rename object  

##########
## 1994 ##
##########
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","lisnom","dextra")
# actual districts
d <- v94; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
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
d$disn <- d$dis2018                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d <- d[moveme(names(d), "efec before lisnom")] # order columns
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

##########
## 1997 ##
##########
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","lisnom","dextra")
# actual districts
d <- v97; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v97d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v97; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v97d79 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v97; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v97d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v97; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v97d18 <- d                                    # rename object  
# actual municipalities
d <- v97; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d[1,]
v97m <- d                                      # rename object  

##########
## 2000 ##
##########
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","lisnom","dpanc","dprdc","dextra")
# actual districts
d <- v00; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v00d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v00; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v00d79 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v00; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v00d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v00; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v00d18 <- d                                    # rename object  
# actual municipalities
d <- v00; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
v00m <- d                                      # rename object  

##########
## 2003 ##
##########
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","lisnom","dpric","dextra")
# actual districts
d <- v03; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v03d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v03; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v03d79 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v03; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v03d06 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v03; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v03d18 <- d                                    # rename object  
# actual municipalities
d <- v03; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
v03m <- d                                      # rename object  

##########
## 2006 ##
##########
sel.c <- c("pan","pric","prdc","pna","asdc","efec","lisnom","dpric","dprdc","dextra")
# actual districts
d <- v06; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v06d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v06; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v06d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v06; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v06d97 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v06; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v06d18 <- d                                    # rename object  
# actual municipalities
d <- v06; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0)               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
v06m <- d                                      # rename object  

##########
## 2009 ##
##########
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc","dextra")
# actual districts
d <- v09; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v09d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v09; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v09d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v09; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v09d97 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v09; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v09d18 <- d                                    # rename object  
# actual municipalities
d <- v09; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dptc  <- as.numeric(d$dptc>0 )               # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
v09m <- d                                      # rename object  

##########
## 2012 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc","dextra")
# actual districts
d <- v12; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v12d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v12; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v12d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v12; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v12d97 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v12; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v12d18 <- d                                    # rename object  
# actual municipalities
d <- v12; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
v12m <- d                                      # rename object  

##########
## 2015 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc","dextra")
# actual districts
d <- v15; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v15d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v15; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v15d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v15; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v15d97 <- d                                    # rename object  
# 2018 counterfactual districts
d <- v15; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2018", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2018                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v15d18 <- d                                    # rename object  
# actual municipalities
d <- v15; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc <- as.numeric(d$dpanc>0)               # fix coalition dummies
d$dpric <- as.numeric(d$dpric>0)               # fix coalition dummies
d$dprdc <- as.numeric(d$dprdc>0 )              # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
v15m <- d                                      # rename object  

##########
## 2018 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# actual districts
d <- v18; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v18d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v18; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v18d79 <- d                                    # rename object  
# 1997 counterfactual districts
d <- v18; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1997", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1997                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v18d97 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v18; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v18d06 <- d                                    # rename object  
# actual municipalities
d <- v18; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
#d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife; mun after inegi")] # order columns
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
v18m <- d                                      # rename object  

##########
## 2021 ##
##########
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac","dextra")
# actual districts
d <- v21; d[is.na(d)] <- 0
sel.drop <- which(d$disn==0)                   # drop secciones added to keep v.. objects square
if (length(sel.drop)>0) d <- d[-sel.drop,]     # drop secciones added to keep v.. objects square
d <- my_agg(d=d, sel.c=sel.c, by="disn", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v21d <- d                                      # rename object  
# 1979 counterfactual districts
d <- v21; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis1979", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis1979                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v21d79 <- d                                    # rename object  
# 2006 counterfactual districts
d <- v21; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="dis2006", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d$disn <- d$dis2006                            # district ids for the historic map
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$ife <- d$inegi <- d$mun <- NULL              # drop municipio ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom")] # order columns
v21d06 <- d                                    # rename object  
# actual municipalities
d <- v21; d[is.na(d)] <- 0
d <- my_agg(d=d, sel.c=sel.c, by="ife", y1991=FALSE) # use aggregating function
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]     # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))]   # drop ife-yr vars
d <- d[,-grep("^dis[0-9]{4}$", colnames(d))]   # drop counterfactual districts
d$disn <- NULL                                 # drop actual distric ids
d$edosecn <- d$seccion    <- NULL              # drop seccion ids
d$dextra <- as.numeric(d$dextra>0)             # fix special elec dummy
d$dpanc    <- as.numeric(d$dpanc>0)            # fix coalition dummies
d$dpric    <- as.numeric(d$dpric>0)            # fix coalition dummies
d$dmorenac <- as.numeric(d$dmorenac>0 )        # fix coalition dummies
d <- d[moveme(names(d), "efec before lisnom; ife after edon; inegi after ife")] # order columns
v21m <- d                                      # rename object  

# verify nrow==300
table(
    c(nrow(v91d), nrow(v94d), nrow(v97d), nrow(v00d), nrow(v03d), nrow(v06d), nrow(v09d), nrow(v12d), nrow(v15d), nrow(v18d), nrow(v21d), nrow(v97d79), nrow(v00d79), nrow(v03d79), nrow(v06d79), nrow(v09d79), nrow(v12d79), nrow(v15d79), nrow(v18d79), nrow(v21d79), nrow(v94d18), nrow(v97d18), nrow(v00d18), nrow(v03d18), nrow(v06d18), nrow(v09d18), nrow(v12d18), nrow(v15d18))
)

13abr23: now prep all to re run dis and mun regressions

## ####################################################################################
## ## TEMPORARY: 1991 secciones miss proper identifier and aggregate incorrectly     ##
## ## Until that is fixed, this loads imperfectly manipulated municipio aggregates   ##
## ## Commented lines were used towards this solution, which resides in older commit ##
## ####################################################################################
## d <- read.csv("dip1991.csv", header=TRUE, stringsAsFactors=FALSE)
## d <- d[order(d$edon, d$seccion),]
## sel.c <-            c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- to.num(d,sel.c) # clean data
## d <- within(d, efec <- pan + pri + pps + prd + pfcrn + parm + pdm + prt + pem + pt)
## sel.r <- grep("Anulada", d$estatus) # casillas anuladas
## d[sel.r,sel.c] <- 0 # anuladas to 0
## d <- within(d, tot <- nul <- nr <- estatus <- NULL)
## ## aggregate municipio-level votes ##
## sel.c <- which(colnames(d) %in% sel.c); # extract indices
## for (i in sel.c){
##     #i <- sel.c[1] #debug
##     d[,i] <- ave(d[,i], as.factor(paste(d$edon, d$mun)), FUN=sum, na.rm=TRUE)
## }
## sel.r <- which(duplicated(as.factor(paste(d$edon, d$mun)))==TRUE)
## d <- d[-sel.r,]
## # clean
## d <- within(d, disn <- seccion <- casilla <- NULL)
## # export to process so that municipios appear in same order as remainder of v..m files
## # write.csv(d, file = "tmp91.csv", row.names=FALSE)
# load manipulated version
d <- read.csv(paste(dd, "../municipios/dipfed1991.csv", sep = ""), header=TRUE, stringsAsFactors=FALSE)
# NAs to zero in v91 -- still non-existing municipios
d[is.na(d)] <- 0
d$mun <- NULL
v91m <- d

##################################################################################################
## Generate counterfactual municipal aggregates to use as regressors for each year's regressand ##
## Note: full period counterfactuals needed to compute means for missing values                 ##
## After 2024 election, uncheck appropriate lines                                               ##
##################################################################################################
#
#################################
## 1991 counterfactuals needed ##
#################################
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2006)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2006 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf06 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2009)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2009 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf09 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2012)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2012 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf12 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2015)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2015 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf15 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2018)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2018 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf18 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2021)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2021 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf21 <- d
## #
## ## d <- v91; d[is.na(d)] <- 0
## ## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## ## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## ## d$edosecn <- d$seccion <- d$disn <- NULL
## ## d$ife <- d$ife2024 # use appropriate ife code
## ## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## ## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## ## v91m.cf24 <- d
#################################
## 1994 counterfactuals needed ##
#################################
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf06 <- d
#
d <- v94; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf09 <- d
#
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf12 <- d
#
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf15 <- d
#
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf18 <- d
#
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf21 <- d
#
## d <- v94; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v94m.cf24 <- d
#################################
## 1997 counterfactuals needed ##
#################################
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf06 <- d
#
d <- v97; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf09 <- d
#
d <- v97; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf12 <- d
#
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf15 <- d
#
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf18 <- d
#
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf21 <- d
#
## d <- v97; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v97m.cf24 <- d
#################################
## 2000 counterfactuals needed ##
#################################
d <- v00; d[is.na(d)] <- 0
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf06 <- d
#
d <- v00; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf09 <- d
#
d <- v00; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf12 <- d
#
d <- v00; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf15 <- d
#
d <- v00; d[is.na(d)] <- 0
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf18 <- d
#
d <- v00; d[is.na(d)] <- 0
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf21 <- d
#
## d <- v00; d[is.na(d)] <- 0
## sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dprdc")
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpanc <- as.numeric(d$dpanc>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v00m.cf24 <- d
#################################
## 2003 counterfactuals needed ##
#################################
d <- v03; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf06 <- d
#
d <- v03; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf09 <- d
#
d <- v03; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf12 <- d
#
d <- v03; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf15 <- d
#
d <- v03; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf18 <- d
#
d <- v03; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf21 <- d
#
## d <- v03; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v03m.cf24 <- d
#################################
## 2006 counterfactuals needed ##
#################################
## # UNNEEDED
## d <- v06; d[is.na(d)] <- 0
## sel.c <- c("pan","pric","prdc","pna","asdc","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2009)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2009
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v06m.cf06 <- d
#
d <- v06; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf09 <- d
#
d <- v06; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf12 <- d
#
d <- v06; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf15 <- d
#
d <- v06; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf18 <- d
#
d <- v06; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf21 <- d
#
d <- v06; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf24 <- d
#################################
## 2009 counterfactuals needed ##
#################################
d <- v09; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf06 <- d
#
## UNNEEDED
## d <- v09; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc")
## d <- ag.mun(d,sel.c, grouping=d$ife2009)
## d$dpric <- as.numeric(d$dpric>0)
## d$dptc <- as.numeric(d$dptc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2009
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v09m.cf09 <- d
#
d <- v09; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc")
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf12 <- d
#
d <- v09; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf15 <- d
#
d <- v09; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf18 <- d
#
d <- v09; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf21 <- d
#
## d <- v09; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$dptc <- as.numeric(d$dptc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v09m.cf24 <- d
#################################
## 2012 counterfactuals needed ##
#################################
d <- v12; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf06 <- d
#
d <- v12; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf09 <- d
#
## # UNNEEDED
## d <- v12; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc")
## d <- ag.mun(d,sel.c, grouping=d$ife2012)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2012
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v12m.cf12 <- d
#
d <- v12; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf15 <- d
#
d <- v12; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf18 <- d
#
d <- v12; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf21 <- d
#
## d <- v12; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v12m.cf24 <- d
## #
## d <- v12; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2027)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2027
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v12m.cf27 <- d
#################################
## 2015 counterfactuals needed ##
#################################
d <- v15; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf06 <- d
#
d <- v15; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf09 <- d
#
d <- v15; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf12 <- d
#
## # UNNEEDED
## d <- v15; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
## d <- ag.mun(d,sel.c, grouping=d$ife2015)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2015
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf15 <- d
#
d <- v15; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf18 <- d
#
d <- v15; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf21 <- d
#
## d <- v15; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf24 <- d
## #
## d <- v15; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2027)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2027
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf27 <- d
## #
## d <- v15; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2030)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2030
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf30 <- d
#################################
## 2018 counterfactuals needed ##
#################################
d <- v18; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v18m.cf06 <- d
#
d <- v18; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v18m.cf09 <- d
#
d <- v18; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v18m.cf12 <- d
#
d <- v18; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v18m.cf15 <- d
#
## # UNNEEDED
## d <- v18; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
## d <- ag.mun(d,sel.c, grouping=d$ife2018)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2018
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf18 <- d
#
d <- v18; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v18m.cf21 <- d
#
## d <- v18; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf24 <- d
## #
## d <- v18; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2027)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2027
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf27 <- d
## #
## d <- v18; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2030)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2030
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf30 <- d
## #
## d <- v18; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2033)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2033
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf33 <- d
#################################
## 2021 counterfactuals needed ##
#################################
d <- v21; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v21m.cf06 <- d
#
d <- v21; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v21m.cf09 <- d
#
d <- v21; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v21m.cf12 <- d
#
d <- v21; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v21m.cf15 <- d
#
d <- v21; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v21m.cf18 <- d
#
## # UNNEEDED
## d <- v21; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
## d <- ag.mun(d,sel.c, grouping=d$ife2021)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2021
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf21 <- d
#
## d <- v21; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf24 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2027)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2027
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf27 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2030)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2030
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf30 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2033)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2033
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf33 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2036)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2036
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf36 <- d

# for future cleaning
tmp.ls <- ls()
#tmp <- ls()
#setdiff(tmp, tmp.ls)


# drop columns before saving raw vote municipio files
v91m <-  within(v91m,  ord <- munn <- NULL)
v94m <-  within(v94m,  disn <- NULL) #  <- dpanc <- dpric <- dprdc <- NULL)
v97m <-  within(v97m,  disn <- NULL) #  <- dpanc <- dpric <- dprdc <- NULL)
v00m <-  within(v00m,  disn <- NULL) #  <- dpanc <- dpric <- dprdc <- NULL)
v03m <-  within(v03m,  disn <- NULL) #  <- dpanc <- dpric <- dprdc <- NULL)
v06m <-  within(v06m,  disn <- NULL) #  <- dpanc <- dpric <- dprdc <- NULL)
v09m <-  within(v09m,  disn <- NULL) #  <- dpanc <- dpric <- dprdc <- dptc <- NULL)
v12m <-  within(v12m,  disn <- NULL) #  <- dpanc <- dpric <- dprdc <- NULL)
v15m <-  within(v15m,  disn <- NULL) #  <- dpanc <- dpric <- dprdc <- dmorenac <- NULL)
v18m <-  within(v18m,  disn <- NULL) #  <- dpanc <- dpric <- dmorenac <- NULL)
v21m <-  within(v21m,  disn <- NULL) #  <- dpanc <- dpric <- dmorenac <- NULL)
v21mw <- within(v21mw, disn <- NULL) #  <- dpanc <- dpric <- dmorenac <- NULL)

# add missing municipios to v..m objects to get same dimensionality
tmp <- c(v91m$ife, v94m$ife, v97m$ife, v00m$ife, v03m$ife, v06m$ife, v09m$ife, v12m$ife, v15m$ife, v18m$ife, v21m$ife)
tmp <- c(tmp, v94m.cf06$ife, v94m.cf09$ife, v94m.cf12$ife, v94m.cf15$ife, v94m.cf18$ife, v94m.cf21$ife)
tmp <- unique(tmp)
tmp <- tmp[order(tmp)]
tmp <- data.frame(ife=tmp)
#head(tmp)
homog <- function(x){
    x <- merge(x, tmp, by = "ife", all = TRUE, sort = TRUE)
    return(x)
}
v91m <- homog(v91m)
v94m <- homog(v94m)
v97m <- homog(v97m)
v00m <- homog(v00m)
v03m <- homog(v03m)
v06m <- homog(v06m)
v09m <- homog(v09m)
v12m <- homog(v12m)
v15m <- homog(v15m)
v18m <- homog(v18m)
v21m <- homog(v21m)
v21mw <- homog(v21mw)
tmp.w  <- homog(tmp.w)
tmp.w2 <- homog(tmp.w2)
#
#v91m.cf06 <- homog(v91m.cf06)
#v91m.cf09 <- homog(v91m.cf09)
#v91m.cf12 <- homog(v91m.cf12)
#v91m.cf15 <- homog(v91m.cf15)
#v91m.cf18 <- homog(v91m.cf18)
#v91m.cf21 <- homog(v91m.cf21)
#
v94m.cf06 <- homog(v94m.cf06)
v94m.cf09 <- homog(v94m.cf09)
v94m.cf12 <- homog(v94m.cf12)
v94m.cf15 <- homog(v94m.cf15)
v94m.cf18 <- homog(v94m.cf18)
v94m.cf21 <- homog(v94m.cf21)
#
v97m.cf06 <- homog(v97m.cf06)
v97m.cf09 <- homog(v97m.cf09)
v97m.cf12 <- homog(v97m.cf12)
v97m.cf15 <- homog(v97m.cf15)
v97m.cf18 <- homog(v97m.cf18)
v97m.cf21 <- homog(v97m.cf21)
#
v00m.cf06 <- homog(v00m.cf06)
v00m.cf09 <- homog(v00m.cf09)
v00m.cf12 <- homog(v00m.cf12)
v00m.cf15 <- homog(v00m.cf15)
v00m.cf18 <- homog(v00m.cf18)
v00m.cf21 <- homog(v00m.cf21)
#
v03m.cf06 <- homog(v03m.cf06)
v03m.cf09 <- homog(v03m.cf09)
v03m.cf12 <- homog(v03m.cf12)
v03m.cf15 <- homog(v03m.cf15)
v03m.cf18 <- homog(v03m.cf18)
v03m.cf21 <- homog(v03m.cf21)
#
# v06m.cf06 <- homog(v06m.cf06) # UNNEEDED
v06m.cf09 <- homog(v06m.cf09)
v06m.cf12 <- homog(v06m.cf12)
v06m.cf15 <- homog(v06m.cf15)
v06m.cf18 <- homog(v06m.cf18)
v06m.cf21 <- homog(v06m.cf21)
#
v09m.cf06 <- homog(v09m.cf06)
# v09m.cf09 <- homog(v09m.cf09) # UNNEEDED
v09m.cf12 <- homog(v09m.cf12)
v09m.cf15 <- homog(v09m.cf15)
v09m.cf18 <- homog(v09m.cf18)
v09m.cf21 <- homog(v09m.cf21)
#
v12m.cf06 <- homog(v12m.cf06)
v12m.cf09 <- homog(v12m.cf09)
# v12m.cf12 <- homog(v12m.cf12) # UNNEEDED
v12m.cf15 <- homog(v12m.cf15)
v12m.cf18 <- homog(v12m.cf18)
v12m.cf21 <- homog(v12m.cf21)
#
v15m.cf06 <- homog(v15m.cf06)
v15m.cf09 <- homog(v15m.cf09)
v15m.cf12 <- homog(v15m.cf12)
# v15m.cf15 <- homog(v15m.cf15) # UNNEEDED
v15m.cf18 <- homog(v15m.cf18)
v15m.cf21 <- homog(v15m.cf21)
#
v18m.cf06 <- homog(v18m.cf06)
v18m.cf09 <- homog(v18m.cf09)
v18m.cf12 <- homog(v18m.cf12)
v18m.cf15 <- homog(v18m.cf15)
# v18m.cf18 <- homog(v18m.cf18) # UNNEEDED
v18m.cf21 <- homog(v18m.cf21)
#
v21m.cf06 <- homog(v21m.cf06)
v21m.cf09 <- homog(v21m.cf09)
v21m.cf12 <- homog(v21m.cf12)
v21m.cf15 <- homog(v21m.cf15)
v21m.cf18 <- homog(v21m.cf18)
# v21m.cf21 <- homog(v21m.cf21) # UNNEEDED

# verify
tmp <- c(
nrow(v91m), nrow(v94m), nrow(v97m), nrow(v00m), nrow(v03m), nrow(v06m), nrow(v09m), nrow(v12m), nrow(v15m), nrow(v18m), nrow(v21m),
nrow(v21mw), nrow(tmp.w),
#nrow(v91m.cf06),
nrow(v94m.cf06), nrow(v94m.cf09), nrow(v94m.cf12), nrow(v94m.cf15), nrow(v94m.cf18), nrow(v94m.cf21), 
nrow(v97m.cf06), nrow(v97m.cf09), nrow(v97m.cf12), nrow(v97m.cf15), nrow(v97m.cf18), nrow(v97m.cf21), 
nrow(v00m.cf06), nrow(v00m.cf09), nrow(v00m.cf12), nrow(v00m.cf15), nrow(v00m.cf18), nrow(v00m.cf21), 
nrow(v03m.cf06), nrow(v03m.cf09), nrow(v03m.cf12), nrow(v03m.cf15), nrow(v03m.cf18), nrow(v03m.cf21), 
                 nrow(v06m.cf09), nrow(v06m.cf12), nrow(v06m.cf15), nrow(v06m.cf18), nrow(v06m.cf21),
nrow(v09m.cf06),                  nrow(v09m.cf12), nrow(v09m.cf15), nrow(v09m.cf18), nrow(v09m.cf21), 
nrow(v12m.cf06), nrow(v12m.cf09),                  nrow(v12m.cf15), nrow(v12m.cf18), nrow(v12m.cf21), 
nrow(v15m.cf06), nrow(v15m.cf09), nrow(v15m.cf12),                  nrow(v15m.cf18), nrow(v15m.cf21), 
nrow(v18m.cf06), nrow(v18m.cf09), nrow(v18m.cf12), nrow(v18m.cf15),                  nrow(v18m.cf21), 
nrow(v21m.cf06), nrow(v21m.cf09), nrow(v21m.cf12), nrow(v21m.cf15), nrow(v21m.cf18))
#
if (min(tmp)==max(tmp)){
    print("OK, v..ms ALL HAVE SAME DIMENSIONALITY")
} else {
    print("ERROR: SOME v..m HAS DIFFERENT DIMENSIONALITY")
}
rm(homog)

# drop columns before saving raw vote seccion files
#v91s <- within(v91, munn <- NULL)
v94s <- within(v94, edosecn <- dpanc <- dpric <- dprdc <- NULL)
v97s <- within(v97, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- dpanc <- dpric <- dprdc <- NULL)
v00s <- within(v00, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v03s <- within(v03, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v06s <- within(v06, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v09s <- within(v09, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v12s <- within(v12, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v15s <- within(v15, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v18s <- within(v18, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v21s <- within(v21, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v94s <- within(v94s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v97s <- within(v97s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v00s <- within(v00s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v03s <- within(v03s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v06s <- within(v06s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v09s <- within(v09s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v12s <- within(v12s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v15s <- within(v15s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v18s <- within(v18s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
v21s <- within(v21s, ife1994 <- ife1997 <- ife2000 <- ife2003 <- ife2006 <- ife2009 <- ife2012 <- ife2015 <- ife2018 <- ife2021 <- NULL)
#
#write.csv(v91s, file = paste(wd, "data/dipfed-seccion-vraw-1991.csv", sep = ""), row.names = FALSE)
write.csv(v94s, file = paste(wd, "data/dipfed-seccion-vraw-1994.csv", sep = ""), row.names = FALSE)
write.csv(v97s, file = paste(wd, "data/dipfed-seccion-vraw-1997.csv", sep = ""), row.names = FALSE)
write.csv(v00s, file = paste(wd, "data/dipfed-seccion-vraw-2000.csv", sep = ""), row.names = FALSE)
write.csv(v03s, file = paste(wd, "data/dipfed-seccion-vraw-2003.csv", sep = ""), row.names = FALSE)
write.csv(v06s, file = paste(wd, "data/dipfed-seccion-vraw-2006.csv", sep = ""), row.names = FALSE)
write.csv(v09s, file = paste(wd, "data/dipfed-seccion-vraw-2009.csv", sep = ""), row.names = FALSE)
write.csv(v12s, file = paste(wd, "data/dipfed-seccion-vraw-2012.csv", sep = ""), row.names = FALSE)
write.csv(v15s, file = paste(wd, "data/dipfed-seccion-vraw-2015.csv", sep = ""), row.names = FALSE)
write.csv(v18s, file = paste(wd, "data/dipfed-seccion-vraw-2018.csv", sep = ""), row.names = FALSE)
write.csv(v21s, file = paste(wd, "data/dipfed-seccion-vraw-2021.csv", sep = ""), row.names = FALSE)


# save municipal winners with correct manipulated data for new municipalities
# v.. needed to work with winner script
v91 <- v91m;
v94 <- v94m; v97 <- v97m; v00 <- v00m; v03 <- v03m; v06 <- v06m; v09 <- v09m; v12 <- v12m; v15 <- v15m; v18 <- v18m; v21 <- v21m; v21w <- v21mw
# get unit winners and margins: will output object winner for chosen agg
agg <- "m"
source(paste(wd, "code/get-winners.r", sep = ""))
tail(winner) # NAs before new mun creation
# save first part of output
write.csv(winner,
          file = paste(wd, "data/dipfed-municipio-win.csv", sep = ""), row.names = FALSE)
#
rm(tmp,tmp.w,tmp.w2) # drop to avoid confusion


# saves fixed mun raw aggregates
#write.csv(v91m, file = paste(wd, "data/dipfed-municipio-vraw-1991.csv", sep = ""), row.names = FALSE)
write.csv(v94m,  file = paste(wd, "data/dipfed-municipio-vraw-1994.csv", sep = ""), row.names = FALSE)
write.csv(v97m,  file = paste(wd, "data/dipfed-municipio-vraw-1997.csv", sep = ""), row.names = FALSE)
write.csv(v00m,  file = paste(wd, "data/dipfed-municipio-vraw-2000.csv", sep = ""), row.names = FALSE)
write.csv(v03m,  file = paste(wd, "data/dipfed-municipio-vraw-2003.csv", sep = ""), row.names = FALSE)
write.csv(v06m,  file = paste(wd, "data/dipfed-municipio-vraw-2006.csv", sep = ""), row.names = FALSE)
write.csv(v09m,  file = paste(wd, "data/dipfed-municipio-vraw-2009.csv", sep = ""), row.names = FALSE)
write.csv(v12m,  file = paste(wd, "data/dipfed-municipio-vraw-2012.csv", sep = ""), row.names = FALSE)
write.csv(v15m,  file = paste(wd, "data/dipfed-municipio-vraw-2015.csv", sep = ""), row.names = FALSE)
write.csv(v18m,  file = paste(wd, "data/dipfed-municipio-vraw-2018.csv", sep = ""), row.names = FALSE)
write.csv(v21m,  file = paste(wd, "data/dipfed-municipio-vraw-2021.csv", sep = ""), row.names = FALSE)

# clean
rm(ag.mun,ag.sec,d,sel,sel.c,sel.drop,sel.r,to.num)

ToDo jul2021:
1)  [x] push reload and seccion data manip for later
2)  [x] v5 for factual and counterfactual v..ms
3)  [x] alpha regs seem to need little manipulation: generate year swings with each v..m, then regress as before
4)  [x] beta regs need to rely on appropriate counterfactuals instead of factual v..ms
5)  [x] Fix winners 2021: prepare temp coal agg object to use with pri in it to determine correct unit winners
6)  [x] Why do vhats have NA 2467 line? 
7)  [ ] Why do vhats have NA in ife 7124?
8)  [ ] Seybaplaya+Dzitbalche have 0s d.pan, bhats, and betahats... usa means en vez de cf? Quizás reseccionamiento post 2018 juega papel
9)  [x] Al agregar votos 2021, no suman bien el voto PRI en vhat (ver foto en ife.ine/data)
10) [x] Separar 2021 pvem de morenac
11) [ ] Separar tambien prd y pt de sus coaliciones? otros años también?
12) [ ] Ya puedo generar vhat.2024 (con municipios 21)
Cuando haya codificado historia de AMGE:
13) [ ] Debug seccion winners (crear un tmp.w con pan21pri y v21sw, como con municipios)
14) [X] Fix seccion action and to.from
15) [ ] Fix pob18 missing secciones in order to project inter-census years for turnout

## TEMPORARY FOR DEBUGGING
#save.image("../../datosBrutos/not-in-git/tmp.RData")

rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(dd)
load("../../datosBrutos/not-in-git/tmp.RData")

# this may be needed in case coal dummies cause troubles below
v94m  <-  within(v94m,  dpanc <- dpric <- dprdc <- NULL)
v97m  <-  within(v97m,  dpanc <- dpric <- dprdc <- NULL)
v00m  <-  within(v00m,  dpanc <- dpric <- dprdc <- NULL)
v03m  <-  within(v03m,  dpanc <- dpric <- dprdc <- NULL)
v06m  <-  within(v06m,  dpanc <- dpric <- dprdc <- NULL)
v09m  <-  within(v09m,  dpanc <- dpric <- dprdc <- dptc <- NULL)
v12m  <-  within(v12m,  dpanc <- dpric <- dprdc <- NULL)
v15m  <-  within(v15m,  dpanc <- dpric <- dprdc <- dmorenac <- NULL)
v18m  <-  within(v18m,  dpanc <- dpric <- dmorenac <- NULL)
v21m  <-  within(v21m,  dpanc <- dpric <- dmorenac <- NULL)
v21mw <- within(v21mw, dpanc <- dpric <- dmorenac <- NULL)

#########################################################################################
## reload data to restore unmanipulated vote files (but keeping manipulated mun votes) ##
#########################################################################################
# save munic manipulations for use below
tmp <- ls(); tmp <- tmp[grep("^v..m", tmp)]
save(list=tmp, file = paste0(wd, "data/too-big-4-github/tmp-mun.RData"))
# clean all
rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(wd)
load(paste0(wd, "data/too-big-4-github/tmp.RData"))
# rename seccion vote objects
v94s <- v94; v97s <- v97; v00s <- v00; v03s <- v03; v06s <- v06; v09s <- v09; v12s <- v12; v15s <- v15; v18s <- v18; v21s <- v21;
rm(v94, v97, v00, v03, v06, v09, v12, v15, v18, v21)
# ignore NAs warning, inegi codes for new 2021 campeche munic still missing
# reload manipulated munic votes
load(paste0(wd, "data/too-big-4-github/tmp-mun.RData"))

#####################################################################
## Manipulate reseccionamiento cases to preserve them in analysis. ##
## Note: will not affect municipal aggregates, done above.         ##
## Note 16jul2021: why not do this before manipulating municipios, ##
## which now have been manipulated in ife.1991, ife.1994 etc.?     ##
#####################################################################
code/manip-dis1979-dis2018-to-assign-dropped-secciones.r
does this for federal districts. Adapt it to also deal with
couterfactual secciones



## ##########################################################################################
## ## generate yearly linear projections of pob18 (routine takes care of reseccionamiento) ##
## ##########################################################################################
## # fix seccion with p18=ptot
## sel <- which(pob18$edosecn==120348)
## pob18$p18_2005[sel] <- 100 # set to 45% ptot, as in 2010
## # fix seccion 152717 --- inside campo militar 1, casilla prob moved to contiguous seccion
## ## 2006 lisnom=318
## ## 2009 lisnom=312
## ## 2012 lisnom=156
## ## 2015-on vanished
## sel <- which(pob18$edosecn==152717)
## pob18$p18_2020[sel] <- 1
## pobtot$ptot_2020[sel] <- 1
## # fix 5 secciones in censo 2020 with ptot<p18 (all tiny)
## sel <- which(pob18$edosecn %in% c(51480, 143253, 250329, 250640, 252415))
## pobtot$ptot_2020[sel] <- pob18$p18_2020[sel]
## #
## # start by making a generic object for manipulation
## generic <- pob18
## head(generic)
## colnames(generic) <- c("edosecn","cen_2005","cen_2010","cen_2020")
## #
## source(paste0(wd, "code/code-to-manip-census-in-split-secciones.r"))
## # 
## # output is an object named eq2, rename it
## eq2[1,]
## table(eq2$times.manip, eq2$action, useNA = "always")
## eq2$dmanip <- as.numeric(eq2$times.manip>1)
## #sel <- c("edosecn", paste0("y", 1997:2022), "dmanip") # keep select columns only
## pob18y <- eq2#[,sel] # rename year-by-year projections
## rm(eq2, eq3) # clean
## 
## ##########################################################################################
## ## generate yearly linear projections of pobtot (routine takes care of reseccionamiento) ##
## ##########################################################################################
## # start by making a generic object for manipulation
## generic <- pobtot
## head(generic)
## colnames(generic) <- c("edosecn","cen_2005","cen_2010","cen_2020")
## #
## source(paste0(wd, "code/code-to-manip-census-in-split-secciones.r"))
## # 
## # output is an object named eq2, rename it
## eq2[1,]
## eq2$dmanip <- as.numeric(eq2$times.manip>1)
## #sel <- c("edosecn", paste0("y", 1997:2022), "dmanip") # keep select columns only
## pobtoty <- eq2#[,sel] # rename year-by-year projections
## rm(eq2, eq3) # clean
## 
## ########################################################
## ## there are negative projections, make those equal 1 ##
## ########################################################
## # pob18
## sel.c <- paste0("y", 1997:2022) # yearly projection cols
## sel <- which(apply(X=pob18y[,sel.c], 1, min) < 0) # row w neg values
## tmp <- pob18y[,sel.c]
## tmp[tmp<=0] <- 1
## pob18y[,sel.c] <- tmp
## # pobtot
## sel <- which(apply(X=pobtoty[,sel.c], 1, min) < 0) # row w neg values
## tmp <- pobtoty[,sel.c]
## tmp[tmp<=0] <- 1
## pobtoty[,sel.c] <- tmp
## tmp[2,]
## # fix 152716 v=83 in 1994, v=88 in 2000, v=0 in 2003 pop=0 in 2005 (merged to 2717 in 2010)---make p18=170=ptot constant since 1997
## sel <- which(pobtoty$edosecn==152716)
## selc <- grep("y199[7-9]|y200[0-2]", colnames(pob18y))
## pobtoty[sel,selc] <- pob18y[sel,selc] <- 170
## selc <- grep("y200[3-9]|y201[0-9]|y202[0-9]", colnames(pob18y))
## pobtoty[sel,selc] <- 1
## pob18y[sel,selc] <- 1
## # fix seccion 190439
## sel <- which(pobtoty$edosecn==190439)
## selc <- grep("y200[5-9]|y201[0-9]|y202[0-9]", colnames(pob18y))
## pobtoty[sel,selc] <- 1
## pob18y[sel,selc] <- 1
## selc <- grep("y2003", colnames(pob18y))
## pobtoty[sel,selc] <- 37000
## pob18y[sel,selc] <- 20000
## selc <- grep("y2004", colnames(pob18y))
## pobtoty[sel,selc] <- 50000
## pob18y[sel,selc] <- 30000
## 
## ## ######################################
## ## ## compare lisnom to p18 projection ##
## ## ######################################
## ## share <- cbind(edosecn=pob18y$edosecn,
## ##                pob18y[,sel.c] / pobtoty[,sel.c])
## ## summary(share)
## ## share[1,]
## ## # there are units where p18=ptot---they're either small or special (eg 152717 is inside campo militar 1)
## ## summary(share[,"y2005"]>.999)
## ## summary(share[,"y2010"]>.999)
## ## summary(share[,"y2020"]>.999)
## ## sel <- which(share[,"y2005"]>.999)
## ## tmp <- data.frame(edosecn=pob18y$edosecn[sel],
## ##                   p18=pob18y[sel,"y2005"],
## ##                   ptot=pobtoty[sel,"y2005"],
## ##                   dif=pob18y[sel,"y2005"]-pobtoty[sel,"y2005"])
## ## table(tmp$dif)
## ## summary(tmp$p18)
## ## sel <- which(share[,"y2010"]>.999)
## ## tmp <- data.frame(edosecn=pob18y$edosecn[sel],
## ##                   p18=pob18y[sel,"y2010"],
## ##                   ptot=pobtoty[sel,"y2010"],
## ##                   dif=pob18y[sel,"y2010"]-pobtoty[sel,"y2010"])
## ## table(tmp$dif)
## ## summary(tmp$p18)
## ## sel1 <- which(tmp$p18>20)
## ## tmp[sel1,]
## ## #
## ## sel <- which(share[,"y2020"]>.999)
## ## tmp <- data.frame(edosecn=pob18y$edosecn[sel],
## ##                   p18=pob18y[sel,"y2020"],
## ##                   ptot=pobtoty[sel,"y2020"],
## ##                   dif=pob18y[sel,"y2020"]-pobtoty[sel,"y2020"])
## ## table(tmp$dif)
## ## summary(tmp$p18)
## ## sel1 <- which(tmp$p18>20)
## ## tmp[sel1,]
## 
## # good number of projections below 45degree line
## sel <- which(as.integer(pobtoty$edosecn/10000)==24) # pick one edon
## lo <- 0; hi <- 12000
## plot(x=c(lo, hi), y=c(lo, hi), type = "n", xlab = "p18", ylab = "ptot")
## for (i in sel){
##     points(x=pob18y[i,sel.c], y=pobtoty[i,sel.c], pch=19, cex = .75, col = rgb(1,0,0, alpha = .15))
## }
## abline(a=0,b=1)
## 
## tmp <- pobtoty[sel,sel.c] - pob18y[sel,sel.c]
## tmp.f <- function(x) min(x, na.rm = TRUE)
## sel1 <- which(apply(X=tmp, 1, FUN=tmp.f) < 0)
## 
## pobtoty[sel[sel1[1]],sel.c]
## pob18y[sel[sel1[1]],sel.c]
## pobtoty[sel[sel1[1]],sel.c] - pob18y[sel[sel1[1]],sel.c]
## 
## # manipulate out-of-range projections
## tmp.mean <- mean(as.matrix(share[,sel.c]), na.rm = TRUE) # will force out-of-range projections to mean
## sel <- which(pobtoty[,sel.c[1]]== 1   &   pob18y[,sel.c[1]]==1) # rows equal 1 in both
## x
## 
## tmp <-  pob18y [, c("edosecn", "y2021")]
## tmp2 <- pobtoty[, c("edosecn", "y2021")]
## colnames(tmp) <- c("edosecn","p18")
## colnames(tmp2) <- c("edosecn","ptot")
## share <- data.frame(edosecn = tmp[,1],
##                     sh = tmp[,-1] / tmp2[,-1])
## head(share)
## 
## tmp <- merge(tmp, tmp2, by = "edosecn")
## tmp <- within(tmp, sh <-  p18 / ptot)
## summary(tmp$sh)
## head(tmp)
## 
## 
## tmp <- tmp[order(tmp$sh),]
## head(tmp)
## 
## head(v21s)
## tmp <- pob18y[, c("edosecn", "y2021")]
## tmp$p18 <- tmp[,2]
## head(tmp)
## tmp2 <-  v21s[, c("edosecn","lisnom")]
## tmp $v1 <- 1
## tmp2$v2 <- 2
## tmp <- merge(x = tmp, y = tmp2, by = "edosecn", all = TRUE)
## tmp$orig <- "neither"
## tmp$orig[tmp$v1==1] <- "p18y"
## tmp$orig[tmp$v2==2] <- "v09s"
## tmp$orig[tmp$v1==1 & tmp$v2==2] <- "both"
## table(tmp$orig)
## tmp <- within(tmp, dif <- abs(lisnom - p18) / lisnom)
## summary(tmp$dif)
## 
## tmp <- tmp[order(-tmp$dif),]
## tmp[70721:70730,]
## tail(tmp)
## x

rm(d,d2,sel,sel.c,sel.drop,sel.r,tmp)

# 13ago2021: eq$action is not just "split" but "split to" or "split from" 
source(paste(wd, "code/resecc-deal-with-splits.r", sep = ""))

############################################################
## ###################################################### ##
## ## Determine level of aggregation to work with here ## ##
## ## by choosing s, m, d...                           ## ##
## ###################################################### ##
############################################################
agg <- c("m","s","d")[1]
if (agg=="m") {
    v91 <- v91m;
    v94 <- v94m; v97 <- v97m; 
    v00 <- v00m; v03 <- v03m; v06 <- v06m; v09 <- v09m; v12 <- v12m; v15 <- v15m; v18 <- v18m; v21 <- v21m;
}
if (agg=="s") {
    ## v91 <- v91m;
    v94 <- v94s; v97 <- v97s; 
    v00 <- v00s; v03 <- v03s; v06 <- v06s; v09 <- v09s; v12 <- v12s; v15 <- v15s; v18 <- v18s; v21 <- v21s;
}
if (agg=="d") {
    ## v91 <- v91m;
    v94 <- v94d; v97 <- v97d; 
    v00 <- v00d; v03 <- v03d; v06 <- v06d; v09 <- v09d; v12 <- v12d; v15 <- v15d; v18 <- v18d; v21 <- v21d;
}
#
# get unit winners and margins: will output object winner for chosen agg
if (agg=="s"|agg=="d") { # done for municipalities above, with manipulated data
    source(paste(wd, "code/get-winners.r", sep = ""))
}
#head(winner)
# save first part of output <-- OJO 20ago2020 ESTO TIENE ERRORES EN NUEVOS MUNICIPIOS, HAY QUE MANIPULAR ANTES DE GUARDAR
if (agg=="s") {
    write.csv(winner,
              file = paste(wd, "data/dipfed-seccion-win.csv", sep = ""), row.names = FALSE)
}

############################################
## prepare manipulated party objects      ##
## for time-series and alpha regressions  ##
## After 2024 election, uncheck/add lines ##
############################################
#
# version 1: extend partial coalitions across the board
# shares
pan <- data.frame(v91 =      with(v91,       ifelse(efec==0, NA,  pan  / efec)),
                  v94 =      with(v94,       ifelse(efec==0, NA,  pan  / efec)),
                  v97 =      with(v97,       ifelse(efec==0, NA,  pan  / efec)),
                  v00 =      with(v00,       ifelse(efec==0, NA,  panc / efec)),
                  v03 =      with(v03,       ifelse(efec==0, NA,  pan  / efec)),
                  v06 =      with(v06,       ifelse(efec==0, NA,  pan  / efec)),
                  v09 =      with(v09,       ifelse(efec==0, NA,  pan  / efec)),
                  v12 =      with(v12,       ifelse(efec==0, NA,  pan  / efec)),
                  v15 =      with(v15,       ifelse(efec==0, NA,  pan  / efec)),
                  v18 =      with(v18,       ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)), # drop mc?
                  v21 =      with(v21,       ifelse(efec==0, NA, (pan + panc + prd) / efec)))      # drop prd?
pan <- round(pan, 3)
#
pri <- data.frame(v91 =      with(v91,       ifelse(efec==0, NA,  pri  / efec)),
                  v94 =      with(v94,       ifelse(efec==0, NA,  pri  / efec)),
                  v97 =      with(v97,       ifelse(efec==0, NA,  pri  / efec)),
                  v00 =      with(v00,       ifelse(efec==0, NA,  pri / efec)),
                  v03 =      with(v03,       ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                  v06 =      with(v06,       ifelse(efec==0, NA,  pric / efec)),
                  v09 =      with(v09,       ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                  v12 =      with(v12,       ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                  v15 =      with(v15,       ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                  v18 =      with(v18,       ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)),  # drop pvem + pna?
                  v21 =      with(v21,       ifelse(efec==0, NA, (pri + pric) / efec)))
pri <- round(pri, 3)
#
left <- data.frame(v91 =      with(v91,       ifelse(efec==0, NA,  prd  / efec)),
                   v94 =      with(v94,       ifelse(efec==0, NA,  prd  / efec)),
                   v97 =      with(v97,       ifelse(efec==0, NA,  prd  / efec)),
                   v00 =      with(v00,       ifelse(efec==0, NA,  prdc / efec)),
                   v03 =      with(v03,       ifelse(efec==0, NA, (prd + pt + conve) / efec)),
                   v06 =      with(v06,       ifelse(efec==0, NA,  prdc / efec)),
                   v09 =      with(v09,       ifelse(efec==0, NA, (prd + pt + ptc + conve) / efec)),
                   v12 =      with(v12,       ifelse(efec==0, NA, (prd + prdc + pt + mc)  / efec)),
                   v15 =      with(v15,       ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
                   v18 =      with(v18,       ifelse(efec==0, NA, (morena + morenac + pt + pes) / efec)),
                   v21 =      with(v21,       ifelse(efec==0, NA, (morena + morenac + pt + pvem) / efec)))    # drop pt + pvem?
left <- round(left, 3)
#
oth <- data.frame(v91 =      with(v91,       ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt) / efec)),
                  v94 =      with(v94,       ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
                  v97 =      with(v97,       ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm) / efec)),
                  v00 =      with(v00,       ifelse(efec==0, NA, (pcd + parm + dsppn) / efec)),
                  v03 =      with(v03,       ifelse(efec==0, NA, (psn + pas + mp + plm + fc) / efec)),
                  v06 =      with(v06,       ifelse(efec==0, NA, (pna + asdc) / efec)),
                  v09 =      with(v09,       ifelse(efec==0, NA, (pna + psd) / efec)),
                  v12 =      with(v12,       ifelse(efec==0, NA,  pna / efec)),
                  v15 =      with(v15,       ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2) / efec)),
                  v18 =      with(v18,       ifelse(efec==0, NA, (indep1 + indep2) / efec)),
                  v21 =      with(v21,       ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep) / efec)))
oth <- round(oth, 3)
#
efec <- data.frame(v91 =      v91$efec,
                   v94 =      v94$efec,
                   v97 =      v97$efec,
                   v00 =      v00$efec,
                   v03 =      v03$efec,
                   v06 =      v06$efec,
                   v09 =      v09$efec,
                   v12 =      v12$efec,
                   v15 =      v15$efec,
                   v18 =      v18$efec,
                   v21 =      v21$efec)
#
# transpose to plug columns into new data.frames
pan <- t(pan)
pri <- t(pri)
left <- t(left)
oth <- t(oth)
efec <- t(efec)
#

extendCoal <- as.list(rep(NA, nrow(v00))) # empty list will receive one data.frame per municipio
if (agg=="m"){
    names(extendCoal) <- v00$ife
    # replicate for counterfactual munic data for regressions
    extendCoal.cf06 <- extendCoal.cf09 <- extendCoal.cf12 <- extendCoal.cf15 <- extendCoal.cf18 <- extendCoal.cf21 <- extendCoal
}
if (agg=="s") names(extendCoal) <- v00$edon*10000 + v00$seccion # untested
# loop over municipios/secciones
for (i in 1:nrow(v00)){
    #i <- 81 # debug
    message(sprintf("loop %s of %s", i, nrow(v00)))
    # votes with actual municipios to plug into distributed data
    tmp <- data.frame(yr   = seq(from=1991, to=2021, by=3),
                      pan  = pan[,i],
                      pri  = pri[,i],
                      left = left[,i],
                      oth  = oth[,i],
                      efec = efec[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec[is.na(tmp$efec) | tmp$efec==0] <- 1
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    # fill info to new list
    extendCoal[[i]] <- tmp
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
yr.means <- data.frame(yr = seq(1991,2021,3),
                       pan    = rep(NA,11),
                       pri    = rep(NA,11),
                       left   = rep(NA,11),
                       oth    = rep(NA,11))
#cs <- function(x) colSums(x, na.rm=TRUE)
if (agg=="s"){
    cs <- function(x) colSums(x[x$dunbaja==0,], na.rm=TRUE) # drops secciones that received aggregates upon splitting
} else {
    cs <- function(x) colSums(x, na.rm=TRUE) # 21jul2021: stopped working, asked for numeric, replaced with sum
}
#
# change with v91s when available
yr.means$pan   [1] <-  cs(v91)["pan"]                                                      / cs(v91)["efec"]
yr.means$pri   [1] <-  cs(v91)["pri"]                                                      / cs(v91)["efec"]
yr.means$left  [1] <-  cs(v91)["prd"]                                                      / cs(v91)["efec"]
yr.means$oth   [1] <- (cs(v91)["efec"] - cs(v91)["pan"] - cs(v91)["pri"] - cs(v91)["prd"]) / cs(v91)["efec"]
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
yr.means$pri   [11] <- (cs(v21s)["pri"]    + cs(v21s)["pric"])                                                            / cs(v21s)["efec"]
yr.means$left  [11] <- (cs(v21s)["morena"] + cs(v21s)["morenac"] + cs(v21s)["pt"]  + cs(v21s)["pvem"])                    / cs(v21s)["efec"]
yr.means$oth   [11] <- (cs(v21s)["mc"]     + cs(v21s)["pes"]     + cs(v21s)["rsp"] + cs(v21s)["fxm"] + cs(v21s)["indep"]) / cs(v21s)["efec"]
#
yr.means <- within(yr.means, mean.rpan    <- pan/pri)
yr.means <- within(yr.means, mean.rleft   <- left/pri)
yr.means <- within(yr.means, mean.roth    <- oth/pri)
#
yr.means[,2:8] <- round(yr.means[,2:8], 3)
#
# plug into data
for (i in 1:nrow(v00)){
    #i <- 2 # debug
    extendCoal     [[i]] <- cbind(extendCoal     [[i]], yr.means[,6:8])
#    extendCoal.cf06[[i]] <- cbind(extendCoal.cf06[[i]], yr.means[,6:8])
#    extendCoal.cf09[[i]] <- cbind(extendCoal.cf09[[i]], yr.means[,6:8])
#    extendCoal.cf12[[i]] <- cbind(extendCoal.cf12[[i]], yr.means[,6:8])
#    extendCoal.cf15[[i]] <- cbind(extendCoal.cf15[[i]], yr.means[,6:8])
#    extendCoal.cf18[[i]] <- cbind(extendCoal.cf18[[i]], yr.means[,6:8])
#    extendCoal.cf21[[i]] <- cbind(extendCoal.cf21[[i]], yr.means[,6:8])
}



#################################################################################################
## - should also try jags estimation to get post-sample of vhats and alphas                    ##
## - report mg effect of unit change in bar(v) at year's level instead of betahat (cf. Linzer) ##
#################################################################################################
#
###############################
## código de las regresiones ##
###############################
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
        data.frame(pan    = rep(NA, nrow(v00)),
                   pri  = rep(NA, nrow(v00)),
                   left = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       left   = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      left   = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                   # regression per municipio, each used to
                                   # predict votes in 2006:2021
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006 <- regs.2009 <- regs.2012 <- regs.2015 <- regs.2018 <- regs.2021 <- 
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regs <- list(pan    = tmp,
                  left   = tmp,
                  oth    = tmp,
                  readme = "No pri regs bec DVs are pri-ratios")
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoal, sum)
non.nas <- unlist(non.nas)
non.nas                     # debug
extendCoal[[206]]           # debug: 20jul2021 NA due to unreported sole sección in cps municipio
which(is.na(non.nas)==TRUE) # debug
non.nas <- which(is.na(non.nas)==FALSE)
#length(non.nas)
#    
for (i in non.nas){
    #i <- 81 # debug
    #i <- 44508 # debug
    message(sprintf("loop %s of %s", i, max(non.nas)))
    # subset data to single unit
    data.tmp <- extendCoal[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA,data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA,data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA,data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ##################################
    ## predict 2006 with last 5 els ## ojo: v91 needed
    ##################################
    year <- 2006
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2006[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006$pan[[i]]    <- reg.pan
    regs.2006$left[[i]]   <- reg.left
    regs.2006$oth[[i]]    <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
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
    regs.2009$pan[[i]]    <- reg.pan
    regs.2009$left[[i]]   <- reg.left
    regs.2009$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    year <- 2012
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-   lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    vhat.2012[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    regs.2012$pan[[i]]    <- reg.pan
    regs.2012$left[[i]]   <- reg.left  
    regs.2012$oth[[i]]    <- reg.oth
    # add unit id to name to list element
    if (agg=="m"){
        names(regs.2012$pan) [i] <- extendCoal[[i]]$ife[1]
        names(regs.2012$left)[i] <- extendCoal[[i]]$ife[1]
        names(regs.2012$oth) [i] <- extendCoal[[i]]$ife[1]
    }
    if (agg=="s"){
        names(regs.2012$pan) [i] <- extendCoal[[i]]$edosecn[1]
        names(regs.2012$left)[i] <- extendCoal[[i]]$edosecn[1]
        names(regs.2012$oth) [i] <- extendCoal[[i]]$edosecn[1]
    }
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left  
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left  
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left   <- lm(formula = log(left  /pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left  )$coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2015[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    regs.2015$pan[[i]]    <- reg.pan
    regs.2015$left[[i]] <- reg.left  
    regs.2015$oth[[i]]    <- reg.oth
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left  
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left  
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left   <- lm(formula = log(left  /pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left  )$coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2018[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    regs.2018$pan   [[i]] <- reg.pan
    regs.2018$left  [[i]] <- reg.left  
    regs.2018$oth   [[i]] <- reg.oth
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left  
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left  
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    year <- 2021
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left   <- lm(formula = log(left  /pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left  )$coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2021[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    regs.2021$pan   [[i]] <- reg.pan
    regs.2021$left  [[i]] <- reg.left  
    regs.2021$oth   [[i]] <- reg.oth
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left  
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left  
    #
    ##################################
    ## predict 2024 with last 5 els ##
    ##################################
    year <- 2024
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left   <- lm(formula = log(left  /pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left  )$coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2024[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    regs.2024$pan   [[i]] <- reg.pan
    regs.2024$left  [[i]] <- reg.left  
    regs.2024$oth   [[i]] <- reg.oth
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left  
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)    ~ mean.rpan, data = data.tmp)
    reg.left   <- lm(formula = log(left  /pri) ~ mean.rleft  , data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)    ~ mean.roth, data = data.tmp)
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
    mean.regs$pan   [[i]] <- reg.pan
    mean.regs$left  [[i]] <- reg.left  
    mean.regs$oth   [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left   <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.left   <- alphahat$left  [i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.left   <- betahat$left  [i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
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
    extendCoal[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################
#
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


# clean, all this is saved in extendCoal, mean.regs, regs.2006, regs.2009, regs.2012, regs.2015, regs.2018
extendCoalmanip[[1]]
rm(alphahat, betahat, bhat.left, bhat.pan, reg.left, reg.oth, reg.pan, rhat.left, rhat.oth, rhat.pan, vhat.2006, vhat.2009, vhat.2012, vhat.2015, vhat.2018, vhat.left, vhat.pan, vhat.pri)

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



##############################################################################################
## which municipios (or secciones within) have been relocated by parties in 2005 2013 2017? ##
##############################################################################################
e <- 2
y <- 2013
edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[e]
tmp <- read.csv(paste("redisProcess/maps-with-all-proposals/", y, "/", edo, "Fed.csv", sep = ""), stringsAsFactors = FALSE)
colnames(tmp)

moves <- tmp

work <- tmp[,grep("1", colnames(tmp))] # get escenario 1 columns


tmp.w <- work[,grep("pan.*1", colnames(work))] # get pan's counteroffers
if (ncol(tmp.w)>0){
    tmp.w <- tmp.w - work$escenario1 # get differences
    tmp.w[tmp.w!=0] <- 1 # make dummy
    dpanmoved <- rowSums(tmp.w)
}
#
tmp.w <- work[,grep("pri.*1", colnames(work))] # get pri's counteroffers
if (ncol(tmp.w)>0){
    tmp.w <- tmp.w - work$escenario1 # get differences
    tmp.w[tmp.w!=0] <- 1 # make dummy
    dprimoved <- rowSums(tmp.w)
}
#
tmp.w <- work[,grep("prd.*1", colnames(work))] # get prd's counteroffers
if (ncol(tmp.w)>0){
    tmp.w <- tmp.w - work$escenario1 # get differences
    tmp.w[tmp.w!=0] <- 1 # make dummy
    dprdmoved <- rowSums(tmp.w)
}
#
tmp.w <- work[,grep("morena.*1", colnames(work))] # get morena's counteroffers
if (ncol(tmp.w)>0){
    tmp.w <- tmp.w - work$escenario1 # get differences
    tmp.w[tmp.w!=0] <- 1 # make dummy
    dmorenamoved <- rowSums(tmp.w)
}
#
tmp.w <- work # get everyone's counteroffers
if (ncol(tmp.w)>0){
    tmp.w <- tmp.w - work$escenario1 # get differences
    tmp.w[tmp.w!=0] <- 1 # make dummy
    dmoved <- rowSums(tmp.w)
}



table(morena.w)

colnames(tmp)[e1]

tmp <- as.data.frame(matrix(1:9, nrow=3, ncol=3)) - c(1,0,1)
x

summary(eq)
colnames(eq)
x



# check these
table(v12$dpric, v12$pri>0)
sel <- which(v12$dpric==1 & (v12$pri>0)==TRUE)
sel <- which(v12$edon==4 & v12$disn==2)
v12m[sel,]
table(v03$dpric)
x

# falta intentar jags estimation
# falta leer presidenciales 94 00 06 12 18
# falta leer dipfed 91 94 97
# falta leer municipales 91--19

                                                
# rank, margin, winner
n <- nrow(v15)
win <- runnerup <- data.frame(e06=character(n), e09=character(n), e12=character(n), e15=character(n))
mg <- enp <- data.frame(e06=numeric(n), e09=numeric(n), e12=numeric(n), e15=numeric(n))
nwin <- data.frame(pan=numeric(n), pri=numeric(n), prd=numeric(n), oth=numeric(n))
meanNep <- volat <- maxChg <- vector(length=n)
rm(n)
#
# 2015
vot <- v15[,c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","ps","indep1","indep2","pric","prdc")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
win$e15 <- etiq[,1]
win$e15[rowSums(vot)==0] <- "."
runnerup$e15 <- etiq[,2]
mg$e15 <- (vot[,1] - vot[,2]) / rowSums(vot)
enp$e15 <- 1/rowSums((vot/rowSums(vot))^2)
#
## o15 <- vot  # object with ordered votes
## c15 <- etiq # object with ordered party labels

# 2012
vot <- v12[,c("pan","pri","pvem","pna","pric","prdc")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
win$e12 <- etiq[,1] 
win$e12[rowSums(vot)==0] <- "."
runnerup$e12 <- etiq[,2]
mg$e12 <- (vot[,1] - vot[,2]) / rowSums(vot)
enp$e12 <- 1/rowSums((vot/rowSums(vot))^2)
#
## o12 <- vot  # object with ordered votes
## c12 <- etiq # object with ordered party labels

# 2009
vot <- v09[,c("pan","pri","prd","pvem","pna","psd","pric","ptc")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
win$e09 <- etiq[,1] 
win$e09[rowSums(vot)==0] <- "."
runnerup$e09 <- etiq[,2]
mg$e09 <- (vot[,1] - vot[,2]) / rowSums(vot)
enp$e09 <- 1/rowSums((vot/rowSums(vot))^2)
#
## o09 <- vot  # object with ordered votes
## c09 <- etiq # object with ordered party labels

# 2006
vot <- v06[,c("pan","pric","prdc","pna","asdc")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
win$e06 <- etiq[,1] 
win$e06[rowSums(vot)==0] <- "."
runnerup$e06 <- etiq[,2]
mg$e06 <- (vot[,1] - vot[,2]) / rowSums(vot)
enp$e06 <- 1/rowSums((vot/rowSums(vot))^2)
#
## o06 <- vot  # object with ordered votes
## c06 <- etiq # object with ordered party labels
#

# 2003
vot <- v03[,c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
win$e03 <- etiq[,1] 
win$e03[rowSums(vot)==0] <- "."
runnerup$e03 <- etiq[,2]
mg$e03 <- (vot[,1] - vot[,2]) / rowSums(vot)
enp$e03 <- 1/rowSums((vot/rowSums(vot))^2)
#
## o03 <- vot  # object with ordered votes
## c03 <- etiq # object with ordered party labels

# 2000
vot <- v00[,c("panc","pri","prdc","pcd","parm","dsppn")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
win$e00 <- etiq[,1] 
win$e00[rowSums(vot)==0] <- "."
runnerup$e00 <- etiq[,2]
mg$e00 <- (vot[,1] - vot[,2]) / rowSums(vot)
enp$e00 <- 1/rowSums((vot/rowSums(vot))^2)
#
## o00 <- vot  # object with ordered votes
## c00 <- etiq # object with ordered party labels
rm(vot, etiq)
save.image(file = "tmp.RData")
#
# nwin 
tmp <- win
tmp[] <- 0
tmp[win=="pan" | win=="panc"] <- 1
nwin$pan <- rowSums(tmp)
tmp[] <- 0
tmp[win=="pri" | win=="pric"] <- 1
nwin$pri <- rowSums(tmp)
tmp[] <- 0
tmp[win=="prd" | win=="prdc" | win=="morena"] <- 1 # por ahora, daré triunfos morena al prd
nwin$prd<- rowSums(tmp)
#
nwin$edon <- v06$edon
nwin$seccion <- v06$seccion
#
rm(tmp)

# get num casillas in each sección
ncasillas <- v06[,c("edosecn","edon","seccion")] # will receive data
# 
tmp <- read.csv(file=paste(dd, "DatosBrutos/resultCasillas/casillaRes91-on/dip2000.csv", sep = ""), stringsAsFactors = FALSE)
tmp$edosecn <- tmp$edon*10000 + tmp$seccion
tmp <- tmp[-grep("^S", tmp$casilla),] # drop casillas especiales
tmp$k <- 1
tmp$ncasillas <- ave(tmp$k, as.factor(tmp$edosecn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edosecn)==FALSE, c("edosecn","ncasillas")]
colnames(tmp) <- c("edosecn","e00")
ncasillas <- merge(x = ncasillas, y = tmp, by = "edosecn", all.x = TRUE, all.y = FALSE)
ncasillas[is.na(ncasillas)] <- 0
# 
tmp <- read.csv(file=paste(dd, "DatosBrutos/resultCasillas/casillaRes91-on/dip2003.csv", sep = ""), stringsAsFactors = FALSE)
tmp$edosecn <- tmp$edon*10000 + tmp$seccion
tmp <- tmp[-grep("^S", tmp$casilla),] # drop casillas especiales
tmp$k <- 1
tmp$ncasillas <- ave(tmp$k, as.factor(tmp$edosecn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edosecn)==FALSE, c("edosecn","ncasillas")]
colnames(tmp) <- c("edosecn","e03")
ncasillas <- merge(x = ncasillas, y = tmp, by = "edosecn", all.x = TRUE, all.y = FALSE)
ncasillas[is.na(ncasillas)] <- 0
# 
tmp <- read.csv(file=paste(dd, "DatosBrutos/resultCasillas/casillaRes91-on/dip2006.csv", sep = ""), stringsAsFactors = FALSE)
tmp$edosecn <- tmp$edon*10000 + tmp$seccion
tmp <- tmp[-grep("^S", tmp$casilla),] # drop casillas especiales
tmp$k <- 1
tmp$ncasillas <- ave(tmp$k, as.factor(tmp$edosecn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edosecn)==FALSE, c("edosecn","ncasillas")]
colnames(tmp) <- c("edosecn","e06")
ncasillas <- merge(x = ncasillas, y = tmp, by = "edosecn", all.x = TRUE, all.y = FALSE)
ncasillas[is.na(ncasillas)] <- 0
# 
tmp <- read.csv(file=paste(dd, "DatosBrutos/resultCasillas/casillaRes91-on/dip2009.csv", sep = ""), stringsAsFactors = FALSE)
tmp$edosecn <- tmp$edon*10000 + tmp$seccion
tmp <- tmp[-grep("^S", tmp$casilla),] # drop casillas especiales
tmp$k <- 1
tmp$ncasillas <- ave(tmp$k, as.factor(tmp$edosecn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edosecn)==FALSE, c("edosecn","ncasillas")]
colnames(tmp) <- c("edosecn","e09")
ncasillas <- merge(x = ncasillas, y = tmp, by = "edosecn", all.x = TRUE, all.y = FALSE)
ncasillas[is.na(ncasillas)] <- 0
# 
tmp <- read.csv(file=paste(dd, "DatosBrutos/resultCasillas/casillaRes91-on/dip2012.csv", sep = ""), stringsAsFactors = FALSE)
tmp$edosecn <- tmp$ID_ESTADO*10000 + tmp$SECCION
tmp <- tmp[-grep("^S", tmp$TIPO_CASILLA),] # drop casillas especiales
tmp$k <- 1
tmp$ncasillas <- ave(tmp$k, as.factor(tmp$edosecn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edosecn)==FALSE, c("edosecn","ncasillas")]
colnames(tmp) <- c("edosecn","e12")
ncasillas <- merge(x = ncasillas, y = tmp, by = "edosecn", all.x = TRUE, all.y = FALSE)
ncasillas[is.na(ncasillas)] <- 0
# 
tmp <- read.csv(file=paste(dd, "DatosBrutos/resultCasillas/casillaRes91-on/dip2015.csv", sep = ""), stringsAsFactors = FALSE)
tmp$edosecn <- tmp$edon*10000 + tmp$seccion
tmp <- tmp[-grep("^S", tmp$TIPO_CASILLA),] # drop casillas especiales
tmp$k <- 1
tmp$ncasillas <- ave(tmp$k, as.factor(tmp$edosecn), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edosecn)==FALSE, c("edosecn","ncasillas")]
colnames(tmp) <- c("edosecn","e15")
ncasillas <- merge(x = ncasillas, y = tmp, by = "edosecn", all.x = TRUE, all.y = FALSE)
ncasillas[is.na(ncasillas)] <- 0

setwd(wd)
save.image(file="elDatForMaps.RData")

wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/data/")
setwd(wd)
load(file = "elDatForMaps.RData")

table(ncasillas$e15)
