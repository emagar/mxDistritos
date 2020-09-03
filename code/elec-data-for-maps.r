## Code is a replica, updated, of /home/eric/Downloads/Desktop/MXelsCalendGovt/atlasDis/code/mapPrep.r
## Prepares various seccion-level measures of party performance in federal diputado elections
## retrieves electoral info from data in another repository: github.com/emagar/elecRturns

## esto prepara los datos electorales para mapear los distritos de cada estado.
## el contenido queda guardado en un archivo de datos.
## sólo tiene que correrse en caso de cambios.
rm(list=ls())
options(width = 140)
#
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")

# DROP #wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/data/")
# cambiar por cartografía 2017 en /home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/shp/disfed2018
md <- c("/home/eric/Dropbox/data/mapas/cartografia28feb2013rojano/")

setwd(dd)

###############################################
## function to aggregate seccion-level votes ##
###############################################
ag.sec <- function(d=d, sel.c=sel.c){
    sel.c <- which(colnames(d) %in% sel.c); # extract indices
    for (i in sel.c){
        #i <- sel.c[1] #debug
        d[,i] <- ave(d[,i], d$edon*10000+d$seccion, FUN=sum, na.rm=TRUE)
    }
    sel.r <- which(duplicated(d$edon*10000+d$seccion)==TRUE)
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


###################################
## ############################# ##
## ## read seccion-level data ## ##
## ############################# ##
###################################
## ##########
## ## 1991 ##
## ##########
## d <- read.csv("dip1991.csv", header=TRUE, stringsAsFactors=FALSE)
## d <- d[order(d$edon, d$seccion),]
## sel.c <-            c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- to.num(d,sel.c) # clean data
## d <- within(d, efec <- pan + pri + pps + prd + pfcrn + parm + pdm + prt + pem + pt)
## sel.r <- grep("Anulada", d$estatus) # casillas anuladas
## d[sel.r,sel.c] <- 0 # anuladas to 0
## d <- within(d, tot <- nul <- nr <- estatus <- NULL)
## ## aggregate seccion-level votes ##
## d <- ag.sec(d, sel.c)
## # clean
## d <- within(d, casilla <- NULL)
## v91 <- d
#
##########
## 1994 ##
##########
d <- read.csv("dip1994.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- to.num(d,sel.c) # clean data
d <- within(d, efec <- pan + pri + pps + prd + pfcrn + parm + uno.pdm + pt + pvem)
d <- within(d, tot <- nul <- nr <- NULL)
sel.r <- grep("Anulada", d$STATUS) # casillas anuladas
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
## coalition dummies
d <- within(d, dpanc <- dpric <- dprdc <- 0)
# clean
d <- within(d, casilla <- STATUS <- ID_ELEC <- NULL)
v94 <- d
#
##########
## 1997 ##
##########
d <- read.csv("dip1997.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- to.num(d,sel.c) # clean data
d <- within(d, efec <- pan + pri + prd + pc + pt + pvem + pps + pdm)
d <- within(d, tot <- nul <- nr <- NULL)
sel.r <- grep("MR", d$STATUS) # casillas anuladas
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
## coalition dummies
d <- within(d, dpanc <- dpric <- dprdc <- 0)
# clean
d <- within(d, casilla <- STATUS <- ID_ELEC <- NULL)
v97 <- d
#
##########
## 2000 ##
##########
d <- read.csv("dip2000.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
colnames(d)[which(colnames(d)=="ac")] <- "panc" # "pan-pvem"
colnames(d)[which(colnames(d)=="am")] <- "prdc" # "prd y muchos más"
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec")
d <- to.num(d,sel.c) # clean data
d <- within(d, efec <- panc + pri + prdc + pcd + parm + dsppn)
d <- within(d, tot <- nul <- nr <- NULL)
sel.r <- grep("MR", d$status) # casillas anuladas
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
d$dpric <- 0
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpanc, useNA = "always")
table(d$dprdc, useNA = "always")
# clean
d <- within(d, casilla <- status <- ID_ELEC <- NULL)
v00 <- d
#
##########
## 2003 ##
##########
d <- read.csv( "dip2003.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
colnames(d)[which(colnames(d)=="apt")] <- "pric" # "pri-pvem"
sel.c <- c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric","efec")
d <- to.num(d,sel.c) # clean data
d <- within(d, efec <- pan + pri + prd + pt + pvem + conve + psn + pas + mp + plm + fc + pric)
d <- within(d, tot <- nul <- nr <- NULL)
#
sel.r <- grep("MR|[Nn]o ", d$status) # casillas anuladas no instaladas
d[sel.r,sel.c] <- 0 # anuladas to 0
#
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# district coalition dummies
d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
table(d$dpric, useNA = "always")
d$dprdc <- 0
# aggregate coalitions where present for correct winner assesment
sel <- which(d$dpric==1)
d[sel,] <- within(d[sel,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0
})
# clean
d <- within(d, casilla <- status <- ID_ELEC <- NULL)
v03 <- d
#
##########
## 2006 ##
##########
d <- read.csv( "dip2006.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
# rename cols
colnames(d)[which(colnames(d)=="apm")] <- "pric" # "pri-pvem"
colnames(d)[which(colnames(d)=="pbt")] <- "prdc" # "prd-pt-conve"
colnames(d)[which(colnames(d)=="panal")] <- "pna" 
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- to.num(d,sel.c) # clean data
d <- within(d, efec <- pan + pric + prdc + pna + asdc)
d <- within(d, tot <- nul <- nr <- NULL)
sel.r <- grep("Anulada|No ", d$status) # casillas anuladas no instaladas
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
## coalition dummies
d <- within(d, {
    dpanc <- 0;
    dpric <- dprdc <- 1
})
# clean
d <- within(d, casilla <- ID_ELEC <- ord <- status <- nota <- valid <- NULL)
v06 <- d
#
##########
## 2009 ##
##########
d <- read.csv( "dip2009.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
colnames(d)[which(colnames(d)=="PRIMERO_MEXICO")] <- "pric" # pri-pvem
colnames(d)[which(colnames(d)=="SALVEMOS_MEXICO")] <- "ptc" # pt-conve"
colnames(d)[which(colnames(d)=="panal")] <- "pna"
sel.c <- c("pan","pri","prd","pvem","pt","conve","pna","psd","pric","ptc","efec","lisnom")
d <- to.num(d,sel.c) # clean data
d <- within(d, efec <- pan + pri + prd + pvem + pt + conve + pna + psd + pric + ptc)
d <- within(d, tot <- nul <- nr <- NULL)
sel.r <- grep("NO INSTALADA|SIN ACTA|NO ENTREGADO", d$status)
sel.r2 <- grep("ANULADA", d$tepjf) # casillas anuladas no instaladas
sel.r <- union(sel.r, sel.r2); rm(sel.r2)
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# district coalition dummies
d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- 0
d$dptc <- ave(d$ptc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dptc <- as.numeric(d$dptc>0)
table(d$dpric, useNA = "always")
table(d$dptc, useNA = "always")
# aggregate coalitions where present for correct winner assesment
sel <- which(d$dpric==1)
d[sel,] <- within(d[sel,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0
})
# clean
d <- within(d, circun <- edo <- cabecera <- munn <- casilla <- status <- tepjf <- NULL)
v09 <- d
#
##########
## 2012 ##
##########
d <- read.csv( "dip2012.csv", header=TRUE, , stringsAsFactors=FALSE)
colnames(d)[which(colnames(d)=="ID_ESTADO")] <- "edon"
colnames(d)[which(colnames(d)=="D_DISTRITO")] <- "disn"
colnames(d)[which(colnames(d)=="ID_CASILLA")] <- "casilla"
colnames(d)[which(colnames(d)=="SECCION")] <- "seccion"
colnames(d)[which(colnames(d)=="LISTA_NOMINAL_CASILLA")] <- "lisnom"
colnames(d)[which(colnames(d)=="ID_MUNICIPIO")] <- "munn"
colnames(d)[which(colnames(d)=="NUMERO_VOTOS_VALIDOS")] <- "efec"
colnames(d)[which(colnames(d)=="NUM_VOTOS_NULOS")] <- "nul"
colnames(d)[which(colnames(d)=="NUM_VOTOS_CAN_NREG")] <- "nr"
colnames(d)[which(colnames(d)=="TOTAL_VOTOS")] <- "tot"
d <- within(d, ORDEN <- LISTA_NOMINAL <- TIPO_RECUENTO <- ID_GRUPO <- VOTOS_RESERVADOS <- BOLETAS_INUTILIZADAS <- EXT_CONTIGUA <- TIPO_CANDIDATURA <- CASILLA <- NULL)
d <- d[order(d$edon, d$seccion),]
colnames(d)[which(colnames(d)=="PAN")]   <- "pan"
colnames(d)[which(colnames(d)=="PRI")]   <- "pri"
colnames(d)[which(colnames(d)=="PRD")]   <- "prd"
colnames(d)[which(colnames(d)=="PVEM")]  <- "pvem"
colnames(d)[which(colnames(d)=="PT")]    <- "pt"
colnames(d)[which(colnames(d)=="MC")]    <- "mc"
colnames(d)[which(colnames(d)=="PANAL")] <- "pna"
colnames(d)[which(colnames(d)=="PRI_PVEM")] <- "pric"
d <- within(d, prdc <- PRD_PT_MC  + PRD_PT  + PRD_MC  + PT_MC)
d <- within(d,         PRD_PT_MC <- PRD_PT <- PRD_MC <- PT_MC <- NULL)
#
sel.c <-            c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom")
d <- to.num(d,sel.c) # clean data
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + pric + prdc)
d <- within(d, tot <- nul <- nr <- NULL)
# recode status
table(d$ESTATUS_ACTA) # missing codebook, quizás 3 y 4 que sólo tienen NAs sean anulados/noentregados o algo así
d[which(d$ESTATUS_ACTA==3),]
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# coalition dummies
d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpric, useNA = "always")
table(d$dprdc, useNA = "always")
# aggregate coalitions where present for correct winner assesment
sel <- which(d$dpric==1)
d[sel,] <- within(d[sel,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0
})
sel <- which(d$dprdc==1)
d[sel,] <- within(d[sel,], {
    prdc <- prd + prdc + pt + mc;
    prd <- pt <- mc <- 0
})
# clean
d <- within(d, casilla <- TIPO_CASILLA <- ESTATUS_ACTA <- NULL)
v12 <- d
#
##########
## 2015 ##
##########
d <- read.csv( "dip2015.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
#
colnames(d)[which(colnames(d)=="panal")] <- "pna"
colnames(d)[which(colnames(d)=="pri_pvem")] <- "pric"
colnames(d)[which(colnames(d)=="prd_pt")] <- "prdc"
#
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom")
d <- to.num(d,sel.c) # clean data
#
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + morena + ph + pes + pric + prdc + indep1 + indep2)
d <- within(d, tot <- nul <- nr <- NULL)
#
d$dpanc <- 0
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
d$dmorenac <- 0
table(d$dpric, useNA = "always")
table(d$dprdc, useNA = "always")
# aggregate coalitions where present for correct winner assesment
sel <- which(d$dpric==1)
d[sel,] <- within(d[sel,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0
})
sel <- which(d$dprdc==1)
d[sel,] <- within(d[sel,], {
    prdc <- prd + prdc + pt;
    prd <- pt <- 0
})
#
sel.r <- grep("E6|E7", d$OBSERVACIONES) # casillas no instaladas
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# clean
d <- within(d, ord <- ID_CASILLA <- TIPO_CASILLA <- EXT_CONTIGUA <- OBSERVACIONES <- NULL)
v15 <- d
#
##########
## 2018 ##
##########
d <- read.csv( "dip2018.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
#
colnames(d)[which(colnames(d)=="panal")]     <- "pna"
colnames(d)[which(colnames(d)=="pan.prd.mc")]     <- "panc"
colnames(d)[which(colnames(d)=="pri.pvem.panal")] <- "pric"
colnames(d)[which(colnames(d)=="pt.morena.pes")]  <- "morenac"
#
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom")
d <- to.num(d,sel.c) # clean data
#
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + morena + pes + panc + pric + morenac + indep1 + indep2)
d <- within(d, nr <- nul <- tot <- NULL)
#
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dmorenac <- ave(d$morenac, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dmorenac <- as.numeric(d$dmorenac>0)
table(d$dpanc, useNA = "always")
table(d$dpric, useNA = "always")
table(d$dmorenac, useNA = "always")
# aggregate coalitions where present for correct winner assesment
sel <- which(d$dpanc==1)
d[sel,] <- within(d[sel,], {
    panc <- pan + panc + prd + mc;
    pan <- prd <- mc <- 0
})
sel <- which(d$dpric==1)
d[sel,] <- within(d[sel,], {
    pric <- pri + pric + pvem + pna;
    pri <- pvem <- pna <- 0
})
sel <- which(d$dmorenac==1)
d[sel,] <- within(d[sel,], {
    morenac <- morena + morenac + pt + pes;
    morena <- pt  <- pes <- 0
})
#
# sel.r <- grep("E6|E7", d$OBSERVACIONES) # casillas no instaladas
#d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# clean
d <- within(d, edo <- cabecera <- casn <- TIPO_CASILLA <- NULL)
v18 <- d

# clean
rm(d,sel.c,sel.r)

################################
## district winners 2006-2015 ##
################################
v06d <- v06; v09d <- v09; v12d <- v12; v15d <- v15; v18d <- v18
v06d <- within(v06d, {
    pan   <- ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pric  <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    prdc  <- ave(prdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pna   <- ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    asdc  <- ave(asdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
})
v06d <- v06d[duplicated(v06d$edon*100 + v06d$disn)==FALSE,]
v06d <- v06d[order(v06d$edon*100, v06d$disn),]
#
v09d <- within(v09d, {
    pan  <- ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pri  <- ave(pri,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pric <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    prd  <- ave(prd,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pvem <- ave(pvem, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pna  <- ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    psd  <- ave(psd,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    ptc  <- ave(ptc,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
})
v09d <- v09d[duplicated(v09d$edon*100 + v09d$disn)==FALSE,]
v09d <- v09d[order(v09d$edon*100, v09d$disn),]
#
v12d <- within(v12d, {
    pan <-  ave(pan,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pri <-  ave(pri,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pric <- ave(pric, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    prdc <- ave(prdc, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pvem <- ave(pvem, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pna <-  ave(pna,  as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
})
v12d <- v12d[duplicated(v12d$edon*100 + v12d$disn)==FALSE,]
v12d <- v12d[order(v12d$edon*100, v12d$disn),]
#
v15d <- within(v15d, {
    pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pna <-    ave(pna,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    ph <-     ave(ph,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    indep1 <- ave(indep1, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    indep2 <- ave(indep2, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    prdc <-   ave(prdc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
})
v15d <- v15d[duplicated(v15d$edon*100 + v15d$disn)==FALSE,]
v15d <- v15d[order(v15d$edon*100, v15d$disn),]
#
v18d <- within(v18d, {
    pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pna <-    ave(pna,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    panc <-   ave(panc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    morenac<- ave(morenac,as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    indep1 <- ave(indep1, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    indep2 <- ave(indep2, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
})
v18d <- v18d[duplicated(v18d$edon*100 + v18d$disn)==FALSE,]
v18d <- v18d[order(v18d$edon*100, v18d$disn),]
#
windis <- v15d[,c("edon","disn")] # will receive data
#
# handy function to sort one data frame by order of another, matching data frame
sortBy <- function(target, By){
    t <- target; b <- By;
    do.call(rbind, lapply(seq_len(nrow(b)), 
            function(i) as.character(unlist(t[i,])[order(unlist(-b[i,]))]))) # change to -b if decreasing wished
}
## # example
## v <- data.frame(c1=c(30,15,3), c2=c(10,25,2), c3=c(20,35,4))
## w <- data.frame(c1=c("thirty","fifteen","three"), c2=c("ten","twenty-five","two"), c3=c("twenty","thirty-five","four"))
## v.sorted <- t(apply(v, 1, function(x) sort(x, decreasing = TRUE))) # sort each row of df -- http://stackoverflow.com/questions/6063881/sorting-rows-alphabetically
## w.sorted <- sortBy(target = w, By = v)
## sortBy(target = v, By = v)
#
# 2006
vot <- v06d[,c("pan","pric","prdc","pna","asdc")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
windis$e06 <- etiq[,1] 
## runnerup$e06 <- etiq[,2]
## mg$e06 <- (vot[,1] - vot[,2]) / rowSums(vot)
## enp$e06 <- 1/rowSums((vot/rowSums(vot))^2)
#
# 2009
vot <- v09d[,c("pan","pri","prd","pvem","pna","psd","pric","ptc")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
windis$e09 <- etiq[,1] 
## runnerup$e09 <- etiq[,2]
## mg$e09 <- (vot[,1] - vot[,2]) / rowSums(vot)
## enp$e09 <- 1/rowSums((vot/rowSums(vot))^2)
#
# 2012
vot <- v12d[,c("pan","pri","pvem","pna","pric","prdc")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
windis$e12 <- etiq[,1] 
## runnerup$e12 <- etiq[,2]
## mg$e12 <- (vot[,1] - vot[,2]) / rowSums(vot)
## enp$e12 <- 1/rowSums((vot/rowSums(vot))^2)
#
# 2015
vot <- v15d[,c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","indep1","indep2","pric","prdc")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
windis$e15 <- etiq[,1]
## runnerup$e15 <- etiq[,2]
## mg$e15 <- (vot[,1] - vot[,2]) / rowSums(vot)
## enp$e15 <- 1/rowSums((vot/rowSums(vot))^2)
#
# 2018
vot <- v18d[,c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","indep1","indep2","panc","pric","morenac")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
windis$e18 <- etiq[,1]
## runnerup$e15 <- etiq[,2]
## mg$e15 <- (vot[,1] - vot[,2]) / rowSums(vot)
## enp$e15 <- 1/rowSums((vot/rowSums(vot))^2)
#
rm(vot,etiq)

#
## write.csv(windis, file = paste(dd, "dfdf2006-2015winners.csv", sep = ""))

# get equivalencias seccionales
tmp <- paste(wd, "equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", sep = "")
eq <- read.csv(tmp, stringsAsFactors = FALSE)
# get municipio info to merge into votes
muns <- eq[,c("edon","seccion","ife","inegi")]

# match yearly observations (secciones)
#dim(v06); dim(v09); dim(v12); dim(v15) # something amiss in 2009?
## v91 <- within(v91, {
##     edosecn <- edon*10000 + seccion;
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
# dummies d91 to d18 indicate if seccion exists each year
## tmp <- merge(x=v91[,c("edosecn","d91")], y=v94[,c("edosecn","d94")], by = "edosecn", all = TRUE)
## tmp <- merge(x=tmp,                      y=v97[,c("edosecn","d97")], by = "edosecn", all = TRUE)
tmp <- merge(x=v94[,c("edosecn","d94")], y=v97[,c("edosecn","d97")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v00[,c("edosecn","d00")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v03[,c("edosecn","d03")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v06[,c("edosecn","d06")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v09[,c("edosecn","d09")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v12[,c("edosecn","d12")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v15[,c("edosecn","d15")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v18[,c("edosecn","d18")], by = "edosecn", all = TRUE)
## v91$d91 <-
v94$d94 <- v97$d97 <- v00$d00 <- v03$d03 <- v06$d06 <- v09$d09 <- v12$d12 <- v15$d15 <- v18$d18 <- NULL # clean
#
# adds any missing secciones to each object
## v91 <- merge(x=tmp, y=v91, by = "edosecn", all = TRUE)
v94 <- merge(x=tmp, y=v94, by = "edosecn", all = TRUE)
v97 <- merge(x=tmp, y=v97, by = "edosecn", all = TRUE)
v00 <- merge(x=tmp, y=v00, by = "edosecn", all = TRUE)
v03 <- merge(x=tmp, y=v03, by = "edosecn", all = TRUE)
v06 <- merge(x=tmp, y=v06, by = "edosecn", all = TRUE)
v09 <- merge(x=tmp, y=v09, by = "edosecn", all = TRUE)
v12 <- merge(x=tmp, y=v12, by = "edosecn", all = TRUE)
v15 <- merge(x=tmp, y=v15, by = "edosecn", all = TRUE)
v18 <- merge(x=tmp, y=v18, by = "edosecn", all = TRUE)
# verify dimensionality
## dim(v91);
dim(v94); dim(v97); dim(v00); dim(v03); dim(v06); dim(v06); dim(v12); dim(v15);
# fill in missing edon and seccion numbers
tmp.func <- function(x) {
    within(x, {
        edon    <- as.integer(edosecn/10000);
        seccion <- edosecn - edon*10000;
    })
}
## v91 <- tmp.func(v91)
v94 <- tmp.func(v94)
v97 <- tmp.func(v97)
v00 <- tmp.func(v00)
v03 <- tmp.func(v03)
v06 <- tmp.func(v06)
v09 <- tmp.func(v09)
v12 <- tmp.func(v12)
v15 <- tmp.func(v15)
v18 <- tmp.func(v18)
rm(tmp,tmp.func) # clean


###################################################
## consolidate municipios before secciones       ##
## are manipulated to deal with reseccionamiento ##
###################################################
muns$edosecn <- muns$edon*10000 + muns$seccion
# add municipio names to votes
sel.drop <- which(colnames(muns) %in% c("edon","seccion")) # do not merge these columns
# v91 <- merge(x = v91, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v91$munn <- NULL
v94 <- merge(x = v94, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v94$munn <- NULL
v97 <- merge(x = v97, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v97$munn <- NULL
v00 <- merge(x = v00, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v00$munn <- NULL
v03 <- merge(x = v03, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v03$munn <- NULL
v06 <- merge(x = v06, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v06$munn <- NULL
v09 <- merge(x = v09, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v09$munn <- NULL
v12 <- merge(x = v12, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v12$munn <- NULL
v15 <- merge(x = v15, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v15$munn <- NULL
v18 <- merge(x = v18, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v18$munn <- NULL
rm(muns)

###########################################################################################################
## These objects will receive new municipios' vote manipulations, which otherwise aggregate incorrectly. ##
## There is an excel example of fixes in reelec/code/example-new-munic-manip.ods                         ##
###########################################################################################################
eq.new <- eq[which(eq$dmunchg==1),]      # dmunchg indicates new municipios only, not parents
sel.c <- grep("^mun[0-9]+",colnames(eq)) # select munyyyy columns
tmp <- as.matrix(eq.new[,sel.c])
tmp <- as.vector(tmp)
tmp <- unique(tmp)                       # vector with all parents and children
sel.r <- which(eq$ife %in% tmp)
# subset new municipios and their parents' secciones
eq.new <- eq[sel.r,]
# fill parents (dmunchg==0) munyyyy columns with ife 
eq.new[eq.new$dmunchg==0,sel.c] <- eq.new$ife[eq.new$dmunchg==0]
# re-compute edosecn as in v94, v97 etc
eq.new <- within(eq.new, edosecn <- edon*10000 + seccion)
# shave useless cols
eq.new <- eq.new[,-grep("^dis[0-9]+$", colnames(eq.new))]
#eq.new <- eq.new[,-grep("OBSERVACIONES|action|fr.to|orig.dest|when|color|coment", colnames(eq.new))]
eq.new <- eq.new[,-grep("action|fr.to|orig.dest|when|color|coment", colnames(eq.new))]
eq.new <- eq.new[,-grep("ord|^edo.?$|^mun.?$|inegi|ife|seccion", colnames(eq.new))]
#
# include manipulated ife in vote data
tmp <- eq.new[,  c("edosecn","dmunchg","mun1994")] # subset 1994 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v94manip <- merge(x = v94, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
tmp <- eq.new[,  c("edosecn","dmunchg","mun1997")] # subset 1997 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v97manip <- merge(x = v97, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
tmp <- eq.new[,  c("edosecn","dmunchg","mun2000")] # subset 2000 colv
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v00manip <- merge(x = v00, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
tmp <- eq.new[,  c("edosecn","dmunchg","mun2003")] # subset 2003 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v03manip <- merge(x = v03, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
tmp <- eq.new[,  c("edosecn","dmunchg","mun2006")] # subset 2006 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v06manip <- merge(x = v06, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
tmp <- eq.new[,  c("edosecn","dmunchg","mun2009")] # subset 2009 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v09manip <- merge(x = v09, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
tmp <- eq.new[,  c("edosecn","dmunchg","mun2012")] # subset 2012 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v12manip <- merge(x = v12, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
tmp <- eq.new[,  c("edosecn","dmunchg","mun2015")] # subset 2015 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v15manip <- merge(x = v15, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
tmp <- eq.new[,  c("edosecn","dmunchg","mun2018")] # subset 2018 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v18manip <- merge(x = v18, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
# replace ife with manipulated version
homog <- function(x){
    within(x, {
        ife <- ife.y;
        ife.x <- ife.y <- dmunchg <- NULL
        inegi <- NULL # no longer valid
        edon <- as.integer(edosecn/10000) # this eliminates missing edons
        }
    )
}
v94manip <- homog(v94manip)
v97manip <- homog(v97manip)
v00manip <- homog(v00manip)
v03manip <- homog(v03manip)
v06manip <- homog(v06manip)
v09manip <- homog(v09manip)
v12manip <- homog(v12manip)
v15manip <- homog(v15manip)
v18manip <- homog(v18manip)
rm(homog)

#################################################
## function to aggregate municipio-level votes ##
#################################################
ag.mun <- function(d=d, sel.c=sel.c){
    for (i in 1:length(sel.c)){
        # d[,sel.c[i]] <- ave(d[,sel.c[i]], d$edon*1000+d$inegi, FUN=sum, na.rm=TRUE) # use inegi codes
        # d[,sel.c[i]] <- ave(d[,sel.c[i]], d$edon*1000+d$ife, FUN=sum, na.rm=TRUE) # use ife codes
        d[,sel.c[i]] <- ave(d[,sel.c[i]],              d$ife, FUN=sum, na.rm=TRUE) # use ife codes
    }
    # sel.r <- which(duplicated(d$edon*1000+d$inegi)==TRUE) # use inegi codes
    # sel.r <- which(duplicated(d$edon*1000+d$ife)==TRUE) # use ife codes
    sel.r <- which(duplicated(d$ife)==TRUE) # use ife codes
    d <- d[-sel.r,]
    return(d)
}
## ##########
## ## 1991 ##
## ##########
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c)
## d$edosecn <- d$seccion <- NULL
## v91m <- d
##########
## 1994 ##
##########
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c)
d$edosecn <- d$seccion <- NULL
v94m <- d
##########
## 1997 ##
##########
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c)
d$edosecn <- d$seccion <- NULL
v97m <- d
##########
## 2000 ##
##########
d <- v00; d[is.na(d)] <- 0
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dprdc")
d <- ag.mun(d,sel.c)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v00m <- d
##########
## 2003 ##
##########
d <- v03; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- NULL
v03m <- d
##########
## 2006 ##
##########
d <- v06; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.mun(d,sel.c)
d$edosecn <- d$seccion <- NULL
v06m <- d
##########
## 2009 ##
##########
d <- v09; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- NULL
v09m <- d
##########
## 2012 ##
##########
d <- v12; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v12m <- d
##########
## 2015 ##
##########
d <- v15; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v15m <- d
##########
## 2018 ##
##########
d <- v18; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- NULL
v18m <- d
#
## drop seccion dummies
v94m <- within(v94m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v97m <- within(v97m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v00m <- within(v00m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v03m <- within(v03m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v06m <- within(v06m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v09m <- within(v09m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v12m <- within(v12m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v15m <- within(v15m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v18m <- within(v18m, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)

#
##########################################
## AGGREGATE MANIPULATED NEW MUNICIPIOS ##
##########################################
# select secciones in eq.new in any v94:v18
sel <-        which(eq.new$edosecn %in% v94$edosecn)
sel <- c(sel, which(eq.new$edosecn %in% v97$edosecn))
sel <- c(sel, which(eq.new$edosecn %in% v00$edosecn))
sel <- c(sel, which(eq.new$edosecn %in% v03$edosecn))
sel <- c(sel, which(eq.new$edosecn %in% v06$edosecn))
sel <- c(sel, which(eq.new$edosecn %in% v09$edosecn))
sel <- c(sel, which(eq.new$edosecn %in% v12$edosecn))
sel <- c(sel, which(eq.new$edosecn %in% v15$edosecn))
sel <- c(sel, which(eq.new$edosecn %in% v18$edosecn))
sel <- unique(sel)
# following eq.new secciones not in any v94:v18
eq.new$OBSERVACIONES[-sel] # all are new/dropped secciones, why not in any v..?
#
## ################
## ## 1991 manip ##
## ################
## d <- v91manip;
## d <- merge(x=d, y=tmp, by=c("edosecn","ife"), all=TRUE, sort=TRUE)
## d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c)
## d$edosecn <- d$seccion <- NULL
## v91manip <- d
################
## 1994 manip ##
################
d <- v94manip; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c)
d$edosecn <- d$seccion <- NULL
v94manip <- d
################
## 1997 manip ##
################
d <- v97manip; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c)
d$edosecn <- d$seccion <- NULL
v97manip <- d
################
## 2000 manip ##
################
d <- v00manip; d[is.na(d)] <- 0
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dprdc")
d <- ag.mun(d,sel.c)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v00manip <- d
################
## 2003 manip ##
################
d <- v03manip; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- NULL
v03manip <- d
################
## 2006 manip ##
################
d <- v06manip; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.mun(d,sel.c)
d$edosecn <- d$seccion <- NULL
v06manip <- d
################
## 2009 manip ##
################
d <- v09manip; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- NULL
v09manip <- d
################
## 2012 manip ##
################
d <- v12manip; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v12manip <- d
################
## 2015 manip ##
################
d <- v15manip; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
v15manip <- d
################
## 2018 manip ##
################
d <- v18manip; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- NULL
v18manip <- d
#
## drop seccion dummies
v94manip <- within(v94manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v97manip <- within(v97manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v00manip <- within(v00manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v03manip <- within(v03manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v06manip <- within(v06manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v09manip <- within(v09manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v12manip <- within(v12manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v15manip <- within(v15manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v18manip <- within(v18manip, d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
#
# tmp will add missing municipios to each v..manip object (to produce square matrix)
tmp <- c(v94manip$ife, v97manip$ife, v00manip$ife, v03manip$ife, v06manip$ife, v09manip$ife, v12manip$ife, v15manip$ife, v18manip$ife)
tmp <- unique(tmp)
tmp <- tmp[order(tmp)]
tmp <- data.frame(ife=tmp)
homog <- function(x){
    x <- merge(x, tmp, by = "ife", all = TRUE, sort = TRUE)
    return(x)
}
v94manip <- homog(v94manip)
v97manip <- homog(v97manip)
v00manip <- homog(v00manip)
v03manip <- homog(v03manip)
v06manip <- homog(v06manip)
v09manip <- homog(v09manip)
v12manip <- homog(v12manip)
v15manip <- homog(v15manip)
v18manip <- homog(v18manip)
rm(homog)

# create a v91manip by subsetting parent/child municipios only (drop this when seccion-level data for 1991 available)
d <- read.csv(paste(dd, "../municipios/dipfed1991.csv", sep = ""), header=TRUE, stringsAsFactors=FALSE)
d$mun <- d$munn <- d$ord <- NULL
tmp <- v94manip; tmp[,-which(colnames(tmp)=="ife")] <- NULL             # keep these municipios only
## tmp2 <- data.frame(ife = c(1001,1010,1011,3003,3005,15026,15040,15122)) # plus these new for 1994--they dont need manipulation
## tmp <- rbind(tmp,tmp2)
d <- merge(x = tmp, y = d, by = "ife", all.x = TRUE, all.y = FALSE)
d <- d[order(d$ife),]
v91manip <- d
# OJO: absent 1991 seccion level vote, can't fix vhat-2009 in parents of muns created in 1994 


################################################################################
## TEMPORARY: 1991 secciones miss proper identifier and aggregate incorrectly ##
## Until that is fixed, this aggregates municipios in an ad-hoc fashion to    ##
## re-do v91m properly                                                        ##
################################################################################
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

# verify symmetric dimensionality
dim(v91m); dim(v94m); dim(v97m); dim(v00m); dim(v03m); dim(v06m); dim(v09m); dim(v12m); dim(v15m); dim(v18m); 
dim(v91manip); dim(v94manip); dim(v97manip); dim(v00manip); dim(v03manip); dim(v06manip); dim(v09manip); dim(v12manip); dim(v15manip); dim(v18manip); 
# rename section-level aggregates to free v00 v03 etc for general use
## v91s <- v91;
v94s <- v94; v97s <- v97; 
v00s <- v00; v03s <- v03; v06s <- v06; v09s <- v09; v12s <- v12; v15s <- v15; v18s <- v18;
#
rm(v94,v97,v00,v03,v06,v09,v12,v15,v18)

# identify first federal election after municipio was created in a dataframe
# (parent and child need manipulation) 
tmp <- function(x){ # function to add rows
    x <- matrix(x, nrow=1)
    return(x)
    }
treat.yrs <- data.frame()
##
#treat.yrs <- rbind(treat.yrs, tmp(c( 1001,1994,"parent",1))) # need seccion-level 1991 to manipulate these
#treat.yrs <- rbind(treat.yrs, tmp(c( 1010,1994,"child", 1))) # need seccion-level 1991 to manipulate these
#treat.yrs <- rbind(treat.yrs, tmp(c( 1011,1994,"child", 1))) # need seccion-level 1991 to manipulate these
#
treat.yrs <- rbind(treat.yrs, tmp(c( 2004,2000,"parent","TIJUANA",2))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 2005,2000,"child","PLAYAS DE ROSARITO", 2))) # 
##
#treat.yrs <- rbind(treat.yrs, tmp(c( 3003,1994,"parent",3))) # need seccion-level 1991 to manipulate these
#treat.yrs <- rbind(treat.yrs, tmp(c( 3005,1994,"child", 3))) # need seccion-level 1991 to manipulate these
#
treat.yrs <- rbind(treat.yrs, tmp(c( 4006,1997,"parent","HOPELCHEN",4))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 4011,1997,"child","CALAKMUL", 4))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 4003,2000,"parent","CARMEN",5))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 4010,2000,"child","ESCARCEGA", 5))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7049,2003,"parent","LARRAINZAR",6))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7118,2003,"child","SANTIAGO EL PINAR", 6))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7082,2003,"parent","SIMOJOVEL",7))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7117,2003,"child","SAN ANDRES DURAZNAL", 7))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7008,2003,"parent","ANGEL ALBINO CORZO",8))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7116,2003,"child","MONTECRISTO DE GUERRERO", 8))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7108,2015,"parent","VILLA CORZO",9))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7122,2015,"child","EL PARRAL", 9))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7026,2003,"parent","CHENALHO",10))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7112,2003,"child","ALDAMA", 10))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7059,2003,"parent","OCOSINGO",11))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7113,2003,"child","BENEMERITO DE LAS AMERICAS", 11))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7115,2003,"child","MARQUES DE COMILLAS", 11))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7052,2003,"parent","LAS MARGARITAS",12))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7114,2003,"child","MARAVILLA TENEJAPA", 12))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7002,2015,"parent","ACALA",13))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7120,2015,"child","EMILIANO ZAPATA", 13))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7093,2018,"parent","TECPATAN",14))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7121,2018,"child","MEZCALAPA", 14))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7081,2018,"parent","SILTEPEC",15))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7123,2018,"child","CAPITAN LUIS ANGEL VIDAL", 15))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c( 7071,2018,"parent","PUEBLO NUEVO SOLISTLAHUACAN",16))) # 
treat.yrs <- rbind(treat.yrs, tmp(c( 7124,2018,"child","RINCON CHAMULA", 16))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12073,1997,"parent","ZAPOTITLAN TABLAS",17))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12076,1997,"child","ACATEPEC", 17))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12013,2006,"parent","AZOYU",18))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12023,2006,"parent","CUAJINICUALAPA",18))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12080,2006,"child","MARQUELIA", 18))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12044,2009,"parent","METLATONOC",19))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12079,2009,"child","COCHOAPA EL GRANDE", 19))) # 
#
#treat.yrs <- rbind(treat.yrs, tmp(c(12013,2009,"parent-again",20))) # AZOYU no need to double, will be dealt with by hand
treat.yrs <- rbind(treat.yrs, tmp(c(12081,2009,"child","JUCHITAN", 20))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12028,2009,"parent","CHILAPA DE ALVAREZ",21))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12077,2009,"child","JOSE JOAQUIN DE HERRERA", 21))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(12042,2009,"parent","MALINALTEPEC",22))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12053,2009,"parent","SAN LUIS ACATLAN",22))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(12078,2009,"child","ILIATENCO", 22))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(14008,2006,"parent","ARANDAS",23))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(14125,2006,"child","SAN IGNACIO CERRO GORDO", 23))) # 
##
#treat.yrs <- rbind(treat.yrs, tmp(c(15026,1994,"parent",24))) # need seccion-level 1991 to manipulate these
#treat.yrs <- rbind(treat.yrs, tmp(c(15040,1994,"parent",24))) # need seccion-level 1991 to manipulate these
#treat.yrs <- rbind(treat.yrs, tmp(c(15122,1994,"child", 24))) # need seccion-level 1991 to manipulate these
#
treat.yrs <- rbind(treat.yrs, tmp(c(15083,2003,"parent","TEJUPILCO",25))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(15123,2003,"child","LUVIANOS", 25))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(15075,2003,"parent","SAN FELIPE DEL PROGRESO",26))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(15124,2003,"child","SAN JOSE DEL RINCON", 26))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(15045,2006,"parent","JALTENCO",27))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(15125,2006,"child","TONATITLA", 27))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(23002,1997,"parent","COZUMEL",28))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(23008,1997,"child","SOLIDARIDAD", 28))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(23003,2009,"parent","FELIPE CARRILLO PUERTO",29))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(23009,2009,"child","TULUM", 29))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(23007,2015,"parent","OTHON P BLANCO",30))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(23010,2015,"child","BACALAR", 30))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(23001,2018,"parent","BENITO JUAREZ",31))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(23011,2018,"child","PUERTO MORELOS", 31))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(24037,1997,"parent","TAMAZUNCHALE",32))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(24058,1997,"child","MATLAPA", 32))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(24010,1997,"parent","CIUDAD DEL MAIZ",33))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(24057,1997,"child","EL NARANJO", 33))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(26063,1997,"parent","ETCHOJOA",34))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(26071,1997,"child","BENITO JUAREZ", 34))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(26061,1997,"parent","GUAYMAS",35))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(26072,1997,"child","SAN IGNACIO RIO MUERTO", 35))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29020,1997,"parent","SANCTORUM DE LAZARO CARDENAS",36))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29060,1997,"child","BENITO JUAREZ", 36))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29030,1997,"parent","TERRENATE",37))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29054,1997,"child","EMILIANO ZAPATA", 37))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29055,1997,"child","LARARO CARDENAS", 37))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29010,1997,"parent","CHIAUTEMPAN",38))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29051,1997,"child","SAN FRANCISCO TETLANOHCAN", 38))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29052,1997,"child","LA MAGDALENA TLALTELULCO", 38))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29032,1997,"parent","TETLATLAHUCA",39))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29053,1997,"child","SAN DAMIAN TEXOLOC", 39))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29056,1997,"child","SAN JERONIMO ZACUALPAN", 39))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29038,1997,"parent","TZOMPANTEPEC",40))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29050,1997,"child","SAN JOSE TEACALCO", 40))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29029,1997,"parent","TEPEYANCO",41))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29047,1997,"child","SAN JUAN HUACTZINCO", 41))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29049,1997,"child","SANTA ISABEL XILOXOXTLA", 41))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29044,1997,"parent","ZACATELCO",42))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29048,1997,"child","SANTA CATARINA AYOMETLA", 42))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29059,1997,"child","SAN LORENZO AXOCOMANITLA", 42))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29040,1997,"parent","XALTOCAN",43))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29057,1997,"child","SAN LUCAS TECOPILCO", 43))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29015,1997,"parent","IXTACUIXTLA DE MARIANO MATAMOROS",44))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29058,1997,"child","SANTA ANA NOPALUCAN", 44))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29023,1997,"parent","NATIVITAS",45))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29045,1997,"child","SANTA APOLONIA TEACALCO", 45))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(29022,1997,"parent","ACUAMANALA DE MIGUEL HIDALGO",46))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(29046,1997,"child","SANTA CRUZ QUILEHTLA", 46))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30047,1997,"parent","COSAMALOAPAN DE CARPIO",47))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30208,1997,"child","CARLOS A. CARRILLO", 47))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30105,1997,"parent","MECAYAPAN",48))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30150,1997,"parent","SOTEAPAN",48))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30210,1997,"child","TATAHUICAPAN DE JUAREZ", 48))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30072,1997,"parent","HIDALGOTITLAN",49))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30093,1997,"parent","JESUS CARRANZA",49))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30109,1997,"parent","MINATITLAN",49))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30209,1997,"child","UXPANAPA", 49))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30103,2006,"parent","MARTINEZ DE LA TORRE",50))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30211,2006,"child","SAN RAFAEL", 50))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(30131,2006,"parent","PLAYA VICENTE",51))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(30212,2006,"child","SANTIAGO SOCHIAPA", 51))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(32017,2003,"parent","GUADALUPE",52))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(32057,2003,"child","TRANCOSO", 52))) # 
#
treat.yrs <- rbind(treat.yrs, tmp(c(32047,2009,"parent","TEUL DE GONZALEZ ORTEGA",53))) # 
treat.yrs <- rbind(treat.yrs, tmp(c(32058,2009,"child","SANTA MARIA DE LA PAZ", 53))) # 
colnames(treat.yrs) <- c("ife","yr.chg","childparent","mun","dyad")
# remove factors
treat.yrs <- as.matrix(treat.yrs)
treat.yrs <- data.frame(treat.yrs, stringsAsFactors = FALSE)
# make numeric
treat.yrs$ife <- as.numeric(treat.yrs$ife)
treat.yrs$yr.chg <- as.numeric(treat.yrs$yr.chg)
treat.yrs$dyad <- as.numeric(treat.yrs$dyad)

# save all to restore after saving raw votes manipulated to fix wrong mun aggregates)
setwd(wd)
save.image("data/too-big-4-github/tmp.RData") 

# manipulate mun votes for export
v91m <- v91m[order(v91m$ife),]; v91manip <- v91manip[order(v91manip$ife),] # sort
v94m <- v94m[order(v94m$ife),]; v94manip <- v94manip[order(v94manip$ife),] # sort
v97m <- v97m[order(v97m$ife),]; v97manip <- v97manip[order(v97manip$ife),] # sort
v00m <- v00m[order(v00m$ife),]; v00manip <- v00manip[order(v00manip$ife),] # sort
v03m <- v03m[order(v03m$ife),]; v03manip <- v03manip[order(v03manip$ife),] # sort
v06m <- v06m[order(v06m$ife),]; v06manip <- v06manip[order(v06manip$ife),] # sort
v09m <- v09m[order(v09m$ife),]; v09manip <- v09manip[order(v09manip$ife),] # sort
v12m <- v12m[order(v12m$ife),]; v12manip <- v12manip[order(v12manip$ife),] # sort
v15m <- v15m[order(v15m$ife),]; v15manip <- v15manip[order(v15manip$ife),] # sort
v18m <- v18m[order(v18m$ife),]; v18manip <- v18manip[order(v18manip$ife),] # sort
##############
## for 1994 ##
##############
sel <- which(treat.yrs$yr.chg==1997)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
sel1 <- which(v94m$ife     %in% target.ife)
sel2 <- which(v94manip$ife %in% target.ife)
if (length(grep(FALSE,v94m$ife[sel1]==v94manip$ife[sel2]))==0){
    #table(v94m$ife[sel1]==v94manip$ife[sel2]) # debug
    #v94m[sel1[1],] # debug
    #v94manip[sel2[1],] # debug
    sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","dpanc","dpric","dprdc")
    v94m[sel1,sel.c] <- v94manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
###################
## for 1994 1997 ##
###################
sel <- which(treat.yrs$yr.chg==2000)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
sel1 <- which(v94m$ife     %in% target.ife)
sel2 <- which(v94manip$ife %in% target.ife)
if (length(grep(FALSE,v94m$ife[sel1]==v94manip$ife[sel2]))==0){
    #table(v94m$ife[sel1]==v94manip$ife[sel2]) # debug
    #v94m[sel1[1],] # debug
    #v94manip[sel2[1],] # debug
    sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","dpanc","dpric","dprdc")
    v94m[sel1,sel.c] <- v94manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v97m$ife     %in% target.ife)
sel2 <- which(v97manip$ife %in% target.ife)
if (length(grep(FALSE,v97m$ife[sel1]==v97manip$ife[sel2]))==0){
    #table(v97m$ife[sel1]==v97manip$ife[sel2]) # debug
    #v97m[sel1[1],] # debug
    #v97manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","dpanc","dpric","dprdc")
    v97m[sel1,sel.c] <- v97manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
########################
## for 1994 1997 2000 ##
########################
sel <- which(treat.yrs$yr.chg==2003)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
sel1 <- which(v94m$ife     %in% target.ife)
sel2 <- which(v94manip$ife %in% target.ife)
if (length(grep(FALSE,v94m$ife[sel1]==v94manip$ife[sel2]))==0){
    #table(v94m$ife[sel1]==v94manip$ife[sel2]) # debug
    #v94m[sel1[1],] # debug
    #v94manip[sel2[1],] # debug
    sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","dpanc","dpric","dprdc")
    v94m[sel1,sel.c] <- v94manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v97m$ife     %in% target.ife)
sel2 <- which(v97manip$ife %in% target.ife)
if (length(grep(FALSE,v97m$ife[sel1]==v97manip$ife[sel2]))==0){
    #table(v97m$ife[sel1]==v97manip$ife[sel2]) # debug
    #v97m[sel1[1],] # debug
    #v97manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","dpanc","dpric","dprdc")
    v97m[sel1,sel.c] <- v97manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v00m$ife     %in% target.ife)
sel2 <- which(v00manip$ife %in% target.ife)
if (length(grep(FALSE,v00m$ife[sel1]==v00manip$ife[sel2]))==0){
    #table(v00m$ife[sel1]==v00manip$ife[sel2]) # debug
    #v00m[sel1[1],] # debug
    #v00manip[sel2[1],] # debug
    sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dpric","dprdc")
    v00m[sel1,sel.c] <- v00manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
#############################
## for 1994 1997 2000 2003 ##
#############################
sel <- which(treat.yrs$yr.chg==2006)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
sel1 <- which(v94m$ife     %in% target.ife)
sel2 <- which(v94manip$ife %in% target.ife)
if (length(grep(FALSE,v94m$ife[sel1]==v94manip$ife[sel2]))==0){
    #table(v94m$ife[sel1]==v94manip$ife[sel2]) # debug
    #v94m[sel1[1],] # debug
    #v94manip[sel2[1],] # debug
    sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","dpanc","dpric","dprdc")
    v94m[sel1,sel.c] <- v94manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v97m$ife     %in% target.ife)
sel2 <- which(v97manip$ife %in% target.ife)
if (length(grep(FALSE,v97m$ife[sel1]==v97manip$ife[sel2]))==0){
    #table(v97m$ife[sel1]==v97manip$ife[sel2]) # debug
    #v97m[sel1[1],] # debug
    #v97manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","dpanc","dpric","dprdc")
    v97m[sel1,sel.c] <- v97manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v00m$ife     %in% target.ife)
sel2 <- which(v00manip$ife %in% target.ife)
if (length(grep(FALSE,v00m$ife[sel1]==v00manip$ife[sel2]))==0){
    #table(v00m$ife[sel1]==v00manip$ife[sel2]) # debug
    #v00m[sel1[1],] # debug
    #v00manip[sel2[1],] # debug
    sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dpric","dprdc")
    v00m[sel1,sel.c] <- v00manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v03m$ife     %in% target.ife)
sel2 <- which(v03manip$ife %in% target.ife)
if (length(grep(FALSE,v03m$ife[sel1]==v03manip$ife[sel2]))==0){
    #table(v03m$ife[sel1]==v03manip$ife[sel2]) # debug
    #v03m[sel1[1],] # debug
    #v03manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric","efec","dpanc","dpric","dprdc")
    v03m[sel1,sel.c] <- v03manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
##################################
## for 1994 1997 2000 2003 2006 ##
##################################
sel <- which(treat.yrs$yr.chg==2009)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
sel1 <- which(v94m$ife     %in% target.ife)
sel2 <- which(v94manip$ife %in% target.ife)
if (length(grep(FALSE,v94m$ife[sel1]==v94manip$ife[sel2]))==0){
    #table(v94m$ife[sel1]==v94manip$ife[sel2]) # debug
    #v94m[sel1[1],] # debug
    #v94manip[sel2[1],] # debug
    sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","dpanc","dpric","dprdc")
    v94m[sel1,sel.c] <- v94manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v97m$ife     %in% target.ife)
sel2 <- which(v97manip$ife %in% target.ife)
if (length(grep(FALSE,v97m$ife[sel1]==v97manip$ife[sel2]))==0){
    #table(v97m$ife[sel1]==v97manip$ife[sel2]) # debug
    #v97m[sel1[1],] # debug
    #v97manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","dpanc","dpric","dprdc")
    v97m[sel1,sel.c] <- v97manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v00m$ife     %in% target.ife)
sel2 <- which(v00manip$ife %in% target.ife)
if (length(grep(FALSE,v00m$ife[sel1]==v00manip$ife[sel2]))==0){
    #table(v00m$ife[sel1]==v00manip$ife[sel2]) # debug
    #v00m[sel1[1],] # debug
    #v00manip[sel2[1],] # debug
    sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dpric","dprdc")
    v00m[sel1,sel.c] <- v00manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v03m$ife     %in% target.ife)
sel2 <- which(v03manip$ife %in% target.ife)
if (length(grep(FALSE,v03m$ife[sel1]==v03manip$ife[sel2]))==0){
    #table(v03m$ife[sel1]==v03manip$ife[sel2]) # debug
    #v03m[sel1[1],] # debug
    #v03manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric","efec","dpanc","dpric","dprdc")
    v03m[sel1,sel.c] <- v03manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v06m$ife     %in% target.ife)
sel2 <- which(v06manip$ife %in% target.ife)
if (length(grep(FALSE,v06m$ife[sel1]==v06manip$ife[sel2]))==0){
    #table(v06m$ife[sel1]==v06manip$ife[sel2]) # debug
    #v06m[sel1[1],] # debug
    #v06manip[sel2[1],] # debug
    sel.c <- c("pan","pric","prdc","pna","asdc","efec","dpanc","dpric","dprdc")
    v06m[sel1,sel.c] <- v06manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
## #######################################
## ## for 1994 1997 2000 2003 2006 2009 ##
## #######################################
## # OJO: no new municipalities in 2012
#
############################################
## for 1994 1997 2000 2003 2006 2009 2012 ##
############################################
sel <- which(treat.yrs$yr.chg==2015)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
sel1 <- which(v94m$ife     %in% target.ife)
sel2 <- which(v94manip$ife %in% target.ife)
if (length(grep(FALSE,v94m$ife[sel1]==v94manip$ife[sel2]))==0){
    #table(v94m$ife[sel1]==v94manip$ife[sel2]) # debug
    #v94m[sel1[1],] # debug
    #v94manip[sel2[1],] # debug
    sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","dpanc","dpric","dprdc")
    v94m[sel1,sel.c] <- v94manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v97m$ife     %in% target.ife)
sel2 <- which(v97manip$ife %in% target.ife)
if (length(grep(FALSE,v97m$ife[sel1]==v97manip$ife[sel2]))==0){
    #table(v97m$ife[sel1]==v97manip$ife[sel2]) # debug
    #v97m[sel1[1],] # debug
    #v97manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","dpanc","dpric","dprdc")
    v97m[sel1,sel.c] <- v97manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v00m$ife     %in% target.ife)
sel2 <- which(v00manip$ife %in% target.ife)
if (length(grep(FALSE,v00m$ife[sel1]==v00manip$ife[sel2]))==0){
    #table(v00m$ife[sel1]==v00manip$ife[sel2]) # debug
    #v00m[sel1[1],] # debug
    #v00manip[sel2[1],] # debug
    sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dpric","dprdc")
    v00m[sel1,sel.c] <- v00manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v03m$ife     %in% target.ife)
sel2 <- which(v03manip$ife %in% target.ife)
if (length(grep(FALSE,v03m$ife[sel1]==v03manip$ife[sel2]))==0){
    #table(v03m$ife[sel1]==v03manip$ife[sel2]) # debug
    #v03m[sel1[1],] # debug
    #v03manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric","efec","dpanc","dpric","dprdc")
    v03m[sel1,sel.c] <- v03manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v06m$ife     %in% target.ife)
sel2 <- which(v06manip$ife %in% target.ife)
if (length(grep(FALSE,v06m$ife[sel1]==v06manip$ife[sel2]))==0){
    #table(v06m$ife[sel1]==v06manip$ife[sel2]) # debug
    #v06m[sel1[1],] # debug
    #v06manip[sel2[1],] # debug
    sel.c <- c("pan","pric","prdc","pna","asdc","efec","dpanc","dpric","dprdc")
    v06m[sel1,sel.c] <- v06manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v09m$ife     %in% target.ife)
sel2 <- which(v09manip$ife %in% target.ife)
if (length(grep(FALSE,v09m$ife[sel1]==v09manip$ife[sel2]))==0){
    #table(v09m$ife[sel1]==v09manip$ife[sel2]) # debug
    #v09m[sel1[1],] # debug
    #v09manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pvem","pt","conve","pna","psd","pric","ptc","efec","dpanc","dpric","dprdc","dptc","lisnom")
    v09m[sel1,sel.c] <- v09manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v12m$ife     %in% target.ife)
sel2 <- which(v12manip$ife %in% target.ife)
if (length(grep(FALSE,v12m$ife[sel1]==v12manip$ife[sel2]))==0){
    #table(v12m$ife[sel1]==v12manip$ife[sel2]) # debug
    #v12m[sel1[1],] # debug
    #v12manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","dpanc","dpric","dprdc")
    v12m[sel1,sel.c] <- v12manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
#################################################
## for 1994 1997 2000 2003 2006 2009 2012 2015 ##
#################################################
sel <- which(treat.yrs$yr.chg==2018)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
sel1 <- which(v94m$ife     %in% target.ife)
sel2 <- which(v94manip$ife %in% target.ife)
if (length(grep(FALSE,v94m$ife[sel1]==v94manip$ife[sel2]))==0){
    #table(v94m$ife[sel1]==v94manip$ife[sel2]) # debug
    #v94m[sel1[1],] # debug
    #v94manip[sel2[1],] # debug
    sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec","dpanc","dpric","dprdc")
    v94m[sel1,sel.c] <- v94manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v97m$ife     %in% target.ife)
sel2 <- which(v97manip$ife %in% target.ife)
if (length(grep(FALSE,v97m$ife[sel1]==v97manip$ife[sel2]))==0){
    #table(v97m$ife[sel1]==v97manip$ife[sel2]) # debug
    #v97m[sel1[1],] # debug
    #v97manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec","dpanc","dpric","dprdc")
    v97m[sel1,sel.c] <- v97manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v00m$ife     %in% target.ife)
sel2 <- which(v00manip$ife %in% target.ife)
if (length(grep(FALSE,v00m$ife[sel1]==v00manip$ife[sel2]))==0){
    #table(v00m$ife[sel1]==v00manip$ife[sel2]) # debug
    #v00m[sel1[1],] # debug
    #v00manip[sel2[1],] # debug
    sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dpric","dprdc")
    v00m[sel1,sel.c] <- v00manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v03m$ife     %in% target.ife)
sel2 <- which(v03manip$ife %in% target.ife)
if (length(grep(FALSE,v03m$ife[sel1]==v03manip$ife[sel2]))==0){
    #table(v03m$ife[sel1]==v03manip$ife[sel2]) # debug
    #v03m[sel1[1],] # debug
    #v03manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","pric","efec","dpanc","dpric","dprdc")
    v03m[sel1,sel.c] <- v03manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v06m$ife     %in% target.ife)
sel2 <- which(v06manip$ife %in% target.ife)
if (length(grep(FALSE,v06m$ife[sel1]==v06manip$ife[sel2]))==0){
    #table(v06m$ife[sel1]==v06manip$ife[sel2]) # debug
    #v06m[sel1[1],] # debug
    #v06manip[sel2[1],] # debug
    sel.c <- c("pan","pric","prdc","pna","asdc","efec","dpanc","dpric","dprdc")
    v06m[sel1,sel.c] <- v06manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v09m$ife     %in% target.ife)
sel2 <- which(v09manip$ife %in% target.ife)
if (length(grep(FALSE,v09m$ife[sel1]==v09manip$ife[sel2]))==0){
    #table(v09m$ife[sel1]==v09manip$ife[sel2]) # debug
    #v09m[sel1[1],] # debug
    #v09manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pvem","pt","conve","pna","psd","pric","ptc","efec","dpanc","dpric","dprdc","dptc","lisnom")
    v09m[sel1,sel.c] <- v09manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v12m$ife     %in% target.ife)
sel2 <- which(v12manip$ife %in% target.ife)
if (length(grep(FALSE,v12m$ife[sel1]==v12manip$ife[sel2]))==0){
    #table(v12m$ife[sel1]==v12manip$ife[sel2]) # debug
    #v12m[sel1[1],] # debug
    #v12manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","dpanc","dpric","dprdc")
    v12m[sel1,sel.c] <- v12manip[sel2,sel.c]
v12m[sel1,]
v12manip[sel2,]
} else {
    print("ERROR: dest != orig")
}
#
sel1 <- which(v15m$ife     %in% target.ife)
sel2 <- which(v15manip$ife %in% target.ife)
if (length(grep(FALSE,v15m$ife[sel1]==v15manip$ife[sel2]))==0){
    #table(v15m$ife[sel1]==v15manip$ife[sel2]) # debug
    #v15m[sel1[1],] # debug
    #v15manip[sel2[1],] # debug
    sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","dpanc","dpric","dprdc","dmorenac","lisnom")
    v15m[sel1,sel.c] <- v15manip[sel2,sel.c]
} else {
    print("ERROR: dest != orig")
}

# drop columns before saving raw vote municipio files
v91m <- within(v91m, ord <- munn <- NULL)
v94m <- within(v94m, disn <- dpanc <- dpric <- dprdc <- NULL)
v97m <- within(v97m, disn <- dpanc <- dpric <- dprdc <- NULL)
v00m <- within(v00m, disn <- dpanc <- dpric <- dprdc <- NULL)
v03m <- within(v03m, disn <- dpanc <- dpric <- dprdc <- NULL)
v06m <- within(v06m, disn <- dpanc <- dpric <- dprdc <- NULL)
v09m <- within(v09m, disn <- dpanc <- dpric <- dprdc <- dptc <- NULL)
v12m <- within(v12m, disn <- dpanc <- dpric <- dprdc <- NULL)
v15m <- within(v15m, disn <- dpanc <- dpric <- dprdc <- dmorenac <- NULL)
v18m <- within(v18m, disn <- dpanc <- dpric <- dmorenac <- NULL)
#

# saves fixed mun raw aggregates
write.csv(v91m, file = paste(wd, "data/dipfed-municipio-vraw-1991.csv", sep = ""), row.names = FALSE)
write.csv(v94m, file = paste(wd, "data/dipfed-municipio-vraw-1994.csv", sep = ""), row.names = FALSE)
write.csv(v97m, file = paste(wd, "data/dipfed-municipio-vraw-1997.csv", sep = ""), row.names = FALSE)
write.csv(v00m, file = paste(wd, "data/dipfed-municipio-vraw-2000.csv", sep = ""), row.names = FALSE)
write.csv(v03m, file = paste(wd, "data/dipfed-municipio-vraw-2003.csv", sep = ""), row.names = FALSE)
write.csv(v06m, file = paste(wd, "data/dipfed-municipio-vraw-2006.csv", sep = ""), row.names = FALSE)
write.csv(v09m, file = paste(wd, "data/dipfed-municipio-vraw-2009.csv", sep = ""), row.names = FALSE)
write.csv(v12m, file = paste(wd, "data/dipfed-municipio-vraw-2012.csv", sep = ""), row.names = FALSE)
write.csv(v15m, file = paste(wd, "data/dipfed-municipio-vraw-2015.csv", sep = ""), row.names = FALSE)
write.csv(v18m, file = paste(wd, "data/dipfed-municipio-vraw-2018.csv", sep = ""), row.names = FALSE)

# drop columns before saving raw vote seccion files
#v91s <- within(v91s, munn <- NULL)
v94s <- within(v94s, edosecn <- dpanc <- dpric <- dprdc <- NULL)
v97s <- within(v97s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- dpanc <- dpric <- dprdc <- NULL)
v00s <- within(v00s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v03s <- within(v03s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v06s <- within(v06s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v09s <- within(v09s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v12s <- within(v12s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v15s <- within(v15s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
v18s <- within(v18s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- NULL)
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

# save municipal winners with correct manipulated data for new municipalities
# v.. needed to work with winner script
v91 <- v91m;
v94 <- v94m; v97 <- v97m; v00 <- v00m; v03 <- v03m; v06 <- v06m; v09 <- v09m; v12 <- v12m; v15 <- v15m; v18 <- v18m;
# get unit winners and margins: will output object winner for chosen agg
agg <- "m"
source(paste(wd, "code/get-winners.r", sep = ""))
head(winner)
# save first part of output
write.csv(winner,
          file = paste(wd, "data/dipfed-municipio-win.csv", sep = ""), row.names = FALSE)


######################################################
## reload data to restore unmanipulated vote files  ##
######################################################
rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(wd)
load("data/too-big-4-github/tmp.RData")

# clean
rm(ag.mun,ag.sec,d,sel,sel.c,sel.drop,sel.r,tmp,to.num)

####################################################################
## manipulate reseccionamiento cases to preserve them in analysis ##
## note: will not affect municipal aggregates, done above         ##
####################################################################
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
    v00 <- v00m; v03 <- v03m; v06 <- v06m; v09 <- v09m; v12 <- v12m; v15 <- v15m; v18 <- v18m;
}
if (agg=="s") {
    ## v91 <- v91m;
    v94 <- v94s; v97 <- v97s; 
    v00 <- v00s; v03 <- v03s; v06 <- v06s; v09 <- v09s; v12 <- v12s; v15 <- v15s; v18 <- v18s;
}
if (agg=="d") {
    ## v91 <- v91m;
    v94 <- v94d; v97 <- v97d; 
    v00 <- v00d; v03 <- v03d; v06 <- v06d; v09 <- v09d; v12 <- v12d; v15 <- v15d; v18 <- v18d;
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

###########################################
## prepare manipulated party objects     ##
## for time-series and alpha regressions ##
###########################################
#
# version 1: extend partial coalitions across the board
# shares
pan <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$pan  / v91$efec),
                  v94 = ifelse(v94$efec==0, NA,  v94$pan  / v94$efec),
                  v97 = ifelse(v97$efec==0, NA,  v97$pan  / v97$efec),
                  v00 = ifelse(v00$efec==0, NA,  v00$panc / v00$efec),
                  v03 = ifelse(v03$efec==0, NA,  v03$pan  / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pan  / v06$efec),
                  v09 = ifelse(v09$efec==0, NA,  v09$pan  / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pan  / v12$efec),
                  v15 = ifelse(v15$efec==0, NA,  v15$pan  / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pan + v18$panc + v18$prd + v18$mc) / v18$efec))
pan <- round(pan, 3)
#
pri <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$pri  / v91$efec),
                  v94 = ifelse(v94$efec==0, NA,  v94$pri  / v94$efec),
                  v97 = ifelse(v97$efec==0, NA,  v97$pri  / v97$efec),
                  v00 = ifelse(v00$efec==0, NA,  v00$pri / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$pri + v03$pric + v03$pvem) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pric / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pri + v09$pric + v09$pvem) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA, (v12$pri + v12$pric + v12$pvem) / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$pri + v15$pric + v15$pvem) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pri + v18$pric + v18$pvem + v18$pna) / v18$efec))
pri <- round(pri, 3)
#
left <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$prd  / v91$efec),
                     v94 = ifelse(v94$efec==0, NA,  v94$prd  / v94$efec),
                     v97 = ifelse(v97$efec==0, NA,  v97$prd  / v97$efec),
                     v00 = ifelse(v00$efec==0, NA,  v00$prdc / v00$efec),
                     v03 = ifelse(v03$efec==0, NA, (v03$prd + v03$pt + v03$conve) / v03$efec),
                     v06 = ifelse(v06$efec==0, NA,  v06$prdc / v06$efec),
                     v09 = ifelse(v09$efec==0, NA, (v09$prd + v09$pt + v09$ptc + v09$conve) / v09$efec),
                     v12 = ifelse(v12$efec==0, NA, (v12$prd + v12$prdc + v12$pt + v12$mc)  / v12$efec),
                     v15 = ifelse(v15$efec==0, NA, (v15$prd + v15$prdc + v15$pt + v15$morena + v15$pes) / v15$efec),
                     v18 = ifelse(v18$efec==0, NA, (v18$morena + v18$morenac + v18$pt + v18$pes) / v18$efec))
left <- round(left, 3)
#
oth <- data.frame(v91 = ifelse(v91$efec==0, NA, (v91$parm + v91$pdm + v91$pfcrn + v91$pps + v91$pem + v91$prt) / v91$efec),
                  v94 = ifelse(v94$efec==0, NA, (v94$pps + v94$pfcrn + v94$parm + v94$uno.pdm + v94$pt + v94$pvem) / v94$efec),
                  v97 = ifelse(v97$efec==0, NA, (v97$pc + v97$pt + v97$pvem + v97$pps + v97$pdm) / v97$efec),
                  v00 = ifelse(v00$efec==0, NA, (v00$pcd + v00$parm + v00$dsppn) / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$psn + v03$pas + v03$mp + v03$plm + v03$fc) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA, (v06$pna + v06$asdc) / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pna + v09$psd) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pna / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$mc + v15$pna + v15$ph + v15$indep1 + v15$indep2) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$indep1 + v18$indep2) / v18$efec))
oth <- round(oth, 3)
#
efec <- data.frame(v91 = v91$efec,
                   v94 = v94$efec,
                   v97 = v97$efec,
                   v00 = v00$efec,
                   v03 = v03$efec,
                   v06 = v06$efec,
                   v09 = v09$efec,
                   v12 = v12$efec,
                   v15 = v15$efec,
                   v18 = v18$efec)
#
# transpose to plug columns into new data.frames
pan <- t(pan)
pri <- t(pri)
left <- t(left)
oth <- t(oth)
efec <- t(efec)
#
extendCoal <- as.list(rep(NA, nrow(v00))) # empty list will receive one data.frame per municipio
if (agg=="m") names(extendCoal) <- v00$ife
if (agg=="s") names(extendCoal) <- v00$edon*10000 + v00$seccion # untested
# loop over municipios/secciones
for (i in 1:nrow(v00)){
    #i <- 81 # debug
    tmp <- data.frame(yr = seq(from=1991, to=2018, by=3),
                      pan = pan[,i],
                      pri = pri[,i],
                      left = left[,i],
                      oth = oth[,i],
                      efec = efec[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
    }
    # add epsilon = 2*max(rounding error) to zeroes 
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    # fill info to new list
    extendCoal[[i]] <- tmp
}
#
# datos para regresión de alfa
yr.means <- data.frame(yr = seq(1991,2018,3),
                       pan    = rep(NA,10),
                       pri    = rep(NA,10),
                       left   = rep(NA,10),
                       oth    = rep(NA,10))
#cs <- function(x) colSums(x, na.rm=TRUE)
if (agg=="s"){
    cs <- function(x) colSums(x[x$dunbaja==0,], na.rm=TRUE) # drops secciones that received aggregates upon splitting
} else {
    cs <- function(x) colSums(x, na.rm=TRUE)
}
#
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
yr.means$pan   [5] <-   cs(v03s)["pan"]                                                                         / cs(v03s)["efec"]
yr.means$pri   [5] <-  (cs(v03s)["pri"] + cs(v03s)["pric"] + cs(v03s)["pvem"])                                  / cs(v03s)["efec"]
yr.means$left  [5] <-  (cs(v03s)["prd"] + cs(v03s)["pt"]   + cs(v03s)["conve"])                                 / cs(v03s)["efec"]
yr.means$oth   [5] <-  (cs(v03s)["psn"] + cs(v03s)["pas"]  + cs(v03s)["mp"] + cs(v03s)["plm"] + cs(v03s)["fc"]) / cs(v03s)["efec"]
#                
yr.means$pan   [6] <-   cs(v06s)["pan"]                     / cs(v06s)["efec"]
yr.means$pri   [6] <-   cs(v06s)["pric"]                    / cs(v06s)["efec"]
yr.means$left  [6] <-   cs(v06s)["prdc"]                    / cs(v06s)["efec"]
yr.means$oth   [6] <-  (cs(v06s)["pna"] + cs(v06s)["asdc"]) / cs(v06s)["efec"]
#                
yr.means$pan   [7] <-   cs(v09s)["pan"]                                                           / cs(v09s)["efec"]
yr.means$pri   [7] <-  (cs(v09s)["pri"] + cs(v09s)["pric"] + cs(v09s)["pvem"])                    / cs(v09s)["efec"]
yr.means$left  [7] <-  (cs(v09s)["prd"] + cs(v09s)["pt"]   + cs(v09s)["ptc"] + cs(v09s)["conve"]) / cs(v09s)["efec"]
yr.means$oth   [7] <-  (cs(v09s)["pna"] + cs(v09s)["psd"])                                        / cs(v09s)["efec"]
#                
yr.means$pan   [8] <-    cs(v12s)["pan"]                                                       / cs(v12s)["efec"]
yr.means$pri   [8] <-   (cs(v12s)["pri"] + cs(v12s)["pric"] + cs(v12s)["pvem"])                / cs(v12s)["efec"]
yr.means$left  [8] <-   (cs(v12s)["prd"] + cs(v12s)["prdc"] + cs(v12s)["pt"] + cs(v12s)["mc"]) / cs(v12s)["efec"]
yr.means$oth   [8] <-    cs(v12s)["pna"]                                                       / cs(v12s)["efec"]
#                
yr.means$pan   [9] <-    cs(v15s)["pan"]                                                                                / cs(v15s)["efec"]
yr.means$pri   [9] <-   (cs(v15s)["pri"] + cs(v15s)["pric"] + cs(v15s)["pvem"])                                         / cs(v15s)["efec"]
yr.means$left  [9] <-   (cs(v15s)["prd"] + cs(v15s)["prdc"] + cs(v15s)["pt"] + cs(v15s)["morena"] + cs(v15s)["pes"])    / cs(v15s)["efec"]
yr.means$oth   [9] <-   (cs(v15s)["mc"]  + cs(v15s)["pna"]  + cs(v15s)["ph"] + cs(v15s)["indep1"] + cs(v15s)["indep2"]) / cs(v15s)["efec"]
#
yr.means$pan   [10] <-   (cs(v18s)["pan"]    + cs(v18s)["panc"]    + cs(v18s)["prd"]  + cs(v18s)["mc"])  / cs(v18s)["efec"]
yr.means$pri   [10] <-   (cs(v18s)["pri"]    + cs(v18s)["pric"]    + cs(v18s)["pvem"] + cs(v18s)["pna"]) / cs(v18s)["efec"]
yr.means$left  [10] <-   (cs(v18s)["morena"] + cs(v18s)["morenac"] + cs(v18s)["pt"]   + cs(v18s)["pes"]) / cs(v18s)["efec"]
yr.means$oth   [10] <-   (cs(v18s)["indep1"] + cs(v18s)["indep2"])                                       / cs(v18s)["efec"]
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
    extendCoal[[i]] <- cbind(extendCoal[[i]], yr.means[,6:8])
}

save.image("data/too-big-4-github/tmp3.RData")

rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(wd)
load("data/too-big-4-github/tmp3.RData")


#################################################################################################
## - should also try jags estimation to get post-sample of vhats and alphas                    ##
## - report mg effect of unit change in bar(v) at year's level instead of betahat (cf. Linzer) ##
#################################################################################################
#
###############################
## código de las regresiones ##
###############################
vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
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
                                   # predict votes in 2006:2018 
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006 <- regs.2009 <- regs.2012 <- regs.2015 <- regs.2018 <-
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
non.nas <- which(is.na(non.nas)==FALSE)
tail(non.nas)
#    
for (i in non.nas){
    #i <- 81 # debug
    #i <- 44508 # debug
    message(sprintf("loop %s of %s", i, max(non.nas)))
    # subset data
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
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
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
rm(alphahat, betahat, bhat.left, bhat.pan, reg.left, reg.oth, reg.pan, rhat.left, rhat.oth, rhat.pan, vhat.2006, vhat.2009, vhat.2012, vhat.2015, vhat.2018, vhat.left, vhat.pan, vhat.pri)

##############################################################
## ESTIMATE MANIPULATED MUNICIPAL REGRESSIONS (NEW MUN FIX) ##
##############################################################
#agg <- "m"
if (agg=="m") {
    v91 <- v91manip; # change with true v91manip when 1991 seccion-level data available
    v94 <- v94manip; v97 <- v97manip; 
    v00 <- v00manip; v03 <- v03manip; v06 <- v06manip; v09 <- v09manip; v12 <- v12manip; v15 <- v15manip; v18 <- v18manip;
}
#
###########################################
## prepare manipulated party objects     ##
## for time-series and alpha regressions ##
###########################################
#
# version 1: extend partial coalitions across the board
# shares
pan <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$pan  / v91$efec),
                  v94 = ifelse(v94$efec==0, NA,  v94$pan  / v94$efec),
                  v97 = ifelse(v97$efec==0, NA,  v97$pan  / v97$efec),
                  v00 = ifelse(v00$efec==0, NA,  v00$panc / v00$efec),
                  v03 = ifelse(v03$efec==0, NA,  v03$pan  / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pan  / v06$efec),
                  v09 = ifelse(v09$efec==0, NA,  v09$pan  / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pan  / v12$efec),
                  v15 = ifelse(v15$efec==0, NA,  v15$pan  / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pan + v18$panc + v18$prd + v18$mc) / v18$efec))
pan <- round(pan, 3)
#
pri <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$pri  / v91$efec),
                  v94 = ifelse(v94$efec==0, NA,  v94$pri  / v94$efec),
                  v97 = ifelse(v97$efec==0, NA,  v97$pri  / v97$efec),
                  v00 = ifelse(v00$efec==0, NA,  v00$pri / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$pri + v03$pric + v03$pvem) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pric / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pri + v09$pric + v09$pvem) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA, (v12$pri + v12$pric + v12$pvem) / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$pri + v15$pric + v15$pvem) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pri + v18$pric + v18$pvem + v18$pna) / v18$efec))
pri <- round(pri, 3)
#
left <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$prd  / v91$efec),
                     v94 = ifelse(v94$efec==0, NA,  v94$prd  / v94$efec),
                     v97 = ifelse(v97$efec==0, NA,  v97$prd  / v97$efec),
                     v00 = ifelse(v00$efec==0, NA,  v00$prdc / v00$efec),
                     v03 = ifelse(v03$efec==0, NA, (v03$prd + v03$pt + v03$conve) / v03$efec),
                     v06 = ifelse(v06$efec==0, NA,  v06$prdc / v06$efec),
                     v09 = ifelse(v09$efec==0, NA, (v09$prd + v09$pt + v09$ptc + v09$conve) / v09$efec),
                     v12 = ifelse(v12$efec==0, NA, (v12$prd + v12$prdc + v12$pt + v12$mc)  / v12$efec),
                     v15 = ifelse(v15$efec==0, NA, (v15$prd + v15$prdc + v15$pt + v15$morena + v15$pes) / v15$efec),
                     v18 = ifelse(v18$efec==0, NA, (v18$morena + v18$morenac + v18$pt + v18$pes) / v18$efec))
left <- round(left, 3)
#
oth <- data.frame(v91 = ifelse(v91$efec==0, NA, (v91$parm + v91$pdm + v91$pfcrn + v91$pps + v91$pem + v91$prt) / v91$efec),
                  v94 = ifelse(v94$efec==0, NA, (v94$pps + v94$pfcrn + v94$parm + v94$uno.pdm + v94$pt + v94$pvem) / v94$efec),
                  v97 = ifelse(v97$efec==0, NA, (v97$pc + v97$pt + v97$pvem + v97$pps + v97$pdm) / v97$efec),
                  v00 = ifelse(v00$efec==0, NA, (v00$pcd + v00$parm + v00$dsppn) / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$psn + v03$pas + v03$mp + v03$plm + v03$fc) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA, (v06$pna + v06$asdc) / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pna + v09$psd) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pna / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$mc + v15$pna + v15$ph + v15$indep1 + v15$indep2) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$indep1 + v18$indep2) / v18$efec))
oth <- round(oth, 3)
#
efec <- data.frame(v91 = v91$efec,
                   v94 = v94$efec,
                   v97 = v97$efec,
                   v00 = v00$efec,
                   v03 = v03$efec,
                   v06 = v06$efec,
                   v09 = v09$efec,
                   v12 = v12$efec,
                   v15 = v15$efec,
                   v18 = v18$efec)
#
# transpose to plug columns into new data.frames
pan <- t(pan)
pri <- t(pri)
left <- t(left)
oth <- t(oth)
efec <- t(efec)
#
extendCoalmanip <- as.list(rep(NA, nrow(v00))) # empty list will receive one data.frame per municipio
if (agg=="m") names(extendCoalmanip) <- v00$ife
if (agg=="s") names(extendCoalmanip) <- v00$edon*10000 + v00$seccion # untested
# loop over municipios/secciones
for (i in 1:nrow(v00)){
    #i <- 60 # debug
    tmp <- data.frame(yr = seq(from=1991, to=2018, by=3),
                      pan = pan[,i],
                      pri = pri[,i],
                      left = left[,i],
                      oth = oth[,i],
                      efec = efec[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
    }
    # add epsilon = 2*max(rounding error) to zeroes 
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    # fill info to new list
    extendCoalmanip[[i]] <- tmp
}
# datos para regresión de alfa
# son los mismos que para las regresiones con v..m
# [... skipped many lines of code here...]
# plug into data
for (i in 1:nrow(v00)){
    #i <- 2 # debug
    extendCoalmanip[[i]] <- cbind(extendCoalmanip[[i]], yr.means[,6:8])
}
#
###############################
## código de las regresiones ##
###############################
vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
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
                                   # predict votes in 2006:2018 
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006manip <- regs.2009manip <- regs.2012manip <- regs.2015manip <- regs.2018manip <-
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regsmanip <- list(pan    = tmp,
                       left   = tmp,
                       oth    = tmp,
                       readme = "No pri regs bec DVs are pri-ratios")
#
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoalmanip, sum)
non.nas <- unlist(non.nas)
non.nas <- which(is.na(non.nas)==FALSE)
tail(non.nas)
#    
for (i in non.nas){
    #i <- 81 # debug
    #i <- 44508 # debug
    message(sprintf("loop %s of %s", i, max(non.nas)))
    # subset data
    data.tmp <- extendCoalmanip[[i]]
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
    regs.2006manip$pan[[i]]    <- reg.pan
    regs.2006manip$left[[i]]   <- reg.left
    regs.2006manip$oth[[i]]    <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
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
    regs.2009manip$pan[[i]]    <- reg.pan
    regs.2009manip$left[[i]]   <- reg.left
    regs.2009manip$oth[[i]]    <- reg.oth
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
    regs.2012manip$pan[[i]]    <- reg.pan
    regs.2012manip$left[[i]]   <- reg.left  
    regs.2012manip$oth[[i]]    <- reg.oth
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
    regs.2018manip$pan   [[i]] <- reg.pan
    regs.2018manip$left  [[i]] <- reg.left  
    regs.2018manip$oth   [[i]] <- reg.oth
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
    mean.regsmanip$pan   [[i]] <- reg.pan
    mean.regsmanip$left  [[i]] <- reg.left  
    mean.regsmanip$oth   [[i]] <- reg.oth
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
    extendCoalmanip[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################

# clean, all this is saved in extendCoal, mean.regs, regs.2006, regs.2009, regs.2012, regs.2015, regs.2018
extendCoalmanip[[1]]
rm(alphahat, betahat, bhat.left, bhat.pan, reg.left, reg.oth, reg.pan, rhat.left, rhat.oth, rhat.pan, vhat.2006, vhat.2009, vhat.2012, vhat.2015, vhat.2018, vhat.left, vhat.pan, vhat.pri)

save.image("data/too-big-4-github/tmp4.RData")

rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(wd)
load("data/too-big-4-github/tmp4.RData")


##############################################
## REPLACE WRONG REGRESSION ESTIMATES       ##
## KEEPING THE TRUE VOTE RETURNS INSTEAD    ##
## OF MANIPULATIONS TO GET ESTIMATES RIGHT  ##
##############################################
#
## # debug routine
## sel1 <- which(names(extendCoal)=="24037")
## sel2 <- which(names(extendCoalmanip)=="24037")
## sel3 <- which(v94m$ife==24037|v94m$ife==24058)
## sel4 <- which(v94manip$ife==24037|v94manip$ife==24058)
## extendCoal[[sel1]]$pan
## extendCoalmanip[[sel2]]$pan
## v94m[sel3,]
## v94manip[sel4,]
## v97m[sel3,]
## v97manip[sel4,]
## dim(v94)
#
################################################
## chg 1997                                   ##
## ########                                   ##
## 2006 <- 1991manip 1994manip 1997 2000 2003 ##
## 2009 <- 1994manip 1997 2000 2003 2006      ##
## 2012 <- 1997 2000 2003 2006 2009           ##
## 2015 <- 2000 2003 2006 2009 2012           ##
## 2018 <- 2003 2006 2009 2012 2015           ##
################################################
sel <- which(treat.yrs$yr.chg==1997)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 44 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
        regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
        regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
        regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
        regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
        regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#        
##############
## chg 2000 ##
##############
## 2006 <- 1991manip 1994manip 1997manip 2000 2003
## 2009 <- 1994manip 1997manip 2000 2003 2006
## 2012 <- 1997manip 2000 2003 2006 2009
## 2015 <- 2000 2003 2006 2009 2012
## 2018 <- 2003 2006 2009 2012 2015
sel <- which(treat.yrs$yr.chg==2000)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
        regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
        regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
        regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
        regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
        regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
        regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
        regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
        regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#
##############
## chg 2003 ##
##############
## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003
## 2009 <- 1994manip 1997manip 2000manip 2003 2006
## 2012 <- 1997manip 2000manip 2003 2006 2009
## 2015 <- 2000manip 2003 2006 2009 2012
## 2018 <- 2003 2006 2009 2012 2015
sel <- which(treat.yrs$yr.chg==2003)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
        regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
        regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
        regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
        regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
        regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
        regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
        regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
        regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
        regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
        regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
        regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#
##############
## chg 2006 ##
##############
## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003manip
## 2009 <- 1994manip 1997manip 2000manip 2003manip 2006
## 2012 <- 1997manip 2000manip 2003manip 2006 2009
## 2015 <- 2000manip 2003manip 2006 2009 2012
## 2018 <- 2003manip 2006 2009 2012 2015
sel <- which(treat.yrs$yr.chg==2006)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
        regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
        regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
        regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
        regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
        regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
        regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
        regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
        regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
        regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
        regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
        regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
        regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
        regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
        regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#
##############
## chg 2009 ##
##############
## 2006 <- 1991 1994 1997 2000 2003
## 2009 <- 1994manip 1997manip 2000manip 2003manip 2006manip
## 2012 <- 1997manip 2000manip 2003manip 2006manip 2009
## 2015 <- 2000manip 2003manip 2006manip 2009 2012
## 2018 <- 2003manip 2006manip 2009 2012 2015
sel <- which(treat.yrs$yr.chg==2009)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2009$pan [[sel1]] <- regs.2009manip$pan [[sel2]]
        regs.2009$left[[sel1]] <- regs.2009manip$left[[sel2]]
        regs.2009$oth [[sel1]] <- regs.2009manip$oth [[sel2]]
        regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
        regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
        regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
        regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
        regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
        regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
        regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
        regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
        regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#
##############
## chg 2012 ##
##############
## 2006 <- 1991 1994 1997 2000 2003
## 2009 <- 1994 1997 2000 2003 2006
## 2012 <- 1997manip 2000manip 2003manip 2006manip 2009manip
## 2015 <- 2000manip 2003manip 2006manip 2009manip 2012
## 2018 <- 2003manip 2006manip 2009manip 2012 2015
sel <- which(treat.yrs$yr.chg==2012)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2012$pan [[sel1]] <- regs.2012manip$pan [[sel2]]
        regs.2012$left[[sel1]] <- regs.2012manip$left[[sel2]]
        regs.2012$oth [[sel1]] <- regs.2012manip$oth [[sel2]]
        regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
        regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
        regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
        regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
        regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
        regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#
###############################################################
## chg 2015                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994 1997 2000 2003 2006                          ##
## 2012 <- 1997 2000 2003 2006 2009                          ##
## 2015 <- 2000manip 2003manip 2006manip 2009manip 2012manip ##
## 2018 <- 2003manip 2006manip 2009manip 2012manip 2015      ##
###############################################################
sel <- which(treat.yrs$yr.chg==2015)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2015$pan [[sel1]] <- regs.2015manip$pan [[sel2]]
        regs.2015$left[[sel1]] <- regs.2015manip$left[[sel2]]
        regs.2015$oth [[sel1]] <- regs.2015manip$oth [[sel2]]
        regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
        regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
        regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}
#
###############################################################
## chg 2018                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994 1997 2000 2003 2006                          ##
## 2012 <- 1997 2000 2003 2006 2009                          ##
## 2015 <- 2000 2003 2006 2009 2012                          ##
## 2018 <- 2003manip 2006manip 2009manip 2012manip 2015manip ##
###############################################################
sel <- which(treat.yrs$yr.chg==2018)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        #names(regs.2006$pan)[sel1]      # debug
        #names(regs.2006manip$pan)[sel2] # debug
        extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
        regs.2018$pan [[sel1]] <- regs.2018manip$pan [[sel2]]
        regs.2018$left[[sel1]] <- regs.2018manip$left[[sel2]]
        regs.2018$oth [[sel1]] <- regs.2018manip$oth [[sel2]]
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}

################################
## FIX TWICE SPLIT MUNICIPIOS ##
################################
#
# read estimates (produced externally with script code/script-to-fix-twice-split-muns.r)
load(file = paste(wd, "data/regs-to-fix-twice-split-muns.RData", sep = "/"))
###############################################################
## chg 2006                                                  ##
## ########                                                  ##
## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003manip ##
###############################################################
target.ife <- 12013 # azoyu
#for (i in 1:length(sel)){
    i <- 1 # debug
    sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
    sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
    #names(regs.2006$pan)[sel1]      # debug
    #names(regs.2006manip$pan)[sel2] # debug
    extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
    regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
    regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
    regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
    #need to figure if mean.regsmanip should also be used---manips input skipped
#}
#
####################################################################
## chg 2009                                                       ##
## ########                                                       ##
## 2009 <- 1994manip2 1997manip2 2000manip2 2003manip2 2006manip2 ##
## 2012 <- 1997manip2 2000manip2 2003manip2 2006manip2 2009       ##
## 2015 <- 2000manip2 2003manip2 2006manip2 2009 2012             ##
## 2018 <- 2003manip2 2006manip2 2009 2012 2015                   ##
####################################################################
i <- 1 # debug
sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
#names(regs.2006$pan)[sel1]      # debug
#names(regs.2006manip$pan)[sel2] # debug
extendCoal[[sel1]] <- extendCoalmanip2
regs.2009$pan [[sel1]] <- regs.2009manip2$pan 
regs.2009$left[[sel1]] <- regs.2009manip2$left
regs.2009$oth [[sel1]] <- regs.2009manip2$oth 
regs.2012$pan [[sel1]] <- regs.2012manip2$pan 
regs.2012$left[[sel1]] <- regs.2012manip2$left
regs.2012$oth [[sel1]] <- regs.2012manip2$oth 
regs.2015$pan [[sel1]] <- regs.2015manip2$pan 
regs.2015$left[[sel1]] <- regs.2015manip2$left
regs.2015$oth [[sel1]] <- regs.2015manip2$oth 
regs.2018$pan [[sel1]] <- regs.2018manip2$pan 
regs.2018$left[[sel1]] <- regs.2018manip2$left
regs.2018$oth [[sel1]] <- regs.2018manip2$oth 
#need to figure if mean.regsmanip should also be used---manips input skipped
#
# clean
rm(v91manip, v94manip, v97manip, v00manip, v03manip, v06manip, v09manip, v12manip, v15manip, v18manip,
   regs.2006manip, regs.2009manip, regs.2012manip, regs.2015manip, regs.2018manip, mean.regsmanip,
   regs.2006manip2, regs.2009manip2, regs.2012manip2, regs.2015manip2, regs.2018manip2, mean.regsmanip2,
   extendCoalmanip, extendCoalmanip2)
rm(v91,v94,v97,v00,v03,v06,v09,v12,v15,v18)
rm(pan,pri,left,oth,efec)
rm(sel,sel1,sel2,sel.to,target.ife,i)




# adds manipulation indicator to all data frames in list
if (agg=="s") {
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
                pan <- pri <- left   <- d.pan <- d.pri <- d.left   <- NA; # drop mean vote used, use NAs
            })
            # columns to manipulate
            sel.col <- c("vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left", "alphahat.pan","alphahat.pri","alphahat.left  ","betahat.pan","betahat.left")
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

eric  xx OJO hay dataframes en extendcoal que no tienen vhats
##########################################################################
## generate data frame with one year's predictions/estimates for export ##
##########################################################################
tmp.func <- function(year) {
    #year <- 2009         # debug
    #X <- extendCoal[[1]] # debug
    sel <- which(extendCoal[[1]]$yr==year) # which row reports year (symmetric in all other objects in list)
    tmp <- lapply(extendCoal, FUN = function(X) {
        prune <- X[sel,] # keep selected row only
        return(prune)
    }) # keep sel yr only in every municipio
    # spot NAs in list
    tmp.sel <- setdiff(1:length(extendCoal), non.nas)
    # fill with same-dim NA data.frame
    tmp.manip <- tmp[[non.nas[1]]]
    tmp.manip[,-1] <- NA # all but 1st col (yr) to NA
    tmp[tmp.sel] <- lapply(tmp[tmp.sel], function(x) tmp.manip)
    # turn into one dataframe
    tmp <- do.call("rbind", tmp)
    rownames(tmp) <- NULL
    if (agg=="m") sel.col <- c("edon","ife","inegi")       # cols to merge when using municipios
    if (agg=="s") sel.col <- c("edon","seccion","edosecn","ife","inegi") # when using secciones
    tmp <- cbind(tmp, v00[,sel.col])
    rm(sel.col)
    return(tmp)
}

extendCoal.2006 <- tmp.func(year=2006)
extendCoal.2009 <- tmp.func(year=2009)
extendCoal.2012 <- tmp.func(year=2012)
extendCoal.2015 <- tmp.func(year=2015)
extendCoal.2018 <- tmp.func(year=2018)
#rm(extendCoal.2015) # clean memory

# drop some columns
extendCoal.2006 <- within(extendCoal.2006, yr <- edosecn <- NULL)
extendCoal.2009 <- within(extendCoal.2009, yr <- edosecn <- NULL)
extendCoal.2012 <- within(extendCoal.2012, yr <- edosecn <- NULL)
extendCoal.2015 <- within(extendCoal.2015, yr <- edosecn <- NULL)
extendCoal.2018 <- within(extendCoal.2018, yr <- edosecn <- NULL)

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
}

# save municipal regression objects
save(mean.regs, file = paste(wd, "data/dipfed-municipio-mean-regs.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(regs.2006, file = paste(wd, "data/dipfed-municipio-regs-2006.RData", sep = ""), compress = "gzip")
save(regs.2009, file = paste(wd, "data/dipfed-municipio-regs-2009.RData", sep = ""), compress = "gzip")
save(regs.2012, file = paste(wd, "data/dipfed-municipio-regs-2012.RData", sep = ""), compress = "gzip")
save(regs.2015, file = paste(wd, "data/dipfed-municipio-regs-2015.RData", sep = ""), compress = "gzip")
save(regs.2018, file = paste(wd, "data/dipfed-municipio-regs-2018.RData", sep = ""), compress = "gzip")

# save sección regression objects
save(mean.regs, file = paste(wd, "data/too-big-4-github/dipfed-seccion-mean-regs.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(regs.2009, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2009.RData", sep = ""), compress = "gzip")
save(regs.2012, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2012.RData", sep = ""), compress = "gzip")
save(regs.2015, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2015.RData", sep = ""), compress = "gzip")
save(regs.2018, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2018.RData", sep = ""), compress = "gzip")

# load regression object
load(file = paste(wd, "data/dipfed-seccion-regs-2015.RData", sep = ""))
summary.lm(regs.2015$left[[1]])$coef[2,1]



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
