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

# cambiar por cartografía 2017 en /home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/shp/disfed2018
# también preparé cartografía 2020   <---  OJO
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

##########
## 2021 ##
##########
d <- read.csv( "dip2021.csv", header=TRUE, , stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
d <- within(d, nr <- nul <- tot <- NULL)
#
# assign 2021 pan-pri-prd coalition to pan and pri in proportion to votes each won by itself
tmp <- d[,c("pan","pri")]
tmp[which(rowSums(tmp)==0),] <- tmp[which(rowSums(tmp)==0),] + 1 # avoid zero denominators (0,0 wil turn into half and half)
tmp <- tmp/rowSums(tmp) # two-party shares
#
d$pan.pri.prd[is.na(d$pan.pri.prd)] <- 0 # replace NAs w 0 votes
d$panc <- d$pan.pri.prd * tmp[,1] # pan's proportional votes
d$pric <- d$pan.pri.prd * tmp[,2] # pri's proportional votes
d$pan.pri.prd <- NULL
#
## # alt method: give all coal votes to pty last controlling state govt
## sel <- which(d$edon %in% c(1:3,8:10,12,21:22,24,28,30:31)); d$panc[sel] <- d$pan.pri.prd[sel]
## sel <- which(d$edon %in% c(4:7,11,13:20,23,25:27,29,32)); d$pric[sel] <- d$pan.pri.prd[sel]
# morena coal
colnames(d)[which(colnames(d)=="pvem.pt.morena")]  <- "morenac"
#
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","lisnom")
d <- to.num(d,sel.c) # clean data
#
# efec
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + morena + pes + rsp + fxm + indep + panc + pric + morenac)
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
    panc <- pan + panc + pri + prd;
    pan <- pri <- prd <- 0
})
sel <- which(d$dpric==1)
d[sel,] <- within(d[sel,], {
    pric <- pan + pri + pric + prd;
    pan <- pri <- prd <- 0
})
sel <- which(d$dmorenac==1)
d[sel,] <- within(d[sel,], {
    morenac <- pvem + pt + morena + morenac;
    pvem <- pt <- morena <- 0
})
#
# sel.r <- grep("E6|E7", d$OBSERVACIONES) # casillas no instaladas
#d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# clean
d <- within(d, ord <- edo <- cabecera <- casn <- TIPO_CASILLA <- NULL)
# drop seccion=0
sel <- which(d$seccion==0)
d <- d[-sel,]
v21 <- d
# head(v21) # debug

# clean
rm(d,sel.c,sel.r)

################################
## district winners 2006-2021 ##
################################
v06d <- v06; v09d <- v09; v12d <- v12; v15d <- v15; v18d <- v18; v21d <- v21
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
v21d <- within(v21d, {
    pan <-    ave(pan,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pri <-    ave(pri,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    prd <-    ave(prd,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pvem <-   ave(pvem,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pt <-     ave(pt,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    mc <-     ave(mc,     as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    morena <- ave(morena, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pes <-    ave(pes,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    rsp <-    ave(rsp,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    fxm <-    ave(fxm,    as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    panc <-   ave(panc,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    pric <-   ave(pric,   as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    morenac<- ave(morenac,as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    indep <- ave(indep, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
    efec  <- ave(efec, as.factor(edon*100 + disn), FUN=sum, na.rm=TRUE);
})
v21d <- v21d[duplicated(v21d$edon*100 + v21d$disn)==FALSE,]
v21d <- v21d[order(v21d$edon*100, v21d$disn),]
#
windis <- v15d[,c("edon","disn")] # will receive data
#
# handy function to sort one data frame by order of another, matching data frame
source("/home/eric/Dropbox/data/useful-functions/sortBy.r")
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
# 2021
vot <- v21d[,c("pan","pri","prd","pvem","pt","mc","morena","pes","fxm","rsp","indep","panc","pric","morenac")]
vot[is.na(vot)==TRUE] <- 0 # drop NAs
# crea objeto de etiquetas
etiq <- data.frame(matrix(rep(colnames(vot), nrow(vot)), nrow=nrow(vot), byrow = TRUE), stringsAsFactors = FALSE)
colnames(etiq) <- paste("l", 1:ncol(vot), sep = "")
#
etiq <- sortBy(target = etiq, By = vot)
vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#
windis$e21 <- etiq[,1]
## runnerup$e21 <- etiq[,2]
## mg$e21 <- (vot[,1] - vot[,2]) / rowSums(vot)
## enp$e21 <- 1/rowSums((vot/rowSums(vot))^2)
#
rm(vot,etiq)

#
## write.csv(windis, file = paste(dd, "dfdf2006-2015winners.csv", sep = ""))

# not in function
source("/home/eric/Dropbox/data/useful-functions/notin.r")

# get equivalencias seccionales # OJO check new secciones 2021, get new file
tmp <- paste(wd, "equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", sep = "")
eq <- read.csv(tmp, stringsAsFactors = FALSE)
eq$check <- NULL # drop column meant to clean within excel file
eq[1,]
# any secciones missing?
tmp <- v21 # which year to evaluate
sel <- which(as.factor(tmp$edon+tmp$seccion/10000) %notin% as.factor(eq$edon+eq$seccion/10000))
if (length(sel)>0){
    tmp$edon[sel]+tmp$seccion[sel]/10000
} else {
    print("All v-secciones in eq object")
}
## sel <- which(as.factor(eq$edon+eq$seccion/10000) %notin% as.factor(tmp$edon+tmp$seccion/10000))
## if (length(sel)>0){
##     eq$edon[sel]+eq$seccion[sel]/10000
## } else {
##     print("All eq-secciones in v object")
## }

# get municipio info to merge into votes
#muns <- eq[, c("edon", "seccion", "ife", "inegi")]
muns <- eq[, c("edon", "seccion", "ife", "inegi", "ife1994", "ife1997", "ife2000", "ife2003", "ife2006", "ife2009", "ife2012", "ife2015", "ife2018", "ife2021")]

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
v21 <- within(v21, {
    edosecn <- edon*10000 + seccion;
    d21    <- 1;
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
tmp <- merge(x=tmp,                      y=v21[,c("edosecn","d21")], by = "edosecn", all = TRUE)
## v91$d91 <-
v94$d94 <- v97$d97 <- v00$d00 <- v03$d03 <- v06$d06 <- v09$d09 <- v12$d12 <- v15$d15 <- v18$d18 <- v21$d21 <- NULL # clean
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
v21 <- merge(x=tmp, y=v21, by = "edosecn", all = TRUE)
# verify dimensionality
## dim(v91);
dim(v94); dim(v97); dim(v00); dim(v03); dim(v06); dim(v06); dim(v12); dim(v15); dim(v18); dim(v21)
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
v21 <- tmp.func(v21)
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
v21 <- merge(x = v21, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v18$munn <- NULL
rm(muns)

#######################################################
## correct ife codes in secciones that changed munic ##
#######################################################
ife.inegi <- eq[,c("inegi","ife")] # correct inegi where ife changed
ife.inegi <- ife.inegi[duplicated(ife.inegi$ife)==FALSE,]
library(plyr)
v94$ife <- v94$ife1994
v94$inegi <- mapvalues(v94$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v97$ife <- v97$ife1997
v97$inegi <- mapvalues(v97$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v00$ife <- v00$ife2000
v00$inegi <- mapvalues(v00$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v03$ife <- v03$ife2003
v03$inegi <- mapvalues(v03$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v06$ife <- v06$ife2006
v06$inegi <- mapvalues(v06$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v09$ife <- v09$ife2009
v09$inegi <- mapvalues(v09$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v12$ife <- v12$ife2012
v12$inegi <- mapvalues(v12$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v15$ife <- v15$ife2015
v15$inegi <- mapvalues(v15$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v18$ife <- v18$ife2018
v18$inegi <- mapvalues(v18$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
v21$ife <- v21$ife2021
v21$inegi <- mapvalues(v21$ife, from = ife.inegi$ife, to = ife.inegi$inegi)
rm(ife.inegi)


# save all to restore after exporting raw vote manipulations (11jul2021: maybe redundant)
save.image(paste0(wd, "data/too-big-4-github/tmp.RData"))

#################################################
## function to aggregate municipio-level votes ##
#################################################
ag.mun <- function(d=d, sel.c=sel.c, grouping=d$ife){
    for (i in 1:length(sel.c)){
        # d[,sel.c[i]] <- ave(d[,sel.c[i]], d$edon*1000+d$inegi, FUN=sum, na.rm=TRUE) # use inegi codes
        # d[,sel.c[i]] <- ave(d[,sel.c[i]], d$edon*1000+d$ife, FUN=sum, na.rm=TRUE) # use ife codes
        d[,sel.c[i]] <- ave(d[,sel.c[i]],              grouping, FUN=sum, na.rm=TRUE) # use ife codes
    }
    # sel.r <- which(duplicated(d$edon*1000+d$inegi)==TRUE) # use inegi codes
    # sel.r <- which(duplicated(d$edon*1000+d$ife)==TRUE) # use ife codes
    sel.r <- which(duplicated(grouping)==TRUE) # use ife codes
    d <- d[-sel.r,]
    return(d)
}
#################################
## aggregate municipio returns ##
#################################
## ##########
## ## 1991 ## OJO: seccion identifiers are wrong, 1991 mun aggregates are loaded later
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
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m <- d
##########
## 1997 ##
##########
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
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
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m <- d
##########
## 2003 ##
##########
d <- v03; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m <- d
##########
## 2006 ##
##########
d <- v06; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.mun(d,sel.c)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
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
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
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
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
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
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
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
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v18m <- d
##########
## 2021 ##
##########
d <- v21; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v21m <- d

####################################################################################
## TEMPORARY: 1991 secciones miss proper identifier and aggregate incorrectly     ##
## Until that is fixed, this loads imperfectly manipulated municipio aggregates   ##
## Commented lines were used towards this solution, which resides in older commit ##
####################################################################################
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
v21m <- within(v21m, disn <- dpanc <- dpric <- dmorenac <- NULL)

# add missing municipios to v..m objects to get same dimensionality
tmp <- c(v91m$ife, v94m$ife, v97m$ife, v00m$ife, v03m$ife, v06m$ife, v09m$ife, v12m$ife, v15m$ife, v18m$ife, v21m$ife)
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
rm(homog)
# verify
dim(v91m); dim(v94m); dim(v97m); dim(v00m); dim(v03m); dim(v06m); dim(v09m); dim(v12m); dim(v15m); dim(v18m); dim(v21m); 


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
v21s <- within(v18, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
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
v94 <- v94m; v97 <- v97m; v00 <- v00m; v03 <- v03m; v06 <- v06m; v09 <- v09m; v12 <- v12m; v15 <- v15m; v18 <- v18m; v21 <- v21m
# get unit winners and margins: will output object winner for chosen agg
agg <- "m"
source(paste(wd, "code/get-winners.r", sep = ""))
head(winner)
# save first part of output
write.csv(winner,
          file = paste(wd, "data/dipfed-municipio-win.csv", sep = ""), row.names = FALSE)


# saves fixed mun raw aggregates
#write.csv(v91m, file = paste(wd, "data/dipfed-municipio-vraw-1991.csv", sep = ""), row.names = FALSE)
write.csv(v94m, file = paste(wd, "data/dipfed-municipio-vraw-1994.csv", sep = ""), row.names = FALSE)
write.csv(v97m, file = paste(wd, "data/dipfed-municipio-vraw-1997.csv", sep = ""), row.names = FALSE)
write.csv(v00m, file = paste(wd, "data/dipfed-municipio-vraw-2000.csv", sep = ""), row.names = FALSE)
write.csv(v03m, file = paste(wd, "data/dipfed-municipio-vraw-2003.csv", sep = ""), row.names = FALSE)
write.csv(v06m, file = paste(wd, "data/dipfed-municipio-vraw-2006.csv", sep = ""), row.names = FALSE)
write.csv(v09m, file = paste(wd, "data/dipfed-municipio-vraw-2009.csv", sep = ""), row.names = FALSE)
write.csv(v12m, file = paste(wd, "data/dipfed-municipio-vraw-2012.csv", sep = ""), row.names = FALSE)
write.csv(v15m, file = paste(wd, "data/dipfed-municipio-vraw-2015.csv", sep = ""), row.names = FALSE)
write.csv(v18m, file = paste(wd, "data/dipfed-municipio-vraw-2018.csv", sep = ""), row.names = FALSE)
write.csv(v21m, file = paste(wd, "data/dipfed-municipio-vraw-2021.csv", sep = ""), row.names = FALSE)


#################################################################################################
## generate counterfactual municipal aggregates to use as regressors for each years regressand ##
#################################################################################################
#
#################################
## 1991 counterfactuals needed ##
#################################
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.mun(d,sel.c, grouping=d$ife2006)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf06 <- d
#################################
## 1994 counterfactuals needed ##
#################################
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf06 <- d
d <- v94; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf09 <- d
#################################
## 1997 counterfactuals needed ##
#################################
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf06 <- d
d <- v97; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf09 <- d
d <- v97; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf12 <- d
#################################
## 2000 counterfactuals needed ##
#################################
d <- v00; d[is.na(d)] <- 0
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec","dpanc","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf06 <- d
d <- v00; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf09 <- d
d <- v00; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf12 <- d
d <- v00; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf15 <- d
#################################
## 2003 counterfactuals needed ##
#################################
d <- v03; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
d <- ag.mun(d,sel.c, grouping=d$ife2006)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf06 <- d
d <- v03; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf09 <- d
d <- v03; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf12 <- d
d <- v03; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf15 <- d
d <- v03; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf18 <- d
#################################
## 2006 counterfactuals needed ##
#################################
d <- v06; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.mun(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf09 <- d
d <- v06; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf12 <- d
d <- v06; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf15 <- d
d <- v06; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf18 <- d
d <- v06; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf21 <- d
#################################
## 2009 counterfactuals needed ##
#################################
d <- v09; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pvem","pt","ptc","conve","pna","psd","efec","lisnom","dpric","dptc")
d <- ag.mun(d,sel.c, grouping=d$ife2012)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf12 <- d
d <- v09; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf15 <- d
d <- v09; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf18 <- d
d <- v09; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf21 <- d
## d <- v09; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$dptc <- as.numeric(d$dptc>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v09m.cf24 <- d
#################################
## 2012 counterfactuals needed ##
#################################
d <- v12; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf15 <- d
d <- v12; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf18 <- d
d <- v12; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf21 <- d
## d <- v12; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v12m.cf24 <- d
## d <- v12; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2027)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v12m.cf27 <- d
#################################
## 2015 counterfactuals needed ##
#################################
d <- v15; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf18 <- d
d <- v15; d[is.na(d)] <- 0
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf21 <- d
## d <- v15; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf24 <- d
## d <- v15; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2027)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf27 <- d
## d <- v15; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2030)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf30 <- d
#################################
## 2018 counterfactuals needed ##
#################################
d <- v18; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.mun(d,sel.c, grouping=d$ife2021)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v18m.cf21 <- d
## d <- v18; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf24 <- d
## d <- v18; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2027)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf27 <- d
## d <- v18; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2030)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf30 <- d
## d <- v18; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2033)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v18m.cf33 <- d
#################################
## 2021 counterfactuals needed ##
#################################
## d <- v21; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
## d <- ag.mun(d,sel.c, grouping=d$ife2024)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf24 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2027)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf27 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2030)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf30 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2033)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf33 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.mun(d,sel.c, grouping=d$ife2036)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- NULL
## d$disn <- NULL
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf36 <- d

# temporary for debugging
save.image("../../datosBrutos/not-in-git/tmp.RData")

rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(dd)
load("../../datosBrutos/not-in-git/tmp.RData")


pasted tmp.r here
cut old block manipulating new municipios (saved as tmp.r)
1) push reload and seccion data manip for later
2) v5 for factual and counterfactual v..ms
3) alpha regs seem to need little manipulation: generate year swings with each v..m, then regress as before
4) beta regs need to rely on appropriate counterfactuals instead of factual v..ms


######################################################
## reload data to restore unmanipulated vote files  ##
######################################################
rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(wd)
load(paste0(wd, "data/too-big-4-github/tmp.RData"))

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
                  v18 = ifelse(v18$efec==0, NA, (v18$pan + v18$panc + v18$prd + v18$mc) / v18$efec),
                  v21 = ifelse(v21$efec==0, NA, (v21$pan + v21$panc + v21$prd) / v21$efec))
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
                  v18 = ifelse(v18$efec==0, NA, (v18$pri + v18$pric + v18$pvem + v18$pna) / v18$efec),
                  v21 = ifelse(v21$efec==0, NA, (v21$pri + v21$pric) / v21$efec))
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
                     v18 = ifelse(v18$efec==0, NA, (v18$morena + v18$morenac + v18$pt + v18$pes) / v18$efec),
                     v21 = ifelse(v21$efec==0, NA, (v21$morena + v21$morenac) / v21$efec)) # 12jul21: should add pt*dmorenac + pvem
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
                  v18 = ifelse(v18$efec==0, NA, (v18$indep1 + v18$indep2) / v18$efec),
                  v21 = ifelse(v21$efec==0, NA, (v21$pvem + v21$pt + v21$mc + v21$pes + v21$rsp + v21$fxm + v21$indep) / v21$efec))
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
                   v18 = v18$efec,
                   v21 = v21$efec)
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
    tmp <- data.frame(yr = seq(from=1991, to=2021, by=3),
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
#tail(non.nas)
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
non.nas.orig <- non.nas # duplicate to restore
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

# restore
non.nas <- non.nas.orig; rm(non.nas.orig)
#
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
# parents
sel <- which(treat.yrs$yr.chg==1997 & treat.yrs$childparent=="parent")
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
# children
sel <- which(treat.yrs$yr.chg==1997 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs forward in new muns
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
# parents
sel <- which(treat.yrs$yr.chg==2000 & treat.yrs$childparent=="parent")
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
# children
sel <- which(treat.yrs$yr.chg==2000 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to fwd change regs in new muns
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
# parents
sel <- which(treat.yrs$yr.chg==2003 & treat.yrs$childparent=="parent")
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
# children
sel <- which(treat.yrs$yr.chg==2003 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
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
# parents
sel <- which(treat.yrs$yr.chg==2006 & treat.yrs$childparent=="parent")
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
# children
sel <- which(treat.yrs$yr.chg==2006 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
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
# parents
sel <- which(treat.yrs$yr.chg==2009 & treat.yrs$childparent=="parent")
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
# children
sel <- which(treat.yrs$yr.chg==2009 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
        tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
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
# parents
sel <- which(treat.yrs$yr.chg==2012 & treat.yrs$childparent=="parent")
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
# children
sel <- which(treat.yrs$yr.chg==2012 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
        tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
        tmp[tmp$yr==2009,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2009)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
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
# parents
sel <- which(treat.yrs$yr.chg==2015 & treat.yrs$childparent=="parent")
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
# children
sel <- which(treat.yrs$yr.chg==2015 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
        tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
        tmp[tmp$yr==2009,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2009)        
        tmp[tmp$yr==2012,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2012)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
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
# parents
sel <- which(treat.yrs$yr.chg==2018 & treat.yrs$childparent=="parent")
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
# children
sel <- which(treat.yrs$yr.chg==2018 & treat.yrs$childparent=="child")
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
if (length(sel)>0){
    for (i in 1:length(sel)){
        #i <- 1 # debug
        sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
        sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
        tmp <- extendCoal[[sel1]]     # duplicate for manipulation
        sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
        tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
        tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
        tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
        tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
        tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
        tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
        tmp[tmp$yr==2009,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2009)        
        tmp[tmp$yr==2012,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2012)        
        tmp[tmp$yr==2015,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2015)        
        extendCoal[[sel1]] <- tmp     # return manipulated data
        # no need to change regs fwd in new muns
        #need to figure if mean.regsmanip should also be used---manips input skipped
    }
}

################################
## FIX TWICE SPLIT MUNICIPIOS ##
################################
#
# read estimates (produced externally with script code/script-to-fix-twice-split-muns.r)
load(file = paste(wd, "data/regs-to-fix-twice-split-muns.RData", sep = "/"))
## # BLOCK MANIPULATED ABOVE
## ###############################################################
## ## chg 2006                                                  ##
## ## ########                                                  ##
## ## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003manip ##
## ###############################################################
## target.ife <- 12013 # azoyu parent
## #for (i in 1:length(sel)){
##     i <- 1 # debug
##     sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##     sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##     #names(regs.2006$pan)[sel1]      # debug
##     #names(regs.2006manip$pan)[sel2] # debug
##     extendCoal[[sel1]] <- extendCoalmanip[[sel2]]
##     regs.2006$pan [[sel1]] <- regs.2006manip$pan [[sel2]]
##     regs.2006$left[[sel1]] <- regs.2006manip$left[[sel2]]
##     regs.2006$oth [[sel1]] <- regs.2006manip$oth [[sel2]]
##     #need to figure if mean.regsmanip should also be used---manips input skipped
## #}
## target.ife <- 12080 # child
## #for (i in 1:length(sel)){
##     i <- 1 # debug
##     sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
##     sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
##     tmp <- extendCoal[[sel1]]     # duplicate for manipulation
##     sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
##     tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
##     tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
##     tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
##     tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
##     tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
##     extendCoal[[sel1]] <- tmp     # return manipulated data
##     # no need to change regs fwd in new muns
##     #need to figure if mean.regsmanip should also be used---manips input skipped
## #}
#
####################################################################
## chg 2009                                                       ##
## ########                                                       ##
## 2009 <- 1994manip2 1997manip2 2000manip2 2003manip2 2006manip2 ##
## 2012 <- 1997manip2 2000manip2 2003manip2 2006manip2 2009       ##
## 2015 <- 2000manip2 2003manip2 2006manip2 2009 2012             ##
## 2018 <- 2003manip2 2006manip2 2009 2012 2015                   ##
####################################################################
target.ife <- 12013 # azoyu parent
i <- 1 # debug
sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
extendCoal[[sel1]] <- extendCoalmanip2[[1]]
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
target.ife <- 12081 # child
#for (i in 1:length(sel)){
    i <- 1 # debug
    sel1 <- which(as.numeric(names(extendCoal))      %in% target.ife[i])
    sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife[i])
    tmp <- extendCoal[[sel1]]     # duplicate for manipulation
    sel.c <- c("pan","pri","left","efec","d.pan","d.pri","d.left","vhat.pan","vhat.pri","vhat.left","bhat.pan","bhat.left","alphahat.pan","alphahat.pri","alphahat.left","betahat.pan","betahat.left")
    tmp[tmp$yr==1991,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1991)        
    tmp[tmp$yr==1994,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1994)        
    tmp[tmp$yr==1997,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 1997)        
    tmp[tmp$yr==2000,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2000)        
    tmp[tmp$yr==2003,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2003)        
    tmp[tmp$yr==2006,sel.c] <- NA # empty incorrect regression coefs and predicts with NAs (mun did not exist in 2006)        
    extendCoal[[sel1]] <- tmp     # return manipulated data
    # no need to change regs fwd in new muns
    #need to figure if mean.regsmanip should also be used---manips input skipped
#}
#
# clean
rm(v91manip, v94manip, v97manip, v00manip, v03manip, v06manip, v09manip, v12manip, v15manip, v18manip,
   regs.2006manip, regs.2009manip, regs.2012manip, regs.2015manip, regs.2018manip, mean.regsmanip,
   regs.2006manip2, regs.2009manip2, regs.2012manip2, regs.2015manip2, regs.2018manip2, mean.regsmanip2,
   extendCoalmanip, extendCoalmanip2)
rm(v91,v94,v97,v00,v03,v06,v09,v12,v15,v18)
rm(pan,pri,left,oth,efec)
rm(sel,sel1,sel2,sel.to,sel.c,target.ife,i,tmp)

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
#rm(extendCoal.2015) # clean memory

# plug inegi into data for export
tmp <- v00m[,c("ife","inegi")]
#dim(tmp); dim(extendCoal.2006) # debug
extendCoal.2006 <- merge(x = extendCoal.2006, y = tmp, by = "ife", all = TRUE)
extendCoal.2009 <- merge(x = extendCoal.2009, y = tmp, by = "ife", all = TRUE)
extendCoal.2012 <- merge(x = extendCoal.2012, y = tmp, by = "ife", all = TRUE)
extendCoal.2015 <- merge(x = extendCoal.2015, y = tmp, by = "ife", all = TRUE)
extendCoal.2018 <- merge(x = extendCoal.2018, y = tmp, by = "ife", all = TRUE)

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
