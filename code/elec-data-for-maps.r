## Code is a replica, updated, of /home/eric/Downloads/Desktop/MXelsCalendGovt/atlasDis/code/mapPrep.r
## Prepares various seccion-level measures of party performance in federal diputado elections
## retrieves electoral info from data in another repository: github.com/emagar/elecRturns

## esto prepara los datos electorales para mapear los distritos de cada estado.
## el contenido queda guardado en una archivo de datos.
## sólo tiene que correrse en caso de cambios.

rm(list=ls())
options(width = 210)
#
# old #dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/")
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
    for (i in 1:length(sel.c)){
        d[,sel.c[i]] <- ave(d[,sel.c[i]], d$edon*10000+d$seccion, FUN=sum, na.rm=TRUE)
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
sel.c <- which(colnames(d) %in% sel.c)
d <- ag.sec(d, sel.c)
# district coalition dummies
d$dpanc <- ave(d$panc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpanc <- as.numeric(d$dpanc>0)
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
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
#
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# district coalition dummies
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
table(d$dpric, useNA = "always")
#
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
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
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
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# district coalition dummies
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- ave(d$ptc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dptc <- as.numeric(d$dptc>0)
table(d$dpric, useNA = "always")
table(d$dptc, useNA = "always")
#
## Drop
## # need coalition votes only
## d$pric <- (d$pri + d$pvem + d$pripvem) * d$dpric; d$pripvem <- NULL
## d$pri <- d$pri * (1 - d$dpric)
## d$pvem <- d$pvem * (1 - d$dpric)
## d$ptc <- (d$pt + d$conve + d$ptconve) * d$dptc; d$ptconve <- NULL
## d$pt <- d$conve <- d$ptconve <- NULL
#
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
#
# recode status
table(d$ESTATUS_ACTA) # missing codebook, quizás 3 y 4 que sólo tienen NAs sean anulados/noentregados o algo así
d[which(d$ESTATUS_ACTA==3),]
sel.c <- which(colnames(d) %in% sel.c)
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
#
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpric, useNA = "always")
table(d$dprdc, useNA = "always")
#
## DROP
## # need coalition votes only
## d$pric <- (d$pri + d$pvem + d$pric) * d$dpric;
## d$pri <- d$pri * (1 - d$dpric)
## d$pvem <- d$pvem * (1 - d$dpric)
## d$prdc <- (d$prd + d$pt + d$mc + d$prdc) * d$dprdc;
#
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
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpric, useNA = "always")
table(d$dprdc, useNA = "always")
#
sel.r <- grep("E6|E7", d$OBSERVACIONES) # casillas no instaladas
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
## DROP
## # need coalition votes only
## d$pric <- (d$pri + d$pvem + d$pric) * d$dpric
## d$pri <- d$pri * (1 - d$dpric)
## d$pvem <- d$pvem * (1 - d$dpric)
## d$prdc <- (d$prd + d$pt   + d$prdc)   * d$dprdc
## d$prd <- d$prd * (1 - d$dprdc)
## d$pt <- d$pt * (1 - d$dprdc)
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
#
# sel.r <- grep("E6|E7", d$OBSERVACIONES) # casillas no instaladas
sel.c <- which(colnames(d) %in% sel.c)
#d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# clean
d <- within(d, edo <- cabecera <- casn <- TIPO_CASILLA <- NULL)
v18 <- d

# clean
rm(ag.sec,d,sel.c,sel.r)

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
eq <- read.csv(tmp)
# get municipio info to merge into votes
##################################################################
## (pending: adapt secciones to 2017 shapefiles to avoid blanks ##
##################################################################
muns <- eq[,c("edon","seccion","ife","inegi")]
#muns <- eq[,c("edon","seccion","ife","inegi","mun")]

# match yearly observations (secciones)
#dim(v06); dim(v09); dim(v12); dim(v15) # something amiss in 2009?
v00$edosecn <- v00$edon*10000 + v00$seccion; v00$d00 <- 1
v03$edosecn <- v03$edon*10000 + v03$seccion; v03$d03 <- 1
v06$edosecn <- v06$edon*10000 + v06$seccion; v06$d06 <- 1
v09$edosecn <- v09$edon*10000 + v09$seccion; v09$d09 <- 1
v12$edosecn <- v12$edon*10000 + v12$seccion; v12$d12 <- 1
v15$edosecn <- v15$edon*10000 + v15$seccion; v15$d15 <- 1
v18$edosecn <- v18$edon*10000 + v18$seccion; v18$d18 <- 1
# dummies d00 to d18 indicate if seccion exists each year
tmp <- merge(x=v00[,c("edosecn","d00")], y=v03[,c("edosecn","d03")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v06[,c("edosecn","d06")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v09[,c("edosecn","d09")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v12[,c("edosecn","d12")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v15[,c("edosecn","d15")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v18[,c("edosecn","d18")], by = "edosecn", all = TRUE)
v00$d00 <- v03$d03 <- v06$d06 <- v09$d09 <- v12$d12 <- v15$d15 <- v18$d18 <- NULL # clean
#
# adds any missing secciones to each object
v00 <- merge(x=tmp, y=v00, by = "edosecn", all = TRUE)
v03 <- merge(x=tmp, y=v03, by = "edosecn", all = TRUE)
v06 <- merge(x=tmp, y=v06, by = "edosecn", all = TRUE)
v09 <- merge(x=tmp, y=v09, by = "edosecn", all = TRUE)
v12 <- merge(x=tmp, y=v12, by = "edosecn", all = TRUE)
v15 <- merge(x=tmp, y=v15, by = "edosecn", all = TRUE)
v18 <- merge(x=tmp, y=v18, by = "edosecn", all = TRUE)
#dim(v00); dim(v03); dim(v06); dim(v06); dim(v12); dim(v15);
# fill in missing edon and seccion numbers
v00 <- within(v00, {
    edon    <- as.integer(edosecn/10000);
    seccion <- edosecn - edon*10000;
})
v03 <- within(v03, {
    edon    <- as.integer(edosecn/10000);
    seccion <- edosecn - edon*10000;
})
v06 <- within(v06, {
    edon    <- as.integer(edosecn/10000);
    seccion <- edosecn - edon*10000;
})
v09 <- within(v09, {
    edon    <- as.integer(edosecn/10000);
    seccion <- edosecn - edon*10000;
})
v12 <- within(v12, {
    edon    <- as.integer(edosecn/10000);
    seccion <- edosecn - edon*10000;
})
v15 <- within(v15, {
    edon    <- as.integer(edosecn/10000);
    seccion <- edosecn - edon*10000;
})
v18 <- within(v18, {
    edon    <- as.integer(edosecn/10000);
    seccion <- edosecn - edon*10000;
})
rm(tmp)

# consolidate municipios
muns$edosecn <- muns$edon*10000 + muns$seccion
# add municipio names to votes
sel.drop <- which(colnames(muns) %in% c("edon","seccion")) # do not merge these columns
v00 <- merge(x = v00, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v00$munn <- NULL
v03 <- merge(x = v03, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v03$munn <- NULL
v06 <- merge(x = v06, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v06$munn <- NULL
v09 <- merge(x = v09, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v09$munn <- NULL
v12 <- merge(x = v12, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v12$munn <- NULL
v15 <- merge(x = v15, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v15$munn <- NULL
v18 <- merge(x = v18, y = muns[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v18$munn <- NULL

#################################################
## function to aggregate municipio-level votes ##
#################################################
ag.mun <- function(d=d, sel.c=sel.c){
    for (i in 1:length(sel.c)){
        d[,sel.c[i]] <- ave(d[,sel.c[i]], d$edon*1000+d$inegi, FUN=sum, na.rm=TRUE)
    }
    sel.r <- which(duplicated(d$edon*1000+d$inegi)==TRUE)
    d <- d[-sel.r,]
    return(d)
}
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
dim(v00); dim(v03); dim(v06); dim(v09); dim(v12); dim(v15); dim(v18); 

# rename section-level aggregates to free v00 v03 etc for general use
v00s <- v00; v03s <- v03; v06s <- v06; v09s <- v09; v12s <- v12; v15s <- v15; v18s <- v18;
rm(v00,v03,v06,v09,v12,v15,v18)

############################################################
## ###################################################### ##
## ## Determine level of aggregation to work with here ## ##
## ## by choosing s, m, d...                           ## ##
## ###################################################### ##
############################################################
agg <- c("m","s","d")[1]
if (agg=="m") {
v00 <- v00m; v03 <- v03m; v06 <- v06m; v09 <- v09m; v12 <- v12m; v15 <- v15m; v18 <- v18m;
}
if (agg=="s") {
v00 <- v00s; v03 <- v03s; v06 <- v06s; v09 <- v09s; v12 <- v12s; v15 <- v15s; v18 <- v18s;
}
if (agg=="d") {
v00 <- v00d; v03 <- v03d; v06 <- v06d; v09 <- v09d; v12 <- v12d; v15 <- v15d; v18 <- v18d;
}

###########################################
## prepare manipulated party objects     ##
## for time-series and alpha regressions ##
###########################################
#
# version 1: extend partial coalitions across the board
# shares
pan <- data.frame(v00 = ifelse(v00$efec==0, NA,  v00$panc / v00$efec),
                  v03 = ifelse(v03$efec==0, NA,  v03$pan  / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pan  / v06$efec),
                  v09 = ifelse(v09$efec==0, NA,  v09$pan  / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pan  / v12$efec),
                  v15 = ifelse(v15$efec==0, NA,  v15$pan  / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pan + v18$panc + v18$prd + v18$mc) / v18$efec))
pan <- round(pan, 3)
#
pri <- data.frame(v00 = ifelse(v00$efec==0, NA,  v00$pri / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$pri + v03$pric + v03$pvem) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pric / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pri + v09$pric + v09$pvem) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA, (v12$pri + v12$pric + v12$pvem) / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$pri + v15$pric + v15$pvem) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pri + v18$pric + v18$pvem + v18$pna) / v18$efec))
pri <- round(pri, 3)
#
morena <- data.frame(v00 = ifelse(v00$efec==0, NA,  v00$prdc / v00$efec),
                     v03 = ifelse(v03$efec==0, NA, (v03$prd + v03$pt + v03$conve) / v03$efec),
                     v06 = ifelse(v06$efec==0, NA,  v06$prdc / v06$efec),
                     v09 = ifelse(v09$efec==0, NA, (v09$prd + v09$pt + v09$ptc + v09$conve) / v09$efec),
                     v12 = ifelse(v12$efec==0, NA, (v12$prd + v12$prdc + v12$pt + v12$mc)  / v12$efec),
                     v15 = ifelse(v15$efec==0, NA, (v15$prd + v15$prdc + v15$pt + v15$morena + v15$pes) / v15$efec),
                     v18 = ifelse(v18$efec==0, NA, (v18$morena + v18$morenac + v18$pt + v18$pes) / v18$efec))
morena <- round(morena, 3)
#
oth <- data.frame(v00 = ifelse(v00$efec==0, NA, (v00$pcd + v00$parm + v00$dsppn) / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$psn + v03$pas + v03$mp + v03$plm + v03$fc) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA, (v06$pna + v06$asdc) / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pna + v09$psd) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pna / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$mc + v15$pna + v15$ph + v15$indep1 + v15$indep2) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$indep1 + v18$indep2) / v18$efec))
oth <- round(oth, 3)
# transpose to plug columns into new data.frames
pan <- t(pan)
pri <- t(pri)
morena <- t(morena)
oth <- t(oth)
#
extendCoal <- as.list(rep(NA, nrow(v00))) # empty list will receive one data.frame per municipio
# loop over municipios
for (i in 1:nrow(v00)){
    #i <- 2 # debug
    tmp <- data.frame(yr = seq(from=2000, to=2018, by=3),
                      pan = pan[,i],
                      pri = pri[,i],
                      morena = morena[,i],
                      oth = oth[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan[is.na(tmp$pan)] <- per.means["pan"];
        tmp$pri[is.na(tmp$pri)] <- per.means["pri"];
        tmp$morena[is.na(tmp$morena)] <- per.means["morena"];
        tmp$oth[is.na(tmp$oth)] <- per.means["oth"];
    }
    # add epsilon = 2*max(rounding error) to zeroes 
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
        tmp[,2:5]
        rowSums(tmp[,2:5])
        tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3) # re-compute shares
    }
    extendCoal[[i]] <- tmp
}
#
# datos para regresión de alfa
yr.means <- data.frame(yr = seq(2000,2018,3),
                       pan = rep(NA,7),
                       pri = rep(NA,7),
                       morena = rep(NA,7),
                       oth = rep(NA,7))
#
yr.means$pan[1]    <-  apply(v00, 2, sum)["panc"]                              / apply(v00, 2, sum)["efec"]
yr.means$pri[1]    <-  apply(v00, 2, sum)["pri"]                               / apply(v00, 2, sum)["efec"]
yr.means$morena[1] <-  apply(v00, 2, sum)["prdc"]                              / apply(v00, 2, sum)["efec"]
yr.means$oth[1]    <- (apply(v00, 2, sum)["pcd"] + apply(v00, 2, sum)["parm"]) / apply(v00, 2, sum)["efec"]
#
yr.means$pan[2]    <-   apply(v03, 2, sum)["pan"]                                / apply(v03, 2, sum)["efec"]
yr.means$pri[2]    <-  (apply(v03, 2, sum)["pri"] + apply(v03, 2, sum)["pric"] + apply(v03, 2, sum)["pvem"])  / apply(v03, 2, sum)["efec"]
yr.means$morena[2] <-  (apply(v03, 2, sum)["prd"] + apply(v03, 2, sum)["pt"]   + apply(v03, 2, sum)["conve"]) / apply(v03, 2, sum)["efec"]
yr.means$oth[2]    <-  (apply(v03, 2, sum)["psn"] + apply(v03, 2, sum)["pas"]  + apply(v03, 2, sum)["mp"] + apply(v03, 2, sum)["plm"] + apply(v03, 2, sum)["fc"]) / apply(v03, 2, sum)["efec"]
#
yr.means$pan[3]    <-   apply(v06, 2, sum)["pan"]                               / apply(v06, 2, sum)["efec"]
yr.means$pri[3]    <-   apply(v06, 2, sum)["pric"]                              / apply(v06, 2, sum)["efec"]
yr.means$morena[3] <-   apply(v06, 2, sum)["prdc"]                              / apply(v06, 2, sum)["efec"]
yr.means$oth[3]    <-  (apply(v06, 2, sum)["pna"] + apply(v06, 2, sum)["asdc"]) / apply(v06, 2, sum)["efec"]
#
yr.means$pan[4]    <-   apply(v09, 2, sum)["pan"]                                / apply(v09, 2, sum)["efec"]
yr.means$pri[4]    <-  (apply(v09, 2, sum)["pri"] + apply(v09, 2, sum)["pric"] + apply(v09, 2, sum)["pvem"])  / apply(v09, 2, sum)["efec"]
yr.means$morena[4] <-  (apply(v09, 2, sum)["prd"] + apply(v09, 2, sum)["pt"]   + apply(v09, 2, sum)["ptc"] + apply(v09, 2, sum)["conve"]) / apply(v09, 2, sum)["efec"]
yr.means$oth[4]    <-  (apply(v09, 2, sum)["pna"] + apply(v09, 2, sum)["psd"]) / apply(v09, 2, sum)["efec"]
#
yr.means$pan[5]    <-    apply(v12, 2, sum)["pan"] / apply(v12, 2, sum)["efec"]
yr.means$pri[5]    <-   (apply(v12, 2, sum)["pri"] + apply(v12, 2, sum)["pric"] + apply(v12, 2, sum)["pvem"])  / apply(v12, 2, sum)["efec"]
yr.means$morena[5] <-   (apply(v12, 2, sum)["prd"] + apply(v12, 2, sum)["prdc"] + apply(v12, 2, sum)["pt"] + apply(v12, 2, sum)["mc"]) / apply(v12, 2, sum)["efec"]
yr.means$oth[5]    <-    apply(v12, 2, sum)["pna"] / apply(v12, 2, sum)["efec"]
#
yr.means$pan[6]    <-    apply(v15, 2, sum)["pan"] / apply(v15, 2, sum)["efec"]
yr.means$pri[6]    <-   (apply(v15, 2, sum)["pri"] + apply(v15, 2, sum)["pric"] + apply(v15, 2, sum)["pvem"])  / apply(v15, 2, sum)["efec"]
yr.means$morena[6] <-   (apply(v15, 2, sum)["prd"] + apply(v15, 2, sum)["prdc"]   + apply(v15, 2, sum)["pt"] + apply(v15, 2, sum)["morena"] + apply(v15, 2, sum)["pes"]) / apply(v15, 2, sum)["efec"]
yr.means$oth[6]    <-   (apply(v15, 2, sum)["mc"] + apply(v15, 2, sum)["pna"] + apply(v15, 2, sum)["ph"] + apply(v15, 2, sum)["indep1"] + apply(v15, 2, sum)["indep2"]) / apply(v15, 2, sum)["efec"]
#
yr.means$pan[7]    <-   (apply(v18, 2, sum)["pan"] + apply(v18, 2, sum)["panc"] + apply(v18, 2, sum)["prd"] + apply(v18, 2, sum)["mc"]) / apply(v18, 2, sum)["efec"]
yr.means$pri[7]    <-   (apply(v18, 2, sum)["pri"] + apply(v18, 2, sum)["pric"] + apply(v18, 2, sum)["pvem"] + apply(v18, 2, sum)["pna"]) / apply(v18, 2, sum)["efec"]
yr.means$morena[7] <-   (apply(v18, 2, sum)["morena"] + apply(v18, 2, sum)["morenac"] + apply(v18, 2, sum)["pt"] + apply(v18, 2, sum)["pes"]) / apply(v18, 2, sum)["efec"]
yr.means$oth[7]    <-   (apply(v18, 2, sum)["indep1"] + apply(v18, 2, sum)["indep2"]) / apply(v18, 2, sum)["efec"]
#
yr.means <- within(yr.means, mean.rpan    <- pan/pri)
yr.means <- within(yr.means, mean.rmorena <- morena/pri)
yr.means <- within(yr.means, mean.roth    <- oth/pri)
#
yr.means[,2:8] <- round(yr.means[,2:8], 3)
#
# plug into data
for (i in 1:nrow(v00)){
    #i <- 2 # debug
    extendCoal[[i]] <- cbind(extendCoal[[i]], yr.means[,6:8])
}

############################################################################
## should also try jags estimation to get post-sample of vhats and alphas ##
############################################################################
#
###############################
## código de las regresiones ##
###############################
vhat.2018 <- vhat.2015 <- data.frame(pan    = rep(NA, nrow(v00)),
                                     pri    = rep(NA, nrow(v00)),
                                     morena = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       morena = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      morena = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                                       # regression per municipio, each used to
                                                       # predict votes in 2015 and 2018 
#
regs.2015 <- regs.2018 <- list(pan    = tmp,
                               morena = tmp,
                               oth    = tmp,
                               readme = "No pri regs because DVs are pri-ratios")
#
mean.regs <- list(pan    = tmp,
                  morena = tmp,
                  oth    = tmp,
                  readme = "No pri regs bec DVs are pri-ratios")

for (i in 1:nrow(v00)){
    #i <- 500 # debug
    message(sprintf("loop %s of %s", i, nrow(v00)))
    data.tmp <- extendCoal[[i]]
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.morena <- lm(formula = log(morena/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.morena <- exp(predict.lm(reg.morena, newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    vhat.morena <- round(rhat.morena / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    #
    ## plug into results objects ##
    vhat.2015[i,] <- c(vhat.pan, vhat.pri, vhat.morena)
    regs.2015$pan[[i]]    <- reg.pan
    regs.2015$morena[[i]] <- reg.morena
    regs.2015$oth[[i]]    <- reg.oth
    #
    data.tmp$vhat.morena <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # open slots for projections
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.morena[data.tmp$yr==year] <- vhat.morena
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.morena <- lm(formula = log(morena/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.morena <- exp(predict.lm(reg.morena, newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    vhat.morena <- round(rhat.morena / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    #
    ## plug into results objects ##
    vhat.2018[i,] <- c(vhat.pan, vhat.pri, vhat.morena)
    regs.2018$pan   [[i]] <- reg.pan
    regs.2018$morena[[i]] <- reg.morena
    regs.2018$oth   [[i]] <- reg.oth
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.morena[data.tmp$yr==year] <- vhat.morena
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)    ~ mean.rpan, data = data.tmp)
    reg.morena <- lm(formula = log(morena/pri) ~ mean.rmorena, data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)    ~ mean.roth, data = data.tmp)
    #
    new.d <- data.frame(mean.rpan = 0)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    new.d <- data.frame(mean.rmorena = 0)
    rhat.morena <- exp(predict.lm(reg.morena, newdata = new.d))#, interval = "confidence")
    new.d <- data.frame(mean.roth = 0)
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    vhat.morena <- round(rhat.morena / (1 + rhat.pan + rhat.morena + rhat.oth), 3)
    #
    #c(vhat.pan, vhat.pri, vhat.morena, 1-vhat.pan-vhat.pri-vhat.morena)
    alphahat[i,] <- c(vhat.pan, vhat.pri, vhat.morena)
    betahat[i,1] <- coef(reg.pan)   [2]
    betahat[i,2] <- coef(reg.morena)[2]
    betahat[i,3] <- coef(reg.oth)   [2]
    mean.regs$pan   [[i]] <- reg.pan
    mean.regs$morena[[i]] <- reg.morena
    mean.regs$oth   [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.morena <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.morena <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.morena <- alphahat$morena[i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.morena <- betahat$morena[i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
    data.tmp <- round(data.tmp,3)
    #
    ########################################################
    ## optional: plug vhats alphas betas back into data   ##
    ########################################################
    data.tmp$mean.rpan <- data.tmp$mean.rmorena <- data.tmp$mean.roth <- NULL # remove mean ratios
    extendCoal[[i]] <- data.tmp
}

# generate data frame with 2018 values for export
year <- 2018
sel <- which(extendCoal[[1]]$yr==year) # which row reports year (symmetric in all other objects in list)
tmp <- lapply(extendCoal, function(x) {
    prune <- x[sel,] # keep selected row only
    return(prune)
}) # keep sel yr only in every municipio
tmp <- do.call("rbind", tmp)   # turn into one dataframe
rownames(tmp) <- NULL
if (agg=="m") sel.col <- c("edon","ife","inegi","disn")       # cols to merge when using municipios
if (agg=="s") sel.col <- c("edon","seccion","edosecn","ife","inegi","disn") # when using secciones
tmp <- cbind(tmp, v00[,sel.col])
rm(sel.col)
#
extendCoal.2018 <- tmp
#
year <- 2015
sel <- which(extendCoal[[1]]$yr==year) # which row reports year (symmetric in all other objects in list)
tmp <- lapply(extendCoal, function(x) {
    prune <- x[sel,] # keep selected row only
    return(prune)
}) # keep sel yr only in every municipio
tmp <- do.call("rbind", tmp)   # turn into one dataframe
rownames(tmp) <- NULL
if (agg=="m") sel.col <- c("edon","ife","inegi","disn")       # cols to merge when using municipios
if (agg=="s") sel.col <- c("edon","seccion","edosecn","ife","inegi","disn") # when using secciones
tmp <- cbind(tmp, v00[,sel.col])
rm(sel.col)
#
extendCoal.2015 <- tmp

# save to disk
write.csv(extendCoal.2018,
          file = paste(wd, "data/dipfed2018mu-vhat.csv", sep = ""),
          row.names = FALSE)
#
write.csv(extendCoal.2015,
          file = paste(wd, "data/dipfed2015mu-vhat.csv", sep = ""),
          row.names = FALSE)

dim(extendCoal.2018)

round(quantile(x = (extendCoal.2018$pan    - extendCoal.2018$vhat.pan)   , probs = seq(0,1,.1)),3)
round(quantile(x = (extendCoal.2018$pri    - extendCoal.2018$vhat.pri)   , probs = seq(0,1,.1)),3)
round(quantile(x = (extendCoal.2018$morena - extendCoal.2018$vhat.morena), probs = seq(0,1,.1)),3)

col.pan <-    rgb(.18,.24,.73, alpha = .3) # moderate blue
col.pri <-    rgb(.89,.17,.17, alpha = .3) # bright red
col.morena <- rgb(.55,.27,.07, alpha = .3)

extendCoal.2015[1,]

# morena vs pri core
plot(x = extendCoal.2018$alphahat.pri, y = (extendCoal.2018$morena - extendCoal.2018$vhat.morena), type = "n", xlab = "Núcleo del PRI", ylab = "Residual de Morena en 2018")
abline(h = 0, col = col.pri)
points(x = extendCoal.2018$alphahat.pri, y = (extendCoal.2018$morena - extendCoal.2018$vhat.morena), pch = 20, cex = 1.25, col = rgb(.55,.27,.07, alpha = .3))
fit <- lm((morena - vhat.morena) ~ poly(alphahat.pri,3), data = extendCoal.2018)
xx <- seq(from = 0, to = 1, by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.pri=xx)), col = "black", lwd = 1.5)

# morena vs pan core
plot(x = extendCoal.2018$alphahat.pri, y = (extendCoal.2018$morena - extendCoal.2018$vhat.morena), type = "n", xlab = "Núcleo del PAN", ylab = "Residual de Morena en 2018")
abline(h = 0, col = col.pan)
points(x = extendCoal.2018$alphahat.pan, y = (extendCoal.2018$morena - extendCoal.2018$vhat.morena), pch = 20, cex = 1.25, col = rgb(.55,.27,.07, alpha = .3))
fit <- lm((morena - vhat.morena) ~ poly(alphahat.pan,2), data = extendCoal.2018)
xx <- seq(from = 0, to = 1, by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.pan=xx)), col = "black", lwd = 1.5)


plot(x = extendCoal.2018$alphahat.pri, y = (extendCoal.2018$morena - extendCoal.2018$vhat.morena), type = "n", xlab = "Núcleo del PRI", ylab = "Residual del Frente en 2018")
abline(h = 0, col = "red")
points(x = extendCoal.2018$alphahat.pri, y = (extendCoal.2018$pan - extendCoal.2018$vhat.pan), pch = 20, cex = 1.25, col = rgb(.23,.28,.75, alpha = .3))
fit <- lm((pan - vhat.pan) ~ poly(alphahat.pri,3), data = extendCoal.2018)
xx <- seq(from = 0, to = 1, by = .01)
lines(xx, predict.lm(fit, data.frame(alphahat.pri=xx)), col = "black", lwd = 1.5)

points((1:10) + 0.4, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5),
       pch = 16, cex = 4)

x


# version 2: coalitions only in districts where they happened
## morenaRealm <- data.frame(v00 = v00m$prdc / v00m$efec,
##                           v03 = v03m$prd / v03m$efec,
##                           v06 = v06m$prdc / v06m$efec,
##                           v09 = v09m$prd / v09m$efec,
##                           v12 = v12m$prdc / v12m$efec,
##                           v15 = (v15m$prd * (1 - v15m$dprdc) + v15m$prdc * v15m$dprdc + v15m$morena) / v15m$efec,
##                           v18 = (v18m$morena * (1 - v18m$dmorenac) + v18m$morenac * v18m$dmorenac) / v18m$efec)
## esto no jala, parece que morena!=0 cuando dmorenac==1
## morenaRealm[1,]
# version 3: party's own vote plus proportional part of coal
## morenaBrkm


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
