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
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom")
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
d <- within(d, casilla <- TIPO_CASILLA <- ESTATUS_ACTA <- prd <- pt <- mc <- NULL)
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
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec")
d <- ag.mun(d,sel.c)
v00m <- d
##########
## 2003 ##
##########
d <- v03; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
v03m <- d
##########
## 2006 ##
##########
d <- v06; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.mun(d,sel.c)
v06m <- d
##########
## 2009 ##
##########
d <- v09; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pvem","pna","psd","ptc","efec","lisnom","dpric")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
v09m <- d
##########
## 2012 ##
##########
d <- v12; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pvem","pna","pric","prdc","efec","lisnom","dpric")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
v12m <- d
##########
## 2015 ##
##########
d <- v15; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom","dpric","dprdc")
d <- ag.mun(d,sel.c)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
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
v18m <- d
#
dim(v00); dim(v03); dim(v06); dim(v09); dim(v12); dim(v15); dim(v18); 

##################################################################
## prepare manipulated party objects for time-series regression ##
##################################################################
#
# version 1: extend partial coalitions across the board
## # debug
## morenaExtm <- data.frame(v00 = v00m$prdc,
##                          v03 = (v03m$prd + v03m$pt + v03m$conve),
##                          v06 = v06m$prdc,
##                          v09 = (v09m$prd + v09m$ptc),
##                          v12 = v12m$prdc,
##                          v15 = (v15m$prd + v15m$prdc + v15m$pt + v15m$morena + v15m$pes),
##                          v18 = (v18m$morena + v18m$morenac + v18m$pt + v18m$pes))

## tmp.verif <- apply(X = morenaExtm, MARGIN = c(1,2), function(x) as.numeric(x>0)) 
## tmp <- which(rowSums(tmp.verif)<7)
## i <- i+1
## morenaExtm[tmp[i],]
## v00m[tmp[i],]
## v03m[tmp[i],]
## v06m[tmp[i],]
## v09m[tmp[i],]
## v12m[tmp[i],]
## v15m[tmp[i],]
## v18m[tmp[i],]
## #
## tmp.verif <- as.numeric(v00m$efec==0) 
## tmp <- which(tmp.verif==1)
## i <- i+1
## v00m[tmp[i],]
## morenaExtm[tmp[i],]
## x
# shares
morenaExtm <- data.frame(v00 = ifelse(v00m$efec==0, NA, v00m$prdc / v00m$efec),
                         v03 = ifelse(v03m$efec==0, NA, (v03m$prd + v03m$pt + v03m$conve) / v03m$efec),
                         v06 = ifelse(v06m$efec==0, NA, v06m$prdc / v06m$efec),
                         v09 = ifelse(v09m$efec==0, NA, (v09m$prd + v09m$ptc) / v09m$efec),
                         v12 = ifelse(v12m$efec==0, NA, v12m$prdc / v12m$efec),
                         v15 = ifelse(v15m$efec==0, NA, (v15m$prd + v15m$prdc + v15m$pt + v15m$morena + v15m$pes) / v15m$efec),
                         v18 = ifelse(v18m$efec==0, NA, (v18m$morena + v18m$morenac + v18m$pt + v18m$pes) / v18m$efec))
morenaExtm <- round(morenaExtm, 3)
# munic means dropping NAs (to replace those NAs)
tmp <- apply(morenaExtm, 1, function(x) mean(x, na.rm = TRUE))
tmp <- round(tmp, 3)
tmp <- cbind(tmp,tmp,tmp,tmp,tmp,tmp,tmp)
# replace NAs with munic mean
morenaExtm[is.na(morenaExtm)] <- tmp[is.na(morenaExtm)]
# substract .001 to ones to avoid indeterminacy
summary(morenaExtm)
sel <- which(apply(morenaExtm, 1, function(x) as.numeric(max(x)>=1))==1)
morenaExtm[sel,]
v03m[sel,]
morenaExtm[morenaExtm==1] <- 
morenaExtm[morenaExtm==0] <- 


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
table(v12m$dpric, v12m$pri>0)
sel <- which(v12m$dpric==1 & (v12m$pri>0)==TRUE)
sel <- which(v12m$edon==4 & v12m$disn==2)
v12m[sel,]
table(v03m$dpric)
x

# falta leer presidenciales 94 00 06 12 18
# falta leer dipfed 91 94 97
# falta leer municipales 91--19

# pasos para correr las regresiones
dat <- morenaExtm
summary(dat)
Y <- ncol(dat) # total years
M <- nrow(dat) # total municipios
dat <- as.data.frame(t(dat))
vhat <- dat; vhat[] <- NA # prepara df vacío
#
sel.row <- which(rownames(dat) %in% paste("v", c("00","03","06","09","12"), sep = "")) # subset regression data years to predict 2015
dat[,1]
for (m in 1:M){
    #m <- 1 # debug
    sel <- which(rownames(vhat) %in% "v15")
    y <- dat[sel.row,m]; names(y) <- NULL;
    x <- seq(from = 2000, to = 2012, by = 3);
    d <- data.frame(y = y, x = x)
    m <- lm(y ~ x, data = d);
    new.d <- data.frame(x = c(2006,2012,2015,2018))
    p <- predict(m, newdata = new.d)
    v.hat[m,] <- p;
}

                                                                  
Supón que tienes los datos organizados así (cada fila es un municipio):
amlo <- data.frame(v06=c(.4,.2,.6),
                   v12=c(.3,.3,.7),
                   v15=c(.2,.4,.5),
                   v18=c(.5,.7,.8))
Podrías hacer un loop:
v.hat <- data.frame(matrix(NA, nrow = nrow(amlo), ncol = 4)); colnames(v.hat) <- c("v06","v12","v15","v18")# prepara df vacío
for (i in 1:nrow(amlo)){
    #i <- 1 # debug
    y <- amlo[i,1:3]; names(y) <- NULL
    x <- c(2006,2012,2015)
    d <- as.data.frame(t(rbind(y,x))); colnames(d) <- c("y","x")
    m <- lm(y ~ x, data = d);
    new.d <- data.frame(x = c(2006,2012,2015,2018))
    p <- predict(m, newdata = new.d)
    v.hat[i,] <- p;
}
v.hat$v18 # esta es la predicción 2018 para cada municipio
amlo$v18 - v.hat$v18 # éste el residual
                                                
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
