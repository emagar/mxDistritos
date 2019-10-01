## Code is a replica, updated, of /home/eric/Downloads/Desktop/MXelsCalendGovt/atlasDis/code/mapPrep.r
## Prepares various seccion-level measures of party performance in federal diputado elections

## esto prepara los datos electorales para mapear los distritos de cada estado.
## el contenido queda guardado en una archivo de datos.
## sólo tiene que correrse en caso de cambios.

rm(list=ls())
options(width = 70)
#
# old #dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/")
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/data/")
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

#read seccion-level data
##########
## 2000 ##
##########
# old #d <- read.csv( "dfSeccion2000.csv", header=TRUE, stringsAsFactors=FALSE)
d <- read.csv( "dip2000.csv", header=TRUE, stringsAsFactors=FALSE)
d <- d[order(d$edon, d$seccion),]
d <- within(d, efec <- ac + pri + am + pcd + parm + dsppn)
d <- within(d, tot <- nul <- nr <- NULL)
colnames(d)[which(colnames(d)=="ac")] <- "panc" # "pan-pvem"
colnames(d)[which(colnames(d)=="am")] <- "prdc" # "prd y muchos más"
sel.r <- grep("MR", d$status) # casillas anuladas
sel.c <- c("panc","pri","prdc","pcd","parm","dsppn","efec")
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# clean
d <- within(d, casilla <- status <- ID_ELEC <- NULL)
v00 <- d
#
##########
## 2003 ##
##########
d <- read.csv( "dip2003.csv", header=TRUE, , stringsAsFactors=FALSE)
d[is.na(d)] <- 0 # replace NAs
d <- d[order(d$edon, d$seccion),]
d <- within(d, efec <- pan + pri + prd + pt + pvem + conve + psn + pas + mp + plm + fc + apt)
d <- within(d, tot <- nul <- nr <- NULL)
sel.r <- grep("MR|[Nn]o ", d$status) # casillas anuladas no instaladas
sel.c <- c("pan","pri","prd","pt","pvem","conve","psn","pas","mp","plm","fc","apt","efec")
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# clean
colnames(d)[which(colnames(d)=="apt")] <- "pric" # "pri-pvem"
d <- within(d, casilla <- status <- ID_ELEC <- NULL)
v03 <- d
#
##########
## 2006 ##
##########
d <- read.csv( "dip2006.csv", header=TRUE, , stringsAsFactors=FALSE)
d[is.na(d)] <- 0 # replace NAs
d <- d[order(d$edon, d$seccion),]
d <- within(d, efec <- pan + apm + pbt + panal + asdc)
d <- within(d, tot <- nul <- nr <- NULL)
sel.r <- grep("Anulada|No ", d$status) # casillas anuladas no instaladas
sel.c <- c("pan","apm","pbt","panal","asdc","efec")
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# clean
d <- within(d, casilla <- ID_ELEC <- ord <- status <- nota <- valid <- NULL)
# rename cols
colnames(d)[which(colnames(d)=="apm")] <- "pric" # "pri-pvem"
colnames(d)[which(colnames(d)=="pbt")] <- "prdc" # "prd-pt-conve"
colnames(d)[which(colnames(d)=="panal")] <- "pna" 
v06 <- d
#
##########
## 2009 ##
##########
d <- read.csv( "dip2009.csv", header=TRUE, , stringsAsFactors=FALSE)
d[is.na(d)] <- 0
d <- d[order(d$edon, d$seccion),]
colnames(d)[which(colnames(d)=="PRIMERO_MEXICO")] <- "pripvem"
colnames(d)[which(colnames(d)=="SALVEMOS_MEXICO")] <- "ptconve"
colnames(d)[which(colnames(d)=="panal")] <- "pna"
d <- within(d, efec <- pan + pri + prd + pvem + pt + conve + pna + psd + pripvem + ptconve)
d <- within(d, tot <- nul <- nr <- NULL)
sel.r <- grep("NO INSTALADA|SIN ACTA|NO ENTREGADO", d$status)
sel.r2 <- grep("ANULADA", d$tepjf) # casillas anuladas no instaladas
sel.r <- union(sel.r, sel.r2); rm(sel.r2)
sel.c <- c("pan","pri","prd","pvem","pt","conve","pna","psd","pripvem","ptconve","efec","lisnom")
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
###################################
## aggregate seccion-level votes ##
###################################
d <- ag.sec(d, sel.c)
# district coalition dummies
d$dpric <- ave(d$pripvem, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- ave(d$ptconve, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dptc <- as.numeric(d$dptc>0)
table(d$dpric)
table(d$dptc)
#
# need coalition votes only
d$pric <- (d$pri + d$pvem + d$pripvem) * d$dpric; d$pripvem <- NULL
d$pri <- d$pri * (1 - d$dpric)
d$pvem <- d$pvem * (1 - d$dpric)
d$ptc <- (d$pt + d$conve + d$ptconve) * d$dptc; d$ptconve <- NULL
d$pt <- d$conve <- d$ptconve <- NULL
d <- within(d, circun <- edo <- cabecera <- munn <- casilla <- status <- tepjf <- dptc <- NULL)
v09 <- d
#
# 2012
d <- read.csv( "dip2012.csv", header=TRUE, , stringsAsFactors=FALSE)
d[is.na(d)] <- 0 # replace NAs
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
#
colnames(d)[which(colnames(d)=="PRI_PVEM")] <- "pric"
d <- within(d, prdc <- PRD_PT_MC + PRD_PT + PRD_MC + PT_MC)
d <- within(d, PRD_PT_MC <- PRD_PT <- PRD_MC <- PT_MC <- NULL)
#
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + pric + prdc)
d <- within(d, tot <- nul <- nr <- NULL)
#
# recode status
table(d$ESTATUS_ACTA) # no tengo codebook, pareciera que 3 y 4 que sólo tienen NAs son anulados/notregados o algi así...
d[which(d$ESTATUS_ACTA==3),]
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","pric","prdc","efec","lisnom")
sel.c <- which(colnames(d) %in% sel.c)
###################################
## aggregate seccion-level votes ##
###################################
d <- ag.sec(d, sel.c)
#
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE)   # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpric)
table(d$dprdc)
#
# need coalition votes only
d$pric <- (d$pri + d$pvem + d$pric) * d$dpric;
d$pri <- d$pri * (1 - d$dpric)
d$pvem <- d$pvem * (1 - d$dpric)
d$prdc <- (d$prd + d$pt + d$mc + d$prdc) * d$dprdc;
# clean
d <- within(d, casilla <- TIPO_CASILLA <- ESTATUS_ACTA <- prd <- pt <- mc <- dprdc <- NULL)
v12 <- d
#
# 2015
d <- read.csv( "dip2015.csv", header=TRUE, , stringsAsFactors=FALSE)
d[d=="-"] <- 0
d <- d[order(d$edon, d$seccion),]
d <- within(d, pri_pvem <- as.numeric(pri_pvem))
d <- within(d, prd_pt <- as.numeric(prd_pt))
d <- within(d, indep1 <- as.numeric(indep1))
d <- within(d, indep2 <- as.numeric(indep2))
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + panal + morena + ph + pes + pri_pvem + prd_pt + indep1 + indep2)
d <- within(d, tot <- nul <- nr <- NULL)
#
colnames(d)[which(colnames(d)=="panal")] <- "pna"
colnames(d)[which(colnames(d)=="pri_pvem")] <- "pric"
colnames(d)[which(colnames(d)=="prd_pt")] <- "prdc"
#
d$dpric <- ave(d$pric, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- ave(d$prdc, as.factor(d$edon*100+d$disn), FUN=sum, na.rm=TRUE) # if >0 will infer district coalition
d$dprdc <- as.numeric(d$dprdc>0)
table(d$dpric)
table(d$dprdc)
#
sel.r <- grep("E6|E7", d$OBSERVACIONES) # casillas no instaladas
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","ph","pes","pric","prdc","indep1","indep2","efec","lisnom")
sel.c <- which(colnames(d) %in% sel.c)
d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# need coalition votes only
d$pric <- (d$pri + d$pvem + d$pric) * d$dpric
d$pri <- d$pri * (1 - d$dpric)
d$pvem <- d$pvem * (1 - d$dpric)
d$prdc <- (d$prd + d$pt   + d$prdc)   * d$dprdc
d$prd <- d$prd * (1 - d$dprdc)
d$pt <- d$pt * (1 - d$dprdc)
d <- within(d, ID_CASILLA <- TIPO_CASILLA <- EXT_CONTIGUA <- OBSERVACIONES <- NULL)
v15 <- d
#
##########
## 2018 ##
##########
d <- read.csv( "dip2018.csv", header=TRUE, , stringsAsFactors=FALSE)
d[d=="-"] <- 0
d <- within(d, pan <- as.numeric(pan))
d <- within(d, pri <- as.numeric(pri))
d <- within(d, prd <- as.numeric(prd))
d <- within(d, pvem <- as.numeric(pvem))
d <- within(d, pt <- as.numeric(pt))
d <- within(d, mc <- as.numeric(mc))
d <- within(d, panal <- as.numeric(panal))
d <- within(d, morena <- as.numeric(morena))
d <- within(d, pes <- as.numeric(pes))
d <- within(d, indep1 <- as.numeric(indep1))
d <- within(d, indep2 <- as.numeric(indep2))
colnames(d)[which(colnames(d)=="panal")]     <- "pna"
colnames(d)[which(colnames(d)=="pan.prd.mc")]     <- "panc"
colnames(d)[which(colnames(d)=="pri.pvem.panal")] <- "pric"
colnames(d)[which(colnames(d)=="pt.morena.pes")]  <- "morenac"
d <- within(d, efec <- pan + pri + prd + pvem + pt + mc + pna + morena + pes + panc + pric + morenac + indep1 + indep2)
d <- within(d, nr <- nul <- tot <- NULL)
#
# sel.r <- grep("E6|E7", d$OBSERVACIONES) # casillas no instaladas
sel.c <- c("pan","pri","prd","pvem","pt","mc","pna","morena","pes","panc","pric","morenac","indep1","indep2","efec","lisnom")
sel.c <- which(colnames(d) %in% sel.c)
#d[sel.r,sel.c] <- 0 # anuladas to 0
## aggregate seccion-level votes ##
d <- ag.sec(d, sel.c)
# clean
d <- within(d, edo <- cabecera <- casn <- TIPO_CASILLA <- NULL)
d[1,]
v18 <- d

# clean
rm(ag.sec,d,sel.c,sel.r)

# district winners 2006-2015
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

# match yearly observations (secciones)
#dim(v06); dim(v09); dim(v12); dim(v15) # something amiss in 2009?
v00$edosecn <- v00$edon*10000 + v00$seccion; v00$d00 <- 1
v03$edosecn <- v03$edon*10000 + v03$seccion; v03$d03 <- 1
v06$edosecn <- v06$edon*10000 + v06$seccion; v06$d06 <- 1
v09$edosecn <- v09$edon*10000 + v09$seccion; v09$d09 <- 1
v12$edosecn <- v12$edon*10000 + v12$seccion; v12$d12 <- 1
v15$edosecn <- v15$edon*10000 + v15$seccion; v15$d15 <- 1
v18$edosecn <- v18$edon*10000 + v18$seccion; v18$d18 <- 1
tmp <- merge(x=v00[,c("edosecn","d00")], y=v03[,c("edosecn","d03")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v06[,c("edosecn","d06")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v09[,c("edosecn","d09")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v12[,c("edosecn","d12")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v15[,c("edosecn","d15")], by = "edosecn", all = TRUE)
tmp <- merge(x=tmp,                      y=v18[,c("edosecn","d18")], by = "edosecn", all = TRUE)
v00$d00 <- v03$d03 <- v06$d06 <- v09$d09 <- v12$d12 <- v15$d15 <- v18$d18 <- NULL # clean
#
v00 <- merge(x=tmp, y=v00, by = "edosecn", all = TRUE)
v03 <- merge(x=tmp, y=v03, by = "edosecn", all = TRUE)
v06 <- merge(x=tmp, y=v06, by = "edosecn", all = TRUE)
v09 <- merge(x=tmp, y=v09, by = "edosecn", all = TRUE)
v12 <- merge(x=tmp, y=v12, by = "edosecn", all = TRUE)
v15 <- merge(x=tmp, y=v15, by = "edosecn", all = TRUE)
#dim(v00); dim(v03); dim(v06); dim(v06); dim(v12); dim(v15);
# fill in missing edon and seccion numbers
v00$edon <- as.integer(v00$edosecn/10000); v00$seccion <- v00$edosecn - v00$edon*10000
v03$edon <- as.integer(v03$edosecn/10000); v03$seccion <- v03$edosecn - v03$edon*10000
v06$edon <- as.integer(v06$edosecn/10000); v06$seccion <- v06$edosecn - v06$edon*10000
v09$edon <- as.integer(v09$edosecn/10000); v09$seccion <- v09$edosecn - v09$edon*10000
v12$edon <- as.integer(v12$edosecn/10000); v12$seccion <- v12$edosecn - v12$edon*10000
v15$edon <- as.integer(v15$edosecn/10000); v15$seccion <- v15$edosecn - v15$edon*10000
rm(tmp)

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
