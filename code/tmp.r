840
1431
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
save.image(paste0(wd, "data/too-big-4-github/tmp.RData"))

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
v21m <- within(v21m, disn <- dpanc <- dpric <- dmorenac <- NULL)
#


# drop columns before saving raw vote seccion files
#v91s <- within(v91s, munn <- NULL)
v94s <- within(v94s, edosecn <- dpanc <- dpric <- dprdc <- NULL)
v97s <- within(v97s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- dpanc <- dpric <- dprdc <- NULL)
v00s <- within(v00s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v03s <- within(v03s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v06s <- within(v06s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v09s <- within(v09s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v12s <- within(v12s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v15s <- within(v15s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v18s <- within(v18s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
v21s <- within(v18s, edosecn <- d94 <- d97 <- d00 <- d03 <- d06 <- d09 <- d12 <- d15 <- d18 <- d21 <- NULL)
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
v94 <- v94m; v97 <- v97m; v00 <- v00m; v03 <- v03m; v06 <- v06m; v09 <- v09m; v12 <- v12m; v15 <- v15m; v18 <- v18m; v21 <- v21m
# get unit winners and margins: will output object winner for chosen agg
agg <- "m"
source(paste(wd, "code/get-winners.r", sep = ""))
head(winner)
# save first part of output
write.csv(winner,
          file = paste(wd, "data/dipfed-municipio-win.csv", sep = ""), row.names = FALSE)


