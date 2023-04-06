

######################################################
##    consolidate districts before secciones are    ##
##    manipulated to deal with reseccionamiento     ##
######################################################
## eg. 1979 map federal election counterfactuals    ##
## to clean 1988 vote (cf. cantú, but longitudinal) ##
######################################################
DUPLICAR BLOQUE MUNICIPAL PARA ADAPTARLO A DISTRITOS


disf$edosecn <- disf$edon*10000 + disf$seccion
# add municipio names to votes
sel.drop <- which(colnames(disf) %in% c("edon","seccion")) # do not merge these columns
# v91 <- merge(x = v91, y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v91$munn <- NULL
v94 <- merge(x = v94,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v94$munn <- NULL
v97 <- merge(x = v97,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v97$munn <- NULL
v00 <- merge(x = v00,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v00$munn <- NULL
v03 <- merge(x = v03,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v03$munn <- NULL
v06 <- merge(x = v06,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v06$munn <- NULL
v09 <- merge(x = v09,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v09$munn <- NULL
v12 <- merge(x = v12,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v12$munn <- NULL
v15 <- merge(x = v15,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v15$munn <- NULL
v18 <- merge(x = v18,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v18$munn <- NULL
v21 <- merge(x = v21,  y = disf[,-sel.drop], by = "edosecn", all.x = TRUE, all.y = FALSE); v21$munn <- NULL
#
rm(disf)
#
# save all to restore after exporting raw vote manipulations (11jul2021: maybe redundant, check when working with seccion aggs)
save.image("../../datosBrutos/not-in-git/tmp-restore.RData")

################################################
## function aggregating district-level votes ##
################################################
ag.dis <- function(d=d, sel.c=sel.c, grouping=NA){
    for (i in 1:length(sel.c)){
        d[,sel.c[i]] <- ave(d[,sel.c[i]], grouping, FUN=sum, na.rm=TRUE)
    }
    sel.r <- which(duplicated(grouping)==TRUE)
    d <- d[-sel.r,]
    return(d)
}

################################
## aggregate district returns ##
################################
##########
## 1991 ## OJO: 1991 seccion identifiers are wrong, but can aggregate with disn/ife (no counterfactuals, though)
##########
d <- v91; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
d <- ag.dis(d,sel.c)
d$edosecn <- d$seccion <- NULL
v91m <- d
##########
## 1994 ##
##########
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
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
d <- ag.dis(d,sel.c)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v21m <- d
######################
## 2021 for winners ##
######################
d <- v21w; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pvem","pt","mc","morena","pes","rsp","fxm","indep","panc","pric","morenac","efec","lisnom","dpanc","dpric","dmorenac")
d <- ag.dis(d,sel.c)
d$dpanc    <- as.numeric(d$dpanc>0)
d$dpric    <- as.numeric(d$dpric>0)
d$dmorenac <- as.numeric(d$dmorenac>0)
d$edosecn <- d$seccion <- NULL
d$disn <- NULL
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v21mw <- d
# agg mun winner
#tmp <- tmp.w2 # duplicate to debug
#tmp.w2 <- tmp # restore to debug
tmp.w <- cbind(tmp.w, ife=v21w$ife) # add ife code
tmp.w$pan[is.na(tmp.w$pan)] <- 0; tmp.w$pri[is.na(tmp.w$pri)] <- 0
tmp.w$pan <- ave(tmp.w$pan, tmp.w$ife, FUN=sum, na.rm=TRUE) # use ife codes
tmp.w$pri <- ave(tmp.w$pri, tmp.w$ife, FUN=sum, na.rm=TRUE) # use ife codes
tmp.w <- tmp.w[duplicated(tmp.w$ife)==FALSE, c("pan","pri","ife")]
tmp.w[,c("pan","pri")] <- round(tmp.w[,c("pan","pri")] / rowSums(tmp.w[,c("pan","pri")]), 3) # shares add to 1
#
tmp.w2 <- cbind(tmp.w2, ife=v21w$ife) # add ife code
tmp.w2$morena[is.na(tmp.w2$morena)] <- 0; tmp.w2$pvem[is.na(tmp.w2$pvem)] <- 0
tmp.w2$morena <- ave(tmp.w2$morena, tmp.w2$ife, FUN=sum, na.rm=TRUE) # use ife codes
tmp.w2$pvem <- ave(tmp.w2$pvem, tmp.w2$ife, FUN=sum, na.rm=TRUE) # use ife codes
tmp.w2 <- tmp.w2[duplicated(tmp.w2$ife)==FALSE, c("morena","pvem","ife")]
tmp.w2[,c("morena","pvem")] <- round(tmp.w2[,c("morena","pvem")] / rowSums(tmp.w2[,c("morena","pvem")]), 3) # shares add to 1

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
## d <- ag.dis(d,sel.c, grouping=d$ife2006)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2006 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf06 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.dis(d,sel.c, grouping=d$ife2009)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2009 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf09 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.dis(d,sel.c, grouping=d$ife2012)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2012 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf12 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.dis(d,sel.c, grouping=d$ife2015)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2015 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf15 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.dis(d,sel.c, grouping=d$ife2018)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2018 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf18 <- d
## #
## d <- v91; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## d <- ag.dis(d,sel.c, grouping=d$ife2021)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2021 # use appropriate ife code
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v91m.cf21 <- d
## #
## ## d <- v91; d[is.na(d)] <- 0
## ## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","pdm","prt","pem","pt","efec")
## ## d <- ag.dis(d,sel.c, grouping=d$ife2024)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf06 <- d
#
d <- v94; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf09 <- d
#
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2012)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf12 <- d
#
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2015)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf15 <- d
#
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2018)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf18 <- d
#
d <- v94; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2021)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v94m.cf21 <- d
#
## d <- v94; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pps","prd","pfcrn","parm","uno.pdm","pt","pvem","efec")
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf06 <- d
#
d <- v97; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf09 <- d
#
d <- v97; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2012)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf12 <- d
#
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2015)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf15 <- d
#
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2018)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf18 <- d
#
d <- v97; d[is.na(d)] <- 0
sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2021)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v97m.cf21 <- d
#
## d <- v97; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","prd","pc","pt","pvem","pps","pdm","efec")
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf06 <- d
#
d <- v00; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2009)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf09 <- d
#
d <- v00; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2012)
d$dpanc <- as.numeric(d$dpanc>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v00m.cf12 <- d
#
d <- v00; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2015)
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
d <- ag.dis(d,sel.c, grouping=d$ife2018)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2006
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf06 <- d
#
d <- v03; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2009)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf09 <- d
#
d <- v03; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2012)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf12 <- d
#
d <- v03; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf15 <- d
#
d <- v03; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf18 <- d
#
d <- v03; d[is.na(d)] <- 0
sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
d <- ag.dis(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v03m.cf21 <- d
#
## d <- v03; d[is.na(d)] <- 0
## sel.c <- c("pan","pri","pric","prd","pt","pvem","conve","psn","pas","mp","plm","fc","efec","dpric")
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2009)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2009
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v06m.cf06 <- d
#
d <- v06; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2009)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2009
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf09 <- d
#
d <- v06; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2012)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf12 <- d
#
d <- v06; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2015)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf15 <- d
#
d <- v06; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2018)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf18 <- d
#
d <- v06; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2021)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v06m.cf21 <- d
#
d <- v06; d[is.na(d)] <- 0
sel.c <- c("pan","pric","prdc","pna","asdc","efec")
d <- ag.dis(d,sel.c, grouping=d$ife2009)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2009)
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
d <- ag.dis(d,sel.c, grouping=d$ife2012)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2012
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf12 <- d
#
d <- v09; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf15 <- d
#
d <- v09; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf18 <- d
#
d <- v09; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dptc <- as.numeric(d$dptc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v09m.cf21 <- d
#
## d <- v09; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
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
d <- ag.dis(d,sel.c, grouping=d$ife2009)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2012)
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
d <- ag.dis(d,sel.c, grouping=d$ife2015)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2015
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf15 <- d
#
d <- v12; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf18 <- d
#
d <- v12; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v12m.cf21 <- d
#
## d <- v12; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v12m.cf24 <- d
## #
## d <- v12; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2027)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
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
d <- ag.dis(d,sel.c, grouping=d$ife2009)
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
d <- ag.dis(d,sel.c, grouping=d$ife2012)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2015)
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
d <- ag.dis(d,sel.c, grouping=d$ife2018)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2018
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf18 <- d
#
d <- v15; d[is.na(d)] <- 0
d <- ag.dis(d,sel.c, grouping=d$ife2021)
d$dpric <- as.numeric(d$dpric>0)
d$dprdc <- as.numeric(d$dprdc>0)
d$edosecn <- d$seccion <- d$disn <- NULL
d$ife <- d$ife2021
d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
v15m.cf21 <- d
#
## d <- v15; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf24 <- d
## #
## d <- v15; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2027)
## d$dpric <- as.numeric(d$dpric>0)
## d$dprdc <- as.numeric(d$dprdc>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2027
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v15m.cf27 <- d
## #
## d <- v15; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2030)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
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
d <- ag.dis(d,sel.c, grouping=d$ife2009)
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
d <- ag.dis(d,sel.c, grouping=d$ife2012)
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
d <- ag.dis(d,sel.c, grouping=d$ife2015)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2018)
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
d <- ag.dis(d,sel.c, grouping=d$ife2021)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2027)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2030)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2033)
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
d <- ag.dis(d,sel.c, grouping=d$ife2006)
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
d <- ag.dis(d,sel.c, grouping=d$ife2009)
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
d <- ag.dis(d,sel.c, grouping=d$ife2012)
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
d <- ag.dis(d,sel.c, grouping=d$ife2015)
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
d <- ag.dis(d,sel.c, grouping=d$ife2018)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2021)
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
## d <- ag.dis(d,sel.c, grouping=d$ife2024)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2024
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf24 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2027)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2027
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf27 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2030)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2030
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf30 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2033)
## d$dpanc    <- as.numeric(d$dpanc>0)
## d$dpric    <- as.numeric(d$dpric>0)
## d$dmorenac <- as.numeric(d$dmorenac>0)
## d$edosecn <- d$seccion <- d$disn <- NULL
## d$ife <- d$ife2033
## d <- d[,-grep("^d[0-9]{2}$", colnames(d))]   # drop seccion-yr dummies
## d <- d[,-grep("^ife[0-9]{4}$", colnames(d))] # drop ife-yr vars
## v21m.cf33 <- d
## d <- v21; d[is.na(d)] <- 0
## d <- ag.dis(d,sel.c, grouping=d$ife2036)
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




