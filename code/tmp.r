##############################################
## CREATION OF NEW MUNICIPIOS IN THE PERIOD ##
##############################################
## In order to prevent NAs when estimating expected vote with five previous races, this code block
## splits and aggregates secciones belonging to new/old municipalities. In many cases the municipal
## split went to court. IFE/INE are conservative, only incorporating new units after courts have
## ruled. Others usually do this before courts rule: local authorities (prior to 2016) elected municipal
## authorities, INEGI incorporates them in census/counts. These manipulations will use the less conservative
## criterion (in order to analyze municipal races), splitting federal election returns earlier than IFE/INE
## would.

1. [DONE] in eq, subset children and parents
2. [DONE] merge mun94 into v94, mun97 into v97...
3. [DONE] agg municpios: v94m... (non manip)
4. [DONE] agg municpios: v94manip...
5. consolidate square vmanip matrix 
6. list all secciones needing new mun manip in any year 1994:2018 (so that v..manip all have same nrows)
7. fix/save mun data
8. regress vhat for non-manip and manip data
9. using yrchg, identify 5 obs that need replacement
10. save m regs


tmp <- extendCoal
for (i in 1:length(tmp)){
    tmp[[i]] <- tmp[[i]][7,]
}
i


summary(extendCoal[101:150])
extendCoal[["1009"]]
summary(extendCoal.bis)

v97$efec[is.na(v97$efec)]

v91[1:20,]
tmp <- v91
tmp[]
non.nas[['1009']]
non.nas[1]
x


tmp1 <- list(A = data.frame(n=1:3, x=c("a","b","c"), y = c(1,0,0)),
             B = data.frame(n=1:3, x=c("d","e","f"), y = c(0,1,0)))
tmp1[1]
tmp1[[1]]
summary(tmp1)
tmp2 <- list(A = data.frame(n=1:4, x=c("g","h","i","j"), y = c(0,0,1,0)),
             B = data.frame(n=1:4, x=c("k","l","m","n"), y = c(1,1,0,0)))
tmp1[[1]] <- tmp2[[2]]

x



tmp1 <- extendCoal    ["32047"]
tmp.names <- names(tmp1[[1]][[1]])
tmp1 <- as.data.frame(tmp1)
names(tmp1) <- tmp.names



-----------------------


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


-----------------------




##############
## for 2006 ##
##############
sel <- which(treat.yrs$yr.chg==2009)
target.ife <- treat.yrs$ife[sel];  target.ife <- target.ife[order(target.ife)]
ls()
summary(regs.2006$pan[[1]])
sel1 <- which(as.numeric(names(extendCoal)) %in% target.ife)
sel2 <- which(as.numeric(names(extendCoalmanip)) %in% target.ife)

i <- 10 # debug
extendCoal[sel1[i]][[1]]$pan
extendCoalmanip[sel2[i]][[1]]$pan

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

treat.yrs
x
