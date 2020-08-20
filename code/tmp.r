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

   
#######################

ls()
tmp <- eq.new[,  c("edosecn","dmunchg","mun1994")] # subset 1994 col
colnames(tmp) <- c("edosecn","dmunchg","ife")      # rename ife
v94manip <- merge(x = v94, y = tmp, by = "edosecn", all.x = FALSE, all.y = TRUE)
v94manip[1,]

# identify first federal election after municipio was created in a dataframe
# (parent and child need manipulation) 
tmp <- function(x){ # function to add rows
    x <- matrix(x, nrow=1)
    return(x)
    }
treat.yrs <- data.frame()
treat.yrs <- rbind(treat.yrs, tmp(c( 2004,2000,"parent",1)))
treat.yrs <- rbind(treat.yrs, tmp(c( 2005,2000,"child", 1)))
treat.yrs <- rbind(treat.yrs, tmp(c( 4006,1997,"parent",2)))
treat.yrs <- rbind(treat.yrs, tmp(c( 4011,1997,"child", 2)))
treat.yrs <- rbind(treat.yrs, tmp(c( 4003,2000,"parent",3)))
treat.yrs <- rbind(treat.yrs, tmp(c( 4010,2000,"child", 3)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7049,2003,"parent",4)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7118,2003,"child", 4)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7082,2003,"parent",5)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7117,2003,"child", 5)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7008,2003,"parent",6)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7116,2003,"child", 6)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7108,2015,"parent",7)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7122,2015,"child", 7)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7026,2003,"parent",8)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7112,2003,"child", 8)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7059,2003,"parent",9)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7113,2003,"child", 9)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7115,2003,"child", 9)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7052,2003,"parent",10)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7114,2003,"child", 10)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7002,2015,"parent",11)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7120,2015,"child", 11)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7081,2018,"parent",12)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7123,2018,"child", 12)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7071,2018,"parent",13)))
treat.yrs <- rbind(treat.yrs, tmp(c( 7124,2018,"child", 13)))
treat.yrs <- rbind(treat.yrs, tmp(c(12073,1997,"parent",14)))
treat.yrs <- rbind(treat.yrs, tmp(c(12076,1997,"child", 14)))
treat.yrs <- rbind(treat.yrs, tmp(c(12013,2006,"parent",15)))
treat.yrs <- rbind(treat.yrs, tmp(c(12023,2006,"parent",15)))
treat.yrs <- rbind(treat.yrs, tmp(c(12080,2006,"child", 15)))
treat.yrs <- rbind(treat.yrs, tmp(c(12044,2009,"parent",16)))
treat.yrs <- rbind(treat.yrs, tmp(c(12079,2009,"child", 16)))
treat.yrs <- rbind(treat.yrs, tmp(c(12013,2009,"parent-again",17)))
treat.yrs <- rbind(treat.yrs, tmp(c(12081,2009,"child", 17)))
treat.yrs <- rbind(treat.yrs, tmp(c(12028,2009,"parent",18)))
treat.yrs <- rbind(treat.yrs, tmp(c(12077,2009,"child", 18)))
treat.yrs <- rbind(treat.yrs, tmp(c(12042,2009,"parent",19)))
treat.yrs <- rbind(treat.yrs, tmp(c(12053,2009,"parent",19)))
treat.yrs <- rbind(treat.yrs, tmp(c(12078,2009,"child", 19)))
treat.yrs <- rbind(treat.yrs, tmp(c(14008,2006,"parent",20)))
treat.yrs <- rbind(treat.yrs, tmp(c(14125,2006,"child", 20)))
treat.yrs <- rbind(treat.yrs, tmp(c(15083,2003,"parent",21)))
treat.yrs <- rbind(treat.yrs, tmp(c(15123,2003,"child", 21)))
treat.yrs <- rbind(treat.yrs, tmp(c(15075,2003,"parent",22)))
treat.yrs <- rbind(treat.yrs, tmp(c(15124,2003,"child", 22)))
treat.yrs <- rbind(treat.yrs, tmp(c(15045,2006,"parent",23)))
treat.yrs <- rbind(treat.yrs, tmp(c(15125,2006,"child", 23)))
treat.yrs <- rbind(treat.yrs, tmp(c(23002,1997,"parent",24)))
treat.yrs <- rbind(treat.yrs, tmp(c(23008,1997,"child", 24)))
treat.yrs <- rbind(treat.yrs, tmp(c(23003,2009,"parent",25)))
treat.yrs <- rbind(treat.yrs, tmp(c(23009,2009,"child", 25)))
treat.yrs <- rbind(treat.yrs, tmp(c(23007,2015,"parent",26)))
treat.yrs <- rbind(treat.yrs, tmp(c(23010,2015,"child", 26)))
treat.yrs <- rbind(treat.yrs, tmp(c(23001,2018,"parent",27)))
treat.yrs <- rbind(treat.yrs, tmp(c(23011,2018,"child", 27)))
treat.yrs <- rbind(treat.yrs, tmp(c(24037,1997,"parent",28)))
treat.yrs <- rbind(treat.yrs, tmp(c(24058,1997,"child", 28)))
treat.yrs <- rbind(treat.yrs, tmp(c(24010,1997,"parent",29)))
treat.yrs <- rbind(treat.yrs, tmp(c(24057,1997,"child", 29)))
treat.yrs <- rbind(treat.yrs, tmp(c(26063,1997,"parent",30)))
treat.yrs <- rbind(treat.yrs, tmp(c(26071,1997,"child", 30)))
treat.yrs <- rbind(treat.yrs, tmp(c(26061,1997,"parent",31)))
treat.yrs <- rbind(treat.yrs, tmp(c(26072,1997,"child", 31)))
treat.yrs <- rbind(treat.yrs, tmp(c(29020,1997,"parent",32)))
treat.yrs <- rbind(treat.yrs, tmp(c(29060,1997,"child", 32)))
treat.yrs <- rbind(treat.yrs, tmp(c(29030,1997,"parent",33)))
treat.yrs <- rbind(treat.yrs, tmp(c(29054,1997,"child", 33)))
treat.yrs <- rbind(treat.yrs, tmp(c(29055,1997,"child", 33)))
treat.yrs <- rbind(treat.yrs, tmp(c(29010,1997,"parent",34)))
treat.yrs <- rbind(treat.yrs, tmp(c(29051,1997,"child", 34)))
treat.yrs <- rbind(treat.yrs, tmp(c(29052,1997,"child", 34)))
treat.yrs <- rbind(treat.yrs, tmp(c(29032,1997,"parent",35)))
treat.yrs <- rbind(treat.yrs, tmp(c(29053,1997,"child", 35)))
treat.yrs <- rbind(treat.yrs, tmp(c(29056,1997,"child", 35)))
treat.yrs <- rbind(treat.yrs, tmp(c(29038,1997,"parent",36)))
treat.yrs <- rbind(treat.yrs, tmp(c(29050,1997,"child", 36)))
treat.yrs <- rbind(treat.yrs, tmp(c(29029,1997,"parent",37)))
treat.yrs <- rbind(treat.yrs, tmp(c(29047,1997,"child", 37)))
treat.yrs <- rbind(treat.yrs, tmp(c(29049,1997,"child", 37)))
treat.yrs <- rbind(treat.yrs, tmp(c(29044,1997,"parent",38)))
treat.yrs <- rbind(treat.yrs, tmp(c(29048,1997,"child", 38)))
treat.yrs <- rbind(treat.yrs, tmp(c(29059,1997,"child", 38)))
treat.yrs <- rbind(treat.yrs, tmp(c(29040,1997,"parent",39)))
treat.yrs <- rbind(treat.yrs, tmp(c(29057,1997,"child", 39)))
treat.yrs <- rbind(treat.yrs, tmp(c(29015,1997,"parent",40)))
treat.yrs <- rbind(treat.yrs, tmp(c(29058,1997,"child", 40)))
treat.yrs <- rbind(treat.yrs, tmp(c(29023,1997,"parent",41)))
treat.yrs <- rbind(treat.yrs, tmp(c(29045,1997,"child", 41)))
treat.yrs <- rbind(treat.yrs, tmp(c(29022,1997,"parent",42)))
treat.yrs <- rbind(treat.yrs, tmp(c(29046,1997,"child", 42)))
treat.yrs <- rbind(treat.yrs, tmp(c(30047,1997,"parent",43)))
treat.yrs <- rbind(treat.yrs, tmp(c(30208,1997,"child", 43)))
treat.yrs <- rbind(treat.yrs, tmp(c(30105,1997,"parent",44)))
treat.yrs <- rbind(treat.yrs, tmp(c(30150,1997,"parent",44)))
treat.yrs <- rbind(treat.yrs, tmp(c(30210,1997,"child", 44)))
treat.yrs <- rbind(treat.yrs, tmp(c(30072,1997,"parent",45)))
treat.yrs <- rbind(treat.yrs, tmp(c(30093,1997,"parent",45)))
treat.yrs <- rbind(treat.yrs, tmp(c(30109,1997,"parent",45)))
treat.yrs <- rbind(treat.yrs, tmp(c(30209,1997,"child", 45)))
treat.yrs <- rbind(treat.yrs, tmp(c(30103,2006,"parent",46)))
treat.yrs <- rbind(treat.yrs, tmp(c(30211,2006,"child", 46)))
treat.yrs <- rbind(treat.yrs, tmp(c(30131,2006,"parent",47)))
treat.yrs <- rbind(treat.yrs, tmp(c(30212,2006,"child", 47)))
treat.yrs <- rbind(treat.yrs, tmp(c(32017,2003,"parent",48)))
treat.yrs <- rbind(treat.yrs, tmp(c(32057,2003,"child", 48)))
treat.yrs <- rbind(treat.yrs, tmp(c(32047,2009,"parent",49)))
treat.yrs <- rbind(treat.yrs, tmp(c(32058,2009,"child", 49)))
colnames(treat.yrs) <- c("ife","yr.chg","childparent","dyad")
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

# reload data to restore unmanipulated vote files 
rm(list = ls())
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/data/casillas/")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
setwd(wd)
load("data/too-big-4-github/tmp.RData")




x

table(treat.yrs$yr.chg, useNA = "always")
x
################

dim(eq.new)
v94manip[1,]
length(sel.r)
dim(v18)
x

# PLAYAS DE ROSARITO
cycle.chg <- 10
cycle.first <- cycle.chg - 5 
cycle.last  <- cycle.chg + 5
cycles.manip <- seq(cycle.first, cycle.last, by = 1)

child <- c(1194, 1259, 1260, 1261, 1262, 1263, 1264, 1265, 1266, 1267, 1268, 1269, 1270, 1271, 1272, 1273, 1290, 1291, 1295, 1297, 1298, 1363, 1364, 1365, 1366, 1367, 1369, 1370)
parent.new <- c(
735, 736, 737, 738, 739, 740, 741, 742, 743, 744, 745, 746, 747, 748, 749, 750, 751, 752, 753, 754, 755, 756, 757, 758, 759, 760, 761, 762, 763, 764,
765, 766, 767, 768, 769, 770, 771, 772, 773, 774, 775, 776, 777, 778, 779, 780, 781, 782, 783, 784, 785, 786, 787, 788, 789, 790, 791, 792, 793, 794,
795, 796, 797, 798, 799, 800, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815, 816, 817, 818, 819, 820, 821, 822, 823, 824,
825, 826, 827, 828, 829, 830, 831, 832, 833, 834, 835, 836, 837, 838, 839, 840, 841, 842, 843, 844, 845, 846, 847, 848, 849, 850, 851, 852, 853, 854,
855, 856, 857, 858, 859, 860, 861, 862, 863, 864, 865, 866, 867, 868, 869, 870, 871, 872, 873, 874, 875, 876, 877, 878, 879, 880, 881, 882, 883, 884,
885, 886, 887, 888, 889, 890, 891, 892, 893, 894, 895, 896, 897, 898, 899, 900, 901, 902, 903, 904, 905, 906, 907, 908, 909, 910, 911, 912, 913, 914,
915, 916, 917, 918, 919, 920, 921, 922, 923, 924, 925, 926, 927, 928, 929, 930, 931, 932, 933, 934, 935, 936, 937, 938, 939, 940, 941, 942, 943, 944,
945, 946, 947, 948, 949, 950, 951, 952, 953, 954, 955, 956, 957, 958, 959, 960, 961, 962, 963, 964, 965, 966, 967, 968, 969, 970, 971, 972, 973, 974,
975, 976, 977, 978, 979, 980, 981, 982, 983, 984, 985, 986, 987, 988, 989, 990, 991, 992, 993, 994, 995, 996, 997, 998, 999, 1000, 1001, 1002, 1003,
1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 1015, 1016, 1017, 1018, 1019, 1020, 1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028,
1029, 1030, 1031, 1032, 1033, 1034, 1035, 1036, 1037, 1038, 1039, 1040, 1041, 1042, 1043, 1044, 1045, 1046, 1047, 1048, 1049, 1050, 1051, 1052, 1053,
1054, 1055, 1056, 1057, 1058, 1059, 1060, 1061, 1062, 1063, 1064, 1065, 1066, 1067, 1068, 1069, 1070, 1071, 1072, 1073, 1074, 1075, 1076, 1077, 1078,
1079, 1080, 1081, 1082, 1083, 1084, 1085, 1086, 1087, 1088, 1089, 1090, 1091, 1092, 1093, 1094, 1095, 1096, 1097, 1098, 1099, 1100, 1101, 1102, 1103,
1104, 1105, 1106, 1107, 1108, 1109, 1110, 1111, 1112, 1113, 1114, 1115, 1116, 1117, 1118, 1119, 1120, 1121, 1122, 1123, 1124, 1125, 1126, 1127, 1128,
1129, 1130, 1131, 1132, 1133, 1134, 1135, 1136, 1137, 1138, 1139, 1140, 1141, 1142, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 1150, 1151, 1152, 1153,
1154, 1155, 1156, 1157, 1158, 1159, 1160, 1161, 1162, 1163, 1164, 1165, 1166, 1167, 1168, 1169, 1170, 1171, 1172, 1173, 1174, 1175, 1176, 1177, 1178,
1179, 1180, 1181, 1182, 1183, 1184, 1185, 1186, 1187, 1188, 1189, 1190, 1191, 1192, 1193, 1195, 1196, 1197, 1198, 1199, 1200, 1201, 1202, 1203, 1204,
1205, 1206, 1207, 1208, 1209, 1210, 1211, 1212, 1213, 1214, 1215, 1216, 1217, 1218, 1219, 1220, 1221, 1222, 1223, 1224, 1225, 1226, 1227, 1228, 1229,
1230, 1231, 1232, 1233, 1234, 1235, 1236, 1237, 1238, 1239, 1240, 1241, 1242, 1243, 1244, 1245, 1246, 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254,
1255, 1256, 1257, 1258, 1274, 1275, 1276, 1277, 1278, 1279, 1280, 1281, 1282, 1283, 1284, 1285, 1286, 1287, 1288, 1289, 1292, 1293, 1294, 1296, 1299,
1300, 1301, 1302, 1303, 1304, 1305, 1306, 1307, 1308, 1309, 1310, 1311, 1312, 1313, 1314, 1315, 1316, 1317, 1318, 1319, 1320, 1321, 1322, 1323, 1324,
1325, 1326, 1327, 1328, 1329, 1330, 1331, 1332, 1333, 1334, 1335, 1336, 1337, 1338, 1339, 1340, 1341, 1342, 1343, 1344, 1345, 1346, 1347, 1348, 1349,
1350, 1351, 1352, 1353, 1354, 1355, 1356, 1357, 1358, 1359, 1360, 1361, 1362, 1368, 1371, 1372, 1373, 1374, 1375, 1376, 1377, 1378, 1379, 1380, 1381,
1382, 1383, 1384, 1385, 1386, 1387, 1388, 1389, 1390, 1391, 1392, 1393, 1394, 1395, 1396, 1397, 1398, 1399, 1400, 1401, 1402, 1403, 1404, 1405, 1406,
1407, 1408, 1409, 1410, 1411, 1412, 1413, 1414, 1415, 1416, 1417, 1418, 1419, 1420, 1421, 1422, 1423, 1424, 1425, 1426, 1427, 1428, 1429, 1430, 1431,
1432, 1433, 1434, 1435, 1436, 1437, 1438, 1439, 1440, 1441, 1442, 1443, 1444, 1445, 1446, 1447, 1448, 1449, 1450, 1451, 1452, 1453, 1454, 1455, 1456,
1457, 1458, 1459, 1460, 1461, 1462, 1463, 1464, 1465, 1466, 1467, 1468, 1469, 1470, 1471, 1472, 1473, 1474, 1475, 1476, 1477, 1478, 1479, 1480, 1481,
1482, 1483, 1484, 1485, 1486, 1487, 1488, 1489, 1490, 1491, 1492, 1493, 1494, 1495, 1496, 1497, 1498, 1499, 1500, 1501, 1502, 1503, 1504, 1505, 1506,
1507, 1508, 1509, 1510, 1511, 1512, 1513, 1514, 1515, 1516, 1517, 1518, 1519, 1520, 1521, 1522, 1523, 1524, 1525, 1526, 1527, 1528, 1529, 1530, 1531,
1532, 1533, 1534, 1535, 1536, 1537, 1538, 1539, 1540, 1541, 1542, 1543, 1544, 1545, 1546, 1547, 1548, 1549, 1550, 1551, 1552, 1553, 1554, 1555, 1556,
1557, 1558, 1559, 1560, 1561, 1562, 1563, 1564, 1565, 1566, 1567, 1568, 1569, 1570, 1571, 1572, 1573, 1574, 1575, 1576, 1577, 1578, 1579, 1580, 1581,
1582, 1583, 1584, 1585, 1586, 1587, 1588, 1589, 1590, 1591, 1592, 1593, 1594, 1595, 1596, 1597, 1598, 1599, 1600, 1601, 1602, 1603, 1604, 1605, 1606,
1607, 1608, 1609, 1610, 1611, 1612, 1613, 1614, 1615, 1616, 1617, 1618, 1619, 1620, 1621, 1622, 1623, 1624, 1625, 1626, 1627, 1628, 1629, 1630, 1631,
1632, 1633, 1634, 1635, 1636, 1637, 1638, 1639, 1640, 1641, 1678, 1679, 1680, 1681, 1682, 1683, 1684, 1685, 1686, 1687, 1688, 1689, 1690, 1691, 1692,
1693, 1694, 1695, 1696, 1697, 1698, 1699, 1700, 1701, 1702, 1703, 1704, 1705, 1706, 1707, 1708, 1709, 1710, 1711, 1712, 1713, 1714, 1715, 1716, 1717,
1718, 1719, 1720, 1721, 1722, 1723, 1724, 1725, 1726, 1727, 1728, 1729, 1730, 1731, 1732, 1733, 1734, 1735, 1736, 1737, 1738, 1739, 1740, 1741, 1742,
1743, 1744, 1745, 1746, 1747, 1748, 1749, 1750, 1751, 1752, 1753, 1754, 1755, 1756, 1757, 1758, 1759, 1760, 1761, 1762, 1763, 1764, 1765, 1766, 1767,
1768, 1769, 1770, 1771, 1772, 1773, 1774, 1775, 1776, 1777, 1778, 1779, 1780, 1781, 1782, 1783, 1784, 1785, 1786, 1787, 1788, 1789, 1790, 1791, 1792,
1793, 1794, 1795, 1796, 1797, 1798, 1799, 1800, 1801, 1802, 1803, 1804, 1805, 1806, 1868, 1869, 1870, 1871, 1872, 1873, 1874, 1875, 1876, 1877, 1878,
1879, 1880, 1881, 1882, 1883, 1884, 1885, 1886, 1887, 1888, 1889, 1890, 1891, 1892, 1893, 1894, 1895, 1896, 1897, 1898, 1899, 1900, 1901, 1902, 1903,
1904, 1905, 1906, 1907, 1908, 1909, 1910, 1911, 1912, 1913, 1914, 1915, 1916, 1917, 1918, 1919, 1920, 1921, 1922, 1923, 1924, 1925, 1926, 1927, 1928,
1929, 1930, 1931, 1932, 1933, 1934, 1935, 1936, 1937, 1938, 1939, 1940, 1941, 1942)
parent.old <- c(child, parent.new)

