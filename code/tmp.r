
FUNCTION TO CONVERT IFE TO INEGI MUNICIPAL CODES AND VICE VERSA
Input is a vector
Output is a same-length same-order vector
Problem: values not in i2i map remain unchanged (good) but mixed
among changed values (bad); not a problem if all values are present.

Usage:
    inegi.ife(3008, inegi_to_ife=TRUE)
    > 3004
    inegi.ife(3008, inegi_to_ife=TRUE)


inegi.ife <- function(v=NA, inegi_to_ife=TRUE, ife_to_inegi=FALSE){
    require(plyr)
    if (inegi_to_ife==ife_to_inegi) stop("Error: inegi_to_ife and ife_to_inegi can't be simultaneously TRUE or FALSE")
    if (is.vector(v)==FALSE) stop("Error: v must be a vector")
    ##
    ## THE SOURCE MAP IS THIS:
    tmp <- "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/ancillary/mun.yrs.csv"
    i2i <- read.csv(file=tmp)
    i2i <- i2i[,c("inegi","ife","mun","edon")] # drop unneded columns
    ## Drop municipios with codes pending from map
    ##table(is.na(i2i$ife))           # debug
    ##table(is.na(i2i$inegi))         # debug
    drop.r <- which(is.na(i2i$ife))
    ##i2i[drop.r,]                    # debug
    if (length(drop.r)>0) i2i <- i2i[-drop.r,]
                                        #
    #v <- d$ife # debug
        zdata <- data.frame(inp=v)
        zdata$ord <- 1:nrow(zdata) # keep original order
    if (inegi_to_ife==TRUE){
        zdata$outp <-     mapvalues(zdata$inp, from = i2i$inegi, to = i2i$ife,   warn_missing=FALSE)
    }
    if (ife_to_inegi==TRUE){
        zdata$outp <- mapvalues(zdata$inp, from = i2i$ife,   to = i2i$inegi, warn_missing=FALSE)
    }
    zdata <- zdata[order(zdata$ord),] # re-sort into original order 
    outp <- zdata$outp
    return(outp)
}

inegi.ife(3008)

ls()

v94$inegi <- mapvalues(v94$ife, from = ife.inegi$ife, to = ife.inegi$inegi, warn_missing=FALSE)



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



if (sel.map==1979){
    tmp <- extendCoald79                                                       # duplicate for manipulation
    add1988 <- function(x) rbind(v88=c(1988,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), x) # add empty row to each data frame in list
    tmp <- lapply(tmp, add1988)                                                # add row for 1988 to each data frame in list
    for (i in 1:300){
        #i <- 1 #debug
        sel.r <- which(tmp.1988$disn==as.numeric(names(tmp)[i])) # pick row matching disn
        tmp[[i]][1,c("pan","pri","left","oth","efec","lisnom","disn")] <-
            tmp.1988[sel.r,c("pan","pri","left","oth","efec","lisnom","disn")] # fill 1st row w sel.row cols
    }

    extendCoald79 <- tmp
    rm(add1988,tmp,tmp.1988)
}





# compute national mean vote
yr.means$pan   [1] <-  cs(v91s)["pan"]                                                         / cs(v91s)["efec"]
yr.means$pri   [1] <-  cs(v91s)["pri"]                                                         / cs(v91s)["efec"]
yr.means$left  [1] <-  cs(v91s)["prd"]                                                         / cs(v91s)["efec"]
yr.means$oth   [1] <- (cs(v91s)["efec"] - cs(v91s)["pan"] - cs(v91s)["pri"] - cs(v91s)["prd"]) / cs(v91s)["efec"]
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
yr.means$pri   [11] <-  cs(v21s)["pri"]                                                                                   / cs(v21s)["efec"] # dropped cs(v21s)["pric"]
yr.means$left  [11] <- (cs(v21s)["morena"] + cs(v21s)["morenac"] + cs(v21s)["pt"]  + cs(v21s)["pvem"])                    / cs(v21s)["efec"]
yr.means$oth   [11] <- (cs(v21s)["mc"]     + cs(v21s)["pes"]     + cs(v21s)["rsp"] + cs(v21s)["fxm"] + cs(v21s)["indep"]) / cs(v21s)["efec"]
