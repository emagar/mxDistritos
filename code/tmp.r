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
