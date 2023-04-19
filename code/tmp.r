    #########################
    ## votes with 2018 map ##
    #########################
    tmp <- data.frame(yr   = seq(from=1994, to=2021, by=3),
                      pan  = pand18[,i],
                      pri  = prid18[,i],
                      left = leftd18[,i],
                      oth  = othd18[,i],
                      efec = efecd18[,i])
    tmp <- rbind(v91=c(1991,NA,NA,NA,NA,NA), tmp) # add 1991 with no counterfactuals
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec[is.na(tmp$efec) | tmp$efec==0] <- 1
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="d") tmp$disn    <- v00$disn[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    # fill info to new list
    extendCoald18[[i]] <- tmp
    #



