####################################################
## THIS SCRIPT ESTIMATES MANIPULATED REGRESSIONS  ##
## FOR THE SECOND SPLIT OF TWICE SPLIT MUNICIPIOS ##
## IT GENERATES AND SAVES manip2 OBJECTS THAT     ##
## ARE READ INTO elec-data-for-maps.r             ##
####################################################
#
# prepare data
sel <- which(v91m$ife %in% c(12013,12081))
tmp <- v91m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v91 <- tmp
#
sel <- which(v94m$ife %in% c(12013,12081))
tmp <- v94m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v94 <- tmp
#
sel <- which(v97m$ife %in% c(12013,12081))
tmp <- v97m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v97 <- tmp
#
sel <- which(v00m$ife %in% c(12013,12081))
tmp <- v00m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v00 <- tmp
#
sel <- which(v03m$ife %in% c(12013,12081))
tmp <- v03m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v03 <- tmp
#
sel <- which(v06m$ife %in% c(12013,12081))
tmp <- v06m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v06 <- tmp
#
sel <- which(v09m$ife %in% c(12013,12081))
tmp <- v09m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v09 <- tmp
#
sel <- which(v12m$ife %in% c(12013,12081))
tmp <- v12m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v12 <- tmp
#
sel <- which(v15m$ife %in% c(12013,12081))
tmp <- v15m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v15 <- tmp
#
sel <- which(v18m$ife %in% c(12013,12081))
tmp <- v18m[sel,]
sel.c <- which(colnames(tmp) %in% c("edon","ife","inegi"))
tmp[is.na(tmp)] <- 0
tmp[1,-sel.c] <- colSums(tmp[,-sel.c]); tmp <- tmp[1,]; tmp$ife <- 120130 
v18 <- tmp

###########################################
## prepare manipulated party objects     ##
## for time-series and alpha regressions ##
###########################################
#
# version 1: extend partial coalitions across the board
# shares
pan <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$pan  / v91$efec),
                  v94 = ifelse(v94$efec==0, NA,  v94$pan  / v94$efec),
                  v97 = ifelse(v97$efec==0, NA,  v97$pan  / v97$efec),
                  v00 = ifelse(v00$efec==0, NA,  v00$panc / v00$efec),
                  v03 = ifelse(v03$efec==0, NA,  v03$pan  / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pan  / v06$efec),
                  v09 = ifelse(v09$efec==0, NA,  v09$pan  / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pan  / v12$efec),
                  v15 = ifelse(v15$efec==0, NA,  v15$pan  / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pan + v18$panc + v18$prd + v18$mc) / v18$efec))
pan <- round(pan, 3)
#
pri <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$pri  / v91$efec),
                  v94 = ifelse(v94$efec==0, NA,  v94$pri  / v94$efec),
                  v97 = ifelse(v97$efec==0, NA,  v97$pri  / v97$efec),
                  v00 = ifelse(v00$efec==0, NA,  v00$pri / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$pri + v03$pric + v03$pvem) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA,  v06$pric / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pri + v09$pric + v09$pvem) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA, (v12$pri + v12$pric + v12$pvem) / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$pri + v15$pric + v15$pvem) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$pri + v18$pric + v18$pvem + v18$pna) / v18$efec))
pri <- round(pri, 3)
#
left <- data.frame(v91 = ifelse(v91$efec==0, NA,  v91$prd  / v91$efec),
                     v94 = ifelse(v94$efec==0, NA,  v94$prd  / v94$efec),
                     v97 = ifelse(v97$efec==0, NA,  v97$prd  / v97$efec),
                     v00 = ifelse(v00$efec==0, NA,  v00$prdc / v00$efec),
                     v03 = ifelse(v03$efec==0, NA, (v03$prd + v03$pt + v03$conve) / v03$efec),
                     v06 = ifelse(v06$efec==0, NA,  v06$prdc / v06$efec),
                     v09 = ifelse(v09$efec==0, NA, (v09$prd + v09$pt + v09$ptc + v09$conve) / v09$efec),
                     v12 = ifelse(v12$efec==0, NA, (v12$prd + v12$prdc + v12$pt + v12$mc)  / v12$efec),
                     v15 = ifelse(v15$efec==0, NA, (v15$prd + v15$prdc + v15$pt + v15$morena + v15$pes) / v15$efec),
                     v18 = ifelse(v18$efec==0, NA, (v18$morena + v18$morenac + v18$pt + v18$pes) / v18$efec))
left <- round(left, 3)
#
oth <- data.frame(v91 = ifelse(v91$efec==0, NA, (v91$parm + v91$pdm + v91$pfcrn + v91$pps + v91$pem + v91$prt) / v91$efec),
                  v94 = ifelse(v94$efec==0, NA, (v94$pps + v94$pfcrn + v94$parm + v94$uno.pdm + v94$pt + v94$pvem) / v94$efec),
                  v97 = ifelse(v97$efec==0, NA, (v97$pc + v97$pt + v97$pvem + v97$pps + v97$pdm) / v97$efec),
                  v00 = ifelse(v00$efec==0, NA, (v00$pcd + v00$parm + v00$dsppn) / v00$efec),
                  v03 = ifelse(v03$efec==0, NA, (v03$psn + v03$pas + v03$mp + v03$plm + v03$fc) / v03$efec),
                  v06 = ifelse(v06$efec==0, NA, (v06$pna + v06$asdc) / v06$efec),
                  v09 = ifelse(v09$efec==0, NA, (v09$pna + v09$psd) / v09$efec),
                  v12 = ifelse(v12$efec==0, NA,  v12$pna / v12$efec),
                  v15 = ifelse(v15$efec==0, NA, (v15$mc + v15$pna + v15$ph + v15$indep1 + v15$indep2) / v15$efec),
                  v18 = ifelse(v18$efec==0, NA, (v18$indep1 + v18$indep2) / v18$efec))
oth <- round(oth, 3)
#
efec <- data.frame(v91 = v91$efec,
                   v94 = v94$efec,
                   v97 = v97$efec,
                   v00 = v00$efec,
                   v03 = v03$efec,
                   v06 = v06$efec,
                   v09 = v09$efec,
                   v12 = v12$efec,
                   v15 = v15$efec,
                   v18 = v18$efec)
#
# transpose to plug columns into new data.frames
pan <- t(pan)
pri <- t(pri)
left <- t(left)
oth <- t(oth)
efec <- t(efec)
#
extendCoalmanip2 <- as.list(rep(NA, nrow(v00))) # empty list will receive one data.frame per municipio
# loop over municipios/secciones
for (i in 1:nrow(v00)){
    #i <- 1 # debug
    tmp <- data.frame(yr = seq(from=1991, to=2018, by=3),
                      pan = pan[,i],
                      pri = pri[,i],
                      left = left[,i],
                      oth = oth[,i],
                      efec = efec[,i])
    # replace NAs with period's mean
    #if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
#        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
#        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
#        tmp$left[is.na(tmp$left)] <- per.means["left"];
#        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
#    }
    # add epsilon = 2*max(rounding error) to zeroes 
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    # fill info to new list
    extendCoalmanip2[[i]] <- tmp
}
# datos para regresión de alfa
# son los mismos que para las regresiones con v..m
# [... skipped many lines of code here...]
# plug into data
for (i in 1:nrow(v00)){
    #i <- 2 # debug
    extendCoalmanip2[[i]] <- cbind(extendCoalmanip2[[i]], yr.means[,6:8])
}
#
###############################
## código de las regresiones ##
###############################
vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
        data.frame(pan    = rep(NA, nrow(v00)),
                   pri  = rep(NA, nrow(v00)),
                   left = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       left   = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      left   = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                                       # regression per municipio, each used to
                                                       # predict votes in 2015 and 2018 
#
regs.2006manip2 <- regs.2009manip2 <- regs.2012manip2 <- regs.2015manip2 <- regs.2018manip2 <-
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regsmanip2 <- list(pan    = tmp,
                       left   = tmp,
                       oth    = tmp,
                       readme = "No pri regs bec DVs are pri-ratios")
#
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoalmanip2, sum)
non.nas <- unlist(non.nas)
non.nas <- which(is.na(non.nas)==FALSE)
tail(non.nas)
#    
for (i in non.nas){
    message(sprintf("loop %s of %s", i, max(non.nas)))
    # subset data
    data.tmp <- extendCoalmanip2[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA,data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA,data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA,data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ##################################
    ## predict 2006 with last 5 els ## ojo: v91 needed
    ##################################
    year <- 2006
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2006[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006manip2$pan[[i]]    <- reg.pan
    regs.2006manip2$left[[i]]   <- reg.left
    regs.2006manip2$oth[[i]]    <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    year <- 2009
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2009[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2009manip2$pan[[i]]    <- reg.pan
    regs.2009manip2$left[[i]]   <- reg.left
    regs.2009manip2$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    year <- 2012
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-   lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2012[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    regs.2012manip2$pan[[i]]    <- reg.pan
    regs.2012manip2$left[[i]]   <- reg.left  
    regs.2012manip2$oth[[i]]    <- reg.oth
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left  
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left  
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left   <- lm(formula = log(left  /pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left  )$coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2015[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    regs.2015manip2$pan[[i]]    <- reg.pan
    regs.2015manip2$left[[i]] <- reg.left  
    regs.2015manip2$oth[[i]]    <- reg.oth
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left  
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left  
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left   <- lm(formula = log(left  /pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left  )$coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2018[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    regs.2018manip2$pan   [[i]] <- reg.pan
    regs.2018manip2$left  [[i]] <- reg.left  
    regs.2018manip2$oth   [[i]] <- reg.oth
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left  
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left  
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)    ~ mean.rpan, data = data.tmp)
    reg.left   <- lm(formula = log(left  /pri) ~ mean.rleft  , data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)    ~ mean.roth, data = data.tmp)
    #
    # point prediction alpha with mean at zero 
    new.d <- data.frame(mean.rpan = 0)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    new.d <- data.frame(mean.rleft   = 0)
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    new.d <- data.frame(mean.roth = 0)
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    #
    #c(vhat.pan, vhat.pri, vhat.left, 1-vhat.pan-vhat.pri-vhat.left)
    alphahat[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    betahat[i,1] <- coef(reg.pan)   [2]
    betahat[i,2] <- coef(reg.left  )[2]
    betahat[i,3] <- coef(reg.oth)   [2]
    mean.regsmanip2$pan   [[i]] <- reg.pan
    mean.regsmanip2$left  [[i]] <- reg.left  
    mean.regsmanip2$oth   [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left   <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.left   <- alphahat$left  [i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.left   <- betahat$left  [i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
    data.tmp <- round(data.tmp,3)
    #
    ########################################################
    ## optional: plug vhats alphas betas back into data   ##
    ########################################################
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        oth <- NULL; # drop compositional vote complement
        betahat.oth <- NULL; # drop this beta
        #betahat.pan <- betahat.left   <- betahat.oth <- NULL; # drop betas
    })
    extendCoalmanip2[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################

# clean, all this is saved in extendCoal, mean.regs, regs.2006, regs.2009, regs.2012, regs.2015, regs.2018
rm(alphahat, betahat, bhat.left, bhat.pan, reg.left, reg.oth, reg.pan, rhat.left, rhat.oth, rhat.pan, vhat.2006, vhat.2009, vhat.2012, vhat.2015, vhat.2018, vhat.left, vhat.pan, vhat.pri)
# more cleaning
rm(v91,v94,v97,v00,v03,v06,v09,v12,v15,v18)
rm(pan,pri,left,oth,efec)
rm(i,new.d,non.nas,per.means,year)
rm(yr.means)
rm(tmp,data.tmp)

# save fix to import into elec-data-for-maps.r
save(extendCoalmanip2, regs.2006manip2, regs.2009manip2, regs.2012manip2, regs.2015manip2, regs.2018manip2,
     mean.regsmanip2, file = paste(wd, "data/regs-to-fix-twice-split-muns.RData", sep = "/"))

#######################





