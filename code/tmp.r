#####################################################################
## Manipulate reseccionamiento cases to preserve them in analysis. ##
## Note: will not affect municipal aggregates, done above.         ##
## Note 16jul2021: why not do this before manipulating municipios, ##
## which now have been manipulated in ife.1991, ife.1994 etc.?     ##
#####################################################################

# get equivalencias seccionales again (avoids re-running all code above after excel-eq suffered maginal changes) 
tmp <- paste(wd, "equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", sep = "")
eq <- read.csv(tmp, stringsAsFactors = FALSE)
rm(tmp)

# rewrite eq$orig.dest as vectors
sel <- grep("[|]", eq$orig.dest)
eq$orig.dest[sel] <- gsub("[|]",",", eq$orig.dest[sel])
eq$orig.dest[sel] <- paste0("c(", eq$orig.dest[sel], ")")
sel <- grep("[|]", eq$orig.dest2)
eq$orig.dest2[sel] <- gsub("[|]",",", eq$orig.dest2[sel])
eq$orig.dest2[sel] <- paste0("c(", eq$orig.dest2[sel], ")")
sel <- grep("[|]", eq$orig.dest3)
eq$orig.dest3[sel] <- gsub("[|]",",", eq$orig.dest3[sel])
eq$orig.dest3[sel] <- paste0("c(", eq$orig.dest3[sel], ")")

#######################
## clean eq elements ##
#######################
# should verify whether or not modified secciones' dis1979 dis1997 dis2006 dis2018 have consistent 0s before/after action's when...

##############################################
## add missing counterfactual fed districts ##
##############################################
################
## split.from ##
################
# action 3 proceeds backwards
sel <- which(eq$action3=="split.from")
if (length(sel)>0){
    eqs <- eq[sel,] # subset for manipulation
    orig.dests <- eq$orig.dest3[sel] # extract
    # in case of several seccions of origin, next retains first only (assumes all secciones are from same district) 
    orig.dests <- lapply(orig.dests, function(x) eval(parse(text=x))[1])
    orig.dests <- unlist(orig.dests) # vectorize
    whens <- eq$when3[sel]
    #
    for (i in 1:length(whens)){
        #i <- 2  # debug
        #eqs[i,] # debug
        #eqs[,c("ord","dis1979","dis1997","dis2006","dis2018")] # debug
        or.de <- orig.dests[i]
        target <- which(eq$edon==eqs$edon[i] & eq$seccion==or.de)
        if (whens[i] > 1991 & eqs$dis1979[i]==0){
            eqs$dis1979[i] <- eq$dis1979[target] # assign target's dis1979 to split seccion
        }
        if (whens[i] > 1997 & eqs$dis1997[i]==0){
            eqs$dis1997[i] <- eq$dis1997[target] # assign target's dis1997 to split seccion
        }
        if (whens[i] > 2006 & eqs$dis2006[i]==0){
            eqs$dis2006[i] <- eq$dis2006[target] # assign target's dis2006 to split seccion
        }
        if (whens[i] > 2018 & eqs$dis2018[i]==0){
            eqs$dis2018[i] <- eq$dis2018[target] # assign target's dis2018 to split seccion
        }
    }
    #eqs[i,] # debug
    eq[sel,] <- eqs # return to data
}
# action 2
sel <- which(eq$action2=="split.from")
if (length(sel)>0){
    eqs <- eq[sel,] # subset for manipulation
    orig.dests <- eq$orig.dest2[sel] # extract secciones of origin
    # next makes numeric list; if multi-secciones, keeps 1st only (assumes all multi-secciones from same district) 
    orig.dests <- lapply(orig.dests, function(x) eval(parse(text=x))[1])
    orig.dests <- unlist(orig.dests) # re-vectorize
    whens <- eq$when2[sel]           # extract reseccionamiento's year
    #
    for (i in 1:length(whens)){
        #i <- 2  # debug
        #eqs[i,] # debug
        #eqs[,c("ord","dis1979","dis1997","dis2006","dis2018")] # debug
        or.de <- orig.dests[i]
        target <- which(eq$edon==eqs$edon[i] & eq$seccion==or.de)
        if (whens[i] > 1991 & eqs$dis1979[i]==0){
            eqs$dis1979[i] <- eq$dis1979[target] # assign target's dis1979 to split seccion
        }
        if (whens[i] > 1997 & eqs$dis1997[i]==0){
            eqs$dis1997[i] <- eq$dis1997[target] # assign target's dis1997 to split seccion
        }
        if (whens[i] > 2006 & eqs$dis2006[i]==0){
            eqs$dis2006[i] <- eq$dis2006[target] # assign target's dis2006 to split seccion
        }
        if (whens[i] > 2018 & eqs$dis2018[i]==0){
            eqs$dis2018[i] <- eq$dis2018[target] # assign target's dis2018 to split seccion
        }
    }
    #eqs[i,] # debug
    eq[sel,] <- eqs # return to data
}
# action 1
sel <- which(eq$action=="split.from")
if (length(sel)>0){
    eqs <- eq[sel,] # subset for manipulation
    orig.dests <- eq$orig.dest[sel] # extract
    # in case of several seccions of origin, next retains first only (assumes all secciones are from same district) 
    orig.dests <- lapply(orig.dests, function(x) eval(parse(text=x))[1])
    orig.dests <- unlist(orig.dests) # vectorize
    whens <- eq$when[sel]
    #
    for (i in 1:length(whens)){
        #i <- 2  # debug
        #eqs[i,] # debug
        #eqs[,c("ord","dis1979","dis1997","dis2006","dis2018")] # debug
        or.de <- orig.dests[i]
        target <- which(eq$edon==eqs$edon[i] & eq$seccion==or.de)
        if (whens[i] > 1991 & eqs$dis1979[i]==0){
            eqs$dis1979[i] <- eq$dis1979[target] # assign target's dis1979 to split seccion
        }
        if (whens[i] > 1997 & eqs$dis1997[i]==0){
            eqs$dis1997[i] <- eq$dis1997[target] # assign target's dis1997 to split seccion
        }
        if (whens[i] > 2006 & eqs$dis2006[i]==0){
            eqs$dis2006[i] <- eq$dis2006[target] # assign target's dis2006 to split seccion
        }
        if (whens[i] > 2018 & eqs$dis2018[i]==0){
            eqs$dis2018[i] <- eq$dis2018[target] # assign target's dis2018 to split seccion
        }
    }
    #eqs[i,] # debug
    eq[sel,] <- eqs # return to data
}

##############
## split.to ##
##############
# action proceeds forward
sel <- which(eq$action=="split.to" | eq$action=="merged.to")
if (length(sel)>0){
    eqs <- eq[sel,] # subset for manipulation
    orig.dests <- eq$orig.dest[sel] # extract
    # in case of several seccions of origin, next retains first only (assumes all secciones are from same district) 
    orig.dests <- lapply(orig.dests, function(x) eval(parse(text=x))[1])
    orig.dests <- unlist(orig.dests) # vectorize
    whens <- eq$when[sel]
    #
    for (i in 1:length(whens)){
        #i <- 829  # debug
        #eqs[i,] # debug
        #eqs[,c("ord","dis1979","dis1997","dis2006","dis2018")] # debug
        or.de <- orig.dests[i]
        target <- which(eq$edon==eqs$edon[i] & eq$seccion==or.de)
        if (whens[i] < 1991 & eqs$dis1979[i]==0){
            eqs$dis1979[i] <- eq$dis1979[target] # assign target's dis1979 to split seccion
        }
        if (whens[i] < 1997 & eqs$dis1997[i]==0){
            eqs$dis1997[i] <- eq$dis1997[target] # assign target's dis1997 to split seccion
        }
        if (whens[i] < 2006 & eqs$dis2006[i]==0){
            eqs$dis2006[i] <- eq$dis2006[target] # assign target's dis2006 to split seccion
        }
        if (whens[i] < 2018 & eqs$dis2018[i]==0){
            eqs$dis2018[i] <- eq$dis2018[target] # assign target's dis2018 to split seccion
        }
        #eqs[i,] # debug
    }
    eq[sel,] <- eqs # return to data
}
# action 2
sel <- which(eq$action2=="split.to" | eq$action2=="merged.to")
if (length(sel)>0){
    eqs <- eq[sel,] # subset for manipulation
    orig.dests <- eq$orig.dest2[sel] # extract
    # in case of several seccions of origin, next retains first only (assumes all secciones are from same district) 
    orig.dests <- lapply(orig.dests, function(x) eval(parse(text=x))[1])
    orig.dests <- unlist(orig.dests) # vectorize
    whens <- eq$when2[sel]
    #
    for (i in 1:length(whens)){
        #i <- 829  # debug
        #eqs[i,] # debug
        #eqs[,c("ord","dis1979","dis1997","dis2006","dis2018")] # debug
        or.de <- orig.dests[i]
        target <- which(eq$edon==eqs$edon[i] & eq$seccion==or.de)
        if (whens[i] < 1991 & eqs$dis1979[i]==0){
            eqs$dis1979[i] <- eq$dis1979[target] # assign target's dis1979 to split seccion
        }
        if (whens[i] < 1997 & eqs$dis1997[i]==0){
            eqs$dis1997[i] <- eq$dis1997[target] # assign target's dis1997 to split seccion
        }
        if (whens[i] < 2006 & eqs$dis2006[i]==0){
            eqs$dis2006[i] <- eq$dis2006[target] # assign target's dis2006 to split seccion
        }
        if (whens[i] < 2018 & eqs$dis2018[i]==0){
            eqs$dis2018[i] <- eq$dis2018[target] # assign target's dis2018 to split seccion
        }
        #eqs[i,] # debug
    }
    eq[sel,] <- eqs # return to data
}
# action 3
sel <- which(eq$action3=="split.to" | eq$action3=="merged.to")
if (length(sel)>0){
    eqs <- eq[sel,] # subset for manipulation
    orig.dests <- eq$orig.dest3[sel] # extract
    # in case of several seccions of origin, next retains first only (assumes all secciones are from same district) 
    orig.dests <- lapply(orig.dests, function(x) eval(parse(text=x))[1])
    orig.dests <- unlist(orig.dests) # vectorize
    whens <- eq$when3[sel]
    #
    for (i in 1:length(whens)){
        #i <- 829  # debug
        #eqs[i,] # debug
        #eqs[,c("ord","dis1979","dis1997","dis2006","dis2018")] # debug
        or.de <- orig.dests[i]
        target <- which(eq$edon==eqs$edon[i] & eq$seccion==or.de)
        if (whens[i] < 1991 & eqs$dis1979[i]==0){
            eqs$dis1979[i] <- eq$dis1979[target] # assign target's dis1979 to split seccion
        }
        if (whens[i] < 1997 & eqs$dis1997[i]==0){
            eqs$dis1997[i] <- eq$dis1997[target] # assign target's dis1997 to split seccion
        }
        if (whens[i] < 2006 & eqs$dis2006[i]==0){
            eqs$dis2006[i] <- eq$dis2006[target] # assign target's dis2006 to split seccion
        }
        if (whens[i] < 2018 & eqs$dis2018[i]==0){
            eqs$dis2018[i] <- eq$dis2018[target] # assign target's dis2018 to split seccion
        }
        #eqs[i,] # debug
    }
    eq[sel,] <- eqs # return to data
}

eq[1,]
# export manipulated districts for manual comaprison in excel
write.csv(eq[,c("ord","dis1979","dis1997","dis2006","dis2018")], file="../../../redistrict/ife.ine/equivSecc/tmp.csv", row.names=FALSE)
rm(d,sel,sel.c,sel.drop,whens,eqs,or.de,target,i) # clean


