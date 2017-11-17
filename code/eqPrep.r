###########################################################################
## Code is invoked from other scripts                                    ##
## It imports and prepares data frame mapping secciones to 1977, 1996,   ##
## and 2005 districts, and also two redistricting proposals made in 2013 ##
## ** version adapted from PolGeo replication files                      ##
##                                                                       ##
## Prepared by Eric Magar, 11/11/2017                                    ##
## email: emagar at gmail dot com                                        ##
###########################################################################
#
# START EQ PREP
#
# function to get mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#
# replace NAs with zero in district columns
eq[, grep(x = colnames(eq), pattern = "dis")][is.na(eq[,grep(x = colnames(eq), pattern = "dis")])==TRUE] <- 0 
#
#####################################################################################
## Fills which district new secciones would have belonged to before their creation ##
#####################################################################################
select <- which(eq$action=="new" & eq$fr.to=="from" & eq$orig.dest!="." & eq$orig.dest!=" " & eq$orig.dest!="" & is.na(eq$orig.dest)==FALSE) # new secciones that have info
#select <- which(eq$action=="new" & eq$fr.to=="from" & is.na(eq$orig.dest)==FALSE) # new secciones that have info
if (length(select)>0){
    tmp <- eq[select,]
    tmp$orig.dest <- as.numeric(as.character(tmp$orig.dest))
    tmp$pick.from <- NA
    for (i in 1:nrow(tmp)){
        message(sprintf("Record %s of %s", i, nrow(tmp)))
        tmp$pick.from[i] <- which(eq$edon==tmp$edon[i] & eq$seccion==tmp$orig.dest[i])
    }
    tmp$when <- as.numeric(as.character(tmp$when))
    #
    target.yrs <- unique(tmp$when)
    #
    for (y in 1:length(target.yrs)){
        #y <- 1 # debug
        cols <- colnames(tmp)[grep("dis", colnames(tmp))];
        yrs <- as.numeric(gsub(pattern = ".+([0-9]{4})", replacement = "\\1", cols));
                                        #
        cols <- cols[which(yrs<=target.yrs[y])]
        sel2 <- which(tmp$when==target.yrs[y])
        for (i in 1:length(cols)){
                                        #i <- 3 # debug
            if (Mode(eq[tmp$pick.from[sel2], cols[i]])==0) next # skip if no info (accounts for reseccionamientos same yr but /before/ state election, eg. coa2005)
            tmp[sel2, cols[i]] <- eq[tmp$pick.from[sel2], cols[i]]
        }
    }
    #
    tmp2 <- tmp
    tmp$pick.from <- NULL
    #
    eq[select,] <- tmp # paste back to dataset
}

##########################################################################
# Fill what districts split secciones would have belonged to afterwards ##
# Note: picks info from first of the new secciones only. If the new     ##
# secciones happened to be separated later on into two districts, info  ##
# will be lost.                                                         ##
##########################################################################
if (length(select)>0){
    tmp2$send.to <- tmp2$pick.from; tmp2$pick.from <- NULL
    #tmp2 <- tmp2[order(tmp2$send.to),]
    tmp2$drop <- 0; tmp3 <- tmp2$send.to; tmp3 <- c(NA, tmp3[1:(nrow(tmp2)-1)]); tmp3 <- tmp3 - tmp2$send.to; tmp3[tmp3!=0] <- 1; tmp3 <- 1 - tmp3; tmp2$drop[2:nrow(tmp2)] <- tmp3[-1]; tmp2 <- tmp2[tmp2$drop==0,] # drop repeated send.tos
    rm(tmp3); tmp2$drop <- NULL # clean
    tmp3 <- eq[tmp2$send.to,]
    #
    target.yrs <- unique(tmp3$when)
    #
    for (y in 1:length(target.yrs)){
    #y <- 1 # debug
        cols <- colnames(tmp3)[grep("dis", colnames(tmp))];
        yrs <- as.numeric(gsub(pattern = ".+([0-9]{4})", replacement = "\\1", cols));
        #
        cols <- cols[which(yrs>=target.yrs[y])]
        sel2 <- which(tmp3$when==target.yrs[y])
        for (i in 1:length(cols)){
            #i <- 3 # debug
            if (Mode(tmp2[sel2, cols[i]])==0) next # skip if no info (accounts for reseccionamientos same yr but /before/ state election, eg. coa2005)
            tmp3[sel2, cols[i]] <- tmp2[sel2, cols[i]];
        }
    }
    eq[tmp2$send.to,] <- tmp3 # paste back to dataset
}

#############################################################################
## Fills which district merged secciones would have belonged to afterwards ##
#############################################################################
select <- which(eq$action=="merged" & eq$fr.to=="to" & is.na(eq$orig.dest)==FALSE & eq$when<2014) # new secciones that have info
if (length(select)>0){
    tmp <- eq[select,]
    #
    tmp$orig.dest <- as.numeric(as.character(tmp$orig.dest))
    tmp$pick.from <- NA
    for (i in 1:nrow(tmp)){
        #i <- 1 #debug
        tmp$pick.from[i] <- which(eq$edon==tmp$edon[i] & eq$seccion==tmp$orig.dest[i])
    }
    tmp$when <- as.numeric(as.character(tmp$when))
    #
    # when new "merged to" info appears, will need to check new "when" years and modify next block accordingly
    target.yrs <- unique(tmp$when)
    #
    for (y in 1:length(target.yrs)){
        #y <- 1 # debug
        cols <- colnames(tmp)[grep("dis", colnames(tmp))];
        yrs <- as.numeric(gsub(pattern = ".+([0-9]{4})", replacement = "\\1", cols));
                                        #
        cols <- cols[which(yrs>=target.yrs[y])]
        sel2 <- which(tmp$when==target.yrs[y])
        for (i in 1:length(cols)){
            #i <- 3 # debug
            if (Mode(eq[tmp$pick.from[sel2], cols[i]])==0) next # skip if no info (accounts for reseccionamientos same yr but /before/ state election, eg. coa2005)
            tmp[sel2, cols[i]] <- eq[tmp$pick.from[sel2], cols[i]]
        }
    }
    #tmp
    tmp$pick.from <- NULL
    eq[select,] <- tmp # paste back to dataset
}
#
# END EQ PREP
