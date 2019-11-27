rm(list = ls())
                    
path <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ifeRedist2005/Documentos dat/"

########################################################
## Function to process all party proposals in state e ##
########################################################
get.pty.props <- function(estado = NA, escenario = NA){
    #edon <- 1; esc <- 1 #debug
    edon <- estado; esc <- escenario; 
    edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
    #
    # select/subset edon rows in dat for manipulation
    sel.dat <- which(dat$edon==edon)
    dat.tmp <- dat[sel.dat,] # subset dat rows to manipulate
    #
    # generate appropriate path for workdir
    if (esc==1)  dd <- paste(path, "e1/03obs-part-e1/", sep = "")
    if (esc==2)  dd <- paste(path, "e2/02obs-part-e2/", sep = "")
    setwd(dd)
    #
    # explore directory
    files.tmp <- list.files(edo, recursive = TRUE, pattern = "escenario.dat.ife")
    sel <- grep("eval|bk", files.tmp, ignore.case = TRUE)
    if (length(sel)>0) files.tmp <- files.tmp[-sel] # drop Eval from list if present
    if (length(files.tmp)==0) stop("No party proposals for state")
    #
    # read escenario 1 party proposal for state
    for (i in 1:length(files.tmp)){ # loop over available party proposals
        #i <- 1 # debug
        sel.file <- files.tmp[i];
        ptyn <- sub(pattern = "Observaci[oó]n([0-9]{2})/.+ife", replacement = "\\1", sel.file)
        #
        # determine party authoring the proposal
        if (ptyn=="01")  pty <- "pan"; 
        if (ptyn=="02")  pty <- "pri"; 
        if (ptyn=="03")  pty <- "prd"; 
        if (ptyn=="04")  pty <- "pt"; 
        if (ptyn=="05")  pty <- "pvem"; 
        if (ptyn=="06")  pty <- "conve";
        #
        # read data 
        tmp <- read.csv(file = paste(edo,sel.file,sep="/"), sep = "\t", header = FALSE)
        colnames(tmp) <- c("edon","prop","munn","seccion");
        #colnames(tmp)[2] <- paste(pty, "1", sep="");
        #
        tmp  <- within(tmp, ife <- edon*1000 + munn) # add ife municipio code
        #
        # column to manipulate
        target.col <- grep(paste(pty,esc,sep=""), colnames(dat.tmp))
        #
        # handle seccion==0 cases, ie. full municipios 
        sel.zero <- which(tmp$seccion==0) # select obs where district is reported for munn only
        if (length(sel.zero)>0) {
            for (m in sel.zero){
                #m <- sel.zero[2] # debug
                sel.m <- which(dat.tmp$ife==tmp$ife[m])   # indices with same municipio number as seccion==0 cases
                dat.tmp[sel.m,target.col] <- tmp$prop[m]  # plug district number to those indices
            }
        }
        #
        # handle seccion!=0 cases
        sel.zero <- which(tmp$seccion!=0) # select obs where seccion assigned 
        if (length(sel.zero)>0) {
            for (s in sel.zero){
                #s <- sel.zero[9] # debug
                sel.m <- which(dat.tmp$seccion==tmp$seccion[s])    # indices with same municipio number as seccion==0 cases
                dat.tmp[sel.m,target.col] <- tmp$prop[s]  # plug district number to those indices
            }
        }
    }
    # return manipulated data
    dat[sel.dat,] <- dat.tmp
    ## tmp2 <- dat.tmp[,target.col]
    ## table(tmp2==dat.tmp[,target.col], useNA = "always")
    return(dat)
}


# get master secciones file to pour new data in
eq <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv")
sel <- which(colnames(eq) %in% c("ord","edo","mun","edosecn","dis1994","dis1997","dis2000","dis2003","dis2009","dis2012","dis2013","dis2015","dis2018","OBSERVACIONES","action","fr.to","orig.dest","when","color","coment"))
eq <- eq[,-sel]
colnames(eq)[which(colnames(eq)=="dis2006")] <- "e3"
#
# rename
dat <- eq

#################
## escenario 1 ##
#################
e1 <- data.frame() # will receive data
#
for (e in 1:32){
    #e <- 1
    edon <- e # 1:32
    edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
    dd <- paste(path, "e1/01e1/", edo, sep = "")
    setwd(dd)
    #
    tmp <- read.csv(file = "escenario.dat.ife", sep = "\t", header = FALSE)
    colnames(tmp) <- c("edon","e1","munn","seccion")
    #
    e1 <- rbind(e1, tmp)
}
#
e1  <- within(e1, ife <- edon*1000 + munn) # add ife municipio code
#
# handle seccion!=0 cases
sel <- which(e1$seccion==0) # select obs where district is reported for munn only
dat <- merge(x = dat, y = e1[-sel,c("edon","seccion","e1")], by = c("edon","seccion"), all = TRUE)
# handle seccion==0 cases, ie. full municipios 
for (s in 1:length(sel)){
    #s <- 1 # debug
    sel.m <- which(dat$ife==e1$ife[sel[s]]) # indices with same municipio number as seccion==0 cases
    dat$e1[sel.m] <- e1$e1[sel[s]]          # plug district number to those indices
}
rm(e,e1,sel,sel.m,s,tmp,edo,edon)

#################################
## escenario 1 party proposals ##
#################################
# universe of party proposals in 2005
props <- c("pan", "pri", "prd", "pt", "pvem", "conve")
#
# ADD COLUMNS TO dat WITH ABOVE NAMES AND e1 INFO
tmp <- matrix(dat$e1, length(dat$e1), length(props)) # repeat e1 as many times as there are proposing parties
tmp <- as.data.frame(tmp)
colnames(tmp) <- paste(props,1,sep="") # rename proposal columns
dat <- cbind(dat, tmp) # paste to data

for (e in c(1:3,5:32)){ 
    message(sprintf("loop %s of %s", e, 32))
    #e <- 32 # 1:32
    dat <- get.pty.props(estado = e, escenario = 1)
}

dat[1,]
table(dat$e1==dat$pan)

#################
## escenario 2 ##
#################
e2 <- data.frame() # will receive data
#
for (e in 1:32){
    #e <- 1
    edon <- e # 1:32
    edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
    dd <- paste(path, "e2/01e2/", edo, "/Escenario/", sep = "")
    setwd(dd)
    #
    tmp <- read.csv(file = "escenario.dat.ife", sep = "\t", header = FALSE)
    colnames(tmp) <- c("edon","e2","munn","seccion")
    #
    e2 <- rbind(e2, tmp)
}
#
e2  <- within(e2, ife <- edon*1000 + munn) # add ife municipio code
#
# handle seccion!=0 cases
sel <- which(e2$seccion==0) # select obs where district is reported for munn only
dat <- merge(x = dat, y = e2[-sel,c("edon","seccion","e2")], by = c("edon","seccion"), all = TRUE)
# handle seccion==0 cases, ie. full municipios 
for (s in 1:length(sel)){
    #s <- 1 # debug
    sel.m <- which(dat$ife==e2$ife[sel[s]]) # indices with same municipio number as seccion==0 cases
    dat$e2[sel.m] <- e2$e2[sel[s]]          # plug district number to those indices
}
rm(e,e2,sel,sel.m,s,tmp,edo,edon)

dat[1,]

#################################
## escenario 2 party proposals ##
#################################
# ADD COLUMNS TO dat WITH ABOVE NAMES AND e1 INFO
tmp <- matrix(dat$e2, length(dat$e2), length(props)) # repeat e1 as many times as there are proposing parties
tmp <- as.data.frame(tmp)
colnames(tmp) <- paste(props,2,sep="") # rename proposal columns
dat <- cbind(dat, tmp) # paste to data

for (e in c(1:2,5,8:10,12:17,19,21:22,24:32)){
    message(sprintf("loop %s of %s", e, 32))
    #e <- 32 # 1:32
    dat <- get.pty.props(estado = e, escenario = 2)
}

dat[1,]
table(dat$e2==dat$pan2)

colnames(dat)[which(colnames(dat)=="e1")] <- "escenario1"
colnames(dat)[which(colnames(dat)=="e2")] <- "escenario2"
colnames(dat)[which(colnames(dat)=="e3")] <- "escenario3"

# move escenario 3 to end
sel <- which(colnames(dat)=="escenario3")
tmp <- dat[,sel]
dat <- dat[,-sel]
dat$escenario3 <- tmp
dat[1,]
dim(dat)

# export state by state
dd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/maps-with-all-proposals/2005/"
setwd(dd)
for (e in 1:32){
    edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[e]
    dat.tmp <- dat[which(dat$edon==e),]
    write.csv(dat.tmp, file = paste(edo, "Fed.csv", sep = ""), row.names = FALSE)
}




