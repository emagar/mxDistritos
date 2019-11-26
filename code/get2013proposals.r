rm(list = ls())
                    
path <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ifeRedist2013/"

########################################################
## Function to process all party proposals in state e ##
########################################################
get.pty.props <- function(estado = NA){
    #edon <- 2  #debug
    edon <- estado
    edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
    #dd1   <- paste(path, "1erEscenario2013/", edo, sep = "")
    dd1pp <- paste(path, "1erEscenarioComentariosPartidos/", edo, "/Escenarios/", sep = "")
    #dd2   <- paste(path, "2oEscenario/", edo, sep = "")
    #dd2pp <- paste(path, "2oEscenarioComentariosPartidos/", edo, sep = "")
    #dd3   <- paste(path, "3erEscenario/",      sep = "")
    setwd(dd1pp)
    #
    # explore directory
    files.tmp <- list.files()
    sel <- grep("tmp", files.tmp)
    if (length(sel)>0) files.tmp <- files.tmp[-sel] # drop tmp from list if present
    #
    # select/subset edon rows in dat for manipulation
    sel.dat <- which(dat$edon==edon)
    dat.tmp <- dat[sel.dat,] # subset dat rows to manipulate
    #
    for (i in 1:length(files.tmp)){ # loop over available party proposals
        #i <- 4 # debug
        sel.file <- files.tmp[i];
        ptyn <- sub(pattern = "[A-Za-z_áéíóú]+_[0-9]{1}_([0-9]+)_[0-9]+.zip", replacement = "\\1", sel.file)
        #
        # determine party authoring the proposal
        if (ptyn=="19")  next # pty <- "e"; # DIRECCION DE CARTOGRAFIA (DCE) --- original escenario
        if (ptyn=="2")   next # pty <- "eloc.ignore"; # CLV CARGADO POR EL PRESIDENTE DE LA MISMA
        if (ptyn=="32")  pty <- "pan.clv"; # PAN ANTE LA CLV
        if (ptyn=="33")  pty <- "pri.clv"; # PRI ANTE LA CLV
        if (ptyn=="34")  pty <- "prd.clv"; # PRD ANTE LA CLV
        if (ptyn=="35")  pty <- "pt.clv"; # PT ANTE LA CLV
        if (ptyn=="36")  pty <- "pvem.clv"; # PVEM ANTE LA CLV
        if (ptyn=="37")  pty <- "mc.clv"; # MC ANTE LA CLV
        if (ptyn=="38")  pty <- "pna.clv"; # NA ANTE LA CLV
        if (ptyn=="42")  pty <- "pan.cnv"; # PAN ANTE LA CNV
        if (ptyn=="43")  pty <- "pri.cnv"; # PRI ANTE LA CNV
        if (ptyn=="44")  pty <- "prd.cnv"; # PRD ANTE LA CNV
        if (ptyn=="45")  pty <- "pt.cnv"; # PT ANTE LA CNV
        if (ptyn=="46")  pty <- "pvem.cnv"; # PVEM ANTE LA CNV
        if (ptyn=="47")  pty <- "mc.cnv"; # MC ANTE LA CNV
        if (ptyn=="48")  pty <- "pna.cnv"; # NA ANTE LA CNV
        #
        # unzip relevant data into tmp/ folder
        unzip(sel.file, exdir = "tmp")
        #name.col <- unzip(sel.file, list = TRUE)$Name # pick unzipped file's name for dataframe's column
        #
        # read data 
        tmp <- read.csv(file = "tmp/escenario.dat.ife", sep = "\t", header = FALSE)
        colnames(tmp) <- c("edon","prop","munn","seccion");
        #colnames(tmp)[2] <- paste(pty, "1", sep="");
        #
        tmp  <- within(tmp, ife <- edon*1000 + munn) # add ife municipio code
        #head(tmp); # debug
        #
        # column to manipulate
        target.col <- grep(pty, colnames(dat.tmp))
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
sel <- which(colnames(eq) %in% c("ord","edo","mun","edosecn","dis1994","dis1997","dis2000","dis2003","dis2006","dis2009","dis2012","dis2015","dis2018","OBSERVACIONES","action","fr.to","orig.dest","when","color","coment"))
eq <- eq[,-sel]
colnames(eq)[which(colnames(eq)=="dis2013")] <- "e3"
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
    dd1   <- paste(path, "1erEscenario2013/", edo, sep = "")
    ## dd1pp <- paste(path, "1erEscenarioComentariosPartidos/", edo, sep = "")
    ## dd2   <- paste(path, "2oEscenario/", edo, sep = "")
    ## dd2pp <- paste(path, "2oEscenarioComentariosPartidos/", edo, sep = "")
    ## dd3   <- paste(path, "3erEscenario/",      sep = "")
    #
    setwd(dd1)
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
# universe of party proposals in 2013
props <- c("pan.clv", "pri.clv", "prd.clv", "pt.clv", "pvem.clv", "mc.clv", "pna.clv", "pan.cnv", "pri.cnv", "prd.cnv", "pt.cnv", "pvem.cnv", "mc.cnv", "pna.cnv")
#
# ADD COLUMNS TO dat WITH ABOVE NAMES AND e1 INFO
tmp <- matrix(dat$e1, length(dat$e1), length(props)) # repeat e1 as many times as there are proposing parties
tmp <- as.data.frame(tmp)
colnames(tmp) <- paste(props,1,sep="") # rename proposal columns
dat <- cbind(dat, tmp) # paste to data

dat <- get.pty.props(estado=2)

for (e in 1:32){
    message(sprintf("loop %s of %s", e, 32))
    #e <- 32 # 1:32
    dat <- get.pty.props(estado=e)
}
warnings()

dat[1,]
table(dat$e1==dat$pan.cnv1)

write.csv(output, file = paste(path, edo, "Fed.csv", sep = ""), row.names = FALSE); rm(e1)








