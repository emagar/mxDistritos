############################################################
## Function to process all party proposals in given state ##
############################################################
get.pty.props <- function(edon==NA){
    #edon <- 1  #debug
    edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
    #dd1   <- paste(path, "1erEscenario2013/", edo, sep = "")
    dd1pp <- paste(path, "1erEscenarioComentariosPartidos/", edo, "/Escenarios/", sep = "")
    #dd2   <- paste(path, "2oEscenario/", edo, sep = "")
    #dd2pp <- paste(path, "2oEscenarioComentariosPartidos/", edo, sep = "")
    #dd3   <- paste(path, "3erEscenario/",      sep = "")
    setwd(dd1pp)
    #
    files.tmp <- list.files()
    sel <- grep("tmp", files.tmp)
    if (length(sel)>0) files.tmp <- files.tmp[-sel] # drop tmp from list if present
    for (i in 1:length(files.tmp)){ # loop over available party proposals
        #i <- 3 # debug
        sel.file <- files.tmp[i];
        ptyn <- sub(pattern = "[A-Za-z]+_[0-9]{1}_([0-9]+)_[0-9]+.zip", replacement = "\\1", sel.file)
        #
        # determine party authoring the proposal
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
        if (ptyn=="19")  {warning("Original escenario ignored"); break} # pty <- "e"; # DIRECCION DE CARTOGRAFIA (DCE) --- original escenario
        if (ptyn=="2")   {warning("Cargado x clv is ignored"); break} # pty <- "eloc.ignore"; # CLV CARGADO POR EL PRESIDENTE DE LA MISMA
        #
        # unzip relevant data into tmp/ folder
        unzip(sel.file, exdir = "tmp")
        #name.col <- unzip(sel.file, list = TRUE)$Name # pick unzipped file's name for dataframe's column
        #
        # read data 
        tmp <- read.csv(file = "tmp/escenario.dat.ife", sep = "\t", header = FALSE)
        colnames(tmp) <- c("edon","prop","munn","seccion");
        #colnames(tmp)[2] <- paste(pty, "1", sep="");
        #head(tmp); # debug
        # now need to either export tmp to be added later or (better) to plug it into dat
        # deal with seccion==0 first
        # then with rest
        #
        # select/subset edon rows in dat
        sel.dat <- which(dat$edon==edon)
        dat.tmp <- dat[sel.dat,] # subset dat rows to manipulate
        target.col <- grep(pty, colnames(dat.tmp)) # column to manipulate
        #
        # handle seccion==0 cases, ie. full municipios 
        sel.zero <- which(tmp$seccion==0) # select obs where district is reported for munn only
        if (length(sel.zero)>0) {
            for (m in 1:length(sel.tmp)){
                #m <- 9 # debug
                case <- sel.zero[m];
                sel.m <- which(dat.tmp$ife==e1$ife[case])    # indices with same municipio number as seccion==0 cases
                dat.tmp[sel.m,target.col] <- tmp$prop[case]  # plug district number to those indices
            }
        }
        #
        # handle seccion!=0 cases
        sel.zero <- which(tmp$seccion!=0) # select obs where seccion assigned 
        if (length(sel.zero)>0) {
            for (s in sel.tmp){
                #s <- sel.tmp[9] # debug
                sel.m <- which(dat.tmp$seccion==tmp$seccion[s])    # indices with same municipio number as seccion==0 cases
                dat.tmp[sel.m,target.col] <- tmp$prop[s]  # plug district number to those indices
            }
        }
        #
        # return manipulated data
        dat[sel.dat,] <- dat.tmp
        ## tmp2 <- dat.tmp[,target.col]
        ## table(tmp2==dat.tmp[,target.col], useNA = "always")
    }
}

