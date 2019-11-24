props <- c("pan.clv", "pri.clv", "prd.clv", "pt.clv", "pvem.clv", "mc.clv", "pna.clv", "pan.cnv", "pri.cnv", "prd.cnv", "pt.cnv", "pvem.cnv", "mc.cnv", "pna.cnv")

get.pty <- function(pty=NA){
    if (pty=="pan.clv")  ptyn <- 32; # PAN ANTE LA CLV
    if (pty=="pri.clv")  ptyn <- 33; # PRI ANTE LA CLV
    if (pty=="prd.clv")  ptyn <- 34; # PRD ANTE LA CLV
    if (pty=="pt.clv")   ptyn <- 35; # PT ANTE LA CLV
    if (pty=="pvem.clv") ptyn <- 36; # PVEM ANTE LA CLV
    if (pty=="mc.clv")   ptyn <- 37; # MC ANTE LA CLV
    if (pty=="pna.clv")  ptyn <- 38; # NA ANTE LA CLV
    if (pty=="pan.cnv")  ptyn <- 42; # PAN ANTE LA CNV
    if (pty=="pri.cnv")  ptyn <- 43; # PRI ANTE LA CNV
    if (pty=="prd.cnv")  ptyn <- 44; # PRD ANTE LA CNV
    if (pty=="pt.cnv")   ptyn <- 45; # PT ANTE LA CNV
    if (pty=="pvem.cnv") ptyn <- 46; # PVEM ANTE LA CNV
    if (pty=="mc.cnv")   ptyn <- 47; # MC ANTE LA CNV
    if (pty=="pna.cnv")  ptyn <- 48; # NA ANTE LA CNV
    #
    files.tmp <- list.files()
    sel.file <- grep(paste(ptyn,"[_]", sep = ""), files.tmp)
    if (length(sel.file)==0){
        print(paste("No proposal by", pty));
        stop
    }
    unzip(files.tmp[sel.file], exdir = "tmp")
    name.tmp <- unzip(files.tmp[sel.file], list = TRUE)$Name # pick unzipped file's name for dataframe's column
    #
    tmp <- read.csv(file = "tmp/escenario.dat.ife", sep = "\t", header = FALSE)
    colnames(tmp) <- c("edon","no.name","munn","seccion");
    colnames(tmp)[2] <- paste(pty, "1", sep="");
    #
    return(tmp)
}

for (i in length(props)){
    #i <- 14 # debug
    tmp <- "nihil";                # clear name
    tmp <- get.pty(pty=props[i]);  # use function to read party
    if (tmp=="nihil") next;
    assign(props[i], tmp);         # rename info with pty name
}

pri.clv[1,]

ls()

    
    
