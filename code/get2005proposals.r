wd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ifeRedist2005/Documentos dat/01 PRIMER ESCENARIO/01 1er ESCENARIO/"
setwd(wd)

edos <- c("ags", "bc",  "bcs", "cam", "coa", "col", "cps", "cua", "df",  "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl",  "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")


# get 1st scenario
sc1 <- data.frame() # will receive data

for (i in 1:32){
    edon <- i
    edo <- edos[edon]
    #
    tmp <- read.csv(paste(edo, "escenario.dat.ife", sep = "/"), sep="\t")
    colnames(tmp) <- c("edon","disn","munn","seccion")
    #
    sc1 <- rbind(sc1, tmp)
}

wd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ifeRedist2005/Documentos dat/01 PRIMER ESCENARIO/03 OBSERVACIONES PARTIDOS e1"
setwd(wd)

fls <- list.files(edo, recursive = TRUE, pattern = "escenario")
sel <- grep("Eval", fls)
fls <- fls[-sel]
x

# get obs pan

if (length(grep("01", dir(edo)))>0) {
    
    

paste(edo, )



table(tmp[,4])
dim(sc1)
head(tmp)
