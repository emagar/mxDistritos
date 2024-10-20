# script takes csv files from json files prepared with getJson.r and 
# and does some house cleaning to ensure that no seccion=0 remain (meaning that
# full municipio belongs in a district)

rm(list = ls())

# local districts
path <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/" # datos manipulados
setwd(path)

files <- list.files()
files <- files[grep(".+csv", files)]     # keep only csv in listing

for (i in 1:length(files)){
    i <- 10 #debug
    fl <- files[i]
    dat <- read.csv(fl)
    dat <- dat[order(dat$seccion, dat$munn),]
    if (length(which(dat$seccion==0)) > 0) stop
    write.csv(dat, file = fl, row.names = FALSE)
}

head(dat)


# select a file to inspect
edon <- 9 # 1:32
edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
#
fl <- paste(path, edo, "33Loc.csv", sep = "")
dat <- read.csv(fl)

head(dat)

 
 

# morelos local districts
library(crayon)
# function to fill secciones with a municipality's info
munReplica <- function(dat = dat){
    sel.col <- grep("dis", colnames(dat))
    #sel.col <- setdiff(1:ncol(dat), grep("edon|seccion|munn", colnames(dat))) # version for colnames identifying actor 
    sel.row <- which(dat$seccion==0)
    for (r in sel.row){
        for (c in sel.col){
            #r <- 2; c <- 12 # debug
            if (is.na(dat[r, c])==TRUE) next
            dis <- dat[r, c] # copy district number for municipality's secciones
            sel <- which(dat$munn==dat$munn[r] & is.na(dat[,c])==TRUE) # select rows matching municipality with no district info in the column
            dat[sel, c] <- dis
        }
    }
    dat <- dat[-which(dat$seccion==0), ] # drop seccion=0 rows
    return(dat)
}
#
path <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/"
setwd(path)

# manipulate objects here
edon <- 9 # 1:32
edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
fl <- paste(path, edo, "33Loc.csv", sep = "")
dat <- read.csv(fl)
dat <- dat[order(dat$seccion, dat$munn),] # sort
if (length(which(dat$seccion==0))==0) {
    message(green("Municipalities aren't building block here. Move on to write.csv"))
} else {
    message(red("Execute munReplica()"))
}

dat <- munReplica(dat)

head(dat)
table(dat$dis74.9.157)

write.csv(dat, file = fl, row.names = FALSE)




# federal districts
library(crayon)
# function to fill secciones with a municipality's info
munReplica <- function(dat = dat){
    sel.col <- grep("dis", colnames(dat))
    sel.row <- which(dat$seccion==0)
    for (r in sel.row){
        for (c in sel.col){
            #r <- 1; c <- 19 # debug
            if (is.na(dat[r, c])==TRUE) next
            dis <- dat[r, c] # copy district number for municipality's secciones
            sel <- which(dat$munn==dat$munn[r] & is.na(dat[,c])==TRUE) # select rows matching municipality with no district info in the column
            dat[sel, c] <- dis
        }
    }
    dat <- dat[-which(dat$seccion==0), ] # drop seccion=0 rows
    return(dat)
}
#
path <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/ineRedist2017/deJson/fed/"
setwd(path)

# manipulate federal objects here
edon <- 21 # 1:32
edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")[edon]
fl <- paste(path, edo, "Fed.csv", sep = "")
dat <- read.csv(fl)
dat <- dat[order(dat$seccion, dat$munn),] # sort
if (length(which(dat$seccion==0))==0) {
    message(green("Municipalities aren't building block here. Move on to write.csv"))
} else {
    message(red("Execute munReplica()"))
}

dat <- munReplica(dat)

head(dat)

write.csv(dat, file = fl, row.names = FALSE)



