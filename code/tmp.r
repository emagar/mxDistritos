# when full municipio assigned to a district, this transfers info to secciones
library(crayon)
# function to fill secciones with a municipality's info
munReplica <- function(dat = dat){
    #sel.col <- grep("dis", colnames(dat))
    sel.col <- setdiff(1:ncol(dat), grep("edon|seccion|munn", colnames(dat))) # version for colnames identifying actor 
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

# manipulate objects here
head(df1)
head(df40)
tmp <- df40[,c("seccion","munn")]
table(df1$munn)
dat <- merge(x = df1, y = tmp, by = c("seccion","munn"), all = TRUE) # df1 has full municipios in everyone's districts, need to add missing secciones (present in df40) 
dim(tmp)
dim(dat)
head(dat)
table(dat$munn) # solved
table(dat$escenario3_c8[dat$munn==8], useNA = "always")

dat <- dat[order(dat$seccion, dat$munn),] # sort
if (length(which(dat$seccion==0))==0) {
    message(green("Municipalities aren't building block here. Move on to write.csv"))
} else {
    message(red("Execute munReplica()"))
}

dat <- munReplica(dat)

head(dat)
table(dat$escenario3_c8)

write.csv(dat, file = "df33Loc.csv", row.names = FALSE)
