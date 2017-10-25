rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"
setwd(dd)

#u <- url("http://ericmagar.com/data/redistrict/subnat/coa/coaDisn14toDisn17.csv")
#d <- read.csv(file = u, stringsAsFactors = FALSE)
d <- read.csv(file = "coaLoc.csv", stringsAsFactors = FALSE)

head(d)

# dsi seen from offspring perspective
# new district's "father" and district similarity index, cf. Cox & Katz
d$father14 <- NA
d$dsi <- 0
for (i in 1:16){
    #i <- 16 # debug
    sel.n <- which(d$disn17==i)                  # secciones in new district
    tmp <- table(d$disn14[sel.n])
    target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
    d$father14[sel.n] <- target
    sel.f <- which(d$disn14==target) # secciones in father district
    sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
    d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}

dsi <- d[duplicated(d$disn17)==FALSE, c("disn17","father14","dsi")]
dsi <- dsi[order(dsi$disn17),]
dsi$cab2017 <- c("Acuña", "Piedras Negras", "Sabinas", "San Pedro", "Monclova", "Frontera", "Matamoros", "Torreón", "Torreón", "Torreón", "Torreón", "Ramos Arizpe", "Saltillo", "Saltillo", "Saltillo", "Saltillo")
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_coa.csv", row.names = FALSE)

# dsi seen from parent perspective
# new district's "father" and district similarity index, cf. Cox & Katz
d$son17 <- NA
d$dsi <- 0
for (i in 1:16){
    #i <- 16 # debug
    sel.o <- which(d$disn14==i)                  # secciones in original district
    tmp <- table(d$disn17[sel.o])
    target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
    d$son2017[sel.o] <- target
    sel.s <- which(d$disn17==target) # secciones in son district
    sel.c <- intersect(sel.o, sel.s) # secciones common to original and son districts
    d$dsi[sel.o] <- round( length(sel.c) / (length(sel.o) + length(sel.s) - length(sel.c)) , 3 )
}

dsi <- d[duplicated(d$disn14)==FALSE, c("disn14","son2017","dsi")]
dsi <- dsi[order(dsi$disn14),]
dsi$cab14 <- c("Saltillo", "Saltillo", "Saltillo", "Saltillo", "Ramos Arizpe", "Torreón", "Torreón", "Torreón", "Torreón", "San Pedro", "Frontera", "Monclova", "Múzquiz", "Sabinas", "Acuña", "Piedras Negras")
dsi <- dsi[order(dsi$dsi),]




summary(dsi$dsi)




# añade población
dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/"
load(paste(dd, "votPobDis0018.RData", sep = ""))
ls()
names(votPobDis0018$pob.distMap2015p3)
