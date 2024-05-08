rm(list = ls())
options(width = 120)

dd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/data/dsi"
setwd(dd)

# will work with equivalencia seccionales
# has 2006 2013 2018 (3er escenarios)
es <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv"
es <- read.csv(file = es, stringsAsFactors = FALSE)
head(es)

# missing secciones will need to be re-assigned with from.to
table(is.na(es$dis2018),is.na(es$dis2024))



###############################################################
## ######################################################### ##
## ## prepare district similarity index, cf. Cox & Katz   ## ##
## ## son's perspective: how similar is son to its father ## ##
## ######################################################### ##
###############################################################
##
################################
## son's similarity to father ##
## 2021 = father              ##
## 2024 = son                 ##
################################
dsi <- data.frame()
for (e in 1:32){ # loop over states
    sel <- which(es$edon==e)
    d <- es[sel,]
    father <- d$dis2018 # 2021 used 2018 map and has most reseccionamiento updates
    son <- as.numeric(d$dis2024)
    sel.drop <- which(son==0) # quick fix for re-seccionamiento, should have no impact if uofa = sons only
    ##d$seccion[sel.drop]
    if (length(sel.drop)>0){
        d <- d[-sel.drop,]; father <- father[-sel.drop]; son <- son[-sel.drop]
    }
    father <- father - e*100
    son    <- son    - e*100
    N <- max(son, na.rm = TRUE)
    d$dis <- son
    d$father <- NA
    d$dsi <- 0
    for (i in 1:N){ # loop over districts
        #i <- 1 # debug
        sel.n <- which(son==i)                  # secciones in new district (son)
        tmp <- table(father[sel.n])             # how many secciones did each potnetial father contribute to son
        target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # picks largest as father (first in case of tie) 
        d$father[sel.n] <- target
        sel.f <- which(father==target)          # secciones in father district
        sel.c <- intersect(sel.n, sel.f)        # secciones common to father and son
        d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
    }
    d <- d[duplicated(son)==FALSE,]
    d <- d[,c("edon","dis","father","dsi")]
    d <- d[order(d$dis),]
    d$dis    <- d$dis    + e*100
    d$father <- d$father + e*100
    dsi <- rbind(dsi, d)
}

################################
## father's similarity to son ##
## 2021 = father              ##
## 2024 = son                 ##
################################
dsi2 <- data.frame()
for (e in 1:32){ # loop over states
    ##e <- 1
    sel <- which(es$edon==e)
    d <- es[sel,]
    father <- d$dis2018 # 2021 used 2018 map and has most reseccionamiento updates
    son <- as.numeric(d$dis2024)
    sel.drop <- which(son==0) # quick fix for re-seccionamiento, should have no impact because new secciones map districts too
    ##d$seccion[sel.drop]
    if (length(sel.drop)>0){
        d <- d[-sel.drop,]; father <- father[-sel.drop]; son <- son[-sel.drop]
    }
    father <- father - e*100
    son    <- son    - e*100
    N <- max(father, na.rm = TRUE)
    d$dis <- father
    d$son <- NA
    d$dsi <- 0
    for (i in 1:N){ # loop over districts
        #i <- 3 # debug
        sel.n <- which(father==i)               # secciones in old district (father)
        tmp <- table(son[sel.n])                # how many secciones did each potnetial father contribute to son
        target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # picks largest as father (first in case of tie) 
        d$son[sel.n] <- target
        sel.f <- which(son==target)             # secciones in son district
        sel.c <- intersect(sel.n, sel.f)        # secciones common to father and son
        d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
    }
    d <- d[duplicated(father)==FALSE,]
    d <- d[,c("edon","dis","son","dsi")]
    d <- d[order(d$dis),]
    d$dis    <- d$dis    + e*100
    d$son <- d$son + e*100
    dsi2 <- rbind(dsi2, d)
}


#dsi <- dsi[-which(dsi$dis==0),] # will drop secciones that are missing... dealing w reseccionamiento above will make this unnecessary
#



# export dsis
setwd(dd)
dir()
write.csv(dsi,  file = "dsi24v18.csv",    row.names = FALSE)
write.csv(dsi2, file = "dsi24v18rev.csv", row.names = FALSE)

