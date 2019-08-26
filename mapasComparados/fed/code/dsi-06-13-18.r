rm(list = ls())
options(width = 120)

dm <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/fed"
dd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/data/dsi"
setwd(dm)

# will work with equivalencia seccionales
# has 2006 2013 2018 (3er escenarios)
es <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv"
es <- read.csv(file = es, stringsAsFactors = FALSE)
head(es)

# get first scenarios for each state for 2018
# also has party modifications, not considered for now
tmp2 <- data.frame()
tmp <- read.csv("agsFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("bcFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("bcsFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("camFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("coaFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("colFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("cpsFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("cuaFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("dfFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("dgoFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("guaFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("gueFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("hgoFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("jalFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("mexFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("micFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("morFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("nayFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("nlFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
# oax era una repetición de bc... lo saco de lo que circuló marta
tmp <- read.csv("/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/loQueDioMarta/Primer Escenario (Federal 2017)/20 Oaxaca/Escenarios/72_RS_6.047308_464223850.csv", stringsAsFactors = FALSE)
colnames(tmp) <- c("seccion","e1.2018")
tmp$edon <- 20; tmp <- tmp[,c("edon","seccion","e1.2018")]; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("pueFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("queFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("quiFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("sanFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("sinFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("sonFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("tabFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("tamFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("tlaFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("verFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("yucFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
tmp <- read.csv("zacFed.csv", stringsAsFactors = FALSE)
tmp <- tmp[, c("edon","seccion","escenario1")]; colnames(tmp)[3] <- "e1.2018"; tmp2 <- rbind(tmp2, tmp)
#
es <- merge(x = es, y = tmp2, by = c("edon", "seccion"), all = TRUE)
rm(tmp, tmp2)


# get first scenarios for each state for 2013 (slow)
source("/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/code/e1-2013-prep.r")


# missing secciones will need to be re-assigned with from.to
# OJO: IMPORTANT TO DEAL WITH THIS LATER. DONE FOR ALL EXCEPT FIRST SCENARIOS.
table(is.na(es$e1.2013),is.na(es$e1.2018))
table(is.na(es$dis2013),is.na(es$dis2018))
table(is.na(es$dis2006),is.na(es$dis2018))
table(is.na(es$e1.2013),is.na(es$dis2018))



###############################################################
## ######################################################### ##
## ## prepare district similarity index, cf. Cox & Katz   ## ##
## ## son's perspective: how similar is son to its father ## ##
## ######################################################### ##
###############################################################
##
###################
## 2006 = father ##
## 2018 = son    ##
###################
dsi <- data.frame()
for (e in 1:32){ # loop over states
    #e <- 1 # debug
    sel <- which(es$edon==e)
    d <- es[sel,]
    father <- d$dis2015 # 2015 used 2006 map and has most reseccionamiento updates
    son <- d$dis2018
    sel.drop <- which(son==0) # quick fix for re-seccionamiento
    if (length(sel.drop)>0){
        d <- d[-sel.drop,]; father <- father[-sel.drop]; son <- son[-sel.drop]
    }
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
    dsi <- rbind(dsi, d)
}
#dsi <- dsi[-which(dsi$dis==0),] # will drop secciones that are missing... dealing w reseccionamiento above will make this unnecessary
#
dsi.18v06 <- dsi
rm(e,i,d,N,dsi,sel,sel.n,sel.f,sel.c,target,tmp,father,son,sel.drop)


###################
## 2006 = father ##
## 2013 = son    ##
###################
dsi <- data.frame()
for (e in 1:32){ # loop over states
    #e <- 1 # debug
    sel <- which(es$edon==e)
    d <- es[sel,]
    father <- d$dis2006
    son <- d$dis2013
    sel.drop <- which(son==0)
    if (length(sel.drop)>0){
        d <- d[-sel.drop,]; father <- father[-sel.drop]; son <- son[-sel.drop]
    }
    N <- max(son, na.rm = TRUE)
    d$dis <- son
    d$father <- NA
    d$dsi <- 0
    for (i in 1:N){ # loop over districts
        #i <- 1 # debug
        sel.n <- which(son==i)                  # secciones in new district
        tmp <- table(father[sel.n])
        target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # takes first instance in case of tie (dual fathers) 
        d$father[sel.n] <- target
        sel.f <- which(father==target) # secciones in father district
        sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
        d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
    }
    d <- d[duplicated(son)==FALSE,]
    d <- d[,c("edon","dis","father","dsi")]
    d <- d[order(d$dis),]
    dsi <- rbind(dsi, d)
}
#dsi <- dsi[-which(dsi$dis==0),] # will drop secciones that are missing... dealing w reseccionamiento above will make this unnecessary
#
dsi.13v06 <- dsi
rm(e,i,d,N,dsi,sel,sel.n,sel.f,sel.c,target,tmp,father,son,sel.drop)


#####################
## 2018e1 = father ##
## 2018e3 = son    ##
#####################
dsi <- data.frame()
for (e in 1:32){ # loop over states
    #e <- 1 # debug
    sel <- which(es$edon==e)
    d <- es[sel,]
    father <- d$e1.2018
    son <- d$dis2018
    sel.drop <- which(son==0)
    if (length(sel.drop)>0){
        d <- d[-sel.drop,]; father <- father[-sel.drop]; son <- son[-sel.drop]
    }
    N <- max(son, na.rm = TRUE)
    d$dis <- son
    d$father <- NA
    d$dsi <- 0
    for (i in 1:N){ # loop over districts
        #i <- 1 # debug
        sel.n <- which(son==i)                  # secciones in new district
        tmp <- table(father[sel.n])
        target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # takes first instance in case of tie (dual fathers) 
        d$father[sel.n] <- target
        sel.f <- which(father==target) # secciones in father district
        sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
        d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
    }
    d <- d[duplicated(son)==FALSE,]
    d <- d[,c("edon","dis","father","dsi")]
    d <- d[order(d$dis),]
    dsi <- rbind(dsi, d)
}
#
dsi.18v18e1 <- dsi
rm(e,i,d,N,dsi,sel,sel.n,sel.f,sel.c,target,tmp,father,son,sel.drop)


#####################
## 2013e1 = father ##
## 2013e3 = son    ##
#####################
dsi <- data.frame()
for (e in 1:32){ # loop over states
    #e <- 1 # debug
    sel <- which(es$edon==e)
    d <- es[sel,]
    father <- d$e1.2013
    son <- d$dis2013
    sel.drop <- which(son==0)
    if (length(sel.drop)>0){
        d <- d[-sel.drop,]; father <- father[-sel.drop]; son <- son[-sel.drop]
    }
    N <- max(son, na.rm = TRUE)
    d$dis <- son
    d$father <- NA
    d$dsi <- 0
    for (i in 1:N){ # loop over districts
        #i <- 1 # debug
        sel.n <- which(son==i)                  # secciones in new district
        tmp <- table(father[sel.n])
        target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # takes first instance in case of tie (dual fathers) 
        d$father[sel.n] <- target
        sel.f <- which(father==target) # secciones in father district
        sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
        d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
    }
    d <- d[duplicated(son)==FALSE,]
    d <- d[,c("edon","dis","father","dsi")]
    d <- d[order(d$dis),]
    dsi <- rbind(dsi, d)
}
#
dsi.13v13e1 <- dsi
rm(e,i,d,N,dsi,sel,sel.n,sel.f,sel.c,target,tmp,father,son,sel.drop)


####################################################################################
## next pair takes the father's perspective instead: how similar is father to son ##
## reason: unlike real people (genetics), in districts, if f were son s's father, ##
## then s may (most likely) or may not (unlikely but possible) be f's son. Since  ##
## the father (2006) is common to both new maps, taking its perspective makes the ##
## measure comparable.                                                            ##
####################################################################################
##
###################
## 2006 = father ##
## 2018e1 = son  ##
###################
dsi <- data.frame()
for (e in 1:32){ # loop over states
    #e <- 1 # debug
    sel <- which(es$edon==e)
    d <- es[sel,]
    father <- d$dis2015 # 2015 used 2006 map and has most reseccionamiento updates
    son <- d$e1.2018
    sel.drop <- which(is.na(son)==TRUE)
    if (length(sel.drop)>0){
        d <- d[-sel.drop,]; father <- father[-sel.drop]; son <- son[-sel.drop]
    }
    N <- max(father, na.rm = TRUE)
    d$dis <- father
    d$son <- NA
    d$dsi <- 0
    for (i in 1:N){ # loop over districts
        #i <- 1 # debug
        sel.n <- which(father==i)                  # secciones in old district
        tmp <- table(son[sel.n])
        target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # takes first instance in case of tie (dual sons) 
        d$son[sel.n] <- target
        sel.f <- which(son==target) # secciones in son  district
        sel.c <- intersect(sel.n, sel.f)             # secciones common to son and old districts
        d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
    }
    d <- d[duplicated(father)==FALSE,]
    d <- d[,c("edon","dis","son","dsi")]
    d <- d[order(d$dis),]
    dsi <- rbind(dsi, d)
}
#
sel.drop <- which(dsi$dis==0); dsi <- dsi[-sel.drop,] # drops dis=0, unnecessary once missing secciones fixed
dsi.06v18e1 <- dsi
rm(e,i,d,N,dsi,sel,sel.n,sel.f,sel.c,target,tmp,father,son,sel.drop)


###################
## 2006 = father ##
## 2013e1 = son  ##
###################
dsi <- data.frame()
for (e in 1:32){ # loop over states
    #e <- 1 # debug
    sel <- which(es$edon==e)
    d <- es[sel,]
    father <- d$dis2006
    son <- d$e1.2013
    sel.drop <- which(is.na(son)==TRUE)
    if (length(sel.drop)>0){
        d <- d[-sel.drop,]; father <- father[-sel.drop]; son <- son[-sel.drop]
    }
    N <- max(father, na.rm = TRUE)
    d$dis <- father
    d$son <- NA
    d$dsi <- 0
    for (i in 1:N){ # loop over districts
        #i <- 1 # debug
        sel.n <- which(father==i)                  # secciones in old district
        tmp <- table(son[sel.n])
        target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # takes first instance in case of tie (dual sons) 
        d$son[sel.n] <- target
        sel.f <- which(son==target) # secciones in son  district
        sel.c <- intersect(sel.n, sel.f)             # secciones common to son and old districts
        d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
    }
    d <- d[duplicated(father)==FALSE,]
    d <- d[,c("edon","dis","son","dsi")]
    d <- d[order(d$dis),]
    dsi <- rbind(dsi, d)
}
#
sel.drop <- which(dsi$dis==0); dsi <- dsi[-sel.drop,] # drops dis=0, unnecessary once missing secciones fixed
dsi.06v13e1 <- dsi
rm(e,i,d,N,dsi,sel,sel.n,sel.f,sel.c,target,tmp,father,son,sel.drop)


# export dsis
setwd(dd)
write.csv(dsi.18v06, file = "dsi18v06.csv", row.names = FALSE)
write.csv(dsi.13v06, file = "dsi13v06.csv", row.names = FALSE)
write.csv(dsi.18v18e1, file = "dsi18v18e1.csv", row.names = FALSE)
write.csv(dsi.13v13e1, file = "dsi13v13e1.csv", row.names = FALSE)
#
write.csv(dsi.06v13e1, file = "dsi06v13e1.csv", row.names = FALSE)
write.csv(dsi.06v18e1, file = "dsi06v18e1.csv", row.names = FALSE)

