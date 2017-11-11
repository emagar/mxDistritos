rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló claudia
pue2 <- read.csv("fuenteAlumnos/santiago.puebla2015y2017.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
pue1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/pueLoc.csv", stringsAsFactors = FALSE)

head(pue2)
dim(pue2)
dim(pue1)

# puebla
colnames(pue1)
pue1 <- pue1[,c("edon","seccion","munn","escenario3")]
colnames(pue1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Claudia
colnames(pue2)
pue2$seccion <- pue2$seccion - 210000
table(pue2$Dtto_17==pue2$Dtto_15, useNA = "ifany")
pue2$Dtto_17 <- NULL
colnames(pue2) <- c("seccion","disn2015") # claudia: investiga el año electoral inaugural del mapa abandonado para nombrarlo correctamente

# fusiona siguientes dos columnas
pue <- merge(x = pue1, y = pue2, by = "seccion", all = TRUE)

dim(pue)
dim(pue1)
dim(pue2)

head(pue)

write.csv(pue, file = "pueLoc.csv", row.names = FALSE) # Claudia: usa éste para sacar el insice s de cox y katz



# compute pue's dsi
d <- read.csv(file = "pueLoc.csv", stringsAsFactors = FALSE)
head(d)
son    <- d$disn2018
father <- d$disn2015
N <- max(son, na.rm = TRUE)
d$father <- NA
d$dsi <- 0
for (i in 1:N){
    #i <- 1 # debug
    sel.n <- which(son==i)                  # secciones in new district
    tmp <- table(father[sel.n])
    target <- as.numeric(names(tmp)[tmp==max(tmp)][1]) # takes first instance in case of tie (dual fathers) 
    d$father[sel.n] <- target
    sel.f <- which(father==target) # secciones in father district
    sel.c <- intersect(sel.n, sel.f)             # secciones common to father and new districts
    d$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}
summary(d$dsi)
#######################################################################################################
# santiago: como notaste, los distritos que Puebla usó en 2015 son iguales que los que usará en 2018. #
#           Busca la elección de diputados locales anterior---2010?                                   #
#######################################################################################################
# add 2005 pop

d <- merge(x = d, y = pob05[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob05 <- ave(d$ptot, as.factor(son), FUN = sum, na.rm = TRUE)
d$ptot <- NULL
# add 2010 pop
d <- merge(x = d, y = pob10[,c("seccion","ptot")], by = "seccion", all.x = TRUE, all.y = FALSE)
d$pob10 <- ave(d$ptot, as.factor(son), FUN=sum, na.rm=TRUE)
d$ptot <- NULL

dsi <- d[duplicated(son)==FALSE,]
dsi$seccion <- dsi$munn <- dsi$disn2007 <- NULL
head(dsi)
dsi <- dsi[order(dsi$dsi),]

write.csv(dsi, file = "simIndex/dist_pue.csv", row.names = FALSE)

