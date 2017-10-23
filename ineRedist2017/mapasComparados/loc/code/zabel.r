rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/ineRedist2017/mapasComparados/loc"
setwd(dd)

# Estos son los archivos que circuló Zabel
mic2 <- read.csv("fuenteAlumnos/michoacan.redis.csv", stringsAsFactors = FALSE)
tab2 <- read.csv("fuenteAlumnos/tabasco.redis.csv", stringsAsFactors = FALSE)
tla2 <- read.csv("fuenteAlumnos/tlaxcala.redis.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
mic1 <- read.csv("../../fuenteDeJson/loc/micLoc.csv", stringsAsFactors = FALSE)
tab1 <- read.csv("../../fuenteDeJson/loc/tabLoc.csv", stringsAsFactors = FALSE)
tla1 <- read.csv("../../fuenteDeJson/loc/tla19Loc.csv", stringsAsFactors = FALSE)

head(mic2)
dim(mic2)
dim(mic1)

# michoacan
mic1 <- mic1[,c("edon","seccion","munn","escenario3")]
colnames(mic1) <- c("edon","seccion","munn","disn2018")

# verifica integridad de las primeras dos columnas de los datos de zabel
mic21 <- mic2[,c("seccion17","dist17")]
tmp <- merge(x= mic1, y = mic21, by.x = "seccion", by.y = "seccion17", all = TRUE)
nrow(tmp)==nrow(mic21) # must be TRUE
rm(mic21)

# fusiona siguientes dos columnas
mic22 <- mic2[,c("seccion12","distrito12")]
mic22 <- mic22[is.na(mic22$seccion12)==FALSE,] # quita NAs

mic <- merge(x= mic1, y = mic22, by.x = "seccion", by.y = "seccion12", all = TRUE)

dim(mic)
dim(mic1)
dim(mic22)

head(mic)

write.csv(mic, file = "micLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

#tabasco
colnames(tab1)
tab1 <- tab1[,c("edon","seccion","munn","escenario3")]
colnames(tab1) <- c("edon","seccion","munn","disn2018")

# verifica integridad de las primeras dos columnas de los datos de zabel
colnames(tab2)
tab21 <- tab2[,c("seccion17","distrito17")]
tmp <- merge(x= tab1, y = tab21, by.x = "seccion", by.y = "seccion17", all = TRUE)
nrow(tmp)==nrow(tab21) # must be TRUE
rm(tab21)

# fusiona siguientes dos columnas
tab22 <- tab2[,c("seccion12","distrito12")]
tab22 <- tab22[is.na(tab22$seccion12)==FALSE,] # quita NAs

tab <- merge(x= tab1, y = tab22, by.x = "seccion", by.y = "seccion12", all = TRUE)

dim(tab)
dim(tab1)
dim(tab22)

head(tab)

write.csv(tab, file = "tabLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

# tlaxcala
colnames(tla1)
tla1 <- tla1[,c("edon","seccion","munn","escenario3")]
colnames(tla1) <- c("edon","seccion","munn","disn2018")

# verifica integridad de las primeras dos columnas de los datos de zabel
colnames(tla2)
tla21 <- tla2[,c("seccion17","distrito17")]
tmp <- merge(x= tla1, y = tla21, by.x = "seccion", by.y = "seccion17", all = TRUE)
nrow(tmp)==nrow(tla21) # must be TRUE
rm(tla21)

# fusiona siguientes dos columnas
tla22 <- tla2[,c("seccion12","distrito12")]
tla22 <- tla22[is.na(tla22$seccion12)==FALSE,] # quita NAs

tla <- merge(x= tla1, y = tla22, by.x = "seccion", by.y = "seccion12", all = TRUE)

dim(tla)
dim(tla1)
dim(tla22)

head(tla)

write.csv(tla, file = "tla19Loc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

rm(mic1,mic2,mic22,tab1,tab2,tab22,tla1,tla2,tla22,tmp) # limpieza
ls()



