rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# si los encuentra, lee datos de disco 
tmp <- dir()
sel <- grep(pattern = "(que|zac)Loc.csv", x = tmp)
if (length(sel)>0) {
    mic <- read.csv(file = tmp[sel[1]], stringsAsFactors = FALSE);
    zac <- read.csv(file = tmp[sel[2]], stringsAsFactors = FALSE);
    tla <- read.csv(file = tmp[sel[3]], stringsAsFactors = FALSE);
}

# Estos son los archivos que circuló
que2 <- read.csv("fuenteAlumnos/veronica.QRO.csv", stringsAsFactors = FALSE)
zac2 <- read.csv("fuenteAlumnos/veronica.ZAC.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
que1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/queLoc.csv", stringsAsFactors = FALSE)
zac1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/zacLoc.csv", stringsAsFactors = FALSE)

head(que2)
dim(que2)
dim(que1)

# queretaro
que1 <- que1[,c("edon","seccion","munn","escenario3")]
colnames(que1) <- c("edon","seccion","munn","disn2018")

# verifica integridad de las primeras dos columnas de los datos de zabel
que21 <- que2[,c("Sección_17","Dtto_17")]
tmp <- merge(x= que1, y = que21, by.x = "seccion", by.y = "Sección_17", all = TRUE)
nrow(tmp)==nrow(que21) # must be TRUE
rm(que21)

# fusiona siguientes dos columnas
que22 <- que2[,c("Sección_12","Dtto_12")]
que22 <- que22[is.na(que22$Sección_12)==FALSE,] # quita NAs

que <- merge(x= que1, y = que22, by.x = "seccion", by.y = "Sección_12", all = TRUE)

dim(que)
dim(que1)
dim(que22)

que$Dtto_12 <- as.numeric(as.roman(que$Dtto_12)) # quita numero romano

head(que)

write.csv(que, file = "queLoc.csv", row.names = FALSE) # Veronica: usa éste para sacar el insice s de cox y katz

# zacatecas
colnames(zac1)
zac1 <- zac1[,c("edon","seccion","munn","escenario3")]
colnames(zac1) <- c("edon","seccion","munn","disn2018")

# verifica integridad de las primeras dos columnas de los datos de zabel
colnames(zac2)
zac21 <- zac2[,c("Sección_17","Dtto_17")]
tmp <- merge(x= zac1, y = zac21, by.x = "seccion", by.y = "Sección_17", all = TRUE)
nrow(tmp)==nrow(zac21) # must be TRUE
rm(zac21)

# fusiona siguientes dos columnas
zac22 <- zac2[,c("Sección_13","Dtto_13")]
zac22 <- zac22[is.na(zac22$Sección_13)==FALSE,] # quita NAs

zac <- merge(x= zac1, y = zac22, by.x = "seccion", by.y = "Sección_13", all = TRUE)

dim(zac)
dim(zac1)
dim(zac22)

zac$Dtto_13 <- as.numeric(as.roman(zac$Dtto_13)) # quita numero romano

head(zac)

write.csv(zac, file = "zacLoc.csv", row.names = FALSE) # Zabel: usa éste para sacar el insice s de cox y katz

rm(que1,que2,que22,zac1,zac2,zac22,tmp) # limpieza
ls()

# Verónica:
# 1. no fusionaste cabalmente las secciones. Intenta usar el comando merge de stata o de R.
# 2. pareciera que usaste el reporte de eleccion de 2012/13 para reconstruir el mapa seccion-distrito anterior. Busca por favor un resultado de 2015/16 y repite la fusión de tus datos. Estoy seguro de que caerá mucho el número de secciones faltantes. 
# 3. Prepara el código para obtener el índice s de cox y katz. 






