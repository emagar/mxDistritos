rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló omar
sin2 <- read.csv("fuenteAlumnos/ruben.redistritacion_25.csv", stringsAsFactors = FALSE)
yuc2 <- read.csv("fuenteAlumnos/ruben.redistritacion_31.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
sin1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/sinLoc.csv", stringsAsFactors = FALSE)
yuc1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/yucLoc.csv", stringsAsFactors = FALSE)

head(sin2)
dim(sin2)
dim(sin1)


# sinaloa
colnames(sin1)
sin1 <- sin1[,c("edon","seccion","munn","escenario3")]
colnames(sin1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Ruben
head(sin2)
#sin2$distrito17 <- NULL
colnames(sin2) <- c("seccion","disn2017","disn2006") # ruben: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 

# fusiona
sin <- merge(x = sin1, y = sin2, by = "seccion", all = TRUE)

dim(sin)
dim(sin1)
dim(sin2)

head(sin)

# rubén: algo está mal, la columnas disn2018 tendría que ser idéntica a la tuya (que he llamado d2017). Revisa por favor.

#write.csv(sin, file = "sinLoc.csv", row.names = FALSE) # Ruben: cuando quede, usa éste archivo para sacar el insice s de cox y katz




# yucatan
colnames(yuc1)
yuc1 <- yuc1[,c("edon","seccion","munn","escenario3")]
colnames(yuc1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Ruben
head(yuc2)
#yuc2$distrito17 <- NULL
colnames(yuc2) <- c("seccion","disn2017","disn2006") # ruben: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 

# fusiona
yuc <- merge(x = yuc1, y = yuc2, by = "seccion", all = TRUE)

dim(yuc)
dim(yuc1)
dim(yuc2)

head(yuc)

# rubén: algo está mal, mismo problema

#write.csv(yuc, file = "yucLoc.csv", row.names = FALSE) # Ruben: cuando quede, usa éste archivo para sacar el insice s de cox y katz



rm(sin1,sin2,yuc1,yuc2) # limpieza
ls()

# ruben:
# 1. reporta/investiga los años electorales que puse arriba.
# 2. corrige el proceso de fusión
# 3. Prepara el código para obtener el índice s de cox y katz. 

