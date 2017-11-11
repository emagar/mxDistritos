rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló omar
col2 <- read.csv("fuenteAlumnos/omarDistCol.csv", stringsAsFactors = FALSE)
cua2 <- read.csv("fuenteAlumnos/omarDistChih.csv", stringsAsFactors = FALSE)
mor2 <- read.csv("fuenteAlumnos/omarDistMor.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
col1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/colLoc.csv", stringsAsFactors = FALSE)
cua1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cuaLoc.csv", stringsAsFactors = FALSE)
mor1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/mor12Loc.csv", stringsAsFactors = FALSE)
mor18 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/mor18Loc.csv", stringsAsFactors = FALSE) # también incorporaré el mapa con 18 distritos

head(col2)
dim(cua2)
dim(cua1)



# chihuahua
colnames(cua1)
cua1 <- cua1[,c("edon","seccion","munn","escenario3")]
colnames(cua1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Omar
head(cua2)
table(cua2$merge)
cua2$merge <- NULL
colnames(cua2) <- c("disn2015","seccion","disn2013") # omar: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 

# fusiona
cua <- merge(x = cua1, y = cua2, by = "seccion", all = TRUE)

dim(cua)
dim(cua1)
dim(cua2)

head(cua)
table(cua$disn2015, cua$disn2018) # checa esto Omar: aunque hay correspondencia 1-a-1 entre los mapas 2018 (que saqué de la fuente) y el nuevo que mandaste (2015), los números de distrito se alteraron---el distrito 1 se convirtió en el 11, el 2 en el 20, el 3 en el 1...: 

> table(cua$disn2015, cua$disn2018) # (output de R)
    
       1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
  1    0   0 193   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  2    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  69   0   0
  3    0   0   0   0   0   0   0   0   0   0   0   0   0 151   0   0   0   0
  4    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  5    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  6    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 153
  7    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  8    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  9    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  85   0
  10   0   0   0   0   0   0   0   0   0   0   0   0   0   0 181   0   0   0
  11 161   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  12   0   0   0   0   0   0   0   0  99   0   0   0   0   0   0   0   0   0
  13   0   0   0 259   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  14   0   0   0   0   0   0 126   0   0   0   0   0   0   0   0   0   0   0
  15   0   0   0   0   0   0   0   0   0   0   0 132   0   0   0   0   0   0
  16   0   0   0   0   0   0   0   0   0   0 179   0   0   0   0   0   0   0
  17   0   0   0   0   0   0   0   0   0   0   0   0 127   0   0   0   0   0
  18   0   0   0   0   0   0   0   0   0 115   0   0   0   0   0   0   0   0
  19   0   0   0   0   0   0   0 127   0   0   0   0   0   0   0   0   0   0
  20   0 156   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0
  21   0   0   0   0 216   0   0   0   0   0   0   0   0   0   0   0   0   0
  22   0   0   0   0   0 140   0   0   0   0   0   0   0   0   0   0   0   0
    
      19  20  21  22
  1    0   0   0   0
  2    0   0   0   0
  3    0   0   0   0
  4    0   0 221   0
  5    0  88   0   0
  6    0   0   0   0
  7    0   0   0 121
  8  107   0   0   0
  9    0   0   0   0
  10   0   0   0   0
  11   0   0   0   0
  12   0   0   0   0
  13   0   0   0   0
  14   0   0   0   0
  15   0   0   0   0
  16   0   0   0   0
  17   0   0   0   0
  18   0   0   0   0
  19   0   0   0   0
  20   0   0   0   0
  21   0   0   0   0
  22   0   0   0   0

# Esto se arregla fácil, pero me hace dudar de la relación con distritos 2013 que también mandaste... revisa por favor

#write.csv(cua, file = "cuaLoc.csv", row.names = FALSE) # Omar: cuando quede, usa éste archivo para sacar el insice s de cox y katz




# colima
colnames(col1)
col1 <- col1[,c("edon","seccion","munn","escenario3")]
colnames(col1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Omar
head(col2)
table(col2$X_merge)
col2$X_merge <- NULL
colnames(col2) <- c("disn2016","seccion","disn2014") # omar: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 

# fusiona
col <- merge(x = col1, y = col2, by = "seccion", all = TRUE)

dim(col)
dim(col1)
dim(col2)

head(col)

table(col$disn2016, col$disn2018) # omar: mismo problema que arriba, revisa plis

#write.csv(col, file = "colLoc.csv", row.names = FALSE) # Omar: cuando quede, usa éste archivo para sacar el insice s de cox y katz




# morelos
colnames(mor1)
mor1 <- mor1[,c("edon","seccion","munn","escenario3")]
colnames(mor1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Omar
head(mor2)
mor2$X_merge <- NULL
colnames(mor2) <- c("disn2017","seccion","disn2012") # omar: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 

# fusiona
mor <- merge(x = mor1, y = mor2, by = "seccion", all = TRUE)

dim(mor)
dim(mor1)
dim(mor2)

head(mor)

table(mor$disn2017, mor$disn2018) # omar: mismo problema que arriba, revisa plis

#write.csv(mor, file = "morLoc.csv", row.names = FALSE) # Omar: cuando quede, usa éste archivo para sacar el insice s de cox y katz


rm(cua1,cua2,col1,col2,mor1,mor2) # limpieza
ls()

# omar:
# 1. reporta/investiga los años electorales que puse arriba.
# 2. corrige el proceso de fusión
# 3. Prepara el código para obtener el índice s de cox y katz. 

