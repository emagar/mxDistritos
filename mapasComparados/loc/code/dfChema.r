rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló chema
df2 <- read.csv("fuenteAlumnos/chema_cdmxLoc.csv", stringsAsFactors = FALSE)
df3 <- df2 # duplica

# Estos son los archivos originales
df1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/df33Loc.csv", stringsAsFactors = FALSE)
df40 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/df40Loc.csv", stringsAsFactors = FALSE)

head(df2)
dim(df2)
dim(df1)
dim(df40)

# ciudad de mex
colnames(df1)
df1 <- df1[,c("edon","seccion","munn","escenario3_c8")]
colnames(df1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Chema
head(df2)
df2$father15 <- df2$dsi <- df2$X <- NULL
df2$munn <- NULL
colnames(df2) <- c("seccion","disn2015","disn2017") # chema: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo)
table(df2$disn2015==df2$disn2017)

# fusiona
df <- merge(x = df1, y = df2, by = "seccion", all = TRUE)

dim(df)
dim(df1)
dim(df2)

head(df)
table(df$disn2018) 

#write.csv(df, file = "dfLoc.csv", row.names = FALSE) # Chema: dfndo quede, usa éste archivo para sacar el insice s de cox y katz




# colima
colnames(col1)
col1 <- col1[,c("edon","seccion","munn","escenario3")]
colnames(col1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Chema
head(col2)
table(col2$X_merge)
col2$X_merge <- NULL
colnames(col2) <- c("disn2016","seccion","disn2014") # chema: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 

# fusiona
col <- merge(x = col1, y = col2, by = "seccion", all = TRUE)

dim(col)
dim(col1)
dim(col2)

head(col)

table(col$disn2016, col$disn2018) # chema: mismo problema que arriba, revisa plis

#write.csv(col, file = "colLoc.csv", row.names = FALSE) # Chema: dfndo quede, usa éste archivo para sacar el insice s de cox y katz




# morelos
colnames(mor1)
mor1 <- mor1[,c("edon","seccion","munn","escenario3")]
colnames(mor1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Chema
head(mor2)
mor2$X_merge <- NULL
colnames(mor2) <- c("disn2017","seccion","disn2012") # chema: investiga el año electoral inaugural del mapa abandonado (será el nombre definitivo) 

# fusiona
mor <- merge(x = mor1, y = mor2, by = "seccion", all = TRUE)

dim(mor)
dim(mor1)
dim(mor2)

head(mor)

table(mor$disn2017, mor$disn2018) # chema: mismo problema que arriba, revisa plis

#write.csv(mor, file = "morLoc.csv", row.names = FALSE) # Chema: cuando quede, usa éste archivo para sacar el indice s de cox y katz



