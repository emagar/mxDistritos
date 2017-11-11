rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc"
# dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc"
setwd(dd)

# Estos son los archivos que circuló victor
gua2 <- read.csv("fuenteAlumnos/victor.guanajuato.csv", stringsAsFactors = FALSE)
gue2 <- read.csv("fuenteAlumnos/victor.guerrero.csv", stringsAsFactors = FALSE)
nl2 <- read.csv("fuenteAlumnos/victor.nvoleon.csv", stringsAsFactors = FALSE)

# Estos son los archivos originales
gua1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/guaLoc.csv", stringsAsFactors = FALSE)
gue1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/gueLoc.csv", stringsAsFactors = FALSE)
nl1 <- read.csv("../../redisProcess/ineRedist2017/deJsonConEtiquetas/loc/nlLoc.csv", stringsAsFactors = FALSE)

head(gua2)
dim(gua2)
dim(gua1)

# guanajuato
colnames(gua1)
gua1 <- gua1[,c("edon","seccion","munn","escenario3")]
colnames(gua1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Victor
head(gua2) 
colnames(gua2) <- c("seccion","munn","mun","disnViejo","distritonuevo") # victor: qué año electoral usaste para distritoviejo? úsalo para el nombre (temporal). Investiga también el año electoral inaugural del mapa abandonado (será el nombre definitivo) 
#gua2$munn <- NULL

# fusiona
gua <- merge(x = gua1, y = gua2, by = "seccion", all = TRUE)

dim(gua)
dim(gua1)
dim(gua2)

head(gua)
table(gua$munn.x==gua$munn.y)
table(gua$disnViejo, gua$distritonuevo)

# Víctor: hay algo mal, tu distritonuevo no coincide con la columna disn2018. Tampoco los munn. Verifica por favor.

#write.csv(gua, file = "guaLoc.csv", row.names = FALSE) # Victor: cuando quede, usa éste archivo para sacar el insice s de cox y katz

# contestó
Hola profesor, 
estuve revisando individualmente los datos de los tres estados y parece ser que el distrito nuevo no corresponde a la columna de su archivo porque los distritos tienen diferente numeración. Sin embargo las secciones están correctas.
Mis distritos nuevos los obtuve de los pdf del portal del INE que llaman "descriptivo de la distritación local 2016" por lo que supuse que era el oficial actualizado; algunos números de municipios no coinciden pero esos si podrían ser mi problema ya que los obtuve de algunos catálogos posiblemebte desactualizados. Podría renombrar los distritos para que sean iguales en ambas columnas usando los datos que usted obtuvo,es buena idea?
Con lo anterior los datos estarían listos.
Saludos,
Victor Tamayo
--
Resp: La probabilidad de que el problema que describes ocurra en tus tres estados es ínfima dado que no ha ocurrido con nadie más. Debes estar haciendo algo mal. Revisa por favor.


# guerrero
colnames(gue1)
gue1 <- gue1[,c("edon","seccion","munn","escenario3_c8")]
colnames(gue1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Victor
head(gue2) 
colnames(gue2) <- c("seccion","munn","mun","disnViejo","distritonuevo") # victor: qué año electoral usaste para distritoviejo? úsalo para el nombre (temporal). Investiga también el año electoral inaugural del mapa abandonado (será el nombre definitivo) 
#gue2$munn <- NULL

# fusiona
gue <- merge(x = gue1, y = gue2, by = "seccion", all = TRUE)

dim(gue)
dim(gue1)
dim(gue2)

head(gue)

# Víctor: hay algo mal, mismo problema que con guanajuato

#write.csv(gue, file = "gueLoc.csv", row.names = FALSE) # Victor: cuando quede, usa éste archivo para sacar el insice s de cox y katz



# nuevo leon
colnames(nl1)
nl1 <- nl1[,c("edon","seccion","munn","escenario3")]
colnames(nl1) <- c("edon","seccion","munn","disn2018")

# cambia nombres en datos de Victor
head(nl2) 
colnames(nl2) <- c("seccion","munn","mun","disnViejo","distritonuevo") # victor: qué año electoral usaste para distritoviejo? úsalo para el nombre (temporal). Investiga también el año electoral inaugural del mapa abandonado (será el nombre definitivo) 
#nl2$munn <- NULL

# fusiona
nl <- merge(x = nl1, y = nl2, by = "seccion", all = TRUE)

dim(nl)
dim(nl1)
dim(nl2)

head(nl)

# Víctor: hay algo mal, no hay problema con los munn pero sí con disn2018 y tu nuevodistrito. Corrige por favor.

#write.csv(nl, file = "nlLoc.csv", row.names = FALSE) # Victor: cuando quede, usa éste archivo para sacar el insice s de cox y katz




rm(gua1,gua2,gue1,gue2,nl1,nl2) # limpieza
ls()


