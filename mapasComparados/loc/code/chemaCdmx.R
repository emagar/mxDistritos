rm(list = ls())

############################################
# Título: Redistritación Ciudad de México  #
# Autor: Josemaría Macedo Carrillo         #
# Fecha: 30-10-17                          #
# Materia: Seminario de Investigación D    #
############################################ 

# Instalo paquetes necesarios
install.packages(c("foreign", "readxl", "tidyverse", "dplyr"))

require (foreign)
require (readxl)
require (tidyverse)
require (dplyr)

# Establezco directorios
#inp <- "/Users/josemariamacedo/Desktop/ITAM/8vo semestre/Seminario D/INP"
#out <- "/Users/josemariamacedo/Desktop/ITAM/8vo semestre/Seminario D/OUT"
inp <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/fuenteAlumnos/"
out <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/"

# Abrir bases de datos
dist15 <- read_xls(paste(inp,"bdxsecc.xls", sep = "/"))
dist17 <- read_xlsx (paste(inp, "distritacion 2017 CDMX.xlsx", sep = "/"))

# Cambio nombres de variables de base de distritos en 2015 por nombres de renglón 7 y elimino renglones con nombres innecesarios
names(dist15) <- dist15[7,]
dist15 = dist15[c(-1:-7),]
dist15 = dist15[c(-5537:-5540),]
View(dist15)

# Pongo mismo nombre a variable de sección en ambas bases de datos para juntarlas
colnames(dist15)[5] <- "seccion"
colnames(dist17)[1] <- "seccion"

# Juntamos las dos bases de datos en una nueva base llamada "distritos"
distritos <- merge(x = dist15, y = dist17, by = "seccion", all = TRUE)

# Ordenar por distrito y sección
distritos <- arrange (distritos, distritos$`Distrito Local`, distritos$`Distrito 2017`, distritos$seccion)
View(distritos)

# Quito variables que no necesito y renombro otras
distritos <- distritos[,c("seccion", "Clave de la Delegación", "Distrito Local", "Distrito 2017")]

colnames(distritos)[2] <- "delegacion 2015"
colnames(distritos)[3] <- "distrito 2015"
colnames(distritos)[4] <- "distrito 2017"

# Cambiar números romanos de distritos por números arábigos
distritos$`distrito 2015` <- as.roman(distritos$`distrito 2015`)
distritos$`distrito 2017` <- as.roman(distritos$`distrito 2017`)
str(distritos)

distritos$`distrito 2015` <- as.numeric(distritos$`distrito 2015`)
distritos$`distrito 2017` <- as.numeric(distritos$`distrito 2017`)
str(distritos)



# dsi visto desde la perspectiva del hijo
# "padre" de nuevo distrito e índice de similaridad de distrito (dsi), cf. Cox & Katz
distritos$father15 <- NA
distritos$dsi <- 0
for (i in 1:33){
  sel.n <- which(distritos$`distrito 2017`==i)                  # secciones en nuevo distrito
  tmp <- table(distritos$`distrito 2015`[sel.n])
  target <- as.numeric(names(tmp)[tmp==max(tmp)]) 
  distritos$father15[sel.n] <- target
  sel.f <- which(distritos$`distrito 2015`==target) # secciones en distrito padre
  sel.c <- intersect(sel.n, sel.f)             # secciones comunes entre padre y nuevo distrito
  distritos$dsi[sel.n] <- round( length(sel.c) / (length(sel.f) + length(sel.n) - length(sel.c)) , 3 )
}

distritos <- arrange (distritos, distritos$`distrito 2017`, distritos$`distrito 2015`, distritos$seccion)

#Exportamos nueva base con distrito padre y porcentaje de sección en común
write.csv(distritos, paste(out, "cdmxLoc.csv", sep="/"))

