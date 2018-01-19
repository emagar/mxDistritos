#source(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/code/mapPrep.r") # sólo correr si hubiera cambios en los datos

rm(list = ls())
# get mun names and votes
load("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/aymu1977-present.RData")
munvot <- d
rm(list=setdiff(ls(), "munvot")) # clean
#
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/data/")
setwd(wd)
load(file="elDatForMaps.RData")
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/")
md <- c("/home/eric/Dropbox/data/mapas/cartografia28feb2013rojano/")
md2 <- c("~/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/maps/")
edo <- "oax"
edon <- 20

# geospatial data 
library(spdep); library(maptools)
# used to determine what datum rojano data has
library(rgdal)
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
se.map <- readOGR(dsn = tmp, layer = 'SECCION')
summary(se.map)
# projects to a different datum with long and lat
library(rgdal)
se.map <- spTransform(se.map, CRS("+proj=longlat +datum=WGS84"))

# read all state borders from rojano
ed.map <- list()
library(rgdal)
## tmp <- paste(md, "ags", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$ags <- tmp
## #
## tmp <- paste(md, "bc", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$bc <- tmp
## #
## tmp <- paste(md, "bcs", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$bcs <- tmp
## #
## tmp <- paste(md, "cam", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$cam <- tmp
## #
## tmp <- paste(md, "coa", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$coa <- tmp
## #
## tmp <- paste(md, "col", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$col <- tmp
#
tmp <- paste(md, "cps", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
ed.map$cps <- tmp
#
## tmp <- paste(md, "cua", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$cua <- tmp
## #
## tmp <- paste(md, "df", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$df <- tmp
## #
## tmp <- paste(md, "dgo", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$dgo <- tmp
## #
## tmp <- paste(md, "gua", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$gua <- tmp
#
tmp <- paste(md, "gue", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
ed.map$gue <- tmp
#
## tmp <- paste(md, "hgo", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$hgo <- tmp
## #
## tmp <- paste(md, "jal", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$jal <- tmp
## #
## tmp <- paste(md, "mex", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$mex <- tmp
## #
## tmp <- paste(md, "mic", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$mic <- tmp
## #
## tmp <- paste(md, "mor", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$mor <- tmp
## #
## tmp <- paste(md, "nay", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$nay <- tmp
## #
## tmp <- paste(md, "nl", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$nl <- tmp
#
tmp <- paste(md, "oax", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
ed.map$oax <- tmp
#
tmp <- paste(md, "pue", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
ed.map$pue <- tmp
#
## tmp <- paste(md, "que", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$que <- tmp
## #
## tmp <- paste(md, "qui", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$qui <- tmp
## #
## tmp <- paste(md, "san", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$san <- tmp
## #
## tmp <- paste(md, "sin", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$sin <- tmp
## #
## tmp <- paste(md, "son", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$son <- tmp
#
tmp <- paste(md, "tab", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
ed.map$tab <- tmp
#
## tmp <- paste(md, "tam", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$tam <- tmp
#
tmp <- paste(md, "tla", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
ed.map$tla <- tmp
#
tmp <- paste(md, "ver", sep = "") # archivo con mapas rojano
tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
# projects to a different datum with long and lat
tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
ed.map$ver <- tmp
#
## tmp <- paste(md, "yuc", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$yuc <- tmp
## #
## tmp <- paste(md, "zac", sep = "") # archivo con mapas rojano
## tmp <- readOGR(dsn = tmp, layer = 'ENTIDAD')
## # projects to a different datum with long and lat
## tmp <- spTransform(tmp, CRS("+proj=longlat +datum=WGS84"))
## ed.map$zac <- tmp

# read municipios
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
mu.map <- readOGR(dsn = tmp, layer = 'MUNICIPIO')
# projects to a different datum with long and lat
mu.map <- spTransform(mu.map, CRS("+proj=longlat +datum=WGS84"))
# read cabeceras municipales
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
cab <- readOGR(dsn = tmp, layer = 'CABECERA_MUNICIPAL')
# projects to a different datum with long and lat
cab <- spTransform(cab, CRS("+proj=longlat +datum=WGS84"))
#cab$LOCALIDAD_.1 # names
#
# edit manually to shorten mun names (*need to do this outside!!!*) and plug into mu.map
#tmp <- munvot[munvot$edon==13 & munvot$yr==2008,]
#tmp <- tmp[,c("mun","munn")]
## OJO: EL ORDEN NO JALA
## data.frame(N=mu.map$NOMBRE, M=mu.map$mun)
mu.map$mun <- c("S MARIA GUELACE",         
"TAMAZULAPAM",   
"S INES DEL MONTE",         
"S MIGUEL PERAS",          
"S DOMINGO ALBARRADAS",       
"S JUAN LAJARCIA",          
"S MATEO DEL MAR",          
"S PABLO CUATRO VENADOS",      
"S PEDRO HUILOTEPEC",         
"SALINA CRUZ",             
"S AGUSTIN TLACOTEPEC",        
"S MATEO PEÑASCO",        
"S PEDRO MOLINOS",          
"S MARIA MIXTEQUILLA",       
"BARRIO DE LA SOLEDAD",       
"TRINIDAD ZAACHILA",          
"VILLA DE ZAACHILA",          
"S MARIA YOSOYUA",         
"S JUAN TEITA",            
"S PABLO TIJALTEPEC",        
"S CATARINA TICUA",         
"S DOMINGO CHIHUITAN",       
"MAGDALENA TLACOTEPEC",         
"S PEDRO COMITANCILLO",       
"SANTIAGO LAOLLAGA",          
"S MARIA JALAPA",   
"S MARIA TOTOLAPILLA",       
"NEJAPA DE MADERO",          
"S DOMINGO ZANATEPEC",       
"S ANA TAVELA",          
"REFORMA DE PINEDA",          
"S FRANCISCO DEL MAR",        
"ASUNCION IXTALTEPEC",         
"S MIGUEL CHIMALAPA",        
"S DOMINGO INGENIO",        
"JUCHITAN",        
"S MARIA GUIENAGATI",        
"GUADALUPE DE RMZ",        
"ZIMATLAN",         
"S JUAN IHUALTEPEC",         
"S JUAN BAUTISTA CUICATLAN",     
"S BARTOLOME LOXICHA",        
"S MARIA HUATULCO",         
"SANTIAGO CHAZUMBA",          
"S CATARINA JUQUILA",        
"COSOLTEPEC",             
"CHALCATONGO DE HIDALGO",        
"S PEDRO Y SAN PABLO TEQUIXTEPEC",  
"S CATARINA ZAPOQUILA",       
"VILLA DE TUTUTEPEC DE MELCHOR OCAMPO",
"S MIGUEL EL GRANDE",         
"S ESTEBAN ATATLAHUCA",       
"S JUAN CACAHUATEPEC",        
"S MARIA YUCUHITI",        
"S CATARINA YOSONOTU",       
"SANTIAGO NUYOO",           
"S MARIA TECOMAVACA",        
"SANTIAGO IXTAYUTLA",         
"S LUCIA MONTEVERDE",        
"SANTIAGO TETEPEC",          
"TATALTEPEC DE VALDES",         
"ASUNCION CUYOTEPEJI",         
"SANTIAGO MILTEPEC",          
"SANTIAGO MINAS",           
"S JUAN BAUTISTA SUCHITEPEC",     
"SANTIAGO HUAJOLOTITLAN",       
"SANTIAGO LACHIGUIRI",         
"GUEVEA DE HUMBOLDT",         
"S MARIA COLOTEPEC",        
"TLACOTEPEC PLUMAS",          
"S MARIA IXCATLAN",         
"S FRANCISCO TEOPAN",        
"S PEDRO NOPALA",           
"TEOTONGO",              
"VALERIO TRUJANO",           
"S PEDRO JOCOTIPAC",         
"S MARIA TEXCATITLAN",       
"S PEDRO JALTEPETONGO",       
"S PEDRO MIXTEPEC",          
"S MARIA TLALIXTAC",        
"S FRANCISCO CHAPULAPA",       
"CHIQUIHUITLAN DE BENITO JUAREZ",   
"S BARTOLOME AYAUTLA",        
"S GABRIEL MIXTEPEC",        
"SANTOS REYES NOPALA",         
"S MARIA TEMAXCALTEPEC",      
"SANTIAGO YAITEPEC",          
"S JUAN LACHAO",           
"S PEDRO TEUTILA",          
"S ANDRES TEOTILALPAM",       
"S JUAN BAUTISTA TLACOATZINTEPEC",  
"S MARIA TATALTEPEC",       
"S JUAN DIUXI",            
"S LUCAS ZOQUIAPAM",         
"S MATEO YOLOXOCHITLAN",       
"ELOXOCHITLAN DE FLORES MAGON",    
"S CRUZ ACATEPEC",         
"S LORENZO CUAUNECUILTITLA",     
"S PEDRO OCOPETATILLO",        
"S ANA ATEIXTLAHUACA",       
"S CRUZ TACAHUA",          
"S MARIA YOLOTEPEC",        
"COSOLAPA",               
"VILLA DE TAMAZULAPAM DEL PROGRESO",  
"SANTIAGO YOSONDUA",          
"LA TRINIDAD VISTA HERMOSA",      
"S MAGDALENA JICOTLAN",       
"S MATEO TLAPILTEPEC",        
"SANTIAGO TEPETLAPA",          
"S ANTONIO ACUTLA",         
"S MIGUEL TULANCINGO",        
"S CRISTOBAL SUCHIXTLAHUACA",    
"VILLA TEJUPAM DE LA UNION",      
"SANTIAGO TAPEXTLA",          
"S DOMINGO TONALTEPEC",       
"S JUAN TEPOSCOLULA",        
"S PEDRO YUCUNAMA",          
"S JUAN COATZOSPAM",         
"S ANDRES LAGUNAS",          
"S DOMINGO YODOHINO",       
"S MIGUEL SUCHIXTEPEC",        
"VILLA DE CHILAPA DE DIAZ",      
"S MARCIAL OZOLOTEPEC",        
"S DIONISIO DEL MAR",        
"S MATEO RIO HONDO",         
"S PEDRO YANERI",          
"S JUAN CHICOMEZUCHIL",        
"SANTIAGO NILTEPEC",          
"TEOTITLAN DE FLORES MAGON",      
"S CATARINA IXTEPEJI",       
"S MIGUEL DEL RIO",          
"S MARTIN TOXPALAN",         
"S MATEO PIÑAS",          
"MAGDALENA TEQUISISTLAN",       
"S CATARINA LACHATAO",       
"S MARIA JALTIANGUIS",       
"S PEDRO EL ALTO",          
"S LORENZO VICTORIA",        
"S JUAN DEL RIO",           
"S DOMINGO TEPUXTEPEC",      
"S MARIA TEPANTLALI",        
"CIUDAD IXTEPEC",           
"ASUNCION CACALOTEPEC",         
"S PEDRO OCOTEPEC",         
"S JUAN JUQUILA MIXES",        
"S LUCAS CAMOTLAN",         
"S MIGUEL QUETZALTEPEC",       
"S MIGUEL SANTA FLOR",        
"S MARIA ALOTEPEC",         
"S ANA CUAUHTEMOC",        
"S PEDRO IXTLAHUACA",         
"S DOMINGO ROAYAGA",        
"S JUAN BAUTISTA LO DE SOTO",     
"S ANDRES IXTLAHUACA",        
"S ILDEFONSO VILLA ALTA",       
"S DOMINGO ARMENTA",        
"S MARIA CORTIJO",         
"S SEBASTIAN COATLAN",        
"S JOSE ESTANCIA GRANDE",       
"S MARIA COYOTEPEC",        
"TOTONTEPEC VILLA DE MORELOS",     
"S PABLO COATLAN",          
"S GERTRUDIS",           
"S MARIA PAPALO",         
"CONCEPCION PAPALO",          
"SANTOS REYES PAPALO",         
"YOGANA",                
"SANTIAGO CHOAPAM",          
"S DIONISIO OCOTLAN",         
"S ANA",              
"S CRUZ XITLA",           
"S PEDRO AMUZGOS",          
"S MARIA IPALAPA",         
"S AGUSTIN YATARENI",        
"SANTIAGO NACALTEPEC",         
"S JUAN PETLAPA",          
"SANTIAGO TAMAZOLA",          
"S ANDRES HUAXPALTEPEC",       
"S ANTONIO DE LA CAL",        
"S SEBASTIAN TUTLA",         
"S LORENZO",             
"AYOTZINTEPEC",            
"S ANTONINO CASTILLO VELASCO",    
"S AGUSTIN CHAYUCO",         
"S JUAN CHILATECA",          
"S MARTIN TILCAJETE",        
"ZAPOTITLAN LAGUNAS",          
"S JUAN QUIAHIJE",          
"SANTIAGO YUCUYACHI",          
"S MIGUEL PANIXTLAHUACA",      
"S MIGUEL AHUEHUETITLAN",       
"SANTIAGO LLANO GRANDE",        
"S ANDRES TEPETLAPA",         
"S MATEO NEJAPAM",          
"S JUAN BAUTISTA TLACHICHILCO",    
"S ANTONIO TEPETLAPA",        
"S FRANCISCO TLAPANCINGO",      
"S SEBASTIAN IXCAPA",        
"S CRUZ DE BRAVO",         
"CALIHUALA",              
"MARTIRES DE TACUBAYA",         
"S SEBASTIAN TECOMAXTLAHUACA",    
"COICOYAN DE LAS FLORES",        
"S JUAN QUIOTEPEC",
"S PEDRO YOLOX",
"S CATARINA MECHOACAN",
"S CRUZ MIXTEPEC",
"PINOTEPA DE DON LUIS",
"MAGDALENA APASCO",
"S BERNARDO MIXTEPEC",
"S JUAN COLORADO",
"VILLA DE ETLA",
"S PEDRO ATOYAC",
"GUADALUPE ETLA",
"S TOMAS MAZALTEPEC",
"S FELIPE TEJALAPAM",
"S LORENZO CACAOTEPEC",
"SOLEDAD ETLA",
"S JORGE NUCHITA",
"NAZARENO ETLA",
"REYES ETLA",
"S NICOLAS HIDALGO",
"S CRUZ TACACHE DE MINA",
"S ANDRES ZAUTLA",
"SANTOS REYES YUCUNA",
"S SIMON ZAHUATLAN",
"S MARTIN ZACATEPEC",
"MARISCALA DE JUAREZ",
"S PEDRO JICAYAN",
"S JERONIMO SILACAYOAPILLA",
"S MIGUEL AMATITLAN",
"S MIGUEL TLACAMAMA",
"S MARCOS ARTEAGA",
"S CATARINA LOXICHA",
"S AGUSTIN LOXICHA",
"S MARIA NDUAYACO",
"S JUAN ATEPEC",
"S PABLO MACUILTIANGUIS",
"SANTIAGO ATITLAN",
"S JUAN ACHIUTLA",
"ABEJONES",
"S BARTOLOME YUCUAÑE",
"S JUAN MAZATLAN",
"S JUAN COTZOCON",
"S BARTOLO COYOTEPEC",
"S JUAN TEPEUXILA",
"S JERONIMO TLACOCHAHUAYA",
"S SEBASTIAN ABASOLO",
"TLALIXTAC DE CABRERA",
"S AGUSTIN DE LAS JUNTAS",
"S ANDRES HUAYAPAM",
"S TOMAS JALIEZA",
"S MIGUEL ALOAPAM",
"S ANA YARENI",
"S JUAN BAUTISTA GUELACHE",
"S AGUSTIN ETLA",
"TEOTITLAN DEL VALLE",
"TLACOLULA DE MATAMOROS",
"S DIONISIO OCOTEPEC",
"S JUAN BAUTISTA VALLE NACIONAL",
"S PEDRO COXCALTEPEC CANTAROS",
"ASUNCION NOCHIXTLAN",
"S CRUZ AMILPAS",
"ACATLAN DE PEREZ FIGUEROA",
"S ANA DEL VALLE",
"SANTIAGO ZACATEPEC",
"ANIMAS TRUJANO",
"CAPULALPAM DE MENDEZ",
"SANTIAGO XIACUI",
"S JUAN COMALTEPEC",
"CUILAPAM DE GUERRERO",
"CIENEGA DE ZIMATLAN",
"AYOQUEZCO DE ALDAMA",
"ASUNCION OCOTLAN",
"S CRUZ ZENZONTEPEC",
"S JACINTO TLACOTEPEC",
"S DOMINGO TEOJOMULCO",
"S LORENZO TEXMELUCAN",
"S JERONIMO COATLAN",
"S CRUZ ITUNDUJIA",
"S MATEO CAJONOS",
"S JUAN EVANGELISTA ANALCO",
"S CATARINA CUIXTLA",
"SANTIAGO CACALOXTEPEC",
"TEZOATLAN DE SEGURA Y LUNA",
"S PEDRO Y SAN PABLO AYUTLA",
"SANTIAGO CAMOTLAN",
"S BALTAZAR LOXICHA",
"S PEDRO JUCHATENGO",
"S CATALINA QUIERI",
"S MARIA LACHIXIO",
"S FELIPE JALAPA DE DIAZ",
"S JOSE AYUQUILA",
"HEROICA CIUDAD DE EJUTLA DE CRESPO",
"FRESNILLO DE TRUJANO",
"SANTIAGO AYUQUILILLA",
"S MIGUEL DEL PUERTO",
"S JUAN MIXTEPEC",
"HEROICA CIUDAD DE TLAXIACO",
"SANTIAGO XANICA",
"S MARIA OZOLOTEPEC",
"S PEDRO MIXTEPEC",
"S PEDRO MARTIR QUIECHAPA",
"S MATEO YUCUTINDOO",
"MAGDALENA JALTEPEC",
"SANTIAGO TILANTONGO",
"PUTLA VILLA DE GUERRERO",
"S MARIA DEL ROSARIO",
"S FRANCISCO SOLA",
"SANTIAGO JUXTLAHUACA",
"S ANDRES ZABACHE",
"S MARTIN LACHILA",
"CONSTANCIA DEL ROSARIO",
"SANTIAGO HUAUCLILLA",
"S MARTIN ITUNYOSO",
"S LUIS AMATLAN",
"S MARIA ZOQUITLAN",
"S MARIA SOLA",
"S VICENTE LACHIXIO",
"S ANA TLAPACOYAN",
"S MIGUEL MIXTEPEC",
"S JUAN CIENEGUILLA",
"S FRANCISCO LACHIGOLO",
"CONCEPCION BUENAVISTA",
"MAGDALENA TEITIPAC",
"SANTIAGO TLAZOYALTEPEC",
"S MARIA PEÑOLES",
"TEPELMEME VILLA DE MORELOS",
"ZAPOTITLAN PALMAS",
"S JUAN YUCUITA",
"S MARIA CHACHOAPAM",
"S JUAN GUELAVIA",
"S CRUZ PAPALUTLA",
"S SEBASTIAN TEITIPAC",
"S JUAN TEITIPAC",
"S AGUSTIN ATENANGO",
"MESONES HIDALGO",
"SITIO DE XITLAPEHUA",
"S INES DE ZARAGOZA",
"S ANDRES DINICUITI",
"COATECAS ALTAS",
"S MARIA APAZCO",
"S JUAN LACHIGALLA",
"S MARIA ZANIZA",
"S MIGUEL TLACOTEPEC",
"S TOMAS TAMAZULAPAN",
"IXPANTEPEC NIEVES",
"SANTIAGO MATATLAN",
"S DOMINGO IXCATLAN",
"S LUCAS QUIAVINI",
"S BARTOLOME QUIALANA",
"SANTOS REYES TEPEJILLO",
"MAGDALENA PEÑASCO",
"S ANTONIO SINICAHUA",
"SANTIAGO AMOLTEPEC",
"S LUCIA MIAHUATLAN",
"S MIGUEL COATLAN",
"S MIGUEL TENANGO",
"S TOMAS OCOTEPEC",
"S CRUZ NUNDACO",
"S DOMINGO TONALA",
"S CRUZ TAYATA",
"S ANTONINO EL ALTO",
"S SEBASTIAN NICANANDUTA",
"S PEDRO HUAMELULA",
"SANTIAGO YOLOMECATL",
"S PEDRO Y SAN PABLO TEPOSCOLULA",
"S PABLO VILLA DE MITLA",
"S MARTIN HUAMELULPAM",
"S DOMINGO YANHUITLAN",
"VILLA DIAZ ORDAZ",
"S ANTONIO HUITEPEC",
"S ANTONINO MONTE VERDE",
"S JUAN ÑUMI",
"S DOMINGO TEHUANTEPEC",
"SANTIAGO NUNDICHE",
"S FRANCISCO CAHUACUA",
"S PEDRO MARTIR YUCUXACO",
"S ANDRES NUXIÑO",
"S PEDRO TAVICHE",
"S DOMINGO NUXAA",
"S MARIA TONAMECA",
"S JOSE LACHIGUIRI",
"HUAJUAPAN DE LEON",
"S JUAN TAMAZOLA",
"S MARIA QUIEGOLANI",
"S MIGUEL PIEDRAS",
"S PEDRO TEOZACOALCO",
"YUTANDUCHI DE GUERRERO",
"S FRANCISCO LOGUECHE",
"S MATEO SINDIHUI",
"S MIGUEL TEQUIXTEPEC",
"SANTIAGO IHUITLAN PLUMAS",
"S MARIA ECATEPEC",
"SANTIAGO PINOTEPA NACIONAL",
"ASUNCION TLACOLULITA",
"S FRANCISCO JALTEPETONGO",
"S JUAN BAUTISTA COIXTLAHUACA",
"S BARTOLO YAUTEPEC",
"S VICENTE NUÑU",
"S CRISTOBAL AMOLTEPEC",
"S BARTOLO SOYALTEPEC",
"SANTIAGO ASTATA",
"S JERONIMO TAVICHE",
"S MIGUEL ACHIUTLA",
"MAGDALENA YODOCONO",
"SANTIAGO NEJAPILLA",
"MAGDALENA ZAHUATLAN",
"S MATEO ETLATONGO",
"S JUAN SAYULTEPEC",
"SANTIAGO TILLO",
"S DOMINGO DE MORELOS",
"S ANDRES SINAXTLA",
"S AGUSTIN AMATENGO",
"S PEDRO TIDAA",
"S FRANCISCO NUXAÑO",
"S MIGUEL TECOMATLAN",
"S DOMINGO TLATAYAPAM",
"S FRANCISCO CHINDUA",
"S PEDRO TOPILTEPEC",
"S MARIA NATIVITAS",
"S MIGUEL CHICAHUA",
"LA COMPAÑIA",
"MAGDALENA OCOTLAN",
"S JOSE DEL PROGRESO",
"SANTIAGO APOALA",
"S LUCIA OCOTLAN",
"S MIGUEL HUAUTLA",
"S MARIA HUAZOLOTITLAN",
"SANTIAGO JAMILTEPEC",
"CANDELARIA LOXICHA",
"LA PE",
"S MARTIN DE LOS CANSECOS",
"S MIGUEL EJUTLA",
"TANICHE",
"S PEDRO MARTIR",
"S PEDRO APOSTOL",
"SANTIAGO TEXTITLAN",
"S JUAN OZOLOTEPEC",
"NATIVIDAD",
"S FRANCISCO OZOLOTEPEC",
"SANTIAGO ZOOCHILA",
"SANTIAGO LAXOPA",
"S CATARINA QUIANE",
"S ANA ZEGACHE",
"S DOMINGO OZOLOTEPEC",
"S FRANCISCO CAJONOS",
"S BALTAZAR YATZACHI EL BAJO",
"S BARTOLOME ZOOGOCHO",
"S ANDRES SOLAGA",
"S JUAN TABAA",
"S CATARINA QUIOQUITANI",
"S JUAN MIXTEPEC",
"S CRISTOBAL LACHIRIOAG",
"S PABLO HUIXTEPEC",
"S MARIA YALINA",
"S CRISTOBAL AMATLAN",
"SANTIAGO APOSTOL",
"S JUAN YAEE",
"SANTIAGO LALOPA",
"S JOSE DEL PEÑASCO",
"TANETZE DE ZARAGOZA",
"S INES YATZECHE",
"VILLA TALEA DE CASTRO",
"S MARIA TEMAXCALAPA",
"MAGDALENA MIXTEPEC",
"S JUAN YATZONA",
"S JUAN JUQUILA VIJANOS",
"S MIGUEL YOTAO",
"S ANDRES YAA",
"S MELCHOR BETAZA",
"VILLA HIDALGO",
"S PEDRO CAJONOS",
"S DOMINGO XAGACIA",
"S PABLO YAGANIZA",
"MIXISTLAN DE LA REFORMA",
"S LORENZO ALBARRADAS",
"S DOMINGO PETAPA",
"S MARIA CHIMALAPA",
"S PEDRO TAPANATEPEC",
"CHAHUITES",
"S MARIA TLAHUITOLTEPEC",
"S JUAN GUICHICOVI",
"SANTIAGO SUCHILQUITONGO",
"UNION HIDALGO",
"S MARIA CAMOTLAN",
"S MARIA DEL TULE",
"S MARIA ZACATEPEC",
"S JERONIMO SOSOLA",
"S FRANCISCO IXHUATAN",
"SANTIAGO TENANGO",
"S PABLO HUITZO",
"S FRANCISCO TELIXTLAHUACA",
"S NICOLAS",
"S SIMON ALMOLONGAS",
"SANTIAGO YAVEO",
"S CRUZ XOXOCOTLAN",
"OCOTLAN DE MORELOS",
"S ILDEFONSO AMATLAN",
"S SEBASTIAN RIO HONDO",
"S ANDRES PAXTLAN",
"SANTIAGO JOCOTEPEC",
"SANTIAGO COMALTEPEC",
"MIAHUATLAN",
"LOMA BONITA",
"S BLAS ATEMPA",
"MONJAS",
"S VICENTE COATLAN",
"LA REFORMA",
"S JUAN BAUTISTA JAYACATLAN",
"S JUAN DEL ESTADO",
"S MARTIN PERAS",
"MATIAS ROMERO AVENDAÑO",
"S MARIA PETAPA",
"S JUAN LALANA",
"CUYAMECALCO",
"S CATARINA TAYATA",
"S CARLOS YAUTEPEC",
"YAXE",
"S MIGUEL TILQUIAPAM",
"S CATARINA MINAS",
"S PEDRO TOTOLAPAM",
"S PEDRO QUIATONI",
"S DOMINGO TOMALTEPEC",
"S JUAN BAUTISTA ATATLAHUCA",
"SANTIAGO IXCUINTEPEC",
"ROJAS DE CUAUHTEMOC",
"S PEDRO SOCHIAPAM",
"S JOSE CHILTEPEC",
"S MIGUEL SOYALTEPEC",
"S JOSE INDEPENDENCIA",
"S PEDRO IXCATLAN",
"S JUAN BAUTISTA TUXTEPEC",
"S MARIA CHILCHOTLA",
"HUAUTLA",
"S MARIA LA ASUNCION",
"HUAUTEPEC",
"S LUCAS OJITLAN",
"S FELIPE USILA",
"SANTIAGO TEXCALCINGO",
"S MARIA TEOPOXCO",
"S FRANCISCO HUEHUETLAN",
"S JERONIMO TECOATL",
"S JUAN DE LOS CUES",
"S ANTONIO NANAHUATIPAM",
"SANTIAGO DEL RIO",
"S ANDRES CAB NUEVA",
"S ILDEFONSO SOLA",
"VILLA SOLA DE VEGA",
"S PEDRO POCHUTLA",
"PLUMA HIDALGO",
"EL ESPINAL",
"S MARIA XADANI",
"IXTLAN",
"GUELATAO",
"S MARIA YAVESIA",
"S MIGUEL AMATLAN",
"TEOCOCUILCO DE MARCOS PEREZ",
"NUEVO ZOQUIAPAM",
"S PABLO ETLA",
"OAXACA",
"S LUCIA DEL CAMINO",
"S JACINTO AMILPAS",
"S MARIA ATZOMPA",
"MAZATLAN VILLA DE FLORES",
"S MARIA JACATEPEC",
"SILACAYOAPAM",
"S JOSE TENANGO",
"S BALTAZAR CHICHICAPAM",
"S RAYMUNDO JALPAN")

# read distritos
# a. from seccion2dis map, in order to export into se.map for sub-setting
sec2dis <- read.csv("/home/eric/Dropbox/data/mapas/reseccionamiento/equivSecc/tablaEquivalenciasSeccionales1994-2010.2013.csv", stringsAsFactors = FALSE)
sec2dis <- sec2dis[sec2dis$edon == 20,]
# send to seccion map
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = sec2dis, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("SECCION","dis2012")]
se.map$disn <- tmp$dis2012
rm(tmp)
# di.map <- unionSpatialPolygons(se.map, se.map$disn) # proper way to get federal district objects... if only seccion shapefiles had no problems
#
# b. from rojano's distrito map, which has good-looking shapefiles
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
di.map <- readOGR(dsn = tmp, layer = 'DISTRITO')
# projects to a different datum with long and lat
di.map <- spTransform(di.map, CRS("+proj=longlat +datum=WGS84"))
# read cabeceras distritales (via vocal ejecutivo)
tmp <- paste(md, edo, sep = "") # archivo con mapas rojano
cabDis <- readOGR(dsn = tmp, layer = 'VOCAL_EJECUTIVO_DISTRITAL')
# projects to a different datum with long and lat
cabDis <- spTransform(cabDis, CRS("+proj=longlat +datum=WGS84"))
#
cabDisNames <- read.csv(paste(wd, "cabeceras2006.csv", sep = ""), stringsAsFactors = FALSE)

# add ncasillas in 2012 to seccion map
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = ncasillas[ncasillas$edon== 20, c("seccion","e12")], by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("SECCION","e12")]; 
se.map$ncasillas <- tmp$e12
# make colors
library(RColorBrewer)
nclr <- 6                                    #CATEGORÍAS DE COLOR (MIN=3 MAX=9)
purples <- brewer.pal(nclr,"Purples")            #GENERA CODIGOS DE COLOR QUE CRECEN CON GRADO
#display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, colorblindFriendly=TRUE)
library(plyr)
se.map$ncascol[se.map$ncasillas==1] <- purples[1]
se.map$ncascol[se.map$ncasillas==2] <- purples[2]
se.map$ncascol[se.map$ncasillas>=3  & se.map$ncasillas<=5]  <- purples[3]
se.map$ncascol[se.map$ncasillas>=6  & se.map$ncasillas<=10] <- purples[4]
se.map$ncascol[se.map$ncasillas>=11 & se.map$ncasillas<=20] <- purples[5]
se.map$ncascol[se.map$ncasillas>=21]                        <- purples[6]

# add nwin to seccion map
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = nwin[nwin$edon == 20,], by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden), c("SECCION","pan","pri","prd")]
se.map$nwinpan <- tmp$pan
se.map$nwinpri <- tmp$pri
se.map$nwinprd <- tmp$prd
rm(tmp)
#
# make colors
library(RColorBrewer)
nclr <- 7                                    #CATEGORÍAS DE COLOR (MIN=3 MAX=9)
blues <- brewer.pal(nclr,"Blues")            #GENERA CODIGOS DE COLOR QUE CRECEN CON GRADO
reds <- brewer.pal(nclr,"Reds")              
yellows <- brewer.pal(nclr,"YlOrBr")         
library(plyr)
se.map$pancol <- mapvalues ( se.map$nwinpan, from = 0:6, to = blues )
se.map$pricol <- mapvalues ( se.map$nwinpri, from = 0:6, to = reds )
se.map$prdcol <- mapvalues ( se.map$nwinprd, from = 0:6, to = yellows )
# bastiones = 4+ wins
se.map$bastion <- rgb(190,190,190, maxColorValue = 255) # "gray"
## se.map$bastion[se.map$nwinpan>=4] <- "blue"
## se.map$bastion[se.map$nwinpri>=4] <- "red"
## se.map$bastion[se.map$nwinprd>=4] <- "gold"
se.map$bastion[se.map$nwinpan==4] <- blues[4]
se.map$bastion[se.map$nwinpan==5] <- blues[5]
se.map$bastion[se.map$nwinpan==6] <- blues[6]
se.map$bastion[se.map$nwinpri==4] <- reds[4]
se.map$bastion[se.map$nwinpri==5] <- reds[5]
se.map$bastion[se.map$nwinpri==6] <- reds[6]
se.map$bastion[se.map$nwinprd==4] <- yellows[4]
se.map$bastion[se.map$nwinprd==5] <- yellows[5]
se.map$bastion[se.map$nwinprd==6] <- yellows[6]

# add disloc
### SIGUIENTE RENGLON FALTA
tmp <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/disloc/oax.csv", stringsAsFactors = FALSE) # 2015 districts
tmp$disloc <- tmp$disn; tmp$disn <- NULL
tmp <- merge(x = se.map@data, y = tmp, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
## table(is.na(tmp$disloc))             # verify missing
## tmp$SECCION[is.na(tmp$disloc)==TRUE] # verify missing
se.map$disloc <- tmp$disloc#; se.map$cabloc <- tmp$cabloc
#
# add seccion volat 2012-2105 = max change
# add lisnom2015
tmp1 <- v12[v12$edon==edon,]
tmp1$pri <- tmp1$pri + tmp1$pric; tmp1 <- tmp1[, c("seccion","pan","pri","prdc","pvem","panal")]
tmp1$pt <- tmp1$mc <- tmp1$morena <- tmp1$ph <- tmp1$ps <- tmp1$indep1 <- tmp1$indep2 <- 0
tmp1 <- tmp1[, c("seccion","pan","pri","prdc","pvem","pt","mc","panal","morena","ph","ps","indep1","indep2")]
tmp1[,-1] <- round(tmp1[,-1] / v12[v12$edon==edon, c("efec")], 3) # vote shares
#
tmp2 <- v15[v15$edon==edon,]
tmpv <- tmp2[,c("seccion","lisnom")] # save to add lisnom below
tmp2$pri <- tmp2$pri + tmp2$pric;
tmp2$prd <- tmp2$prd + tmp2$prdc; tmp2 <- tmp2[, c("seccion","pan","pri","prd","pvem","pt","mc","panal","morena","ph","ps","indep1","indep2")]
tmp2[,-1] <- round(tmp2[,-1] / v15[v15$edon==edon, c("efec")], 3) # vote shares
#
tmp <- apply(X = abs(tmp2[,-1] - tmp1[,-1]), MARGIN = 1, FUN = max) # absolute 2015-2012 vote shares by party
tmp3 <- cbind(seccion=tmp1$seccion, volat=tmp, lisnom=tmpv$lisnom)
#
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = tmp3, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
se.map$volat1215 <- tmp$volat
se.map$ln15 <- tmp$lisnom
#
# add seccion fch, epn, amlo
# handy function to sort one data frame by order of another, matching data frame
sortBy <- function(target, By){
    t <- target; b <- By;
    do.call(rbind, lapply(seq_len(nrow(b)), 
            function(i) as.character(unlist(t[i,])[order(unlist(-b[i,]))]))) # change to -b if decreasing wished
}
# 2006
tmpv <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/elecReturns/prSeccion2006.csv", stringsAsFactors = FALSE)
tmpv <- tmpv[tmpv$edon==edon,]
tmpv$edon <- tmpv$disn <- tmpv$munn <- tmpv$id_elec <- tmpv$nr <- tmpv$nul <- tmpv$tot <- tmpv$lisnom <- NULL
colnames(tmpv)[grep("pan$", colnames(tmpv))] <- "fch"
colnames(tmpv)[grep("apm", colnames(tmpv))] <- "pri"
colnames(tmpv)[grep("pbt", colnames(tmpv))] <- "amlo"
tmpv2 <- tmpv[,-1] # votes only
etiq <- data.frame(matrix(rep(colnames(tmpv2), nrow(tmpv2)), nrow=nrow(tmpv2), byrow = TRUE), stringsAsFactors = FALSE)
etiq <- sortBy(target = etiq, By = tmpv2)
tmpv2 <- t(apply(tmpv2, 1, function(x) sort(x, decreasing = TRUE)))
#
pres <- data.frame(seccion=tmpv$seccion, win06p=etiq[,1], mg06p=round( (tmpv2[,1] - tmpv2[,2])/rowSums(tmpv2), 3), fch=round(tmpv$fch/rowSums(tmpv2), 3), amlo06=round(tmpv$amlo/rowSums(tmpv2), 3))
#
tmp <- data.frame(SECCION = se.map$SECCION)
tmp$orden <- 1:nrow(tmp)
tmp <- merge(x = tmp, y = pres, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
#
# 2012
tmpv <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/elecReturns/prSeccion2012.csv", stringsAsFactors = FALSE)
tmpv <- tmpv[tmpv$edon==edon,]
tmpv$edon <- tmpv$disn <- tmpv$munn <- tmpv$urbrur <- tmpv$nr <- tmpv$nul <- tmpv$lisnom <- NULL
tmpv$amlo <- tmpv$prd + tmpv$pt + tmpv$mc + tmpv$prdptmc + tmpv$prdpt + tmpv$prdmc + tmpv$ptmc
tmpv$epn <- tmpv$pri + tmpv$pvem + tmpv$pripvem
tmpv$prd <- tmpv$pt <- tmpv$mc <- tmpv$prdptmc <- tmpv$prdpt <- tmpv$prdmc <- tmpv$ptmc <- tmpv$pri <- tmpv$pvem <- tmpv$pripvem <- NULL
tmpv2 <- tmpv[,-1] # votes only
etiq <- data.frame(matrix(rep(colnames(tmpv2), nrow(tmpv2)), nrow=nrow(tmpv2), byrow = TRUE), stringsAsFactors = FALSE)
etiq <- sortBy(target = etiq, By = tmpv2)
tmpv2 <- t(apply(tmpv2, 1, function(x) sort(x, decreasing = TRUE)))
#
pres <- data.frame(seccion=tmpv$seccion, win12p=etiq[,1], mg12p=round( (tmpv2[,1] - tmpv2[,2])/rowSums(tmpv2), 3), epn=round(tmpv$epn/rowSums(tmpv2), 3), amlo12=round(tmpv$amlo/rowSums(tmpv2), 3))
tmp <- merge(x = tmp, y = pres, by.x = "SECCION", by.y = "seccion", all.x = TRUE, all.y = FALSE)
tmp <- tmp[order(tmp$orden),]
#
se.map@data <- cbind(se.map@data, tmp)
head(se.map@data)
rm(tmp, tmp1, tmp2, tmp3, tmpv)

# add district ptot, rri proj at each election
load(file = "/home/eric/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/votPobDis0018.RData")
tmp <- votPobDis0018$pob.distMap2006
tmp <- tmp[tmp$edon==edon,]
tmp1 <- di.map@data
tmp1$ord <- 1:nrow(tmp1)
tmp1 <- merge(x=tmp1, y=tmp, by.x="DISTRITO", by.y="disn")
tmp1[, grep("rri", colnames(tmp1))] <- round(tmp1[, grep("rri", colnames(tmp1))],2)
tmp1 <- tmp1[order(tmp1$ord), grep("ptot|rri", colnames(tmp1))]
di.map@data <- cbind(di.map@data, tmp1)
di.map$disrri06 <- paste(di.map$DISTRITO, " (", di.map$rris2006, ")", sep="")
di.map$disrri15 <- paste(di.map$DISTRITO, " (", di.map$rris2015, ")", sep="")
                         
# export attributes for maps with other software
se.map$bastion2 <- "swing"
se.map$bastion2[se.map$nwinpan==4] <- "pan4"
se.map$bastion2[se.map$nwinpan==5] <- "pan5"
se.map$bastion2[se.map$nwinpan==6] <- "pan6"
se.map$bastion2[se.map$nwinpri==4] <- "pri4"
se.map$bastion2[se.map$nwinpri==5] <- "pri5"
se.map$bastion2[se.map$nwinpri==6] <- "pri6"
se.map$bastion2[se.map$nwinprd==4] <- "izq4"
se.map$bastion2[se.map$nwinprd==5] <- "izq5"
se.map$bastion2[se.map$nwinprd==6] <- "izq6"
#
# add centroids
tmp <- coordinates(se.map)
tmp <- data.frame(seccion=se.map$SECCION, edon=se.map$ENTIDAD, disn=se.map$disn, munn=se.map$MUNICIPIO, bastion=se.map$bastion2, ncasillas=sqrt(se.map$ncasillas), lon=tmp[,1], lat=tmp[,2], stringsAsFactors = FALSE)
write.csv(tmp, file = paste(md, edo, "/magar.csv", sep = "") )
tmp <- c("\"Integer\"",    "\"Integer\"",    "\"Integer\"",      "\"Integer\"",       "\"Integer\"",        "\"String\"",          "\"Integer\"",                   "\"Real\"", "\"Real\"",        "\"Integer\"",      "\"String\"")
write(tmp, file = paste(md, edo, "/magar.csvt", sep = ""), ncolumns = length(tmp), sep = "," )

# export for google earth
#v1
## library(maptools)
## gd <- paste(md2, "kml/", sep = ""); setwd(gd)
## kmlPolygons(obj = se.map, kmlfile = "mexseccion.kml", border = "white", col = sub("#", "#FF", se.map$bastion))
#v2
library(plotKML)
setwd(paste(md2, "kml/", sep = ""))
#plotKML(se.map, filename = "mexseccion.kml", fill = col2kml(se.map$bastion), open.kml = FALSE)

kml_open(file.name = paste(edo, ".kml", sep = ""), folder.name = edo, kml_open = FALSE, kml_visibility = FALSE)
#
# Bastiones
tmp <- subset(se.map, bastion2 == "swing")
kml_layer.SpatialPolygons(tmp, colour = "gray", subfolder.name = "swingSec", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pan4")
kml_layer.SpatialPolygons(tmp, colour = "royalblue2", subfolder.name = "corePan4", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pan5")
kml_layer.SpatialPolygons(tmp, colour = "royalblue3", subfolder.name = "corePan5", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pan6")
kml_layer.SpatialPolygons(tmp, colour = "royalblue4", subfolder.name = "corePan6", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pri4")
kml_layer.SpatialPolygons(tmp, colour = "red1", subfolder.name = "corePri4", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pri5")
kml_layer.SpatialPolygons(tmp, colour = "red2", subfolder.name = "corePri5", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "pri6")
kml_layer.SpatialPolygons(tmp, colour = "red3", subfolder.name = "corePri6", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "izq4")
kml_layer.SpatialPolygons(tmp, colour = "gold1", subfolder.name = "coreLeft4", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "izq5")
kml_layer.SpatialPolygons(tmp, colour = "gold2", subfolder.name = "coreLeft5", altitudeMode = "clampToGround", alpha = .7)
tmp <- subset(se.map, bastion2 == "izq6")
kml_layer.SpatialPolygons(tmp, colour = "gold3", subfolder.name = "coreLeft6", altitudeMode = "clampToGround", alpha = .7)
#
# Estado
tmp <- as(ed.map$oax, "SpatialLinesDataFrame") # coerce polygons to lines 
kml_layer.SpatialLines(tmp, colour = "hotpink", width = 2.5, subfolder.name = "stateLimit", altitudeMode = "clampToGround")
#
# Distritos federales 2006
tmp <- as(di.map, "SpatialLinesDataFrame") # coerce polygons to lines 
kml_layer.SpatialLines(tmp, colour = "white", width = 2, subfolder.name = "fedDist2006map", altitudeMode = "clampToGround")
tmp <- SpatialPointsDataFrame(coords=coordinates(di.map), data = di.map@data, proj4string = CRS("+proj=longlat +datum=WGS84"))
kml_layer.SpatialPoints(tmp, LabelScale = .75, extrude = TRUE, labels = disrri06, subfolder.name = "fedDistRRI@2006", altitudeMode = "clampToGround")
kml_layer.SpatialPoints(tmp, LabelScale = .75, extrude = TRUE, labels = disrri15, subfolder.name = "fedDistRRI@2015", altitudeMode = "clampToGround")
#
# Municipios
tmp <- as(mu.map, "SpatialLinesDataFrame") # coerce polygons to lines
kml_layer.SpatialLines(tmp, colour = "springgreen", width = 1, subfolder.name = "municLimit", altitudeMode = "clampToGround")
tmp <- SpatialPointsDataFrame(coords=coordinates(mu.map), data = mu.map@data, proj4string = CRS("+proj=longlat +datum=WGS84"))
kml_layer.SpatialPoints(tmp, shape = "", labels = mun, subfolder.name = "municNames", altitudeMode = "clampToGround", LabelScale = .5)
#
# Lista nominal 2015
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
tmp <- SpatialPointsDataFrame(coords=coordinates(se.map), data = se.map@data, proj4string = CRS("+proj=longlat +datum=WGS84"))
tmp$ln15k <- round(tmp$ln15/1000, 1) # lista nominal in thousands for labels
tmp$sqrtln15 <- sqrt(tmp$ln15) # lista nominal sq root
#kml_layer.SpatialPoints(tmp, shape = shape, labels = ln15k, colour = ln15, colour_scale = SAGA_pal$SG_COLORS_WHITE_GREEN, extrude = TRUE, altitude = ln15, altitudeMode = "relativeToGround", subfolder.name = "barras") 
kml_layer.SpatialPoints(tmp, shape = shape, labels = "", colour = "green", size = sqrtln15, alpha = .33, altitudeMode = "clampToGround", subfolder.name = "listaNominal@2015") 
kml_layer.SpatialPoints(tmp, shape = "", labels = ln15k, colour = "white", altitudeMode = "clampToGround", subfolder.name = "listaNominal@2015x1000") 
#
# añadir fch, amlo, epn
#
kml_close(file.name = paste(edo, ".kml", sep = ""))

# display.pal(SAGA_pal[1:5]) # <- useful to pick colors

# compress kml files to kmz
md2 <- c("/home/eric/Dropbox/data/elecs/MXelsCalendGovt/atlasDis/maps/")
library(plotKML)
setwd(paste(md2, "kml/", sep = ""))
kml_compress("oax.kml")

# print the result:
library(XML)
xmlRoot(xmlTreeParse("tmp.kml"))[["Document"]][1:100]

# grafica distritos 1 por 1
portray <- se.map$bastion  # elegir qué reportará el mapa 2
portray2 <- se.map$ncascol # elegir qué reportará el mapa 3
dn <- 11                  # elegir un distrito

## for (dn in 1:max(di.map$DISTRITO)){
##     print(paste("disn =", dn))
## # plot state map with highlighted district
## pdf(file = paste(md2, edo, dn, "-1.pdf", sep = ""))
par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
plot(ed.map$oax, axes = TRUE, main = "Oaxaca (mapa 2006)")
plot(ed.map$cps, add = TRUE, lty = 3)
plot(ed.map$gue, add = TRUE, lty = 3)
#plot(ed.map$pue, add = TRUE, lty = 3)
plot(ed.map$ver, add = TRUE, lty = 3)
# 
plot(di.map, add = TRUE, border = "gray")
plot(di.map[di.map$DISTRITO==dn,], add = TRUE, border = "gray", col = "gray")
# thick state border
plot(ed.map$oax, add = TRUE, lwd = 3)
plot(ed.map$oax, add = TRUE, border = "red", lty = 3, lwd = 2)
## points(cabDis, pch = 3) # cabeceras distritales
## points(cabDis)
## points(cabDis, pch = 19, cex = .75, col = "orange")
text(coordinates(di.map), labels=di.map$DISTRITO, cex=.85)
# add neighboring states
text( x = -94.55, y = 19, labels = "G O L F O   D E   M E X I C O", cex = .9, col = "deepskyblue", srt = -30 )
text( x = -96.25, y = 15.15, labels = "O C E A N O   P A C I F I C O", cex = .9, col = "deepskyblue", srt = 0 )
text( x = -93.8, y = 16.6, labels = "CHIAPAS", col = "darkgray", cex = .9, srt = 90 )
text( x = -98.6, y = 17.15, labels = "GUERRERO", col = "darkgray", cex = .9, srt = 90 )
text( x = -98, y = 18.5, labels = "PUEBLA", col = "darkgray", cex = .9, srt = 25 )
text( x = -95.5, y = 18.2, labels = "VERACRUZ", col = "darkgray", cex = .9, srt = -30 )
## dev.off()

# plot same distrito only
# need to merge disn info into mun and sec object, in order to select just those belonging to dis
## pdf(file = paste(md2, edo, dn, "-2.pdf", sep = ""))
par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
tmp <- cabDisNames$cab[which(cabDisNames$edon == 20 & cabDisNames$disn==dn)]
plot(di.map[di.map$DISTRITO==dn,], axes = TRUE, main = paste("Oaxaca", dn, "-", tmp))
plot(ed.map$oax, add = TRUE)
plot(se.map[se.map$disn==dn,], add = TRUE, border = "darkgray", col = portray[se.map$disn==dn]) # color nwin
#plot(ffcc, add = TRUE, lwd = .75)
#
plot(ed.map$cps, add = TRUE, lty = 1)
plot(ed.map$gue, add = TRUE, lty = 1)
#plot(ed.map$pue, add = TRUE, lty = 1)
plot(ed.map$ver, add = TRUE, lty = 1)
#
plot(di.map[di.map$DISTRITO==dn,], add = TRUE, lwd = 4)
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
plot(ed.map$oax, add = TRUE, lwd = 3)
plot(ed.map$oax, add = TRUE, border = "red", lty = 3, lwd = 2)
## points(coordinates(cab), pch = 19, col = "white", cex = .5)
## points(coordinates(cab), pch = 1, col = "green", cex = .75)
text(coordinates(mu.map), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(mu.map), labels=mu.map$mun, cex=.5)
## dev.off()
#legend(x="bottomleft", legend=rev(0:6), fill=rev(reds), bty="o", cex=.85)

# plot same distrito's nCasillas
loc <- c("topright","topleft","topright","bottomleft","bottomright",    #  5
         "topright","topright","bottomright","topright","bottomleft",   # 10
         "topright")
## pdf(file = paste(md2, edo, dn, "-3.pdf", sep = ""))
par(mar=c(2,2,2,1)) ## SETS B L U R MARGIN SIZES
tmp <- cabDisNames$cab[which(cabDisNames$edon == 20 & cabDisNames$disn==dn)]
plot(di.map[di.map$DISTRITO==dn,], axes = TRUE, main = paste("Oaxaca", dn, "-", tmp))
plot(ed.map$oax, add = TRUE)
plot(se.map[se.map$disn==dn,], add = TRUE, border = "darkgray", col = portray2[se.map$disn==dn]) # color nwin
#plot(ffcc, add = TRUE, lwd = .75)
#
plot(ed.map$cps, add = TRUE, lty = 1)
plot(ed.map$gue, add = TRUE, lty = 1)
#plot(ed.map$pue, add = TRUE, lty = 1)
plot(ed.map$ver, add = TRUE, lty = 1)
#
plot(di.map[di.map$DISTRITO==dn,], add = TRUE, lwd = 4)
plot(mu.map, add = TRUE, border = "green", lwd = 1)
plot(mu.map, add = TRUE, lwd = 1, lty = 3)
plot(ed.map$oax, add = TRUE, lwd = 3)
plot(ed.map$oax, add = TRUE, border = "red", lty = 3, lwd = 2)
points(coordinates(cab), pch = 19, col = "white", cex = .5)
points(coordinates(cab), pch = 1, col = "green", cex = .75)
text(coordinates(mu.map), labels=mu.map$mun, cex=.51, col = "green")
text(coordinates(mu.map), labels=mu.map$mun, cex=.5)
legend(x=loc[dn], legend=c("21+","11-20","6-10","3-5","2","1"), fill=rev(purples), bty="o", bg = "white", cex=.85, title = "N casillas")
## dev.off()
## }

