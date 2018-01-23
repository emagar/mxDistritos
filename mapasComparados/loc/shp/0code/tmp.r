orig <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cpsLoc.csv", stringsAsFactors = FALSE) # archivo Julia
j24 <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cpsLoc(copy)24Dist.csv", stringsAsFactors = FALSE) # archivo Julia
j25 <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cpsLoc(copy)25Dist.csv", stringsAsFactors = FALSE) # archivo Julia
jabc <- read.csv("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/cpsLoc(copy)ABC.csv", stringsAsFactors = FALSE) # archivo Julia

dim(orig)
dim(j24)
dim(j25)
dim(jabc)

orig <- orig[,c("seccion","escenario1")]
j24 <- j24[,c("seccion","escenario1")]

orig <- merge(x = orig, y = j24, by = "seccion", all = TRUE)
head(orig)

write.csv(orig, file = "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc/tmp.csv", row.names=FALSE)
