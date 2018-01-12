setwd("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc")
dj <- read.csv("cpsLoc.csv", stringsAsFactors = FALSE)
colnames(dj)
dj <- dj[, -c(1,3)]


setwd("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc")
d <- read.csv("cpsLoc.csv", stringsAsFactors = FALSE)
head(d)

d2 <- merge(x = d, y = dj, by = "seccion", all = TRUE)
colnames(d2)
dim(dj)
dim(d)
dim(d2)


sel <- which(dj$seccion %in% d$seccion)
dj$seccion[-sel]

sel <- which(d$seccion %in% dj$seccion)
d$seccion[-sel]

setwd("/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/redisProcess/ineRedist2017/deJsonConEtiquetas/loc")
write.csv(d2, file = "cpsLoc.csv", row.names=FALSE)
