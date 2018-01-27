setwd("/home/eric/Desktop/MXelsCalendGovt/redistrict/subnat/disloc/")

de14 <- read.csv(file = "nay2014demarc.csv", stringsAsFactors = FALSE)
de17 <- read.csv(file = "nayDemarcaciones.csv", stringsAsFactors = FALSE)

de <- merge(x = de14, y = de17, by = "seccion", all = TRUE)

str(de14)
str(de17)
str(de)

table(is.na(de$inegi))

data.frame(de$mun.x, de$mun.y)

de$munn <- NULL
colnames(de)[2] <- "munn"

table(de$inegi==de$munn)

write.csv(de, file = "nayDem1417.csv", row.names = FALSE)
