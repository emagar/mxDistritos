wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ife.ine/")
tmp <- paste(wd, "equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", sep = "")
eq <- read.csv(tmp, stringsAsFactors = FALSE)

table(eq$action)
sel <- which(eq$action3=="split.from")

table(eq$alta[sel], useNA = "always")
table(eq$when[sel], useNA = "always")

sel2 <- which(eq$action3=="split.to" & is.na(eq$baja)==TRUE)
eq[sel2,][,c("edon","seccion","orig.dest3")]


