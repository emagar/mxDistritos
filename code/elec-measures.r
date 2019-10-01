# code reads and prepares electoral measures for maps and district analysis

dd <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/fed/data"
 

load(paste(dd, "elDatForMaps.RData", sep = "/"))
ls()
vot <- read.csv(, stringsAsFactors = FALSE)
head(vot)
