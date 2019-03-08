file <- "/home/eric/Downloads/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/cps/escenario1/escenario1_JM.shp"

library(raster)
x <- shapefile(file)
crs(x)
x$area_sqkm <- area(x) / 1000000
