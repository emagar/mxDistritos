

apply(d.ss[,c("disn2005","disn2011","disn2017")] , 2, function(x)unique(x[!is.na(x)])) # identifica valores unicos en cada columna de distritos


write.csv(d, file = "coaLoc2.csv", row.names = FALSE)
