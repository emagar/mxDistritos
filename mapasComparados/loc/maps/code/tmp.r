dn <- 1



dn <- dn + 1
plot(dl.map[dl.map$disloc==dn,], axes = TRUE, main = paste("Ciudad de México ", dn, "-", tmp, " (DSI = ", tmp2, ")", sep = ""))
sel <- which(dl2012.map$disloc==dl.map$father[dl.map$disloc==dn])
plot(dl2012.map[sel,], add = TRUE, lwd = 6, border = "red")
legend(x=lp[dn], bg = "white", legend=c("distrito","padre","lím. edo.","lím. munic.","casilla"), col=c("black","red","black","black","gray"), lty = c(1,1,1,1,1), pch = c(NA,NA,NA,NA,19), lwd = c(6,6,2,2,0), bty="o", cex=.75)
x



library(foreign)
tmp2 <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/shp/df/disloc2018.dbf"
tmp <- read.dbf(file = tmp2)

head(tmp)
colnames(tmp) <- c("edon","edo","disloc")
tmp$edon <- 9

write.dbf(tmp, file = tmp2)
