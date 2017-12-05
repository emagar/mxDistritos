rm(list = ls())

dd <- "/home/eric/Desktop/MXelsCalendGovt/redistrict/ife.ine/mapasComparados/loc/simIndex"

setwd(dd)
dir()

ags1 <- read.csv(file = "dist_ags_cab.csv", stringsAsFactors = FALSE)
ags2 <- read.csv(file = "dist_ags.csv", stringsAsFactors = FALSE)
summary(ags1$dsi)
summary(ags2$dsi) # son iguales
head(ags1) # tiene cabeceras
head(ags2) # el que manipula el código original
all <- merge(x = ags2, y = ags1[,c("disloc2016", "cab")], by = "disloc2016", all = TRUE)
head(all)
rm(ags1, ags2)
all$son <- all$disloc2016; all$disloc2016 <- NULL
all <- all[, c("edon","son","dsi","father","cab")] # ordena columnas
#
new <- read.csv(file = "dist_bc.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
rm(new)
#
new <- read.csv(file = "dist_bcs.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
rm(new)
#
new <- read.csv(file = "dist_cam.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
rm(new)
#
new <- read.csv(file = "dist_coa.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2017; new$disloc2017 <- NULL
new$cab <- new$cab2017; new$cab2017 <- NULL
all <- rbind(all, new[names(all)])
tail(all)
rm(new)
#
new <- read.csv(file = "dist_col.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
new <- new[,c("edon","son","dsi","father","cab")]
all <- rbind(all, new[names(all)])
tail(all)
rm(new)
#
new <- read.csv(file = "dist_cps.csv", stringsAsFactors = FALSE)
head(new)
new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
rm(new)
#
new <- read.csv(file = "dist_cua.csv", stringsAsFactors = FALSE)
head(new)
new$son <- new$disloc2016; new$disloc2016 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
rm(new)
#
new <- read.csv(file = "dist_df33.csv", stringsAsFactors = FALSE)
head(new)
new <- new[-which(new$disloc2018==0),] # drop missing secciones
new$son <- new$disloc2018; new$disloc2018 <- NULL
new$cab <- NA
all <- rbind(all, new[names(all)])
tail(all)
rm(new)
#

seguir con los demás
dir()

# add edo
all$edo <- NA;
all$edo[all$edon==1] <- "ags"
all$edo[all$edon==2] <- "bc"
all$edo[all$edon==3] <- "bcs"
all$edo[all$edon==4] <- "cam"
all$edo[all$edon==5] <- "coa"
all$edo[all$edon==6] <- "col"
all$edo[all$edon==7] <- "cps"
all$edo[all$edon==8] <- "cua"
all$edo[all$edon==9] <- "df"
all$edo[all$edon==10] <- "dgo"
all$edo[all$edon==11] <- "gua"
all$edo[all$edon==12] <- "gue"
all$edo[all$edon==13] <- "hgo"
all$edo[all$edon==14] <- "jal"
all$edo[all$edon==15] <- "mex"
all$edo[all$edon==16] <- "mic"
all$edo[all$edon==17] <- "mor"
all$edo[all$edon==18] <- "nay"
all$edo[all$edon==19] <- "nl"
all$edo[all$edon==20] <- "oax"
all$edo[all$edon==21] <- "pue"
all$edo[all$edon==22] <- "que"
all$edo[all$edon==23] <- "qui"
all$edo[all$edon==24] <- "san"
all$edo[all$edon==25] <- "sin"
all$edo[all$edon==26] <- "son"
all$edo[all$edon==27] <- "tab"
all$edo[all$edon==28] <- "tam"
all$edo[all$edon==29] <- "tla"
all$edo[all$edon==30] <- "ver"
all$edo[all$edon==31] <- "yuc"
all$edo[all$edon==32] <- "zac"
#
all$pct <- ecdf(all$dsi)(all$dsi) # add percentile value 
all <- all[order(all$dsi),]
all$dcrit8 <- as.numeric(all$edon==5 | all$edon==9 | all$edon==12 | all$edon==24 | all$edon==29 | all$edon==31 | all$edon==32)
head(all)

summary(all)
summary(lm(dsi ~ dcrit8, data = all))




