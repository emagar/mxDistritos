
##############################################
## REPLACE WRONG REGRESSION ESTIMATES       ##
## KEEPING THE TRUE VOTE RETURNS INSTEAD    ##
## OF MANIPULATIONS TO GET ESTIMATES RIGHT  ##
##############################################
#
## # old debug routine
## sel1 <- which(names(extendCoal)=="24037")
## sel2 <- which(names(extendCoalmanip)=="24037")
## sel3 <- which(v94m$ife==24037|v94m$ife==24058)
## sel4 <- which(v94manip$ife==24037|v94manip$ife==24058)
## extendCoal[[sel1]]$pan
## extendCoalmanip[[sel2]]$pan
## v94m[sel3,]
## v94manip[sel4,]
## v97m[sel3,]
## v97manip[sel4,]
## dim(v94)
#
################################################
## chg 1994                                   ##
## ########                                   ##
## 2006 <- 1991manip 1994 1997 2000 2003      ##
## 2009 <- 1994 1997 2000 2003 2006           ##
## 2012 <- 1997 2000 2003 2006 2009           ##
## 2015 <- 2000 2003 2006 2009 2012           ##
## 2018 <- 2003 2006 2009 2012 2015           ##
## 2021 <- 2006 2009 2012 2015 2018           ##
## 2024 <- 2009 2012 2015 2018 2021           ##
## 2027 <- 2012 2015 2018 2021 2024           ##
################################################
sel <- which(names(extendCoal) %in% chg1994)
regs.2006$pan [sel] <- regs.2006.cf06$pan [sel]
regs.2006$left[sel] <- regs.2006.cf06$left[sel]
regs.2006$oth [sel] <- regs.2006.cf06$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf06$pan [sel]
mean.regs$left[sel] <- mean.regs.cf06$left[sel]
mean.regs$oth [sel] <- mean.regs.cf06$oth [sel]
extendCoal[sel] <- extendCoal.cf06[sel] # alternative would change reg coefs/preds only
#        
################################################
## chg 1997                                   ##
## ########                                   ##
## 2006 <- 1991manip 1994manip 1997 2000 2003 ##
## 2009 <- 1994manip 1997 2000 2003 2006      ##
## 2012 <- 1997 2000 2003 2006 2009           ##
## 2015 <- 2000 2003 2006 2009 2012           ##
## 2018 <- 2003 2006 2009 2012 2015           ##
## 2021 <- 2006 2009 2012 2015 2018           ##
## 2024 <- 2009 2012 2015 2018 2021           ##
## 2027 <- 2012 2015 2018 2021 2024           ##
################################################
sel <- which(names(extendCoal) %in% chg1997)
#sel <- sel[1] # debug
regs.2006$pan [sel] <- regs.2006.cf06$pan [sel]
regs.2006$left[sel] <- regs.2006.cf06$left[sel]
regs.2006$oth [sel] <- regs.2006.cf06$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf06$pan [sel]
mean.regs$left[sel] <- mean.regs.cf06$left[sel]
mean.regs$oth [sel] <- mean.regs.cf06$oth [sel]
extendCoal[sel]     <- extendCoal.cf06[sel] # alternative would change reg coefs/preds only
#
regs.2009$pan [sel] <- regs.2009.cf09$pan [sel]
regs.2009$left[sel] <- regs.2009.cf09$left[sel]
regs.2009$oth [sel] <- regs.2009.cf09$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf09$pan [sel]
mean.regs$left[sel] <- mean.regs.cf09$left[sel]
mean.regs$oth [sel] <- mean.regs.cf09$oth [sel]
extendCoal[sel]     <- extendCoal.cf09[sel] # alternative would change reg coefs/preds only
#        
#####################################################
## chg 2000                                        ##
## ########                                        ##
## 2006 <- 1991manip 1994manip 1997manip 2000 2003 ##
## 2009 <- 1994manip 1997manip 2000 2003 2006      ##
## 2012 <- 1997manip 2000 2003 2006 2009           ##
## 2015 <- 2000 2003 2006 2009 2012                ##
## 2018 <- 2003 2006 2009 2012 2015                ##
## 2021 <- 2006 2009 2012 2015 2018                ##
## 2024 <- 2009 2012 2015 2018 2021                ##
## 2027 <- 2012 2015 2018 2021 2024                ##
#####################################################
sel <- which(names(extendCoal) %in% chg2000)
regs.2006$pan [sel] <- regs.2006.cf06$pan [sel]
regs.2006$left[sel] <- regs.2006.cf06$left[sel]
regs.2006$oth [sel] <- regs.2006.cf06$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf06$pan [sel]
mean.regs$left[sel] <- mean.regs.cf06$left[sel]
mean.regs$oth [sel] <- mean.regs.cf06$oth [sel]
extendCoal[sel]     <- extendCoal.cf06[sel] # alternative would change reg coefs/preds only
#
regs.2009$pan [sel] <- regs.2009.cf09$pan [sel]
regs.2009$left[sel] <- regs.2009.cf09$left[sel]
regs.2009$oth [sel] <- regs.2009.cf09$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf09$pan [sel]
mean.regs$left[sel] <- mean.regs.cf09$left[sel]
mean.regs$oth [sel] <- mean.regs.cf09$oth [sel]
extendCoal[sel]     <- extendCoal.cf09[sel] # alternative would change reg coefs/preds only
#
regs.2012$pan [sel] <- regs.2012.cf12$pan [sel]
regs.2012$left[sel] <- regs.2012.cf12$left[sel]
regs.2012$oth [sel] <- regs.2012.cf12$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf12$pan [sel]
mean.regs$left[sel] <- mean.regs.cf12$left[sel]
mean.regs$oth [sel] <- mean.regs.cf12$oth [sel]
extendCoal[sel]     <- extendCoal.cf12[sel] # alternative would change reg coefs/preds only
#
##########################################################
## chg 2003                                             ##
## ########                                             ##
## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003 ##
## 2009 <- 1994manip 1997manip 2000manip 2003 2006      ##
## 2012 <- 1997manip 2000manip 2003 2006 2009           ##
## 2015 <- 2000manip 2003 2006 2009 2012                ##
## 2018 <- 2003 2006 2009 2012 2015                     ##
## 2021 <- 2006 2009 2012 2015 2018                     ##
## 2024 <- 2009 2012 2015 2018 2021                     ##
## 2027 <- 2012 2015 2018 2021 2024                     ##
##########################################################
sel <- which(names(extendCoal) %in% chg2003)
regs.2006$pan [sel] <- regs.2006.cf06$pan [sel]
regs.2006$left[sel] <- regs.2006.cf06$left[sel]
regs.2006$oth [sel] <- regs.2006.cf06$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf06$pan [sel]
mean.regs$left[sel] <- mean.regs.cf06$left[sel]
mean.regs$oth [sel] <- mean.regs.cf06$oth [sel]
extendCoal[sel]     <- extendCoal.cf06[sel] # alternative would change reg coefs/preds only
#
regs.2009$pan [sel] <- regs.2009.cf09$pan [sel]
regs.2009$left[sel] <- regs.2009.cf09$left[sel]
regs.2009$oth [sel] <- regs.2009.cf09$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf09$pan [sel]
mean.regs$left[sel] <- mean.regs.cf09$left[sel]
mean.regs$oth [sel] <- mean.regs.cf09$oth [sel]
extendCoal[sel]     <- extendCoal.cf09[sel] # alternative would change reg coefs/preds only
#
regs.2012$pan [sel] <- regs.2012.cf12$pan [sel]
regs.2012$left[sel] <- regs.2012.cf12$left[sel]
regs.2012$oth [sel] <- regs.2012.cf12$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf12$pan [sel]
mean.regs$left[sel] <- mean.regs.cf12$left[sel]
mean.regs$oth [sel] <- mean.regs.cf12$oth [sel]
extendCoal[sel]     <- extendCoal.cf12[sel] # alternative would change reg coefs/preds only
#
regs.2015$pan [sel] <- regs.2015.cf15$pan [sel]
regs.2015$left[sel] <- regs.2015.cf15$left[sel]
regs.2015$oth [sel] <- regs.2015.cf15$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf15$pan [sel]
mean.regs$left[sel] <- mean.regs.cf15$left[sel]
mean.regs$oth [sel] <- mean.regs.cf15$oth [sel]
extendCoal[sel]     <- extendCoal.cf15[sel] # alternative would change reg coefs/preds only
#
###############################################################
## chg 2006                                                  ##
## ########                                                  ##
## 2006 <- 1991manip 1994manip 1997manip 2000manip 2003manip ##
## 2009 <- 1994manip 1997manip 2000manip 2003manip 2006      ##
## 2012 <- 1997manip 2000manip 2003manip 2006 2009           ##
## 2015 <- 2000manip 2003manip 2006 2009 2012                ##
## 2018 <- 2003manip 2006 2009 2012 2015                     ##
## 2021 <- 2006 2009 2012 2015 2018                          ##
## 2024 <- 2009 2012 2015 2018 2021                          ##
## 2027 <- 2012 2015 2018 2021 2024                          ##
###############################################################
sel <- which(names(extendCoal) %in% chg2006)
#sel <- sel[1] # debug
regs.2006$pan [sel] <- regs.2006.cf06$pan [sel]
regs.2006$left[sel] <- regs.2006.cf06$left[sel]
regs.2006$oth [sel] <- regs.2006.cf06$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf06$pan [sel]
mean.regs$left[sel] <- mean.regs.cf06$left[sel]
mean.regs$oth [sel] <- mean.regs.cf06$oth [sel]
extendCoal[sel]     <- extendCoal.cf06[sel] # alternative would change reg coefs/preds only
#
regs.2009$pan [sel] <- regs.2009.cf09$pan [sel]
regs.2009$left[sel] <- regs.2009.cf09$left[sel]
regs.2009$oth [sel] <- regs.2009.cf09$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf09$pan [sel]
mean.regs$left[sel] <- mean.regs.cf09$left[sel]
mean.regs$oth [sel] <- mean.regs.cf09$oth [sel]
extendCoal[sel]     <- extendCoal.cf09[sel] # alternative would change reg coefs/preds only
#
regs.2012$pan [sel] <- regs.2012.cf12$pan [sel]
regs.2012$left[sel] <- regs.2012.cf12$left[sel]
regs.2012$oth [sel] <- regs.2012.cf12$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf12$pan [sel]
mean.regs$left[sel] <- mean.regs.cf12$left[sel]
mean.regs$oth [sel] <- mean.regs.cf12$oth [sel]
extendCoal[sel]     <- extendCoal.cf12[sel] # alternative would change reg coefs/preds only
#
regs.2015$pan [sel] <- regs.2015.cf15$pan [sel]
regs.2015$left[sel] <- regs.2015.cf15$left[sel]
regs.2015$oth [sel] <- regs.2015.cf15$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf15$pan [sel]
mean.regs$left[sel] <- mean.regs.cf15$left[sel]
mean.regs$oth [sel] <- mean.regs.cf15$oth [sel]
extendCoal[sel]     <- extendCoal.cf15[sel] # alternative would change reg coefs/preds only
#
regs.2018$pan [sel] <- regs.2018.cf18$pan [sel]
regs.2018$left[sel] <- regs.2018.cf18$left[sel]
regs.2018$oth [sel] <- regs.2018.cf18$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf18$pan [sel]
mean.regs$left[sel] <- mean.regs.cf18$left[sel]
mean.regs$oth [sel] <- mean.regs.cf18$oth [sel]
extendCoal[sel]     <- extendCoal.cf18[sel] # alternative would change reg coefs/preds only
#
###############################################################
## chg 2009                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994manip 1997manip 2000manip 2003manip 2006manip ##
## 2012 <- 1997manip 2000manip 2003manip 2006manip 2009      ##
## 2015 <- 2000manip 2003manip 2006manip 2009 2012           ##
## 2018 <- 2003manip 2006manip 2009 2012 2015                ##
## 2021 <- 2006manip 2009 2012 2015 2018                     ##
## 2024 <- 2009 2012 2015 2018 2021                          ##
## 2027 <- 2012 2015 2018 2021 2024                          ##
###############################################################
regs.2009$pan [sel] <- regs.2009.cf09$pan [sel]
regs.2009$left[sel] <- regs.2009.cf09$left[sel]
regs.2009$oth [sel] <- regs.2009.cf09$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf09$pan [sel]
mean.regs$left[sel] <- mean.regs.cf09$left[sel]
mean.regs$oth [sel] <- mean.regs.cf09$oth [sel]
extendCoal[sel]     <- extendCoal.cf09[sel] # alternative would change reg coefs/preds only
#
regs.2012$pan [sel] <- regs.2012.cf12$pan [sel]
regs.2012$left[sel] <- regs.2012.cf12$left[sel]
regs.2012$oth [sel] <- regs.2012.cf12$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf12$pan [sel]
mean.regs$left[sel] <- mean.regs.cf12$left[sel]
mean.regs$oth [sel] <- mean.regs.cf12$oth [sel]
extendCoal[sel]     <- extendCoal.cf12[sel] # alternative would change reg coefs/preds only
#
regs.2015$pan [sel] <- regs.2015.cf15$pan [sel]
regs.2015$left[sel] <- regs.2015.cf15$left[sel]
regs.2015$oth [sel] <- regs.2015.cf15$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf15$pan [sel]
mean.regs$left[sel] <- mean.regs.cf15$left[sel]
mean.regs$oth [sel] <- mean.regs.cf15$oth [sel]
extendCoal[sel]     <- extendCoal.cf15[sel] # alternative would change reg coefs/preds only
#
regs.2018$pan [sel] <- regs.2018.cf18$pan [sel]
regs.2018$left[sel] <- regs.2018.cf18$left[sel]
regs.2018$oth [sel] <- regs.2018.cf18$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf18$pan [sel]
mean.regs$left[sel] <- mean.regs.cf18$left[sel]
mean.regs$oth [sel] <- mean.regs.cf18$oth [sel]
extendCoal[sel]     <- extendCoal.cf18[sel] # alternative would change reg coefs/preds only
#
regs.2021$pan [sel] <- regs.2021.cf21$pan [sel]
regs.2021$left[sel] <- regs.2021.cf21$left[sel]
regs.2021$oth [sel] <- regs.2021.cf21$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf21$pan [sel]
mean.regs$left[sel] <- mean.regs.cf21$left[sel]
mean.regs$oth [sel] <- mean.regs.cf21$oth [sel]
extendCoal[sel]     <- extendCoal.cf21[sel] # alternative would change reg coefs/preds only
#
###############################################################
## chg 2012                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994 1997 2000 2003 2006                          ##
## 2012 <- 1997manip 2000manip 2003manip 2006manip 2009manip ##
## 2015 <- 2000manip 2003manip 2006manip 2009manip 2012      ##
## 2018 <- 2003manip 2006manip 2009manip 2012 2015           ##
## 2021 <- 2006manip 2009manip 2012 2015 2018                ##
## 2024 <- 2009manip 2012 2015 2018 2021                     ##
## 2027 <- 2012 2015 2018 2021 2024                          ##
###############################################################
regs.2012$pan [sel] <- regs.2012.cf12$pan [sel]
regs.2012$left[sel] <- regs.2012.cf12$left[sel]
regs.2012$oth [sel] <- regs.2012.cf12$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf12$pan [sel]
mean.regs$left[sel] <- mean.regs.cf12$left[sel]
mean.regs$oth [sel] <- mean.regs.cf12$oth [sel]
extendCoal[sel]     <- extendCoal.cf12[sel] # alternative would change reg coefs/preds only
#
regs.2015$pan [sel] <- regs.2015.cf15$pan [sel]
regs.2015$left[sel] <- regs.2015.cf15$left[sel]
regs.2015$oth [sel] <- regs.2015.cf15$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf15$pan [sel]
mean.regs$left[sel] <- mean.regs.cf15$left[sel]
mean.regs$oth [sel] <- mean.regs.cf15$oth [sel]
extendCoal[sel]     <- extendCoal.cf15[sel] # alternative would change reg coefs/preds only
#
regs.2018$pan [sel] <- regs.2018.cf18$pan [sel]
regs.2018$left[sel] <- regs.2018.cf18$left[sel]
regs.2018$oth [sel] <- regs.2018.cf18$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf18$pan [sel]
mean.regs$left[sel] <- mean.regs.cf18$left[sel]
mean.regs$oth [sel] <- mean.regs.cf18$oth [sel]
extendCoal[sel]     <- extendCoal.cf18[sel] # alternative would change reg coefs/preds only
#
regs.2021$pan [sel] <- regs.2021.cf21$pan [sel]
regs.2021$left[sel] <- regs.2021.cf21$left[sel]
regs.2021$oth [sel] <- regs.2021.cf21$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf21$pan [sel]
mean.regs$left[sel] <- mean.regs.cf21$left[sel]
mean.regs$oth [sel] <- mean.regs.cf21$oth [sel]
extendCoal[sel]     <- extendCoal.cf21[sel] # alternative would change reg coefs/preds only
#
## regs.2024$pan [sel] <- regs.2024.cf24$pan [sel]
## regs.2024$left[sel] <- regs.2024.cf24$left[sel]
## regs.2024$oth [sel] <- regs.2024.cf24$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf24$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf24$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf24$oth [sel]
## extendCoal[sel]     <- extendCoal.cf24[sel] # alternative would change reg coefs/preds only
#
###############################################################
## chg 2015                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994 1997 2000 2003 2006                          ##
## 2012 <- 1997 2000 2003 2006 2009                          ##
## 2015 <- 2000manip 2003manip 2006manip 2009manip 2012manip ##
## 2018 <- 2003manip 2006manip 2009manip 2012manip 2015      ##
## 2021 <- 2006manip 2009manip 2012manip 2015 2018           ##
## 2024 <- 2009manip 2012manip 2015 2018 2021                ##
## 2027 <- 2012manip 2015 2018 2021 2024                     ##
###############################################################
regs.2015$pan [sel] <- regs.2015.cf15$pan [sel]
regs.2015$left[sel] <- regs.2015.cf15$left[sel]
regs.2015$oth [sel] <- regs.2015.cf15$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf15$pan [sel]
mean.regs$left[sel] <- mean.regs.cf15$left[sel]
mean.regs$oth [sel] <- mean.regs.cf15$oth [sel]
extendCoal[sel]     <- extendCoal.cf15[sel] # alternative would change reg coefs/preds only
#
regs.2018$pan [sel] <- regs.2018.cf18$pan [sel]
regs.2018$left[sel] <- regs.2018.cf18$left[sel]
regs.2018$oth [sel] <- regs.2018.cf18$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf18$pan [sel]
mean.regs$left[sel] <- mean.regs.cf18$left[sel]
mean.regs$oth [sel] <- mean.regs.cf18$oth [sel]
extendCoal[sel]     <- extendCoal.cf18[sel] # alternative would change reg coefs/preds only
#
regs.2021$pan [sel] <- regs.2021.cf21$pan [sel]
regs.2021$left[sel] <- regs.2021.cf21$left[sel]
regs.2021$oth [sel] <- regs.2021.cf21$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf21$pan [sel]
mean.regs$left[sel] <- mean.regs.cf21$left[sel]
mean.regs$oth [sel] <- mean.regs.cf21$oth [sel]
extendCoal[sel]     <- extendCoal.cf21[sel] # alternative would change reg coefs/preds only
#
## regs.2024$pan [sel] <- regs.2024.cf24$pan [sel]
## regs.2024$left[sel] <- regs.2024.cf24$left[sel]
## regs.2024$oth [sel] <- regs.2024.cf24$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf24$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf24$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf24$oth [sel]
## extendCoal[sel]     <- extendCoal.cf24[sel] # alternative would change reg coefs/preds only
## #
## regs.2027$pan [sel] <- regs.2027.cf27$pan [sel]
## regs.2027$left[sel] <- regs.2027.cf27$left[sel]
## regs.2027$oth [sel] <- regs.2027.cf27$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf27$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf27$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf27$oth [sel]
## extendCoal[sel]     <- extendCoal.cf27[sel] # alternative would change reg coefs/preds only
#
###############################################################
## chg 2018                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994 1997 2000 2003 2006                          ##
## 2012 <- 1997 2000 2003 2006 2009                          ##
## 2015 <- 2000 2003 2006 2009 2012                          ##
## 2018 <- 2003manip 2006manip 2009manip 2012manip 2015manip ##
## 2021 <- 2006manip 2009manip 2012manip 2015manip 2018      ##
## 2024 <- 2009manip 2012manip 2015manip 2018 2021           ##
## 2027 <- 2012manip 2015manip 2018 2021 2024                ##
## 2030 <- 2015manip 2018 2021 2024 2027                     ##
###############################################################
regs.2018$pan [sel] <- regs.2018.cf18$pan [sel]
regs.2018$left[sel] <- regs.2018.cf18$left[sel]
regs.2018$oth [sel] <- regs.2018.cf18$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf18$pan [sel]
mean.regs$left[sel] <- mean.regs.cf18$left[sel]
mean.regs$oth [sel] <- mean.regs.cf18$oth [sel]
extendCoal[sel]     <- extendCoal.cf18[sel] # alternative would change reg coefs/preds only
#
regs.2021$pan [sel] <- regs.2021.cf21$pan [sel]
regs.2021$left[sel] <- regs.2021.cf21$left[sel]
regs.2021$oth [sel] <- regs.2021.cf21$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf21$pan [sel]
mean.regs$left[sel] <- mean.regs.cf21$left[sel]
mean.regs$oth [sel] <- mean.regs.cf21$oth [sel]
extendCoal[sel]     <- extendCoal.cf21[sel] # alternative would change reg coefs/preds only
#
## regs.2024$pan [sel] <- regs.2024.cf24$pan [sel]
## regs.2024$left[sel] <- regs.2024.cf24$left[sel]
## regs.2024$oth [sel] <- regs.2024.cf24$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf24$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf24$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf24$oth [sel]
## extendCoal[sel]     <- extendCoal.cf24[sel] # alternative would change reg coefs/preds only
## #
## regs.2027$pan [sel] <- regs.2027.cf27$pan [sel]
## regs.2027$left[sel] <- regs.2027.cf27$left[sel]
## regs.2027$oth [sel] <- regs.2027.cf27$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf27$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf27$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf27$oth [sel]
## extendCoal[sel]     <- extendCoal.cf27[sel] # alternative would change reg coefs/preds only
## #
## regs.2030$pan [sel] <- regs.2030.cf30$pan [sel]
## regs.2030$left[sel] <- regs.2030.cf30$left[sel]
## regs.2030$oth [sel] <- regs.2030.cf30$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf30$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf30$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf30$oth [sel]
## extendCoal[sel]     <- extendCoal.cf30[sel] # alternative would change reg coefs/preds only
#
###############################################################
## chg 2021                                                  ##
## ########                                                  ##
## 2006 <- 1991 1994 1997 2000 2003                          ##
## 2009 <- 1994 1997 2000 2003 2006                          ##
## 2012 <- 1997 2000 2003 2006 2009                          ##
## 2015 <- 2000 2003 2006 2009 2012                          ##
## 2018 <- 2003 2006 2009 2012 2015                          ##
## 2021 <- 2006manip 2009manip 2012manip 2015manip 2018manip ##
## 2024 <- 2009manip 2012manip 2015manip 2018manip 2021      ##
## 2027 <- 2012manip 2015manip 2018manip 2021 2024           ##
## 2030 <- 2015manip 2018manip 2021 2024 2027                ##
## 2033 <- 2018manip 2021 2024 2027 2030                     ##
###############################################################
regs.2021$pan [sel] <- regs.2021.cf21$pan [sel]
regs.2021$left[sel] <- regs.2021.cf21$left[sel]
regs.2021$oth [sel] <- regs.2021.cf21$oth [sel]
mean.regs$pan [sel] <- mean.regs.cf21$pan [sel]
mean.regs$left[sel] <- mean.regs.cf21$left[sel]
mean.regs$oth [sel] <- mean.regs.cf21$oth [sel]
extendCoal[sel]     <- extendCoal.cf21[sel] # alternative would change reg coefs/preds only
#
## regs.2024$pan [sel] <- regs.2024.cf24$pan [sel]
## regs.2024$left[sel] <- regs.2024.cf24$left[sel]
## regs.2024$oth [sel] <- regs.2024.cf24$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf24$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf24$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf24$oth [sel]
## extendCoal[sel]     <- extendCoal.cf24[sel] # alternative would change reg coefs/preds only
## #
## regs.2027$pan [sel] <- regs.2027.cf27$pan [sel]
## regs.2027$left[sel] <- regs.2027.cf27$left[sel]
## regs.2027$oth [sel] <- regs.2027.cf27$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf27$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf27$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf27$oth [sel]
## extendCoal[sel]     <- extendCoal.cf27[sel] # alternative would change reg coefs/preds only
## #
## regs.2030$pan [sel] <- regs.2030.cf30$pan [sel]
## regs.2030$left[sel] <- regs.2030.cf30$left[sel]
## regs.2030$oth [sel] <- regs.2030.cf30$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf30$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf30$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf30$oth [sel]
## extendCoal[sel]     <- extendCoal.cf30[sel] # alternative would change reg coefs/preds only
## #
## regs.2033$pan [sel] <- regs.2033.cf33$pan [sel]
## regs.2033$left[sel] <- regs.2033.cf33$left[sel]
## regs.2033$oth [sel] <- regs.2033.cf33$oth [sel]
## mean.regs$pan [sel] <- mean.regs.cf33$pan [sel]
## mean.regs$left[sel] <- mean.regs.cf33$left[sel]
## mean.regs$oth [sel] <- mean.regs.cf33$oth [sel]
## extendCoal[sel]     <- extendCoal.cf33[sel] # alternative would change reg coefs/preds only
#

