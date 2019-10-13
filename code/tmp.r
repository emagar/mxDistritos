# replace NAs with zeroes to compute yearly means
tmp00 <- v00s; tmp03 <- v03s; tmp06 <- v06s; tmp09 <- v09s; tmp12 <- v12s; tmp15 <- v15s; tmp18 <- v18s;
tmp00[is.na(tmp00)] <- 0;
tmp03[is.na(tmp03)] <- 0;
tmp06[is.na(tmp06)] <- 0;
tmp09[is.na(tmp09)] <- 0;
tmp12[is.na(tmp12)] <- 0;
tmp15[is.na(tmp15)] <- 0;
tmp18[is.na(tmp18)] <- 0;
#
yr.means <- data.frame(pan = rep(NA, 7),
                       pri = rep(NA, 7),
                       morena = rep(NA, 7),
                       oth = rep(NA, 7))
#
yr.means$pan[1]    <-  apply(tmp00, 2, sum)["panc"]                                / apply(tmp00, 2, sum)["efec"]
yr.means$pri[1]    <-  apply(tmp00, 2, sum)["pri"]                                 / apply(tmp00, 2, sum)["efec"]
yr.means$morena[1] <-  apply(tmp00, 2, sum)["prdc"]                                / apply(tmp00, 2, sum)["efec"]
yr.means$oth[1]    <- (apply(tmp00, 2, sum)["pcd"] + apply(tmp00, 2, sum)["parm"]) / apply(tmp00, 2, sum)["efec"]
#
yr.means$pan[2]    <-   apply(tmp03, 2, sum)["pan"]                                / apply(tmp03, 2, sum)["efec"]
yr.means$pri[2]    <-  (apply(tmp03, 2, sum)["pri"] + apply(tmp03, 2, sum)["pric"] + apply(tmp03, 2, sum)["pvem"])  / apply(tmp03, 2, sum)["efec"]
yr.means$morena[2] <-  (apply(tmp03, 2, sum)["prd"] + apply(tmp03, 2, sum)["pt"]   + apply(tmp03, 2, sum)["conve"]) / apply(tmp03, 2, sum)["efec"]
yr.means$oth[2]    <-  (apply(tmp03, 2, sum)["psn"] + apply(tmp03, 2, sum)["pas"]  + apply(tmp03, 2, sum)["mp"] + apply(tmp03, 2, sum)["plm"] + apply(tmp03, 2, sum)["fc"]) / apply(tmp03, 2, sum)["efec"]
#
yr.means$pan[3]    <-   apply(tmp06, 2, sum)["pan"]                               / apply(tmp06, 2, sum)["efec"]
yr.means$pri[3]    <-   apply(tmp06, 2, sum)["pric"]                              / apply(tmp06, 2, sum)["efec"]
yr.means$morena[3] <-   apply(tmp06, 2, sum)["prdc"]                              / apply(tmp06, 2, sum)["efec"]
yr.means$oth[3]    <-  (apply(tmp06, 2, sum)["pna"] + apply(tmp06, 2, sum)["asdc"]) / apply(tmp06, 2, sum)["efec"]
#
yr.means$pan[4]    <-   apply(tmp09, 2, sum)["pan"]                                / apply(tmp09, 2, sum)["efec"]
yr.means$pri[4]    <-  (apply(tmp09, 2, sum)["pri"] + apply(tmp09, 2, sum)["pric"] + apply(tmp09, 2, sum)["pvem"])  / apply(tmp09, 2, sum)["efec"]
yr.means$morena[4] <-  (apply(tmp09, 2, sum)["prd"] + apply(tmp09, 2, sum)["pt"]   + apply(tmp09, 2, sum)["ptc"] + apply(tmp09, 2, sum)["conve"]) / apply(tmp09, 2, sum)["efec"]
yr.means$oth[4]    <-  (apply(tmp09, 2, sum)["pna"] + apply(tmp09, 2, sum)["psd"]) / apply(tmp09, 2, sum)["efec"]
#
yr.means$pan[5]    <-    apply(tmp12, 2, sum)["pan"] / apply(tmp12, 2, sum)["efec"]
yr.means$pri[5]    <-   (apply(tmp12, 2, sum)["pri"] + apply(tmp12, 2, sum)["pric"] + apply(tmp12, 2, sum)["pvem"])  / apply(tmp12, 2, sum)["efec"]
yr.means$morena[5] <-   (apply(tmp12, 2, sum)["prd"] + apply(tmp12, 2, sum)["prdc"] + apply(tmp12, 2, sum)["pt"] + apply(tmp12, 2, sum)["mc"]) / apply(tmp12, 2, sum)["efec"]
yr.means$oth[5]    <-    apply(tmp12, 2, sum)["pna"] / apply(tmp12, 2, sum)["efec"]
#
yr.means$pan[6]    <-    apply(tmp15, 2, sum)["pan"] / apply(tmp15, 2, sum)["efec"]
yr.means$pri[6]    <-   (apply(tmp15, 2, sum)["pri"] + apply(tmp15, 2, sum)["pric"] + apply(tmp15, 2, sum)["pvem"])  / apply(tmp15, 2, sum)["efec"]
yr.means$morena[6] <-   (apply(tmp15, 2, sum)["prd"] + apply(tmp15, 2, sum)["prdc"]   + apply(tmp15, 2, sum)["pt"] + apply(tmp15, 2, sum)["morena"] + apply(tmp15, 2, sum)["pes"]) / apply(tmp15, 2, sum)["efec"]
yr.means$oth[6]    <-   (apply(tmp15, 2, sum)["mc"] + apply(tmp15, 2, sum)["pna"] + apply(tmp15, 2, sum)["ph"] + apply(tmp15, 2, sum)["indep1"] + apply(tmp15, 2, sum)["indep2"]) / apply(tmp15, 2, sum)["efec"]
#
yr.means$pan[7]    <-   (apply(tmp18, 2, sum)["pan"] + apply(tmp18, 2, sum)["panc"] + apply(tmp18, 2, sum)["prd"] + apply(tmp18, 2, sum)["mc"]) / apply(tmp18, 2, sum)["efec"]
yr.means$pri[7]    <-   (apply(tmp18, 2, sum)["pri"] + apply(tmp18, 2, sum)["pric"] + apply(tmp18, 2, sum)["pvem"] + apply(tmp18, 2, sum)["pna"]) / apply(tmp18, 2, sum)["efec"]
yr.means$morena[7] <-   (apply(tmp18, 2, sum)["morena"] + apply(tmp18, 2, sum)["morenac"] + apply(tmp18, 2, sum)["pt"] + apply(tmp18, 2, sum)["pes"]) / apply(tmp18, 2, sum)["efec"]
yr.means$oth[7]    <-   (apply(tmp18, 2, sum)["indep1"] + apply(tmp18, 2, sum)["indep2"]) / apply(tmp18, 2, sum)["efec"]
#
yr.means <- round(yr.means,3)

yr.means[1,1] <- NA
colSums(yr.means, na.rm = TRUE)

attempt 2

yr.means <- data.frame(yr = seq(2000,2018,3),
                       pan = rep(NA,7),
                       pri = rep(NA,7),
                       morena = rep(NA,7),
                       oth = rep(NA,7))
cs <- function(x) colSums(x, na.rm=TRUE)
#
yr.means$pan[1]    <-  cs(v00)["panc"]                   / cs(v00)["efec"]
yr.means$pri[1]    <-  cs(v00)["pri"]                    / cs(v00)["efec"]
yr.means$morena[1] <-  cs(v00)["prdc"]                   / cs(v00)["efec"]
yr.means$oth[1]    <- (cs(v00)["pcd"] + cs(v00)["parm"]) / cs(v00)["efec"]
#
yr.means$pan[2]    <-   cs(v03)["pan"]                                                                     / cs(v03)["efec"]
yr.means$pri[2]    <-  (cs(v03)["pri"] + cs(v03)["pric"] + cs(v03)["pvem"])                                / cs(v03)["efec"]
yr.means$morena[2] <-  (cs(v03)["prd"] + cs(v03)["pt"]   + cs(v03)["conve"])                               / cs(v03)["efec"]
yr.means$oth[2]    <-  (cs(v03)["psn"] + cs(v03)["pas"]  + cs(v03)["mp"] + cs(v03)["plm"] + cs(v03)["fc"]) / cs(v03)["efec"]
#
yr.means$pan[3]    <-   cs(v06)["pan"]                    / cs(v06)["efec"]
yr.means$pri[3]    <-   cs(v06)["pric"]                   / cs(v06)["efec"]
yr.means$morena[3] <-   cs(v06)["prdc"]                   / cs(v06)["efec"]
yr.means$oth[3]    <-  (cs(v06)["pna"] + cs(v06)["asdc"]) / cs(v06)["efec"]
#
yr.means$pan[4]    <-   cs(v09)["pan"]                                                        / cs(v09)["efec"]
yr.means$pri[4]    <-  (cs(v09)["pri"] + cs(v09)["pric"] + cs(v09)["pvem"])                   / cs(v09)["efec"]
yr.means$morena[4] <-  (cs(v09)["prd"] + cs(v09)["pt"]   + cs(v09)["ptc"] + cs(v09)["conve"]) / cs(v09)["efec"]
yr.means$oth[4]    <-  (cs(v09)["pna"] + cs(v09)["psd"])                                      / cs(v09)["efec"]
#
yr.means$pan[5]    <-    cs(v12)["pan"]                                                    / cs(v12)["efec"]
yr.means$pri[5]    <-   (cs(v12)["pri"] + cs(v12)["pric"] + cs(v12)["pvem"])               / cs(v12)["efec"]
yr.means$morena[5] <-   (cs(v12)["prd"] + cs(v12)["prdc"] + cs(v12)["pt"] + cs(v12)["mc"]) / cs(v12)["efec"]
yr.means$oth[5]    <-    cs(v12)["pna"]                                                    / cs(v12)["efec"]
#
yr.means$pan[6]    <-    cs(v15)["pan"]                                                                           / cs(v15)["efec"]
yr.means$pri[6]    <-   (cs(v15)["pri"] + cs(v15)["pric"] + cs(v15)["pvem"])                                      / cs(v15)["efec"]
yr.means$morena[6] <-   (cs(v15)["prd"] + cs(v15)["prdc"]   + cs(v15)["pt"] + cs(v15)["morena"] + cs(v15)["pes"]) / cs(v15)["efec"]
yr.means$oth[6]    <-   (cs(v15)["mc"] + cs(v15)["pna"] + cs(v15)["ph"] + cs(v15)["indep1"] + cs(v15)["indep2"])  / cs(v15)["efec"]
#
yr.means$pan[7]    <-   (cs(v18)["pan"] + cs(v18)["panc"] + cs(v18)["prd"] + cs(v18)["mc"])       / cs(v18)["efec"]
yr.means$pri[7]    <-   (cs(v18)["pri"] + cs(v18)["pric"] + cs(v18)["pvem"] + cs(v18)["pna"])     / cs(v18)["efec"]
yr.means$morena[7] <-   (cs(v18)["morena"] + cs(v18)["morenac"] + cs(v18)["pt"] + cs(v18)["pes"]) / cs(v18)["efec"]
yr.means$oth[7]    <-   (cs(v18)["indep1"] + cs(v18)["indep2"])                                   / cs(v18)["efec"]
