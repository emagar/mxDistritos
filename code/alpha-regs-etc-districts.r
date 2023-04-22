####################################################################
## Script for autoregressive vote estimates and alpha regressions ##
## invoked from code/elec-data-for-maps.r                         ##
##                                                                ##
## Author: Eric Magar                                             ##
## emagar at itam dot mx                                          ##
## Date: 17apr2023                                                ##
## Last modified: 20apr2023                                       ##
####################################################################

############################################################################
## # backwards estimation/prediction, unit = districts                    ##
## 1988d <- 1991d   1994d   1997d79 2000d79 2003d79                       ##
## 1991d <- 1994d   1997d79 2000d79 2003d79 2006d79                       ##
## 1994d <- 1997d79 2000d79 2003d79 2006d79 2009d79                       ##
## 1997d <- 2000d   2003d   2006d97 2009d97 2012d97                       ##
## 2000d <- 2003d   2006d97 2009d97 2012d97 2015d97                       ##
## 2003d <- 2006d97 2009d97 2012d97 2015d97 2018d97                       ##
## 2006d <- 2009d   2012d   2015d   2018d06 2021d06                       ##
## # forward estimation/prediction, unit = districts                      ##
##          1991d06 1994d06 1997d06 2000d06 2003d06 -> 2006d # no 1991d06 ##
##          1994d06 1997d06 2000d06 2003d06 2006d   -> 2009d              ##
##          1997d06 2000d06 2003d06 2006d   2009d   -> 2012d              ##
##          2000d06 2003d06 2006d   2009d   2012d   -> 2015d              ##
##          2003d18 2006d18 2009d18 2012d18 2015d18 -> 2018d              ##
##          2006d18 2009d18 2012d18 2015d18 2018d   -> 2021d              ##
##          2009d18 2012d18 2015d18 2018d   2021d   -> 2024d18            ##
############################################################################

############################################
## prepare manipulated party objects      ##
## for time-series and alpha regressions  ##
## After 2024 election, uncheck/add lines ##
############################################
#
# version 1: extend partial coalitions across the board
# shares
pand18 <- data.frame(
    #v91 =  with(v91d18, ifelse(efec==0, NA,  pan                    / efec)),
    v94 =  with(v94d18, ifelse(efec==0, NA,  pan                    / efec)),
    v97 =  with(v97d18, ifelse(efec==0, NA,  pan                    / efec)),
    v00 =  with(v00d18, ifelse(efec==0, NA,  panc                   / efec)),
    v03 =  with(v03d18, ifelse(efec==0, NA,  pan                    / efec)),
    v06 =  with(v06d18, ifelse(efec==0, NA,  pan                    / efec)),
    v09 =  with(v09d18, ifelse(efec==0, NA,  pan                    / efec)),
    v12 =  with(v12d18, ifelse(efec==0, NA,  pan                    / efec)),
    v15 =  with(v15d18, ifelse(efec==0, NA,  pan                    / efec)),
    v18 =  with(v18d,   ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)),   # drop mc?
    v21 =  with(v21d,   ifelse(efec==0, NA, (pan + panc + prd)      / efec))   # drop prd?
)
pand06 <- data.frame(
    #v91 =  with(v91d06, ifelse(efec==0, NA,  pan                    / efec)), #comment if agg=="s"
    v94 =  with(v94d06, ifelse(efec==0, NA,  pan                    / efec)),
    v97 =  with(v97d06, ifelse(efec==0, NA,  pan                    / efec)),
    v00 =  with(v00d06, ifelse(efec==0, NA,  panc                   / efec)),
    v03 =  with(v03d06, ifelse(efec==0, NA,  pan                    / efec)),
    v06 =  with(v06d,   ifelse(efec==0, NA,  pan                    / efec)),
    v09 =  with(v09d,   ifelse(efec==0, NA,  pan                    / efec)),
    v12 =  with(v12d,   ifelse(efec==0, NA,  pan                    / efec)),
    v15 =  with(v15d,   ifelse(efec==0, NA,  pan                    / efec)),
    v18 =  with(v18d06, ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)),   # drop mc?
    v21 =  with(v21d06, ifelse(efec==0, NA, (pan + panc + prd)      / efec))   # drop prd?
)
pand97 <- data.frame(
    #v91 =  with(v91d97, ifelse(efec==0, NA,  pan                    / efec)), # comment if agg=="s"
    v94 =  with(v94d97, ifelse(efec==0, NA,  pan                    / efec)),
    v97 =  with(v97d,   ifelse(efec==0, NA,  pan                    / efec)),
    v00 =  with(v00d,   ifelse(efec==0, NA,  panc                   / efec)),
    v03 =  with(v03d,   ifelse(efec==0, NA,  pan                    / efec)),
    v06 =  with(v06d97, ifelse(efec==0, NA,  pan                    / efec)),
    v09 =  with(v09d97, ifelse(efec==0, NA,  pan                    / efec)),
    v12 =  with(v12d97, ifelse(efec==0, NA,  pan                    / efec)),
    v15 =  with(v15d97, ifelse(efec==0, NA,  pan                    / efec)),
    v18 =  with(v18d97, ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)),  # drop mc?
    v21 =  with(v21d97, ifelse(efec==0, NA, (pan + panc + prd)      / efec))   # drop prd?
)
pand79 <- data.frame(
    v91 =  with(v91d,   ifelse(efec==0, NA,  pan                    / efec)), # comment if agg=="s"
    v94 =  with(v94d,   ifelse(efec==0, NA,  pan                    / efec)),
    v97 =  with(v97d79, ifelse(efec==0, NA,  pan                    / efec)),
    v00 =  with(v00d79, ifelse(efec==0, NA,  panc                   / efec)),
    v03 =  with(v03d79, ifelse(efec==0, NA,  pan                    / efec)),
    v06 =  with(v06d79, ifelse(efec==0, NA,  pan                    / efec)),
    v09 =  with(v09d79, ifelse(efec==0, NA,  pan                    / efec)),
    v12 =  with(v12d79, ifelse(efec==0, NA,  pan                    / efec)),
    v15 =  with(v15d79, ifelse(efec==0, NA,  pan                    / efec)),
    v18 =  with(v18d79, ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)),  # drop mc?
    v21 =  with(v21d79, ifelse(efec==0, NA, (pan + panc + prd)      / efec))   # drop prd?
)
pand18 <- round(pand18, 3)
pand06 <- round(pand06, 3)
pand97 <- round(pand97, 3)
pand79 <- round(pand79, 3)
#
prid18 <- data.frame(
    #v91 =  with(v91d18, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94d18, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97d18, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00d18, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03d18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06d18, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09d18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12d18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15d18, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18d,   ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21d,   ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prid06 <- data.frame(
    #v91 =  with(v91d06, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94d06, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97d06, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00d06, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03d06, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06d,   ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09d,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12d,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15d,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18d06, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21d06, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prid97 <- data.frame(
    #v91 =  with(v91d97, ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94d97, ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97d,   ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00d,   ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03d,   ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06d97, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09d97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12d97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15d97, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18d97, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21d97, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prid79 <- data.frame(
    v91 =  with(v91d,   ifelse(efec==0, NA,  pri                      / efec)),
    v94 =  with(v94d,   ifelse(efec==0, NA,  pri                      / efec)),
    v97 =  with(v97d79, ifelse(efec==0, NA,  pri                      / efec)),
    v00 =  with(v00d79, ifelse(efec==0, NA,  pri                      / efec)),
    v03 =  with(v03d79, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v06 =  with(v06d79, ifelse(efec==0, NA,  pric                     / efec)),
    v09 =  with(v09d79, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v12 =  with(v12d79, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v15 =  with(v15d79, ifelse(efec==0, NA, (pri + pric + pvem)       / efec)), # drop pvem?
    v18 =  with(v18d79, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)), # drop pvem + pna?
    v21 =  with(v21d79, ifelse(efec==0, NA,  pri                      / efec))  # coal vote to pan+prd ok?
)
prid79 <- round(prid79, 3)
prid97 <- round(prid97, 3)
prid06 <- round(prid06, 3)
prid18 <- round(prid18, 3)
#
leftd18 <- data.frame(
    #v91 = with(v91d18, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94d18, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97d18, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00d18, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03d18, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06d18, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09d18, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12d18, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15d18, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18d,   ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21d,   ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftd06 <- data.frame(
    #v91 = with(v91d06, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94d06, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97d06, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00d06, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03d06, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06d,   ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09d,   ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12d,   ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15d,   ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18d06, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21d06, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftd97 <- data.frame(
    #v91 = with(v91d97, ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94d97, ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97d,   ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00d,   ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03d,   ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06d97, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09d97, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12d97, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15d97, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18d97, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21d97, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftd79 <- data.frame(
    v91 = with(v91d,   ifelse(efec==0, NA,  prd                             / efec)),
    v94 = with(v94d,   ifelse(efec==0, NA,  prd                             / efec)),
    v97 = with(v97d79, ifelse(efec==0, NA,  prd                             / efec)),
    v00 = with(v00d79, ifelse(efec==0, NA,  prdc                            / efec)),
    v03 = with(v03d79, ifelse(efec==0, NA, (prd + pt + conve)               / efec)),
    v06 = with(v06d79, ifelse(efec==0, NA,  prdc                            / efec)),
    v09 = with(v09d79, ifelse(efec==0, NA, (prd + pt + ptc + conve)         / efec)),
    v12 = with(v12d79, ifelse(efec==0, NA, (prd + prdc + pt + mc)           / efec)),
    v15 = with(v15d79, ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
    v18 = with(v18d79, ifelse(efec==0, NA, (morena + morenac + pt + pes)    / efec)),
    v21 = with(v21d79, ifelse(efec==0, NA, (morena + morenac + pt + pvem)   / efec))  # drop pt + pvem?
)
leftd79 <- round(leftd79, 3)
leftd97 <- round(leftd97, 3)
leftd06 <- round(leftd06, 3)
leftd18 <- round(leftd18, 3)
#
othd18 <- data.frame(
    #v91 =  with(v91d18, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94d18, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97d18, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00d18, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03d18, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06d18, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09d18, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12d18, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15d18, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18d,   ifelse(efec==0, NA, (indep1 + indep2)                          / efec)),
    v21 =  with(v21d,   ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othd06 <- data.frame(
    #v91 =  with(v91d06, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94d06, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97d06, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00d06, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03d06, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06d,   ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09d,   ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12d,   ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15d,   ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18d06, ifelse(efec==0, NA, (indep1 + indep2)                          / efec)),
    v21 =  with(v21d06, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othd97 <- data.frame(
    #v91 =  with(v91d97, ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94d97, ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97d,   ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00d,   ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03d,   ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06d97, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09d97, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12d97, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15d97, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18d97, ifelse(efec==0, NA, (indep1 + indep2)                          / efec)),
    v21 =  with(v21d97, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othd79 <- data.frame(
    v91 =  with(v91d,   ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt)     / efec)),
    v94 =  with(v94d,   ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
    v97 =  with(v97d79, ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm)               / efec)),
    v00 =  with(v00d79, ifelse(efec==0, NA, (pcd + parm + dsppn)                       / efec)),
    v03 =  with(v03d79, ifelse(efec==0, NA, (psn + pas + mp + plm + fc)                / efec)),
    v06 =  with(v06d79, ifelse(efec==0, NA, (pna + asdc)                               / efec)),
    v09 =  with(v09d79, ifelse(efec==0, NA, (pna + psd)                                / efec)),
    v12 =  with(v12d79, ifelse(efec==0, NA,  pna                                       / efec)),
    v15 =  with(v15d79, ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2)          / efec)),
    v18 =  with(v18d79, ifelse(efec==0, NA, (indep1 + indep2)                          / efec)),
    v21 =  with(v21d79, ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep)             / efec))
)
othd79 <- round(othd79, 3)
othd97 <- round(othd97, 3)
othd06 <- round(othd06, 3)
othd18 <- round(othd18, 3)
#
efecd18 <- data.frame(
    #v91 = v91d18$efec,
    v94 = v94d18$efec,
    v97 = v97d18$efec,
    v00 = v00d18$efec,
    v03 = v03d18$efec,
    v06 = v06d18$efec,
    v09 = v09d18$efec,
    v12 = v12d18$efec,
    v15 = v15d18$efec,
    v18 = v18d  $efec,
    v21 = v21d  $efec
)
efecd06 <- data.frame(
    #v91 = v91d06$efec,
    v94 = v94d06$efec,
    v97 = v97d06$efec,
    v00 = v00d06$efec,
    v03 = v03d06$efec,
    v06 = v06d  $efec,
    v09 = v09d  $efec,
    v12 = v12d  $efec,
    v15 = v15d  $efec,
    v18 = v18d06$efec,
    v21 = v21d06$efec
)
efecd97 <- data.frame(
    #v91 = v91d97$efec,
    v94 = v94d97$efec,
    v97 = v97d  $efec,
    v00 = v00d  $efec,
    v03 = v03d  $efec,
    v06 = v06d97$efec,
    v09 = v09d97$efec,
    v12 = v12d97$efec,
    v15 = v15d97$efec,
    v18 = v18d97$efec,
    v21 = v21d97$efec
)
efecd79 <- data.frame(
    v91 = v91d  $efec,
    v94 = v94d  $efec,
    v97 = v97d79$efec,
    v00 = v00d79$efec,
    v03 = v03d79$efec,
    v06 = v06d79$efec,
    v09 = v09d79$efec,
    v12 = v12d79$efec,
    v15 = v15d79$efec,
    v18 = v18d79$efec,
    v21 = v21d79$efec
)
#
lisnomd18 <- data.frame(
    #v91 = v91d18$lisnom,
    v94 = v94d18$lisnom,
    v97 = v97d18$lisnom,
    v00 = v00d18$lisnom,
    v03 = v03d18$lisnom,
    v06 = v06d18$lisnom,
    v09 = v09d18$lisnom,
    v12 = v12d18$lisnom,
    v15 = v15d18$lisnom,
    v18 = v18d  $lisnom,
    v21 = v21d  $lisnom
)
lisnomd06 <- data.frame(
    #v91 = v91d06$lisnom,
    v94 = v94d06$lisnom,
    v97 = v97d06$lisnom,
    v00 = v00d06$lisnom,
    v03 = v03d06$lisnom,
    v06 = v06d  $lisnom,
    v09 = v09d  $lisnom,
    v12 = v12d  $lisnom,
    v15 = v15d  $lisnom,
    v18 = v18d06$lisnom,
    v21 = v21d06$lisnom
)
lisnomd97 <- data.frame(
    #v91 = v91d97$lisnom,
    v94 = v94d97$lisnom,
    v97 = v97d  $lisnom,
    v00 = v00d  $lisnom,
    v03 = v03d  $lisnom,
    v06 = v06d97$lisnom,
    v09 = v09d97$lisnom,
    v12 = v12d97$lisnom,
    v15 = v15d97$lisnom,
    v18 = v18d97$lisnom,
    v21 = v21d97$lisnom
)
lisnomd79 <- data.frame(
    v91 = v91d  $lisnom,
    v94 = v94d  $lisnom,
    v97 = v97d79$lisnom,
    v00 = v00d79$lisnom,
    v03 = v03d79$lisnom,
    v06 = v06d79$lisnom,
    v09 = v09d79$lisnom,
    v12 = v12d79$lisnom,
    v15 = v15d79$lisnom,
    v18 = v18d79$lisnom,
    v21 = v21d79$lisnom
)
#
# transpose to plug columns (units) into new time-series data.frames
pand79    <- t(pand79)
pand97    <- t(pand97)
pand06    <- t(pand06)
pand18    <- t(pand18)
prid79    <- t(prid79)
prid97    <- t(prid97)
prid06    <- t(prid06)
prid18    <- t(prid18)
leftd79   <- t(leftd79)
leftd97   <- t(leftd97)
leftd06   <- t(leftd06)
leftd18   <- t(leftd18)
othd79    <- t(othd79)
othd97    <- t(othd97)
othd06    <- t(othd06)
othd18    <- t(othd18)
efecd79   <- t(efecd79)
efecd97   <- t(efecd97)
efecd06   <- t(efecd06)
efecd18   <- t(efecd18)
lisnomd79 <- t(lisnomd79)
lisnomd97 <- t(lisnomd97)
lisnomd06 <- t(lisnomd06)
lisnomd18 <- t(lisnomd18)

# will receive data for regressions, one per map
tmp <- as.list(rep(NA, 300))
extendCoald79 <- extendCoald97 <- extendCoald06 <- extendCoald18 <- tmp
#
# loop over districts
for (i in 1:300){
    #i <- 81 # debug
    message(sprintf("loop %s of %s", i, nrow(v00d)))
    #########################
    ## votes with 1979 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1991, to=2021, by=3),
                      pan    = pand79[,i],
                      pri    = prid79[,i],
                      left   = leftd79[,i],
                      oth    = othd79[,i],
                      efec   = efecd79[,i],
                      lisnom = lisnomd79[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$disn    <- v94d$disn[i]
    # fill info to new list
    extendCoald79[[i]] <- tmp
    # name list object
    names(extendCoald79)[i] <- tmp$disn[1]
    #
    #########################
    ## votes with 1997 map ##
    #########################
    tmp <- data.frame(yr     = seq(from=1994, to=2021, by=3),
                      pan    = pand97[,i],
                      pri    = prid97[,i],
                      left   = leftd97[,i],
                      oth    = othd97[,i],
                      efec   = efecd97[,i],
                      lisnom = lisnomd97[,i])
    tmp <- rbind(v91=c(1991,NA,NA,NA,NA,NA,NA), tmp) # add 1991 with no counterfactuals
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]   <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]   <- per.means["pri"];
        tmp$left  [is.na(tmp$left)]  <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]   <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$disn    <- v97d$disn[i]
    # fill info to new list
    extendCoald97[[i]] <- tmp
    # name list object
    names(extendCoald97)[i] <- tmp$disn[1]
    #
    #########################
    ## votes with 2006 map ##
    #########################
    tmp <- data.frame(yr   = seq(from=1994, to=2021, by=3),
                      pan  = pand06[,i],
                      pri  = prid06[,i],
                      left = leftd06[,i],
                      oth  = othd06[,i],
                      efec = efecd06[,i],
                      lisnom = lisnomd06[,i])
    tmp <- rbind(v91=c(1991,NA,NA,NA,NA,NA,NA), tmp) # add 1991 with no counterfactuals
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$disn    <- v06d$disn[i]
    # fill info to new list
    extendCoald06[[i]] <- tmp
    # name list object
    names(extendCoald06)[i] <- tmp$disn[1]
    #
    #########################
    ## votes with 2018 map ##
    #########################
    tmp <- data.frame(yr   = seq(from=1994, to=2021, by=3),
                      pan  = pand18[,i],
                      pri  = prid18[,i],
                      left = leftd18[,i],
                      oth  = othd18[,i],
                      efec = efecd18[,i],
                      lisnom = lisnomd18[,i])
    tmp <- rbind(v91=c(1991,NA,NA,NA,NA,NA,NA), tmp) # add 1991 with no counterfactuals
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan   [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri   [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left  [is.na(tmp$left)] <- per.means["left"];
        tmp$oth   [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec  [is.na(tmp$efec)   | tmp$efec==0]   <- 1
        tmp$lisnom[is.na(tmp$lisnom) | tmp$lisnom==0] <- 2
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    tmp$disn    <- v18d$disn[i]
    # fill info to new list
    extendCoald18[[i]] <- tmp
    # name list object
    names(extendCoald18)[i] <- tmp$disn[1]
}

##################################
## datos para regresión de alfa ##
##################################
#
#############################################################################################################
## Nota 16jul2021: al añadir datos 2021 cambiarán todas las alfas y betas! Una solución fácil (que usaré)  ##
## y otra que requiere más coco. La fácil es reestimar con 2021 e identificar el commit que reportaba la   ##
## versión hasta 2018. La otra: definir una ventana temporal (como cinco elecciones) para producir alfas   ##
## y betas cada elección: alfa.2006, alfa.2009, etc. Debería poder investigar cómo usan esto en el Capital ##
## Asset Pricing Model...                                                                                  ##
#############################################################################################################
yr.means <- data.frame(yr = seq(1991,2021,3), # 11 election-years
                       pan    = rep(NA,11),
                       pri    = rep(NA,11),
                       left   = rep(NA,11),
                       oth    = rep(NA,11))
# function to sum numeric columns
cs <- function(x){
    sel.nums <- unlist(lapply(x, is.numeric), use.names = FALSE) # selects only numeric columns in data frame
    res <- colSums(x[,sel.nums], na.rm=TRUE)
    return(res)
}
#
# compute national mean vote
yr.means$pan [1] <-  cs(v91s)["pan"]  /  cs(v91s)["efec"]
yr.means$pri [1] <-  cs(v91s)["pri"]  /  cs(v91s)["efec"]
yr.means$left[1] <-  cs(v91s)["prd"]  /  cs(v91s)["efec"]
yr.means$oth [1] <-  with(yr.means[1,], 1 - pan - pri - left)
#
yr.means$pan [2] <-  cs(v94s)["pan"]  / cs(v94s)["efec"]
yr.means$pri [2] <-  cs(v94s)["pri"]  / cs(v94s)["efec"]
yr.means$left[2] <-  cs(v94s)["prd"]  / cs(v94s)["efec"]
yr.means$oth [2] <-  with(yr.means[2,], 1 - pan - pri - left)
#                
yr.means$pan [3] <-  cs(v97s)["pan"]  / cs(v97s)["efec"]
yr.means$pri [3] <-  cs(v97s)["pri"]  / cs(v97s)["efec"]
yr.means$left[3] <-  cs(v97s)["prd"]  / cs(v97s)["efec"]
yr.means$oth [3] <-  with(yr.means[3,], 1 - pan - pri - left)
#                
yr.means$pan [4] <-  cs(v00s)["panc"]  / cs(v00s)["efec"]
yr.means$pri [4] <-  cs(v00s)["pri"]   / cs(v00s)["efec"]
yr.means$left[4] <-  cs(v00s)["prdc"]  / cs(v00s)["efec"]
yr.means$oth [4] <-  with(yr.means[4,], 1 - pan - pri - left)
#                
yr.means$pan [5] <-  cs(v03s)["pan"]                                         / cs(v03s)["efec"]
yr.means$pri [5] <- (cs(v03s)["pri"] + cs(v03s)["pric"] + cs(v03s)["pvem"])  / cs(v03s)["efec"]
yr.means$left[5] <- (cs(v03s)["prd"] + cs(v03s)["pt"]   + cs(v03s)["conve"]) / cs(v03s)["efec"]
yr.means$oth [5] <-  with(yr.means[5,], 1 - pan - pri - left)
#                
yr.means$pan [6] <-  cs(v06s)["pan"]   / cs(v06s)["efec"]
yr.means$pri [6] <-  cs(v06s)["pric"]  / cs(v06s)["efec"]
yr.means$left[6] <-  cs(v06s)["prdc"]  / cs(v06s)["efec"]
yr.means$oth [6] <-  with(yr.means[6,], 1 - pan - pri - left)
#                
yr.means$pan [7] <-  cs(v09s)["pan"]                                                           / cs(v09s)["efec"]
yr.means$pri [7] <- (cs(v09s)["pri"] + cs(v09s)["pric"] + cs(v09s)["pvem"])                    / cs(v09s)["efec"]
yr.means$left[7] <- (cs(v09s)["prd"] + cs(v09s)["pt"]   + cs(v09s)["ptc"] + cs(v09s)["conve"]) / cs(v09s)["efec"]
yr.means$oth [7] <-  with(yr.means[7,], 1 - pan - pri - left)
#                
yr.means$pan [8] <-  cs(v12s)["pan"]                                                       / cs(v12s)["efec"]
yr.means$pri [8] <- (cs(v12s)["pri"] + cs(v12s)["pric"] + cs(v12s)["pvem"])                / cs(v12s)["efec"]
yr.means$left[8] <- (cs(v12s)["prd"] + cs(v12s)["prdc"] + cs(v12s)["pt"] + cs(v12s)["mc"]) / cs(v12s)["efec"]
yr.means$oth [8] <-  with(yr.means[8,], 1 - pan - pri - left)
#                
yr.means$pan [9] <-  cs(v15s)["pan"]                                                            / cs(v15s)["efec"]
yr.means$pri [9] <- (cs(v15s)["pri"] + cs(v15s)["pric"] + cs(v15s)["pvem"])                     / cs(v15s)["efec"]
yr.means$left[9] <- (cs(v15s)["prd"] + cs(v15s)["prdc"] + cs(v15s)["pt"] + cs(v15s)["morena"] ) / cs(v15s)["efec"] # dropped cs(v15s)["pes"]
yr.means$oth [9] <-  with(yr.means[9,], 1 - pan - pri - left)
#
yr.means$pan [10] <- (cs(v18s)["pan"]    + cs(v18s)["panc"]    + cs(v18s)["prd"]  + cs(v18s)["mc"])  / cs(v18s)["efec"]
yr.means$pri [10] <- (cs(v18s)["pri"]    + cs(v18s)["pric"]    + cs(v18s)["pvem"] + cs(v18s)["pna"]) / cs(v18s)["efec"]
yr.means$left[10] <- (cs(v18s)["morena"] + cs(v18s)["morenac"] + cs(v18s)["pt"]   + cs(v18s)["pes"]) / cs(v18s)["efec"]
yr.means$oth [10] <-  with(yr.means[10,], 1 - pan - pri - left)
#
yr.means$pan [11] <- (cs(v21s)["pan"]    + cs(v21s)["panc"]    + cs(v21s)["prd"])                    / cs(v21s)["efec"]
yr.means$pri [11] <-  cs(v21s)["pri"]                                                                / cs(v21s)["efec"] # dropped cs(v21s)["pric"]
yr.means$left[11] <- (cs(v21s)["morena"] + cs(v21s)["morenac"] + cs(v21s)["pt"]  + cs(v21s)["pvem"]) / cs(v21s)["efec"]
yr.means$oth [11] <-  with(yr.means[11,], 1 - pan - pri - left)
#
yr.means <- within(yr.means, mean.rpan    <- pan  / pri)
yr.means <- within(yr.means, mean.rleft   <- left / pri)
yr.means <- within(yr.means, mean.roth    <- oth  / pri)
#
yr.means[,2:8] <- round(yr.means[,2:8], 3)
#
# plug into data
for (i in 1:nrow(v00d)){
    #i <- 2 # debug
    extendCoald18[[i]] <- cbind(extendCoald18[[i]], yr.means[,6:8])
    extendCoald06[[i]] <- cbind(extendCoald06[[i]], yr.means[,6:8])
    extendCoald97[[i]] <- cbind(extendCoald97[[i]], yr.means[,6:8])
    extendCoald79[[i]] <- cbind(extendCoald79[[i]], yr.means[,6:8])
}

#################################################################################################
## - should also try jags estimation to get post-sample of vhats and alphas                    ##
## - report mg effect of unit change in bar(v) at year's level instead of betahat (cf. Linzer) ##
#################################################################################################

###############################
## código de las regresiones ##
###############################
vhat.2024 <-                 # <--- OJO 19abr21: assumes no redistricting in 2024 change when 2024 map available
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
vhat.2003 <- vhat.2000 <- vhat.1997 <- vhat.1994 <-
vhat.1991 <- vhat.1988 <-
    data.frame(pan  = rep(NA, nrow(v00d)),
               pri  = rep(NA, nrow(v00d)),
               left = rep(NA, nrow(v00d))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00d)),
                       pri    = rep(NA, nrow(v00d)),
                       left   = rep(NA, nrow(v00d))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00d)),
                      left   = rep(NA, nrow(v00d)),
                      oth    = rep(NA, nrow(v00d))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00d))) # empty list will receive one time-series
                                    # regression per municipio, each used to
                                    # predict votes in 2006:2024
# add names to m and s (to d must be done yearly basis due to redistricting)
tmp18 <- tmp06 <- tmp97 <- tmp79 <- tmp # each map will receive district names
names(tmp79) <- v94d$disn
names(tmp97) <- v97d$disn
names(tmp06) <- v06d$disn
names(tmp18) <- v18d$disn
#
regs.1988 <- regs.1991 <-
    list(pan    = tmp79,
         left   = tmp79,
         oth    = tmp79,
         readme = "No pri regs because DVs are pri-ratios")
regs.2006 <- regs.2009 <- regs.2012 <- regs.2015 <- 
    list(pan    = tmp06,
         left   = tmp06,
         oth    = tmp06,
         readme = "No pri regs because DVs are pri-ratios")
regs.2018 <- regs.2021 <- regs.2024 <-  
    list(pan    = tmp18,
         left   = tmp18,
         oth    = tmp18,
         readme = "No pri regs because DVs are pri-ratios")
# for districts, one mean.reg per map
mean.regs.d79 <-
    list(pan    = tmp79,
         left   = tmp79,
         oth    = tmp79,
         readme = "No pri regs bec DVs are pri-ratios")
mean.regs.d97 <-
    list(pan    = tmp97,
         left   = tmp97,
         oth    = tmp97,
         readme = "No pri regs bec DVs are pri-ratios")
mean.regs.d06 <-
    list(pan    = tmp06,
         left   = tmp06,
         oth    = tmp06,
         readme = "No pri regs bec DVs are pri-ratios")
mean.regs.d18 <-
    list(pan    = tmp18,
         left   = tmp18,
         oth    = tmp18,
         readme = "No pri regs bec DVs are pri-ratios")
rm(tmp,tmp79,tmp97,tmp06,tmp18)


###############################################################
## District 5-yr estimates that can be computed before 2024  ##
## |      | map    |        |        |       |               ##
## | vhat | 1979   | 1997   | 2006   | 2018  |               ##
## |------+--------+--------+--------+-------|               ##
## | 1988 | *back* |   XX   |   XX   |   XX  |               ##
## | 1991 | *back* |  back  |  back  |  back |               ##
## | 1994 | *back* |  back  |  back  |  back |               ##
## | 1997 |  back  | *back* |  back  |  back |               ##
## | 2000 |  back  | *back* |  back  |  back |               ##
## | 2003 |  BOTH  | *back* |  back  |  back |               ##
## | 2006 |  BOTH  |  back  | *back* |  back |               ##
## | 2009 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2012 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2015 |  fwd   |  fwd   | *fwd*  |  fwd  |               ##
## | 2018 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## | 2021 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## | 2024 |  fwd   |  fwd   |  fwd   | *fwd* |               ##
## Notes:                                                    ##
## - Starred are needed                                      ##
## - get 2006 estimates backwards                            ##
## - use 1979 BOTH to compare 2003 and 2006 fwd/backwd vhats ##
###############################################################

wrap district estimations in a function.
It takes the map to use as argument, choosing which election-years to estimate by default.
Setting dbackward=1 performs reverse estimation with future, not past, elections.
Tweaking object yrs.to.estimate should allow to specify non-default estimation years.

# district estimates are contingent on chosen map
sel.map <- c(1979,1997,2006,2018)[2]
#
#######################################################################
## read 1988dfdf from external file to add votes when using 1979 map ##
#######################################################################
if (sel.map==1979){
    tmp <- read.csv(file="../dfdf1979-on.csv")
    tmp <- tmp[tmp$yr==1988,]
    tmp.v <- tmp[, grep("^v[0-9]{2}$", colnames(tmp))]
    tmp.l <- tmp[, grep("^l[0-9]{2}$", colnames(tmp))]
    tmp$efec <- rowSums(tmp.v, na.rm=TRUE)               # recompute efec
    tmp.v <- round(tmp.v / tmp$efec, 3)                  # shares
    sel.c <- apply(tmp.l, 1, function(x) which(x=="pan"))
    tmp$pan <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
    sel.c <- apply(tmp.l, 1, function(x) which(x=="pri"))
    tmp$pri <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
    tmp$left <- NA # left will sum fdn (not prt)
    sel.c <- apply(tmp.l, 1, function(x) which(x=="pdm"))
    tmp$oth <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
    sel.c <- apply(tmp.l, 1, function(x) which(x=="prt"))
    tmp$oth2 <- apply(cbind(tmp.v,sel.c=sel.c), 1, function(x) x[x["sel.c"]])
    tmp$oth <- tmp$oth+tmp$oth2; tmp$oth2 <- NULL 
    tmp <- within(tmp, left <- 1 - pan - pri - oth)
    tmp <- tmp[,c("disn","pan","pri","left","oth","efec","lisnom")]
    tmp.1988 <- tmp
    rm(tmp,sel.c,tmp.v,tmp.l)
    #
    # if map is 1979 will estimate 1988 backwards, add row in data frames
    add1988 <- function(x) rbind(v88=c(1988,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA), x)
    tmp <- extendCoald79 # duplicate for manipulation
    ## OJO: should replace NAs above with 1988 district returns in file
    tmp <- lapply(extendCoald79, add1988) # add row for 1988 to each data frame in list
    # add corresponding 1988 shares
    table(as.numeric(names(tmp))==tmp.1988$disn) # check items in same order 
    data.frame(ext=as.numeric(names(tmp)), v88=tmp.1988$disn) # check items in same order 
    for (i in 1:300){
        tmp[[i]][1,c("pan","pri","left","oth","efec","lisnom","disn")] <-
            tmp.1988[i,c("pan","pri","left","oth","efec","lisnom","disn")] # fill 1st row w ith obs
    }
    extendCoald79 <- tmp
    rm(add1988,tmp,tmp.1988)
}

for (i in 1:300){
    #i <- 81 # debug
    #i <- 44508 # debug
    message(sprintf("loop %s of %s", i, 300))
    # subset data to single unit
if (sel.map==1979) data.tmp <- extendCoald79[[i]]
if (sel.map==1997) data.tmp <- extendCoald97[[i]]
if (sel.map==2006) data.tmp <- extendCoald06[[i]]
if (sel.map==2018) data.tmp <- extendCoald18[[i]]
    #data.tmp <- extendCoal[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA, data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA, data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA, data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ############################################
    ## backwards-predict 1988 with next 5 els ##
    ############################################
    if (sel.map==1979){
    year <- 1988
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.1988[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1988$pan [[i]]   <- reg.pan
    regs.1988$left[[i]]   <- reg.left
    regs.1988$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ############################################
    ## backwards-predict 1991 with next 5 els ##
    ############################################
    if (sel.map==1979){
    year <- 1991
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.1991[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1991$pan [[i]]   <- reg.pan
    regs.1991$left[[i]]   <- reg.left
    regs.1991$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ############################################
    ## backwards-predict 1994 with next 5 els ##
    ############################################
    if (sel.map==1979){
    year <- 1994
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.1994[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1994$pan [[i]]   <- reg.pan
    regs.1994$left[[i]]   <- reg.left
    regs.1994$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ############################################
    ## backwards-predict 1997 with next 5 els ##
    ############################################
    if (sel.map==1997){
    year <- 1997
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.1997[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.1997$pan [[i]]   <- reg.pan
    regs.1997$left[[i]]   <- reg.left
    regs.1997$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ############################################
    ## backwards-predict 2000 with next 5 els ##
    ############################################
    if (sel.map==1997){
    year <- 2000
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2000[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2000$pan [[i]]   <- reg.pan
    regs.2000$left[[i]]   <- reg.left
    regs.2000$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ############################################
    ## backwards-predict 2003 with next 5 els ##
    ############################################
    if (sel.map==1997){
    year <- 2003
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2003[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2003$pan [[i]]   <- reg.pan
    regs.2003$left[[i]]   <- reg.left
    regs.2003$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ############################################
    ## backwards-predict 2006 with next 5 els ##
    ############################################
    if (sel.map==2006){
    year <- 2006
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year+3 & yr <= year+15))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2006[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006$pan [[i]]   <- reg.pan
    regs.2006$left[[i]]   <- reg.left
    regs.2006$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    if (sel.map==2006){
    year <- 2009
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2009[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2009$pan [[i]]   <- reg.pan
    regs.2009$left[[i]]   <- reg.left
    regs.2009$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    if (sel.map==2006){
    year <- 2012
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2012[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2012$pan [[i]]   <- reg.pan
    regs.2012$left[[i]]   <- reg.left
    regs.2012$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    if (sel.map==2006){
    year <- 2015
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2015[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2015$pan [[i]]   <- reg.pan
    regs.2015$left[[i]]   <- reg.left
    regs.2015$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    if (sel.map==2018){
    year <- 2018
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2018[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2018$pan [[i]]   <- reg.pan
    regs.2018$left[[i]]   <- reg.left
    regs.2018$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    if (sel.map==2018){
    year <- 2021
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2021[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2021$pan [[i]]   <- reg.pan
    regs.2021$left[[i]]   <- reg.left
    regs.2021$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    ##################################
    ## predict 2024 with last 5 els ##
    ##################################
    if (sel.map==2018){
    year <- 2024
    reg.pan  <-    lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <-    lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <-    lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    #
    new.d <- data.frame(yr = year)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    rhat.left   <- exp(predict.lm(reg.left,   newdata = new.d))#, interval = "confidence")
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    bhat.pan    <- round(summary.lm(reg.pan)   $coef[2,1], 3)
    bhat.left   <- round(summary.lm(reg.left)  $coef[2,1], 3)
    #
    ## plug into results objects ##
    vhat.2024[i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2024$pan [[i]]   <- reg.pan
    regs.2024$left[[i]]   <- reg.left
    regs.2024$oth [[i]]   <- reg.oth
    #
    if ("vhat.pan"  %notin% colnames(data.tmp)) data.tmp$vhat.pan <- NA  # add slot for projections if absent
    if ("vhat.pri"  %notin% colnames(data.tmp)) data.tmp$vhat.pri <- NA  # add slot for projections if absent
    if ("vhat.left" %notin% colnames(data.tmp)) data.tmp$vhat.left <- NA # add slot for projections if absent
    if ("bhat.pan"  %notin% colnames(data.tmp)) data.tmp$bhat.pan <- NA  # add slot for slope estimates if absent
    if ("bhat.left" %notin% colnames(data.tmp)) data.tmp$bhat.left <- NA # add slot for slope estimates if absent
    #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan   # input vote estimates
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan   # input slope estimates
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    }
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan   <-  lm(formula = log(pan /pri)  ~  mean.rpan,  data = data.tmp)
    reg.left  <-  lm(formula = log(left/pri)  ~  mean.rleft, data = data.tmp)
    reg.oth   <-  lm(formula = log(oth /pri)  ~  mean.roth,  data = data.tmp)
    #
    # point prediction alpha with mean at zero 
    new.d <- data.frame(mean.rpan = 0)
    rhat.pan    <- exp(predict.lm(reg.pan,    newdata = new.d))#, interval = "confidence")
    new.d <- data.frame(mean.rleft   = 0)
    rhat.left   <- exp(predict.lm(reg.left  , newdata = new.d))#, interval = "confidence")
    new.d <- data.frame(mean.roth = 0)
    rhat.oth    <- exp(predict.lm(reg.oth,    newdata = new.d))#, interval = "confidence")
    vhat.pan    <- round(rhat.pan    / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.pri    <- round(1           / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    vhat.left   <- round(rhat.left   / (1 + rhat.pan + rhat.left   + rhat.oth), 3)
    #
    #c(vhat.pan, vhat.pri, vhat.left, 1-vhat.pan-vhat.pri-vhat.left)
    alphahat[i,] <- c(vhat.pan, vhat.pri, vhat.left  )
    betahat[i,1] <- coef(reg.pan)   [2]
    betahat[i,2] <- coef(reg.left  )[2]
    betahat[i,3] <- coef(reg.oth)   [2]
    #
    if (sel.map==1979){
       mean.regs.d79$pan   [[i]] <- reg.pan
       mean.regs.d79$left  [[i]] <- reg.left  
       mean.regs.d79$oth   [[i]] <- reg.oth
    }
    if (sel.map==1997){
       mean.regs.d97$pan   [[i]] <- reg.pan
       mean.regs.d97$left  [[i]] <- reg.left  
       mean.regs.d97$oth   [[i]] <- reg.oth
    }
    if (sel.map==2006){
       mean.regs.d06$pan   [[i]] <- reg.pan
       mean.regs.d06$left  [[i]] <- reg.left  
       mean.regs.d06$oth   [[i]] <- reg.oth
    }
    if (sel.map==2018){
       mean.regs.d18$pan   [[i]] <- reg.pan
       mean.regs.d18$left  [[i]] <- reg.left  
       mean.regs.d18$oth   [[i]] <- reg.oth
    }
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left  <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan   <- alphahat$pan [i]
    data.tmp$alphahat.pri   <- alphahat$pri [i]
    data.tmp$alphahat.left  <- alphahat$left[i]
    data.tmp$betahat.pan    <- betahat$pan  [i]
    data.tmp$betahat.left   <- betahat$left [i]
    data.tmp$betahat.oth    <- betahat$oth  [i]
    data.tmp <- round(data.tmp,3)
    #
    ########################################################
    ## optional: plug vhats alphas betas back into data   ##
    ########################################################
    data.tmp <- within(data.tmp, {
        mean.rpan <- mean.rleft   <- mean.roth <- NULL; # drop mean ratios
        oth <- NULL; # drop compositional vote complement
        betahat.oth <- NULL; # drop this beta
        #betahat.pan <- betahat.left   <- betahat.oth <- NULL; # drop betas
    })
    # return estimates to data object
if (sel.map==1979) extendCoald79[[i]] <- data.tmp
if (sel.map==1997) extendCoald97[[i]] <- data.tmp
if (sel.map==2006) extendCoald06[[i]] <- data.tmp
if (sel.map==2018) extendCoald18[[i]] <- data.tmp
    #extendCoal[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################

# clean, all this is saved in extendCoal, mean.regs, regs.2006, regs.2009, regs.2012, regs.2015, regs.2018
rm(alphahat, betahat, bhat.left, bhat.pan, reg.left, reg.oth, reg.pan, rhat.left, rhat.oth, rhat.pan, vhat.2006, vhat.2009, vhat.2012, vhat.2015, vhat.2018, vhat.2021, vhat.2024, vhat.left, vhat.pan, vhat.pri)


# clean
rm(v91manip, v94manip, v97manip, v00manip, v03manip, v06manip, v09manip, v12manip, v15manip, v18manip,
   regs.2006manip, regs.2009manip, regs.2012manip, regs.2015manip, regs.2018manip, mean.regsmanip,
   regs.2006manip2, regs.2009manip2, regs.2012manip2, regs.2015manip2, regs.2018manip2, mean.regsmanip2,
   extendCoalmanip, extendCoalmanip2)
rm(v91,v94,v97,v00,v03,v06,v09,v12,v15,v18)
rm(pan,pri,left,oth,efec)
rm(sel,sel1,sel2,sel.to,sel.c,target.ife,i,tmp)


##########################################################################
## generate data frame with one year's predictions/estimates for export ##
##########################################################################
tmp.func <- function(year) {
    #year <- 2009         # debug
    #X <- extendCoal[[1]] # debug
    sel <- which(extendCoal[[1]]$yr==year) # which row reports year (symmetric in all other objects in list)
    # generate list with selected row only in every municipio
    tmp <- lapply(extendCoal, FUN = function(X) {
        prune <- X[sel,]
        return(prune)
    })
    # spot NAs in list
    tmp.sel <- setdiff(1:length(extendCoal), non.nas)
    # fill with same-dim NA data.frame
    tmp.manip <- tmp[[non.nas[1]]]
    tmp.manip[,-1] <- NA # all but 1st col (yr) to NA
    if (length(tmp.sel)>0) tmp[tmp.sel] <- lapply(tmp[tmp.sel], function(x) tmp.manip)
    # turn into one dataframe
    # table(summary(tmp)) # debug
    tmp <- do.call("rbind", tmp)
    rownames(tmp) <- NULL
    return(tmp)
}

extendCoal.2006 <- tmp.func(year=2006)
extendCoal.2009 <- tmp.func(year=2009)
extendCoal.2012 <- tmp.func(year=2012)
extendCoal.2015 <- tmp.func(year=2015)
extendCoal.2018 <- tmp.func(year=2018)
extendCoal.2021 <- tmp.func(year=2021)
extendCoal.2024 <- tmp.func(year=2024)
#rm(extendCoal.2015) # clean memory

# plug inegi into data for export
tmp <- v21m[,c("ife","inegi")]
#dim(tmp); dim(extendCoal.2006) # debug
extendCoal.2006 <- merge(x = extendCoal.2006, y = tmp, by = "ife", all = TRUE)
extendCoal.2009 <- merge(x = extendCoal.2009, y = tmp, by = "ife", all = TRUE)
extendCoal.2012 <- merge(x = extendCoal.2012, y = tmp, by = "ife", all = TRUE)
extendCoal.2015 <- merge(x = extendCoal.2015, y = tmp, by = "ife", all = TRUE)
extendCoal.2018 <- merge(x = extendCoal.2018, y = tmp, by = "ife", all = TRUE)
extendCoal.2021 <- merge(x = extendCoal.2021, y = tmp, by = "ife", all = TRUE)
extendCoal.2024 <- merge(x = extendCoal.2024, y = tmp, by = "ife", all = TRUE)

# if missing ife code, that wrongly adds a rown with NAs, drop 
sel <- which(is.na(extendCoal.2006$ife))
if (length(sel)>0){
    extendCoal.2006 <- extendCoal.2006[-sel,];
    extendCoal.2009 <- extendCoal.2009[-sel,];
    extendCoal.2012 <- extendCoal.2012[-sel,];
    extendCoal.2015 <- extendCoal.2015[-sel,];
    extendCoal.2018 <- extendCoal.2018[-sel,];
    extendCoal.2021 <- extendCoal.2021[-sel,];
    extendCoal.2024 <- extendCoal.2024[-sel,];
}

# drop some columns
extendCoal.2006 <- within(extendCoal.2006, yr <- edosecn <- NULL)
extendCoal.2009 <- within(extendCoal.2009, yr <- edosecn <- NULL)
extendCoal.2012 <- within(extendCoal.2012, yr <- edosecn <- NULL)
extendCoal.2015 <- within(extendCoal.2015, yr <- edosecn <- NULL)
extendCoal.2018 <- within(extendCoal.2018, yr <- edosecn <- NULL)
extendCoal.2021 <- within(extendCoal.2021, yr <- edosecn <- NULL)
extendCoal.2024 <- within(extendCoal.2024, yr <- edosecn <- NULL)

# more cleaning
rm(add.split,cs,sel.split)
rm(info,new.d,non.nas,per.means,year)
rm(yr.means)
rm(tmp,data.tmp)


##################
## save to disk ##
##################
if (agg=="m") {
    write.csv(extendCoal.2006,
              file = paste(wd, "data/dipfed-municipio-vhat-2006.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2009,
              file = paste(wd, "data/dipfed-municipio-vhat-2009.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2012,
              file = paste(wd, "data/dipfed-municipio-vhat-2012.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2015,
              file = paste(wd, "data/dipfed-municipio-vhat-2015.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2018,
              file = paste(wd, "data/dipfed-municipio-vhat-2018.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2021,
              file = paste(wd, "data/dipfed-municipio-vhat-2021.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2024,
              file = paste(wd, "data/dipfed-municipio-vhat-2024.csv", sep = ""), row.names = FALSE)
}
if (agg=="s") {
    write.csv(extendCoal.2009,
              file = paste(wd, "data/dipfed-seccion-vhat-2009.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2012,
              file = paste(wd, "data/dipfed-seccion-vhat-2012.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2015,
              file = paste(wd, "data/dipfed-seccion-vhat-2015.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2018,
              file = paste(wd, "data/dipfed-seccion-vhat-2018.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2021,
              file = paste(wd, "data/dipfed-seccion-vhat-2021.csv", sep = ""), row.names = FALSE)
    #
    write.csv(extendCoal.2024,
              file = paste(wd, "data/dipfed-seccion-vhat-2024.csv", sep = ""), row.names = FALSE)
}

# save municipal regression objects
save(mean.regs, file = paste(wd, "data/dipfed-municipio-mean-regs.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(regs.2006, file = paste(wd, "data/dipfed-municipio-regs-2006.RData", sep = ""), compress = "gzip")
save(regs.2009, file = paste(wd, "data/dipfed-municipio-regs-2009.RData", sep = ""), compress = "gzip")
save(regs.2012, file = paste(wd, "data/dipfed-municipio-regs-2012.RData", sep = ""), compress = "gzip")
save(regs.2015, file = paste(wd, "data/dipfed-municipio-regs-2015.RData", sep = ""), compress = "gzip")
save(regs.2018, file = paste(wd, "data/dipfed-municipio-regs-2018.RData", sep = ""), compress = "gzip")
save(regs.2021, file = paste(wd, "data/dipfed-municipio-regs-2021.RData", sep = ""), compress = "gzip")
save(regs.2024, file = paste(wd, "data/dipfed-municipio-regs-2024.RData", sep = ""), compress = "gzip")

# save sección regression objects
save(mean.regs, file = paste(wd, "data/too-big-4-github/dipfed-seccion-mean-regs.RData", sep = ""), compress = c("gzip", "bzip2", "xz")[3])
save(regs.2009, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2009.RData", sep = ""), compress = "gzip")
save(regs.2012, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2012.RData", sep = ""), compress = "gzip")
save(regs.2015, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2015.RData", sep = ""), compress = "gzip")
save(regs.2018, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2018.RData", sep = ""), compress = "gzip")
save(regs.2021, file = paste(wd, "data/too-big-4-github/dipfed-seccion-regs-2021.RData", sep = ""), compress = "gzip")

# load regression object
load(file = paste(wd, "data/dipfed-municipio-regs-2015.RData", sep = ""))
ls()
summary(regs.2015)
summary.lm(regs.2015$oth[[1]])$coef[2,1]



# version 2: coalitions only in districts where they happened
## leftRealm <- data.frame(v00 = v00m$prdc / v00m$efec,
##                         v03 = v03m$prd / v03m$efec,
##                         v06 = v06m$prdc / v06m$efec,
##                         v09 = v09m$prd / v09m$efec,
##                         v12 = v12m$prdc / v12m$efec,
##                         v15 = (v15m$prd * (1 - v15m$dprdc) + v15m$prdc * v15m$dprdc + v15m$morena) / v15m$efec,
##                         v18 = (v18m$morena * (1 - v18m$dmorenac) + v18m$morenac * v18m$dmorenac) / v18m$efec)
## esto no jala, parece que morena!=0 cuando dmorenac==1
## morenaRealm[1,]
# version 3: party's own vote plus proportional part of coal
## morenaBrkm
