################################################################
## Code invoked from within elec-data-for-maps.r              ##
## ESTIMATES MANIPULATED MUNICIPAL REGRESSIONS (NEW MUN FIX)  ##
## 20jul2021: Will run a lot of regressions, most redundant,  ##
## but simplifies cherry-picking those for muns that changed  ##
################################################################
#
############################################
## prepare manipulated party objects      ##
## for time-series and alpha regressions  ##
## After 2024 election, uncheck/add lines ##
############################################
#
# version 1: extend partial coalitions across the board
# shares
# counterfactual aggregates to account for new municipalities
pan.cf06 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  pan  / efec)), # change with v91m.cf06 when available
                       v94 = with(v94m.cf06,  ifelse(efec==0, NA,  pan  / efec)),
                       v97 = with(v97m.cf06,  ifelse(efec==0, NA,  pan  / efec)),
                       v00 = with(v00m.cf06,  ifelse(efec==0, NA,  panc / efec)),
                       v03 = with(v03m.cf06,  ifelse(efec==0, NA,  pan  / efec)),
                       v06 = with(v06,        ifelse(efec==0, NA,  pan  / efec)),
                       v09 = with(v09m.cf06,  ifelse(efec==0, NA,  pan  / efec)),
                       v12 = with(v12m.cf06,  ifelse(efec==0, NA,  pan  / efec)),
                       v15 = with(v15m.cf06,  ifelse(efec==0, NA,  pan  / efec)),
                       v18 = with(v18m.cf06,  ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)), # drop mc?
                       v21 = with(v21m.cf06,  ifelse(efec==0, NA, (pan + panc + prd) / efec)))      # drop prd?
pan.cf06 <- round(pan.cf06, 3)
#
pan.cf09 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  pan  / efec)), # change with v91m.cf09 when available
                       v94 = with(v94m.cf09,  ifelse(efec==0, NA,  pan  / efec)),
                       v97 = with(v97m.cf09,  ifelse(efec==0, NA,  pan  / efec)),
                       v00 = with(v00m.cf09,  ifelse(efec==0, NA,  panc / efec)),
                       v03 = with(v03m.cf09,  ifelse(efec==0, NA,  pan  / efec)),
                       v06 = with(v06m.cf09,  ifelse(efec==0, NA,  pan  / efec)),
                       v09 = with(v09,        ifelse(efec==0, NA,  pan  / efec)),
                       v12 = with(v12m.cf09,  ifelse(efec==0, NA,  pan  / efec)),
                       v15 = with(v15m.cf09,  ifelse(efec==0, NA,  pan  / efec)),
                       v18 = with(v18m.cf09,  ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)), # drop mc?
                       v21 = with(v21m.cf09,  ifelse(efec==0, NA, (pan + panc + prd) / efec)))      # drop prd?
pan.cf09 <- round(pan.cf09, 3)
#
pan.cf12 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  pan  / efec)), # change with v91m.cf12 when available
                       v94 = with(v94m.cf12,  ifelse(efec==0, NA,  pan  / efec)),
                       v97 = with(v97m.cf12,  ifelse(efec==0, NA,  pan  / efec)),
                       v00 = with(v00m.cf12,  ifelse(efec==0, NA,  panc / efec)),
                       v03 = with(v03m.cf12,  ifelse(efec==0, NA,  pan  / efec)),
                       v06 = with(v06m.cf12,  ifelse(efec==0, NA,  pan  / efec)),
                       v09 = with(v09m.cf12,  ifelse(efec==0, NA,  pan  / efec)),
                       v12 = with(v12,        ifelse(efec==0, NA,  pan  / efec)),
                       v15 = with(v15m.cf12,  ifelse(efec==0, NA,  pan  / efec)),
                       v18 = with(v18m.cf12,  ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)), # drop mc?
                       v21 = with(v21m.cf12,  ifelse(efec==0, NA, (pan + panc + prd) / efec)))      # drop prd?
pan.cf12 <- round(pan.cf12, 3)
#
pan.cf15 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  pan  / efec)), # change with v91m.cf15 when available
                       v94 = with(v94m.cf15,  ifelse(efec==0, NA,  pan  / efec)),
                       v97 = with(v97m.cf15,  ifelse(efec==0, NA,  pan  / efec)),
                       v00 = with(v00m.cf15,  ifelse(efec==0, NA,  panc / efec)),
                       v03 = with(v03m.cf15,  ifelse(efec==0, NA,  pan  / efec)),
                       v06 = with(v06m.cf15,  ifelse(efec==0, NA,  pan  / efec)),
                       v09 = with(v09m.cf15,  ifelse(efec==0, NA,  pan  / efec)),
                       v12 = with(v12m.cf15,  ifelse(efec==0, NA,  pan  / efec)),
                       v15 = with(v15,        ifelse(efec==0, NA,  pan  / efec)),
                       v18 = with(v18m.cf15,  ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)), # drop mc?
                       v21 = with(v21m.cf15,  ifelse(efec==0, NA, (pan + panc + prd) / efec)))      # drop prd?
pan.cf15 <- round(pan.cf15, 3)
#
pan.cf18 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  pan  / efec)), # change with v91m.cf18 when available
                       v94 = with(v94m.cf18,  ifelse(efec==0, NA,  pan  / efec)),
                       v97 = with(v97m.cf18,  ifelse(efec==0, NA,  pan  / efec)),
                       v00 = with(v00m.cf18,  ifelse(efec==0, NA,  panc / efec)),
                       v03 = with(v03m.cf18,  ifelse(efec==0, NA,  pan  / efec)),
                       v06 = with(v06m.cf18,  ifelse(efec==0, NA,  pan  / efec)),
                       v09 = with(v09m.cf18,  ifelse(efec==0, NA,  pan  / efec)),
                       v12 = with(v12m.cf18,  ifelse(efec==0, NA,  pan  / efec)),
                       v15 = with(v15m.cf18,  ifelse(efec==0, NA,  pan  / efec)),
                       v18 = with(v18,        ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)), # drop mc?
                       v21 = with(v21m.cf18,  ifelse(efec==0, NA, (pan + panc + prd) / efec)))      # drop prd?
pan.cf18 <- round(pan.cf18, 3)
#
pan.cf21 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  pan  / efec)), # change with v91m.cf21 when available
                       v94 = with(v94m.cf21,  ifelse(efec==0, NA,  pan  / efec)),
                       v97 = with(v97m.cf21,  ifelse(efec==0, NA,  pan  / efec)),
                       v00 = with(v00m.cf21,  ifelse(efec==0, NA,  panc / efec)),
                       v03 = with(v03m.cf21,  ifelse(efec==0, NA,  pan  / efec)),
                       v06 = with(v06m.cf21,  ifelse(efec==0, NA,  pan  / efec)),
                       v09 = with(v09m.cf21,  ifelse(efec==0, NA,  pan  / efec)),
                       v12 = with(v12m.cf21,  ifelse(efec==0, NA,  pan  / efec)),
                       v15 = with(v15m.cf21,  ifelse(efec==0, NA,  pan  / efec)),
                       v18 = with(v18m.cf21,  ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)), # drop mc?
                       v21 = with(v21,        ifelse(efec==0, NA, (pan + panc + prd) / efec)))      # drop prd?
pan.cf21 <- round(pan.cf21, 3)
#
## pan.cf24 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  pan  / efec)), # change with v91m.cf24 when available
##                        v94 = with(v94m.cf24,  ifelse(efec==0, NA,  pan  / efec)),
##                        v97 = with(v97m.cf24,  ifelse(efec==0, NA,  pan  / efec)),
##                        v00 = with(v00m.cf24,  ifelse(efec==0, NA,  panc / efec)),
##                        v03 = with(v03m.cf24,  ifelse(efec==0, NA,  pan  / efec)),
##                        v06 = with(v06m.cf24,  ifelse(efec==0, NA,  pan  / efec)),
##                        v09 = with(v09m.cf24,  ifelse(efec==0, NA,  pan  / efec)),
##                        v12 = with(v12m.cf24,  ifelse(efec==0, NA,  pan  / efec)),
##                        v15 = with(v15m.cf24,  ifelse(efec==0, NA,  pan  / efec)),
##                        v18 = with(v18m.cf24,  ifelse(efec==0, NA, (pan + panc + prd + mc) / efec)), # drop mc?
##                        v21 = with(v21m.cf24,  ifelse(efec==0, NA, (pan + panc + prd) / efec)))      # drop prd?
## pan.cf24 <- round(pan.cf24, 3)
#
pri.cf06 <- data.frame(v91 = with(v91,       ifelse(efec==0, NA,  pri  / efec)), # change with v91m.cf06 when available
                       v94 = with(v94m.cf06, ifelse(efec==0, NA,  pri  / efec)),
                       v97 = with(v97m.cf06, ifelse(efec==0, NA,  pri  / efec)),
                       v00 = with(v00m.cf06, ifelse(efec==0, NA,  pri / efec)),
                       v03 = with(v03m.cf06, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v06 = with(v06,       ifelse(efec==0, NA,  pric / efec)),
                       v09 = with(v09m.cf06, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v12 = with(v12m.cf06, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v15 = with(v15m.cf06, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v18 = with(v18m.cf06, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)),  # drop pvem + pna?
                       v21 = with(v21m.cf06, ifelse(efec==0, NA, (pri + pric) / efec)))
pri.cf06 <- round(pri.cf06, 3)
#
pri.cf09 <- data.frame(v91 = with(v91,       ifelse(efec==0, NA,  pri  / efec)), # change with v91m.cf09 when available
                       v94 = with(v94m.cf09, ifelse(efec==0, NA,  pri  / efec)),
                       v97 = with(v97m.cf09, ifelse(efec==0, NA,  pri  / efec)),
                       v00 = with(v00m.cf09, ifelse(efec==0, NA,  pri / efec)),
                       v03 = with(v03m.cf09, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v06 = with(v06m.cf09, ifelse(efec==0, NA,  pric / efec)),
                       v09 = with(v09,       ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v12 = with(v12m.cf09, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v15 = with(v15m.cf09, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v18 = with(v18m.cf09, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)),  # drop pvem + pna?
                       v21 = with(v21m.cf09, ifelse(efec==0, NA, (pri + pric) / efec)))
pri.cf09 <- round(pri.cf09, 3)
#
pri.cf12 <- data.frame(v91 = with(v91,       ifelse(efec==0, NA,  pri  / efec)), # change with v91m.cf12 when available
                       v94 = with(v94m.cf12, ifelse(efec==0, NA,  pri  / efec)),
                       v97 = with(v97m.cf12, ifelse(efec==0, NA,  pri  / efec)),
                       v00 = with(v00m.cf12, ifelse(efec==0, NA,  pri / efec)),
                       v03 = with(v03m.cf12, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v06 = with(v06m.cf12, ifelse(efec==0, NA,  pric / efec)),
                       v09 = with(v09m.cf12, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v12 = with(v12,       ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v15 = with(v15m.cf12, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v18 = with(v18m.cf12, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)),  # drop pvem + pna?
                       v21 = with(v21m.cf12, ifelse(efec==0, NA, (pri + pric) / efec)))
pri.cf12 <- round(pri.cf12, 3)
#
pri.cf15 <- data.frame(v91 = with(v91,       ifelse(efec==0, NA,  pri  / efec)), # change with v91m.cf15 when available
                       v94 = with(v94m.cf15, ifelse(efec==0, NA,  pri  / efec)),
                       v97 = with(v97m.cf15, ifelse(efec==0, NA,  pri  / efec)),
                       v00 = with(v00m.cf15, ifelse(efec==0, NA,  pri / efec)),
                       v03 = with(v03m.cf15, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v06 = with(v06m.cf15, ifelse(efec==0, NA,  pric / efec)),
                       v09 = with(v09m.cf15, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v12 = with(v12m.cf15, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v15 = with(v15,       ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v18 = with(v18m.cf15, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)),  # drop pvem + pna?
                       v21 = with(v21m.cf15, ifelse(efec==0, NA, (pri + pric) / efec)))
pri.cf15 <- round(pri.cf15, 3)
#
pri.cf18 <- data.frame(v91 = with(v91,       ifelse(efec==0, NA,  pri  / efec)), # change with v91m.cf18 when available
                       v94 = with(v94m.cf18, ifelse(efec==0, NA,  pri  / efec)),
                       v97 = with(v97m.cf18, ifelse(efec==0, NA,  pri  / efec)),
                       v00 = with(v00m.cf18, ifelse(efec==0, NA,  pri / efec)),
                       v03 = with(v03m.cf18, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v06 = with(v06m.cf18, ifelse(efec==0, NA,  pric / efec)),
                       v09 = with(v09m.cf18, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v12 = with(v12m.cf18, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v15 = with(v15m.cf18, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v18 = with(v18,       ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)),  # drop pvem + pna?
                       v21 = with(v21m.cf18, ifelse(efec==0, NA, (pri + pric) / efec)))
pri.cf18 <- round(pri.cf18, 3)
#
pri.cf21 <- data.frame(v91 = with(v91,       ifelse(efec==0, NA,  pri  / efec)), # change with v91m.cf21 when available
                       v94 = with(v94m.cf21, ifelse(efec==0, NA,  pri  / efec)),
                       v97 = with(v97m.cf21, ifelse(efec==0, NA,  pri  / efec)),
                       v00 = with(v00m.cf21, ifelse(efec==0, NA,  pri / efec)),
                       v03 = with(v03m.cf21, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v06 = with(v06m.cf21, ifelse(efec==0, NA,  pric / efec)),
                       v09 = with(v09m.cf21, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v12 = with(v12m.cf21, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v15 = with(v15m.cf21, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
                       v18 = with(v18m.cf21, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)),  # drop pvem + pna?
                       v21 = with(v21,       ifelse(efec==0, NA, (pri + pric) / efec)))
pri.cf21 <- round(pri.cf21, 3)
#
## pri.cf24 <- data.frame(v91 = with(v91,       ifelse(efec==0, NA,  pri  / efec)), # change with v91m.cf24 when available
##                        v94 = with(v94m.cf24, ifelse(efec==0, NA,  pri  / efec)),
##                        v97 = with(v97m.cf24, ifelse(efec==0, NA,  pri  / efec)),
##                        v00 = with(v00m.cf24, ifelse(efec==0, NA,  pri / efec)),
##                        v03 = with(v03m.cf24, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
##                        v06 = with(v06m.cf24, ifelse(efec==0, NA,  pric / efec)),
##                        v09 = with(v09m.cf24, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
##                        v12 = with(v12m.cf24, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
##                        v15 = with(v15m.cf24, ifelse(efec==0, NA, (pri + pric + pvem) / efec)),        # drop pvem?
##                        v18 = with(v18m.cf24, ifelse(efec==0, NA, (pri + pric + pvem + pna) / efec)),  # drop pvem + pna?
##                        v21 = with(v21m.cf24, ifelse(efec==0, NA, (pri + pric) / efec)))
## pri.cf24 <- round(pri.cf24, 3)
#
left.cf06 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  prd  / efec)), # change with v91m.cf06 when available
                        v94 = with(v94m.cf06,  ifelse(efec==0, NA,  prd  / efec)),
                        v97 = with(v97m.cf06,  ifelse(efec==0, NA,  prd  / efec)),
                        v00 = with(v00m.cf06,  ifelse(efec==0, NA,  prdc / efec)),
                        v03 = with(v03m.cf06,  ifelse(efec==0, NA, (prd + pt + conve) / efec)),
                        v06 = with(v06,        ifelse(efec==0, NA,  prdc / efec)),
                        v09 = with(v09m.cf06,  ifelse(efec==0, NA, (prd + pt + ptc + conve) / efec)),
                        v12 = with(v12m.cf06,  ifelse(efec==0, NA, (prd + prdc + pt + mc)  / efec)),
                        v15 = with(v15m.cf06,  ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
                        v18 = with(v18m.cf06,  ifelse(efec==0, NA, (morena + morenac + pt + pes) / efec)),
                        v21 = with(v21m.cf06,  ifelse(efec==0, NA, (morena + morenac + pt + pvem) / efec)))    # drop pt + pvem?
left.cf06 <- round(left.cf06, 3)
#
left.cf09 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  prd  / efec)), # change with v91m.cf09 when available
                        v94 = with(v94m.cf09,  ifelse(efec==0, NA,  prd  / efec)),
                        v97 = with(v97m.cf09,  ifelse(efec==0, NA,  prd  / efec)),
                        v00 = with(v00m.cf09,  ifelse(efec==0, NA,  prdc / efec)),
                        v03 = with(v03m.cf09,  ifelse(efec==0, NA, (prd + pt + conve) / efec)),
                        v06 = with(v06m.cf09,  ifelse(efec==0, NA,  prdc / efec)),
                        v09 = with(v09,        ifelse(efec==0, NA, (prd + pt + ptc + conve) / efec)),
                        v12 = with(v12m.cf09,  ifelse(efec==0, NA, (prd + prdc + pt + mc)  / efec)),
                        v15 = with(v15m.cf09,  ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
                        v18 = with(v18m.cf09,  ifelse(efec==0, NA, (morena + morenac + pt + pes) / efec)),
                        v21 = with(v21m.cf09,  ifelse(efec==0, NA, (morena + morenac + pt + pvem) / efec)))    # drop pt + pvem?
left.cf09 <- round(left.cf09, 3)
#
left.cf12 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  prd  / efec)), # change with v91m.cf12 when available
                        v94 = with(v94m.cf12,  ifelse(efec==0, NA,  prd  / efec)),
                        v97 = with(v97m.cf12,  ifelse(efec==0, NA,  prd  / efec)),
                        v00 = with(v00m.cf12,  ifelse(efec==0, NA,  prdc / efec)),
                        v03 = with(v03m.cf12,  ifelse(efec==0, NA, (prd + pt + conve) / efec)),
                        v06 = with(v06m.cf12,  ifelse(efec==0, NA,  prdc / efec)),
                        v09 = with(v09m.cf12,  ifelse(efec==0, NA, (prd + pt + ptc + conve) / efec)),
                        v12 = with(v12,        ifelse(efec==0, NA, (prd + prdc + pt + mc)  / efec)),
                        v15 = with(v15m.cf12,  ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
                        v18 = with(v18m.cf12,  ifelse(efec==0, NA, (morena + morenac + pt + pes) / efec)),
                        v21 = with(v21m.cf12,  ifelse(efec==0, NA, (morena + morenac + pt + pvem) / efec)))    # drop pt + pvem?
left.cf12 <- round(left.cf12, 3)
#
left.cf15 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  prd  / efec)), # change with v91m.cf15 when available
                        v94 = with(v94m.cf15,  ifelse(efec==0, NA,  prd  / efec)),
                        v97 = with(v97m.cf15,  ifelse(efec==0, NA,  prd  / efec)),
                        v00 = with(v00m.cf15,  ifelse(efec==0, NA,  prdc / efec)),
                        v03 = with(v03m.cf15,  ifelse(efec==0, NA, (prd + pt + conve) / efec)),
                        v06 = with(v06m.cf15,  ifelse(efec==0, NA,  prdc / efec)),
                        v09 = with(v09m.cf15,  ifelse(efec==0, NA, (prd + pt + ptc + conve) / efec)),
                        v12 = with(v12m.cf15,  ifelse(efec==0, NA, (prd + prdc + pt + mc)  / efec)),
                        v15 = with(v15,        ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
                        v18 = with(v18m.cf15,  ifelse(efec==0, NA, (morena + morenac + pt + pes) / efec)),
                        v21 = with(v21m.cf15,  ifelse(efec==0, NA, (morena + morenac + pt + pvem) / efec)))    # drop pt + pvem?
left.cf15 <- round(left.cf15, 3)
#
left.cf18 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  prd  / efec)), # change with v91m.cf18 when available
                        v94 = with(v94m.cf18,  ifelse(efec==0, NA,  prd  / efec)),
                        v97 = with(v97m.cf18,  ifelse(efec==0, NA,  prd  / efec)),
                        v00 = with(v00m.cf18,  ifelse(efec==0, NA,  prdc / efec)),
                        v03 = with(v03m.cf18,  ifelse(efec==0, NA, (prd + pt + conve) / efec)),
                        v06 = with(v06m.cf18,  ifelse(efec==0, NA,  prdc / efec)),
                        v09 = with(v09m.cf18,  ifelse(efec==0, NA, (prd + pt + ptc + conve) / efec)),
                        v12 = with(v12m.cf18,  ifelse(efec==0, NA, (prd + prdc + pt + mc)  / efec)),
                        v15 = with(v15m.cf18,  ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
                        v18 = with(v18,        ifelse(efec==0, NA, (morena + morenac + pt + pes) / efec)),
                        v21 = with(v21m.cf18,  ifelse(efec==0, NA, (morena + morenac + pt + pvem) / efec)))    # drop pt + pvem?
left.cf18 <- round(left.cf18, 3)
#
left.cf21 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  prd  / efec)), # change with v91m.cf21 when available
                        v94 = with(v94m.cf21,  ifelse(efec==0, NA,  prd  / efec)),
                        v97 = with(v97m.cf21,  ifelse(efec==0, NA,  prd  / efec)),
                        v00 = with(v00m.cf21,  ifelse(efec==0, NA,  prdc / efec)),
                        v03 = with(v03m.cf21,  ifelse(efec==0, NA, (prd + pt + conve) / efec)),
                        v06 = with(v06m.cf21,  ifelse(efec==0, NA,  prdc / efec)),
                        v09 = with(v09m.cf21,  ifelse(efec==0, NA, (prd + pt + ptc + conve) / efec)),
                        v12 = with(v12m.cf21,  ifelse(efec==0, NA, (prd + prdc + pt + mc)  / efec)),
                        v15 = with(v15m.cf21,  ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
                        v18 = with(v18m.cf21,  ifelse(efec==0, NA, (morena + morenac + pt + pes) / efec)),
                        v21 = with(v21,        ifelse(efec==0, NA, (morena + morenac + pt + pvem) / efec)))    # drop pt + pvem?
left.cf21 <- round(left.cf21, 3)
#
## left.cf24 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA,  prd  / efec)), # change with v91m.cf24 when available
##                         v94 = with(v94m.cf24,  ifelse(efec==0, NA,  prd  / efec)),
##                         v97 = with(v97m.cf24,  ifelse(efec==0, NA,  prd  / efec)),
##                         v00 = with(v00m.cf24,  ifelse(efec==0, NA,  prdc / efec)),
##                         v03 = with(v03m.cf24,  ifelse(efec==0, NA, (prd + pt + conve) / efec)),
##                         v06 = with(v06m.cf24,  ifelse(efec==0, NA,  prdc / efec)),
##                         v09 = with(v09m.cf24,  ifelse(efec==0, NA, (prd + pt + ptc + conve) / efec)),
##                         v12 = with(v12m.cf24,  ifelse(efec==0, NA, (prd + prdc + pt + mc)  / efec)),
##                         v15 = with(v15m.cf24,  ifelse(efec==0, NA, (prd + prdc + pt + morena + pes) / efec)), 
##                         v18 = with(v18m.cf24,  ifelse(efec==0, NA, (morena + morenac + pt + pes) / efec)),
##                         v21 = with(v21m.cf24,  ifelse(efec==0, NA, (morena + morenac + pt + pvem) / efec)))    # drop pt + pvem?
## left.cf24 <- round(left.cf24, 3)
#
oth.cf06 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt) / efec)),# chg w v91m.cf06 when available
                       v94 = with(v94m.cf06,  ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
                       v97 = with(v97m.cf06,  ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm) / efec)),
                       v00 = with(v00m.cf06,  ifelse(efec==0, NA, (pcd + parm + dsppn) / efec)),
                       v03 = with(v03m.cf06,  ifelse(efec==0, NA, (psn + pas + mp + plm + fc) / efec)),
                       v06 = with(v06,        ifelse(efec==0, NA, (pna + asdc) / efec)),
                       v09 = with(v09m.cf06,  ifelse(efec==0, NA, (pna + psd) / efec)),
                       v12 = with(v12m.cf06,  ifelse(efec==0, NA,  pna / efec)),
                       v15 = with(v15m.cf06,  ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2) / efec)),
                       v18 = with(v18m.cf06,  ifelse(efec==0, NA, (indep1 + indep2) / efec)),
                       v21 = with(v21m.cf06,  ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep) / efec)))
oth.cf06 <- round(oth.cf06, 3)
#
oth.cf09 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt) / efec)), # chg w v91m.cf09 when available
                       v94 = with(v94m.cf09,  ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
                       v97 = with(v97m.cf09,  ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm) / efec)),
                       v00 = with(v00m.cf09,  ifelse(efec==0, NA, (pcd + parm + dsppn) / efec)),
                       v03 = with(v03m.cf09,  ifelse(efec==0, NA, (psn + pas + mp + plm + fc) / efec)),
                       v06 = with(v06m.cf09,  ifelse(efec==0, NA, (pna + asdc) / efec)),
                       v09 = with(v09,        ifelse(efec==0, NA, (pna + psd) / efec)),
                       v12 = with(v12m.cf09,  ifelse(efec==0, NA,  pna / efec)),
                       v15 = with(v15m.cf09,  ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2) / efec)),
                       v18 = with(v18m.cf09,  ifelse(efec==0, NA, (indep1 + indep2) / efec)),
                       v21 = with(v21m.cf09,  ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep) / efec)))
oth.cf09 <- round(oth.cf09, 3)
#
oth.cf12 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt) / efec)), # change with v91m.cf12 when available
                       v94 = with(v94m.cf12,  ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
                       v97 = with(v97m.cf12,  ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm) / efec)),
                       v00 = with(v00m.cf12,  ifelse(efec==0, NA, (pcd + parm + dsppn) / efec)),
                       v03 = with(v03m.cf12,  ifelse(efec==0, NA, (psn + pas + mp + plm + fc) / efec)),
                       v06 = with(v06m.cf12,  ifelse(efec==0, NA, (pna + asdc) / efec)),
                       v09 = with(v09m.cf12,  ifelse(efec==0, NA, (pna + psd) / efec)),
                       v12 = with(v12,        ifelse(efec==0, NA,  pna / efec)),
                       v15 = with(v15m.cf12,  ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2) / efec)),
                       v18 = with(v18m.cf12,  ifelse(efec==0, NA, (indep1 + indep2) / efec)),
                       v21 = with(v21m.cf12,  ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep) / efec)))
oth.cf12 <- round(oth.cf12, 3)
#
oth.cf15 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt) / efec)), # change with v91m.cf15 when available
                       v94 = with(v94m.cf15,  ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
                       v97 = with(v97m.cf15,  ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm) / efec)),
                       v00 = with(v00m.cf15,  ifelse(efec==0, NA, (pcd + parm + dsppn) / efec)),
                       v03 = with(v03m.cf15,  ifelse(efec==0, NA, (psn + pas + mp + plm + fc) / efec)),
                       v06 = with(v06m.cf15,  ifelse(efec==0, NA, (pna + asdc) / efec)),
                       v09 = with(v09m.cf15,  ifelse(efec==0, NA, (pna + psd) / efec)),
                       v12 = with(v12m.cf15,  ifelse(efec==0, NA,  pna / efec)),
                       v15 = with(v15,        ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2) / efec)),
                       v18 = with(v18m.cf15,  ifelse(efec==0, NA, (indep1 + indep2) / efec)),
                       v21 = with(v21m.cf15,  ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep) / efec)))
oth.cf15 <- round(oth.cf15, 3)
#
oth.cf18 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt) / efec)), # change with v91m.cf18 when available
                       v94 = with(v94m.cf18,  ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
                       v97 = with(v97m.cf18,  ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm) / efec)),
                       v00 = with(v00m.cf18,  ifelse(efec==0, NA, (pcd + parm + dsppn) / efec)),
                       v03 = with(v03m.cf18,  ifelse(efec==0, NA, (psn + pas + mp + plm + fc) / efec)),
                       v06 = with(v06m.cf18,  ifelse(efec==0, NA, (pna + asdc) / efec)),
                       v09 = with(v09m.cf18,  ifelse(efec==0, NA, (pna + psd) / efec)),
                       v12 = with(v12m.cf18,  ifelse(efec==0, NA,  pna / efec)),
                       v15 = with(v15m.cf18,  ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2) / efec)),
                       v18 = with(v18,        ifelse(efec==0, NA, (indep1 + indep2) / efec)),
                       v21 = with(v21m.cf18,  ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep) / efec)))
oth.cf18 <- round(oth.cf18, 3)
#
oth.cf21 <- data.frame(v91 = with(v91,        ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt) / efec)), # change with v91m.cf21 when available
                       v94 = with(v94m.cf21,  ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
                       v97 = with(v97m.cf21,  ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm) / efec)),
                       v00 = with(v00m.cf21,  ifelse(efec==0, NA, (pcd + parm + dsppn) / efec)),
                       v03 = with(v03m.cf21,  ifelse(efec==0, NA, (psn + pas + mp + plm + fc) / efec)),
                       v06 = with(v06m.cf21,  ifelse(efec==0, NA, (pna + asdc) / efec)),
                       v09 = with(v09m.cf21,  ifelse(efec==0, NA, (pna + psd) / efec)),
                       v12 = with(v12m.cf21,  ifelse(efec==0, NA,  pna / efec)),
                       v15 = with(v15m.cf21,  ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2) / efec)),
                       v18 = with(v18m.cf21,  ifelse(efec==0, NA, (indep1 + indep2) / efec)),
                       v21 = with(v21,        ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep) / efec)))
oth.cf21 <- round(oth.cf21, 3)
#
## oth.cf24 <- data.frame(91 = with(v91,        ifelse(efec==0, NA, (parm + pdm + pfcrn + pps + pem + prt) / efec)), # change with #v91m.cf24 when available
##                        v94 = with(v94m.cf24,  ifelse(efec==0, NA, (pps + pfcrn + parm + uno.pdm + pt + pvem) / efec)),
##                        v97 = with(v97m.cf24,  ifelse(efec==0, NA, (pc + pt + pvem + pps + pdm) / efec)),
##                        v00 = with(v00m.cf24,  ifelse(efec==0, NA, (pcd + parm + dsppn) / efec)),
##                        v03 = with(v03m.cf24,  ifelse(efec==0, NA, (psn + pas + mp + plm + fc) / efec)),
##                        v06 = with(v06m.cf24,  ifelse(efec==0, NA, (pna + asdc) / efec)),
##                        v09 = with(v09m.cf24,  ifelse(efec==0, NA, (pna + psd) / efec)),
##                        v12 = with(v12m.cf24,  ifelse(efec==0, NA,  pna / efec)),
##                        v15 = with(v15m.cf24,  ifelse(efec==0, NA, (mc + pna + ph + indep1 + indep2) / efec)),
##                        v18 = with(v18m.cf24,  ifelse(efec==0, NA, (indep1 + indep2) / efec)),
##                        v21 = with(v21m.cf24,  ifelse(efec==0, NA, (mc + pes + rsp + fxm + indep) / efec)))
## oth.cf24 <- round(oth.cf24, 3)
#
efec.cf06 <- data.frame(v91 = v91$efec, # change with v91m.cf06 when available
                        v94 = v94m.cf06$efec,
                        v97 = v97m.cf06$efec,
                        v00 = v00m.cf06$efec,
                        v03 = v03m.cf06$efec,
                        v06 = v06$efec,
                        v09 = v09m.cf06$efec,
                        v12 = v12m.cf06$efec,
                        v15 = v15m.cf06$efec,
                        v18 = v18m.cf06$efec,
                        v21 = v21m.cf06$efec)
#
efec.cf09 <- data.frame(v91 = v91$efec, # change with v91m.cf09 when available
                        v94 = v94m.cf09$efec,
                        v97 = v97m.cf09$efec,
                        v00 = v00m.cf09$efec,
                        v03 = v03m.cf09$efec,
                        v06 = v06m.cf09$efec,
                        v09 = v09$efec,
                        v12 = v12m.cf09$efec,
                        v15 = v15m.cf09$efec,
                        v18 = v18m.cf09$efec,
                        v21 = v21m.cf09$efec)
#
efec.cf12 <- data.frame(v91 = v91$efec, # change with v91m.cf12 when available
                        v94 = v94m.cf12$efec,
                        v97 = v97m.cf12$efec,
                        v00 = v00m.cf12$efec,
                        v03 = v03m.cf12$efec,
                        v06 = v06m.cf12$efec,
                        v09 = v09m.cf12$efec,
                        v12 = v12$efec,
                        v15 = v15m.cf12$efec,
                        v18 = v18m.cf12$efec,
                        v21 = v21m.cf12$efec)
#
efec.cf15 <- data.frame(v91 = v91$efec, # change with v91m.cf15 when available
                        v94 = v94m.cf15$efec,
                        v97 = v97m.cf15$efec,
                        v00 = v00m.cf15$efec,
                        v03 = v03m.cf15$efec,
                        v06 = v06m.cf15$efec,
                        v09 = v09m.cf15$efec,
                        v12 = v12m.cf15$efec,
                        v15 = v15$efec,
                        v18 = v18m.cf15$efec,
                        v21 = v21m.cf15$efec)
#
efec.cf18 <- data.frame(v91 = v91$efec, # change with v91m.cf18 when available
                        v94 = v94m.cf18$efec,
                        v97 = v97m.cf18$efec,
                        v00 = v00m.cf18$efec,
                        v03 = v03m.cf18$efec,
                        v06 = v06m.cf18$efec,
                        v09 = v09m.cf18$efec,
                        v12 = v12m.cf18$efec,
                        v15 = v15m.cf18$efec,
                        v18 = v18$efec,
                        v21 = v21m.cf18$efec)
#
efec.cf21 <- data.frame(v91 = v91$efec, # change with v91m.cf21 when available
                        v94 = v94m.cf21$efec,
                        v97 = v97m.cf21$efec,
                        v00 = v00m.cf21$efec,
                        v03 = v03m.cf21$efec,
                        v06 = v06m.cf21$efec,
                        v09 = v09m.cf21$efec,
                        v12 = v12m.cf21$efec,
                        v15 = v15m.cf21$efec,
                        v18 = v18m.cf21$efec,
                        v21 = v21$efec)
#
## efec.cf24 <- data.frame(v91 = v91$efec, # change with v91m.cf24 when available
##                         v94 = v94m.cf24$efec,
##                         v97 = v97m.cf24$efec,
##                         v00 = v00m.cf24$efec,
##                         v03 = v03m.cf24$efec,
##                         v06 = v06m.cf24$efec,
##                         v09 = v09m.cf24$efec,
##                         v12 = v12m.cf24$efec,
##                         v15 = v15m.cf24$efec,
##                         v18 = v18m.cf24$efec,
##                         v21 = v21m.cf24$efec)
#
    pan.cf06  <- t(pan.cf06)
    pri.cf06  <- t(pri.cf06)
    left.cf06 <- t(left.cf06)
    oth.cf06  <- t(oth.cf06)
    efec.cf06 <- t(efec.cf06)
    #
    pan.cf09  <- t(pan.cf09)
    pri.cf09  <- t(pri.cf09)
    left.cf09 <- t(left.cf09)
    oth.cf09  <- t(oth.cf09)
    efec.cf09 <- t(efec.cf09)
    #
    pan.cf12  <- t(pan.cf12)
    pri.cf12  <- t(pri.cf12)
    left.cf12 <- t(left.cf12)
    oth.cf12  <- t(oth.cf12)
    efec.cf12 <- t(efec.cf12)
    #
    pan.cf15  <- t(pan.cf15)
    pri.cf15  <- t(pri.cf15)
    left.cf15 <- t(left.cf15)
    oth.cf15  <- t(oth.cf15)
    efec.cf15 <- t(efec.cf15)
    #
    pan.cf18  <- t(pan.cf18)
    pri.cf18  <- t(pri.cf18)
    left.cf18 <- t(left.cf18)
    oth.cf18  <- t(oth.cf18)
    efec.cf18 <- t(efec.cf18)
    #
    pan.cf21  <- t(pan.cf21)
    pri.cf21  <- t(pri.cf21)
    left.cf21 <- t(left.cf21)
    oth.cf21  <- t(oth.cf21)
    efec.cf21 <- t(efec.cf21)
    #
    ## pan.cf24  <- t(pan.cf24)
    ## pri.cf24  <- t(pri.cf24)
    ## left.cf24 <- t(left.cf24)
    ## oth.cf24  <- t(oth.cf24)
    ## efec.cf24 <- t(efec.cf24)
#
#
#################################################
## counterfactual municipio extendCoal objects ##
#################################################
for (i in 1:nrow(v00)){
    #i <- 34 # debug
    message(sprintf("loop %s of %s", i, nrow(v00)))
    # votes with counterfactual municicios for regressions with new municipios
    ##########
    ## cf06 ##
    ##########
    tmp <- data.frame(yr   = seq(from=1991, to=2021, by=3), # add 1991 when available
                      pan  = pan.cf06[,i],
                      pri  = pri.cf06[,i],
                      left = left.cf06[,i],
                      oth  = oth.cf06[,i],
                      efec = efec.cf06[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec[is.na(tmp$efec) | tmp$efec==0] <- 1
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    #
    # fill info to new list with period means
    tmp <- cbind(tmp, yr.means[,6:8])
    extendCoal.cf06[[i]] <- tmp
    #
    # votes with counterfactual municicios for regressions
    ##########
    ## cf09 ##
    ##########
    tmp <- data.frame(yr   = seq(from=1991, to=2021, by=3), # add 1991 when available
                      pan  = pan.cf09[,i],
                      pri  = pri.cf09[,i],
                      left = left.cf09[,i],
                      oth  = oth.cf09[,i],
                      efec = efec.cf09[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec[is.na(tmp$efec) | tmp$efec==0] <- 1
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    #
    # fill info to new list with period means
    tmp <- cbind(tmp, yr.means[,6:8])
    extendCoal.cf09[[i]] <- tmp
    #
    # votes with counterfactual municicios for regressions
    ##########
    ## cf12 ##
    ##########
    tmp <- data.frame(yr   = seq(from=1991, to=2021, by=3), # add 1991 when available
                      pan  = pan.cf12[,i],
                      pri  = pri.cf12[,i],
                      left = left.cf12[,i],
                      oth  = oth.cf12[,i],
                      efec = efec.cf12[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec[is.na(tmp$efec) | tmp$efec==0] <- 1
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    #
    # fill info to new list with period means
    tmp <- cbind(tmp, yr.means[,6:8])
    extendCoal.cf12[[i]] <- tmp
    #
    # votes with counterfactual municicios for regressions
    ##########
    ## cf15 ##
    ##########
    tmp <- data.frame(yr   = seq(from=1991, to=2021, by=3), # add 1991 when available
                      pan  = pan.cf15[,i],
                      pri  = pri.cf15[,i],
                      left = left.cf15[,i],
                      oth  = oth.cf15[,i],
                      efec = efec.cf15[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec[is.na(tmp$efec) | tmp$efec==0] <- 1
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    #
    # fill info to new list with period means
    tmp <- cbind(tmp, yr.means[,6:8])
    extendCoal.cf15[[i]] <- tmp
    #
    # votes with counterfactual municicios for regressions
    ##########
    ## cf18 ##
    ##########
    tmp <- data.frame(yr   = seq(from=1991, to=2021, by=3), # add 1991 when available
                      pan  = pan.cf18[,i],
                      pri  = pri.cf18[,i],
                      left = left.cf18[,i],
                      oth  = oth.cf18[,i],
                      efec = efec.cf18[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec[is.na(tmp$efec) | tmp$efec==0] <- 1
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    # fill info to new list
    extendCoal.cf18[[i]] <- tmp
    #
    # fill info to new list with period means
    tmp <- cbind(tmp, yr.means[,6:8])
    extendCoal.cf18[[i]] <- tmp
    #
    # votes with counterfactual municicios for regressions
    ##########
    ## cf21 ##
    ##########
    tmp <- data.frame(yr   = seq(from=1991, to=2021, by=3), # add 1991 when available
                      pan  = pan.cf21[,i],
                      pri  = pri.cf21[,i],
                      left = left.cf21[,i],
                      oth  = oth.cf21[,i],
                      efec = efec.cf21[,i])
    # replace NAs with period's mean
    if (length(tmp[is.na(tmp)])>0){
        per.means <- round(apply(tmp, 2, function(x) mean(x, na.rm = TRUE)), 3)
        tmp$pan [is.na(tmp$pan)]  <- per.means["pan"];
        tmp$pri [is.na(tmp$pri)]  <- per.means["pri"];
        tmp$left[is.na(tmp$left)] <- per.means["left"];
        tmp$oth [is.na(tmp$oth)]  <- per.means["oth"];
        tmp$efec[is.na(tmp$efec) | tmp$efec==0] <- 1
    }
    # add epsilon = 2*max(rounding error) to zeroes to avoid indeterminate logs
    if (length(tmp[tmp==0])>0){
        tmp[tmp==0] <- 0.001;
    }
    # re-compute shares to add to 1
    tmp[,2:5] <- round(tmp[,2:5] / rowSums(tmp[,2:5]),3)
    # add id
    if (agg=="m") tmp$ife     <- v00$ife[i]
    if (agg=="s") tmp$edosecn <- v00$edon[i]*10000 + v00$seccion[i] # untested
    #
    # fill info to new list with period means
    tmp <- cbind(tmp, yr.means[,6:8])
    extendCoal.cf21[[i]] <- tmp
}

# save to restore later
non.nas.orig <- non.nas

#####################################
## counterfactual regressions code ##
#####################################
#
##########
## cf06 ##
##########
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
        data.frame(pan    = rep(NA, nrow(v00)),
                   pri  = rep(NA, nrow(v00)),
                   left = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       left   = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      left   = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                   # regression per municipio, each used to
                                   # predict votes in 2006:2021
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006.cf06 <- regs.2009.cf06 <- regs.2012.cf06 <- regs.2015.cf06 <- regs.2018.cf06 <- regs.2021.cf06 <- 
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regs.cf06 <- list(pan    = tmp,
                       left   = tmp,
                       oth    = tmp,
                       readme = "No pri regs bec DVs are pri-ratios")
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoal.cf06, sum)
#non.nas
#extendCoal.cf06[[2466]]
non.nas <- unlist(non.nas)
non.nas <- which(is.na(non.nas)==FALSE)
#length(non.nas)
#    
for (i in non.nas){
    #i <- 34 # debug
    #i <- 44508 # debug
    message(sprintf("Estimating regs for couterfactual 2006 municipal map loop %s of %s", i, max(non.nas)))
    # subset data to single unit
    data.tmp <- extendCoal.cf06[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA,data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA,data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA,data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ##################################
    ## predict 2006 with last 5 els ## ojo: v91 needed
    ##################################
    year <- 2006
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    vhat.2006           [i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006.cf06$pan [[i]] <- reg.pan
    regs.2006.cf06$left[[i]] <- reg.left
    regs.2006.cf06$oth [[i]] <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    year <- 2009
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2009.cf06$pan[[i]]    <- reg.pan
    regs.2009.cf06$left[[i]]   <- reg.left
    regs.2009.cf06$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    year <- 2012
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2012.cf06$pan[[i]]    <- reg.pan
    regs.2012.cf06$left[[i]]   <- reg.left
    regs.2012.cf06$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2015.cf06$pan[[i]]    <- reg.pan
    regs.2015.cf06$left[[i]]   <- reg.left
    regs.2015.cf06$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2018.cf06$pan[[i]]    <- reg.pan
    regs.2018.cf06$left[[i]]   <- reg.left
    regs.2018.cf06$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    year <- 2021
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2021.cf06$pan[[i]]    <- reg.pan
    regs.2021.cf06$left[[i]]   <- reg.left
    regs.2021.cf06$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)  ~ mean.rpan,  data = data.tmp)
    reg.left   <- lm(formula = log(left/pri) ~ mean.rleft, data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)  ~ mean.roth,  data = data.tmp)
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
    mean.regs.cf06$pan [[i]] <- reg.pan
    mean.regs.cf06$left[[i]] <- reg.left  
    mean.regs.cf06$oth [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left   <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.left   <- alphahat$left  [i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.left   <- betahat$left  [i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
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
    extendCoal.cf06[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################
#
##########
## cf09 ##
##########
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
        data.frame(pan    = rep(NA, nrow(v00)),
                   pri  = rep(NA, nrow(v00)),
                   left = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       left   = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      left   = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                   # regression per municipio, each used to
                                   # predict votes in 2006:2021
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006.cf09 <- regs.2009.cf09 <- regs.2012.cf09 <- regs.2015.cf09 <- regs.2018.cf09 <- regs.2021.cf09 <- 
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regs.cf09 <- list(pan    = tmp,
                       left   = tmp,
                       oth    = tmp,
                       readme = "No pri regs bec DVs are pri-ratios")
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoal.cf09, sum)
#non.nas
#extendCoal.cf09[[2466]]
non.nas <- unlist(non.nas)
non.nas <- which(is.na(non.nas)==FALSE)
#length(non.nas)
#    
for (i in non.nas){
    #i <- 34 # debug
    #i <- 44508 # debug
    message(sprintf("Estimating regs for couterfactual 2009 municipal map loop %s of %s", i, max(non.nas)))
    # subset data to single unit
    data.tmp <- extendCoal.cf09[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA,data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA,data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA,data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ##################################
    ## predict 2006 with last 5 els ## ojo: v91 needed
    ##################################
    year <- 2006
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    vhat.2006           [i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006.cf09$pan [[i]] <- reg.pan
    regs.2006.cf09$left[[i]] <- reg.left
    regs.2006.cf09$oth [[i]] <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    year <- 2009
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2009.cf09$pan[[i]]    <- reg.pan
    regs.2009.cf09$left[[i]]   <- reg.left
    regs.2009.cf09$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    year <- 2012
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2012.cf09$pan[[i]]    <- reg.pan
    regs.2012.cf09$left[[i]]   <- reg.left
    regs.2012.cf09$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2015.cf09$pan[[i]]    <- reg.pan
    regs.2015.cf09$left[[i]]   <- reg.left
    regs.2015.cf09$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2018.cf09$pan[[i]]    <- reg.pan
    regs.2018.cf09$left[[i]]   <- reg.left
    regs.2018.cf09$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    year <- 2021
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2021.cf09$pan[[i]]    <- reg.pan
    regs.2021.cf09$left[[i]]   <- reg.left
    regs.2021.cf09$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)  ~ mean.rpan,  data = data.tmp)
    reg.left   <- lm(formula = log(left/pri) ~ mean.rleft, data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)  ~ mean.roth,  data = data.tmp)
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
    mean.regs.cf09$pan [[i]] <- reg.pan
    mean.regs.cf09$left[[i]] <- reg.left  
    mean.regs.cf09$oth [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left   <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.left   <- alphahat$left  [i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.left   <- betahat$left  [i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
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
    extendCoal.cf09[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################
#
##########
## cf12 ##
##########
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
        data.frame(pan    = rep(NA, nrow(v00)),
                   pri  = rep(NA, nrow(v00)),
                   left = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       left   = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      left   = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                   # regression per municipio, each used to
                                   # predict votes in 2006:2021
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006.cf12 <- regs.2009.cf12 <- regs.2012.cf12 <- regs.2015.cf12 <- regs.2018.cf12 <- regs.2021.cf12 <- 
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regs.cf12 <- list(pan    = tmp,
                       left   = tmp,
                       oth    = tmp,
                       readme = "No pri regs bec DVs are pri-ratios")
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoal.cf12, sum)
#non.nas
#extendCoal.cf12[[2466]]
non.nas <- unlist(non.nas)
non.nas <- which(is.na(non.nas)==FALSE)
#length(non.nas)
#    
for (i in non.nas){
    #i <- 34 # debug
    #i <- 44508 # debug
    message(sprintf("Estimating regs for couterfactual 2012 municipal map loop %s of %s", i, max(non.nas)))
    # subset data to single unit
    data.tmp <- extendCoal.cf12[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA,data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA,data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA,data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ##################################
    ## predict 2006 with last 5 els ## ojo: v91 needed
    ##################################
    year <- 2006
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    vhat.2006           [i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006.cf12$pan [[i]] <- reg.pan
    regs.2006.cf12$left[[i]] <- reg.left
    regs.2006.cf12$oth [[i]] <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    year <- 2009
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2009.cf12$pan[[i]]    <- reg.pan
    regs.2009.cf12$left[[i]]   <- reg.left
    regs.2009.cf12$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    year <- 2012
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2012.cf12$pan[[i]]    <- reg.pan
    regs.2012.cf12$left[[i]]   <- reg.left
    regs.2012.cf12$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2015.cf12$pan[[i]]    <- reg.pan
    regs.2015.cf12$left[[i]]   <- reg.left
    regs.2015.cf12$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2018.cf12$pan[[i]]    <- reg.pan
    regs.2018.cf12$left[[i]]   <- reg.left
    regs.2018.cf12$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    year <- 2021
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2021.cf12$pan[[i]]    <- reg.pan
    regs.2021.cf12$left[[i]]   <- reg.left
    regs.2021.cf12$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)  ~ mean.rpan,  data = data.tmp)
    reg.left   <- lm(formula = log(left/pri) ~ mean.rleft, data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)  ~ mean.roth,  data = data.tmp)
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
    mean.regs.cf12$pan [[i]] <- reg.pan
    mean.regs.cf12$left[[i]] <- reg.left  
    mean.regs.cf12$oth [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left   <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.left   <- alphahat$left  [i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.left   <- betahat$left  [i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
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
    extendCoal.cf12[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################
#
##########
## cf15 ##
##########
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
        data.frame(pan    = rep(NA, nrow(v00)),
                   pri  = rep(NA, nrow(v00)),
                   left = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       left   = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      left   = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                   # regression per municipio, each used to
                                   # predict votes in 2006:2021
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006.cf15 <- regs.2009.cf15 <- regs.2012.cf15 <- regs.2015.cf15 <- regs.2018.cf15 <- regs.2021.cf15 <- 
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regs.cf15 <- list(pan    = tmp,
                       left   = tmp,
                       oth    = tmp,
                       readme = "No pri regs bec DVs are pri-ratios")
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoal.cf15, sum)
#non.nas
#extendCoal.cf15[[2466]]
non.nas <- unlist(non.nas)
non.nas <- which(is.na(non.nas)==FALSE)
#length(non.nas)
#    
for (i in non.nas){
    #i <- 34 # debug
    #i <- 44508 # debug
    message(sprintf("Estimating regs for couterfactual 2015 municipal map loop %s of %s", i, max(non.nas)))
    # subset data to single unit
    data.tmp <- extendCoal.cf15[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA,data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA,data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA,data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ##################################
    ## predict 2006 with last 5 els ## ojo: v91 needed
    ##################################
    year <- 2006
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    vhat.2006           [i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006.cf15$pan [[i]] <- reg.pan
    regs.2006.cf15$left[[i]] <- reg.left
    regs.2006.cf15$oth [[i]] <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    year <- 2009
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2009.cf15$pan[[i]]    <- reg.pan
    regs.2009.cf15$left[[i]]   <- reg.left
    regs.2009.cf15$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    year <- 2012
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2012.cf15$pan[[i]]    <- reg.pan
    regs.2012.cf15$left[[i]]   <- reg.left
    regs.2012.cf15$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2015.cf15$pan[[i]]    <- reg.pan
    regs.2015.cf15$left[[i]]   <- reg.left
    regs.2015.cf15$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2018.cf15$pan[[i]]    <- reg.pan
    regs.2018.cf15$left[[i]]   <- reg.left
    regs.2018.cf15$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    year <- 2021
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2021.cf15$pan[[i]]    <- reg.pan
    regs.2021.cf15$left[[i]]   <- reg.left
    regs.2021.cf15$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)  ~ mean.rpan,  data = data.tmp)
    reg.left   <- lm(formula = log(left/pri) ~ mean.rleft, data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)  ~ mean.roth,  data = data.tmp)
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
    mean.regs.cf15$pan [[i]] <- reg.pan
    mean.regs.cf15$left[[i]] <- reg.left  
    mean.regs.cf15$oth [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left   <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.left   <- alphahat$left  [i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.left   <- betahat$left  [i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
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
    extendCoal.cf15[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################
#
##########
## cf18 ##
##########
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
        data.frame(pan    = rep(NA, nrow(v00)),
                   pri  = rep(NA, nrow(v00)),
                   left = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       left   = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      left   = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                   # regression per municipio, each used to
                                   # predict votes in 2006:2021
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006.cf18 <- regs.2009.cf18 <- regs.2012.cf18 <- regs.2015.cf18 <- regs.2018.cf18 <- regs.2021.cf18 <- 
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regs.cf18 <- list(pan    = tmp,
                       left   = tmp,
                       oth    = tmp,
                       readme = "No pri regs bec DVs are pri-ratios")
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoal.cf18, sum)
#non.nas
#extendCoal.cf18[[2466]]
non.nas <- unlist(non.nas)
non.nas <- which(is.na(non.nas)==FALSE)
#length(non.nas)
#    
for (i in non.nas){
    #i <- 34 # debug
    #i <- 44508 # debug
    message(sprintf("Estimating regs for couterfactual 2018 municipal map loop %s of %s", i, max(non.nas)))
    # subset data to single unit
    data.tmp <- extendCoal.cf18[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA,data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA,data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA,data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ##################################
    ## predict 2006 with last 5 els ## ojo: v91 needed
    ##################################
    year <- 2006
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    vhat.2006           [i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006.cf18$pan [[i]] <- reg.pan
    regs.2006.cf18$left[[i]] <- reg.left
    regs.2006.cf18$oth [[i]] <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    year <- 2009
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2009.cf18$pan[[i]]    <- reg.pan
    regs.2009.cf18$left[[i]]   <- reg.left
    regs.2009.cf18$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    year <- 2012
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2012.cf18$pan[[i]]    <- reg.pan
    regs.2012.cf18$left[[i]]   <- reg.left
    regs.2012.cf18$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2015.cf18$pan[[i]]    <- reg.pan
    regs.2015.cf18$left[[i]]   <- reg.left
    regs.2015.cf18$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2018.cf18$pan[[i]]    <- reg.pan
    regs.2018.cf18$left[[i]]   <- reg.left
    regs.2018.cf18$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    year <- 2021
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2021.cf18$pan[[i]]    <- reg.pan
    regs.2021.cf18$left[[i]]   <- reg.left
    regs.2021.cf18$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)  ~ mean.rpan,  data = data.tmp)
    reg.left   <- lm(formula = log(left/pri) ~ mean.rleft, data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)  ~ mean.roth,  data = data.tmp)
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
    mean.regs.cf18$pan [[i]] <- reg.pan
    mean.regs.cf18$left[[i]] <- reg.left  
    mean.regs.cf18$oth [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left   <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.left   <- alphahat$left  [i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.left   <- betahat$left  [i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
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
    extendCoal.cf18[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################
#
##########
## cf21 ##
##########
vhat.2021 <- vhat.2018 <- vhat.2015 <- vhat.2012 <- vhat.2009 <- vhat.2006 <- 
        data.frame(pan    = rep(NA, nrow(v00)),
                   pri  = rep(NA, nrow(v00)),
                   left = rep(NA, nrow(v00))) # will receive vote estimates
#
alphahat <- data.frame(pan    = rep(NA, nrow(v00)),
                       pri    = rep(NA, nrow(v00)),
                       left   = rep(NA, nrow(v00))) # will receive municipio's alphas
betahat <- data.frame(pan    = rep(NA, nrow(v00)),
                      left   = rep(NA, nrow(v00)),
                      oth    = rep(NA, nrow(v00))) # will receive municipio's betas (none for pri)
#
tmp <- as.list(rep(NA, nrow(v00))) # empty list will receive one time-series
                                   # regression per municipio, each used to
                                   # predict votes in 2006:2021
# add names
if (agg=="m") names(tmp) <- v00$ife
if (agg=="s") names(tmp) <- v00$edon*10000 + v00$seccion # untested
#
regs.2006.cf21 <- regs.2009.cf21 <- regs.2012.cf21 <- regs.2015.cf21 <- regs.2018.cf21 <- regs.2021.cf21 <- 
    list(pan    = tmp,
         left   = tmp,
         oth    = tmp,
         readme = "No pri regs because DVs are pri-ratios")
#
mean.regs.cf21 <- list(pan    = tmp,
                       left   = tmp,
                       oth    = tmp,
                       readme = "No pri regs bec DVs are pri-ratios")
# drop list elements that still have NAs from loop
# (happens with some secciones)
non.nas <- lapply(extendCoal.cf21, sum)
#non.nas
#extendCoal.cf21[[2466]]
non.nas <- unlist(non.nas)
non.nas <- which(is.na(non.nas)==FALSE)
#length(non.nas)
#    
for (i in non.nas){
    #i <- 34 # debug
    #i <- 44508 # debug
    message(sprintf("Estimating regs for couterfactual 2021 municipal map loop %s of %s", i, max(non.nas)))
    # subset data to single unit
    data.tmp <- extendCoal.cf21[[i]]
    #
    # add first-differences
    tmp.ln <- nrow(data.tmp)
    data.tmp$d.pan    <- data.tmp$pan    - c(NA,data.tmp$pan   [-tmp.ln])
    data.tmp$d.pri    <- data.tmp$pri    - c(NA,data.tmp$pri   [-tmp.ln])
    data.tmp$d.left   <- data.tmp$left   - c(NA,data.tmp$left  [-tmp.ln])
    rm(tmp.ln)
    #
    ##################################
    ## predict 2006 with last 5 els ## ojo: v91 needed
    ##################################
    year <- 2006
    reg.pan  <- lm(formula = log(pan/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri) ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)  ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    vhat.2006           [i,] <- c(vhat.pan, vhat.pri, vhat.left)
    regs.2006.cf21$pan [[i]] <- reg.pan
    regs.2006.cf21$left[[i]] <- reg.left
    regs.2006.cf21$oth [[i]] <- reg.oth
    #                                                                    ##############################
    #                                                                    # DO THESE WHEN PREDICTING   #
    #                                                                    # FIRST YEAR ONLY:           #
    data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                  ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2009 with last 5 els ##
    ##################################
    year <- 2009
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2009.cf21$pan[[i]]    <- reg.pan
    regs.2009.cf21$left[[i]]   <- reg.left
    regs.2009.cf21$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2012 with last 5 els ##
    ##################################
    year <- 2012
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2012.cf21$pan[[i]]    <- reg.pan
    regs.2012.cf21$left[[i]]   <- reg.left
    regs.2012.cf21$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2015 with last 5 els ##
    ##################################
    year <- 2015
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2015.cf21$pan[[i]]    <- reg.pan
    regs.2015.cf21$left[[i]]   <- reg.left
    regs.2015.cf21$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2018 with last 5 els ##
    ##################################
    year <- 2018
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2018.cf21$pan[[i]]    <- reg.pan
    regs.2018.cf21$left[[i]]   <- reg.left
    regs.2018.cf21$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    ##################################
    ## predict 2021 with last 5 els ##
    ##################################
    year <- 2021
    reg.pan  <- lm(formula = log(pan/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.left <- lm(formula = log(left/pri)   ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
    reg.oth  <- lm(formula = log(oth/pri)    ~ yr, data = data.tmp, subset = (yr >= year-15 & yr <= year-3))
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
    regs.2021.cf21$pan[[i]]    <- reg.pan
    regs.2021.cf21$left[[i]]   <- reg.left
    regs.2021.cf21$oth[[i]]    <- reg.oth
    ## #                                                                    ##############################
    ## #                                                                    # DO THESE WHEN PREDICTING   #
    ## #                                                                    # FIRST YEAR ONLY:           #
    ## data.tmp$vhat.left   <- data.tmp$vhat.pri <- data.tmp$vhat.pan <- NA # slots for projections      #
    ## data.tmp$bhat.left   <- data.tmp$bhat.pan <- NA                      # slots for slope estimates  #
    data.tmp$vhat.pan   [data.tmp$yr==year] <- vhat.pan                     ##############################
    data.tmp$vhat.pri   [data.tmp$yr==year] <- vhat.pri
    data.tmp$vhat.left  [data.tmp$yr==year] <- vhat.left
    data.tmp$bhat.pan   [data.tmp$yr==year] <- bhat.pan
    data.tmp$bhat.left  [data.tmp$yr==year] <- bhat.left
    #
    # ALTERNATIVE: exp(predict.lm(reg.pan,    newdata = new.d, interval = "confidence"))
    # #########################################################################
    ## alpha regressions (cf. Díaz Cayeros, Estévez, Magaloni 2016, p. 90) ##
    #########################################################################
    reg.pan    <- lm(formula = log(pan/pri)  ~ mean.rpan,  data = data.tmp)
    reg.left   <- lm(formula = log(left/pri) ~ mean.rleft, data = data.tmp)
    reg.oth    <- lm(formula = log(oth/pri)  ~ mean.roth,  data = data.tmp)
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
    mean.regs.cf21$pan [[i]] <- reg.pan
    mean.regs.cf21$left[[i]] <- reg.left  
    mean.regs.cf21$oth [[i]] <- reg.oth
    #
    # add alphas and betas for whole period
    data.tmp$alphahat.left   <- data.tmp$alphahat.pri <- data.tmp$alphahat.pan <- NA # open slots for alphas
    data.tmp$betahat.left   <- data.tmp$betahat.pan <- NA # open slots for betas
    data.tmp$alphahat.pan    <- alphahat$pan   [i]
    data.tmp$alphahat.pri    <- alphahat$pri   [i]
    data.tmp$alphahat.left   <- alphahat$left  [i]
    data.tmp$betahat.pan    <- betahat$pan   [i]
    data.tmp$betahat.left   <- betahat$left  [i]
    data.tmp$betahat.oth    <- betahat$oth   [i]
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
    extendCoal.cf21[[i]] <- data.tmp
}
##############################################################################################
## warnings correspond to units with no variance (eg. period mean in new municipio in 2017) ##
##############################################################################################
#
# clean, all this is saved in extendCoal.cf.., mean.regs.cf.., regs.2006.cf.., regs.2009.cf.., ..., regs.2021.cf..
rm(alphahat, betahat, bhat.left, bhat.pan, reg.left, reg.oth, reg.pan, rhat.left, rhat.oth, rhat.pan, vhat.2006, vhat.2009, vhat.2012, vhat.2015, vhat.2018, vhat.left, vhat.pan, vhat.pri)
#

###################################################
## determine which muncipalities changed borders ##
###################################################
## # re-read eq (in case changed and don't want to re-do full code since last read)
## tmp <- paste(wd, "equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", sep = "")
## eq <- read.csv(tmp, stringsAsFactors = FALSE)
## eq$check <- NULL # drop column meant to clean within excel file
# check that dummy hits all mun changes
tmp1 <- eq[,c("ife1991","ife1994","ife1997","ife2000","ife2003","ife2006","ife2009","ife2012","ife2015","ife2018","ife2021")]
tmp1 <- apply(tmp1, 1, max)
tmp2 <- eq[,c("ife1991","ife1994","ife1997","ife2000","ife2003","ife2006","ife2009","ife2012","ife2015","ife2018","ife2021")]
tmp2 <- apply(tmp2, 1, min)
tmp <- tmp1-tmp2
if (length(which(eq$dmunchg==0 & tmp!=0))==0 & length(which(eq$dmunchg!=0 & tmp==0))==0){
    print("CHECKED: all dmunchg coded correctly")
} else {
    print("WARNING: some dmunchg coded wrong!")
    table(tmp==0, eq$dmunchg)
}
## # debug
## sel <- which(eq$dmunchg==0 & tmp!=0)
## eq$ord[sel]
## sel <- which(eq$dmunchg==1 & tmp==0)
## eq$ord[sel]
#
# select secciones that switched municipios
tmp <- eq[eq$dmunchg==1, c("ife1991", "ife1994", "ife1997", "ife2000", "ife2003", "ife2006", "ife2009", "ife2012", "ife2015", "ife2018", "ife2021")]
#
# chg1994:2021 list ife codes of munics that changed that year
sel <- which(tmp$ife1991 != tmp$ife1994)
chg1994 <- unique(c(tmp$ife1991[sel], tmp$ife1994[sel]))
chg1994 <- chg1994[order(chg1994)]
#
sel <- which(tmp$ife1994 != tmp$ife1997)
chg1997 <- unique(c(tmp$ife1994[sel], tmp$ife1997[sel]))
chg1997 <- chg1997[order(chg1997)]
#
sel <- which(tmp$ife1997 != tmp$ife2000)
chg2000 <- unique(c(tmp$ife1997[sel], tmp$ife2000[sel]))
chg2000 <- chg2000[order(chg2000)]
#
sel <- which(tmp$ife2000 != tmp$ife2003)
chg2003 <- unique(c(tmp$ife2000[sel], tmp$ife2003[sel]))
chg2003 <- chg2003[order(chg2003)]
#
sel <- which(tmp$ife2003 != tmp$ife2006)
chg2006 <- unique(c(tmp$ife2003[sel], tmp$ife2006[sel]))
chg2006 <- chg2006[order(chg2006)]
#
sel <- which(tmp$ife2006 != tmp$ife2009)
chg2009 <- unique(c(tmp$ife2006[sel], tmp$ife2009[sel]))
chg2009 <- chg2009[order(chg2009)]
#
sel <- which(tmp$ife2009 != tmp$ife2012)
chg2012 <- unique(c(tmp$ife2009[sel], tmp$ife2012[sel]))
chg2012 <- chg2012[order(chg2012)]
#
sel <- which(tmp$ife2012 != tmp$ife2015)
chg2015 <- unique(c(tmp$ife2012[sel], tmp$ife2015[sel]))
chg2015 <- chg2015[order(chg2015)]
#
sel <- which(tmp$ife2015 != tmp$ife2018)
chg2018 <- unique(c(tmp$ife2015[sel], tmp$ife2018[sel]))
chg2018 <- chg2018[order(chg2018)]
#
sel <- which(tmp$ife2018 != tmp$ife2021)
chg2021 <- unique(c(tmp$ife2018[sel], tmp$ife2021[sel]))
chg2021 <- chg2021[order(chg2021)]
#
## sel <- which(tmp$ife2021 != tmp$ife2024)
## chg2024 <- unique(c(tmp$ife2021[sel], tmp$ife2024[sel]))
## chg2024 <- chg2024[order(chg2024)]
#

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
sel <- which(names(extendCoal) %in% chg2009)
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
sel <- which(names(extendCoal) %in% chg2012)
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
sel <- which(names(extendCoal) %in% chg2015)
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
sel <- which(names(extendCoal) %in% chg2018)
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
sel <- which(names(extendCoal) %in% chg2021)
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



