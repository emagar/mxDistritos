#####################################################################
## Code invoked from within elec-data-for-maps.r                   ##
## manipulates seccion-level census accounting for split secciones ##
#####################################################################

# pob18
#eq$edosecn <- eq$edon*10000 + eq$seccion

# paste pob18 to eq2
eq2 <- eq
eq2 <- eq2[, c("edosecn","edon","seccion","inegi","ife","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3")]
eq2 <- merge(x = eq2, y = pob18, by = "edosecn", all.x = TRUE, all.y = FALSE)
# will receive counterfactual sums
eq2$p18_2020b <- eq2$p18_2010b <- eq2$p18_2005b <- NA
#
eq2 <- within(eq2, {
    pi <- NA;
    case <- NA;
    times.manip <- 0;
    dpre05done  <- d0510done <- d1020done <- dpost20done <- 0;
    #a <- b <- NA;
    y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- y2010 <- y2011 <- y2012 <- y2013 <- y2014 <- y2015 <- y2016 <- y2017 <- y2018 <- y2019 <- y2020 <- y2021 <- y2022 <- NA;
    })

##############################
## municipio changes ignored #
##############################
sel <- which(eq2$action3 == "mun.chg")
length(sel) # is zero, so ignore
# action2
sel <- which(eq2$action2 == "mun.chg")
manip <- eq2[sel,]
manip <- within(manip, {
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
})
eq2[sel,] <- manip
# action
sel <- which(eq2$action == "mun.chg")
manip <- eq2[sel,]
manip <- within(manip, {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq2[sel,] <- manip

# eq3 will receive manipulation indications and generate NAs needed in time series projections
eq3 <- eq2
eq3 <- eq3[, c("edosecn","action","orig.dest","when","action2","orig.dest2","when2","action3","orig.dest3","when3",
               paste0("y",1997:2022))]
eq3[,paste0("y",1997:2022)] <- 1

# secciones that cannot be fixed for census data projection
#no.manip <- c(10487) # eg. 10487 created in 2004 with bits of three secciones (mun limits), then merged to 10378 in 2008
sel <- which(eq2$edosecn==10487);
manip <- eq2[sel,]
# given no way to get change, assign 2005 pop=31 along seccion's lifespan 2004:2008 and return to data
manip <- within(manip, {
    y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- p18_2005; 
    dpre05done  <- d0510done   <- d1020done   <- dpost20done <- 1
})
eq2[sel,] <- manip
# indicate manip
manip <- eq3[sel,]
manip <- within(manip, {
    y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- y2002 <- y2003 <- NA;
    y2009 <- y2010 <- y2011 <- y2012 <- y2013 <- y2014 <- y2015 <- y2016 <- y2017 <- y2018 <- y2019 <- NA;
    y2020 <- y2021 <- y2022 <- NA
    action  <- action2 <- action3 <- "";
    orig.dest  <- orig.dest2 <- orig.dest3 <- "";
    when  <- when2 <- when3 <- NA;
})
eq3[sel,] <- manip

tmp.proj <- function(baseyr = NA, yr = NA){
    selc <- grep(pattern = paste0("y", baseyr), colnames(manip))
    base <- manip[,selc];
    pi <- manip$pi;
    #
    if ((baseyr - yr) >= 0) tmpy <- base / pi ^ (abs(baseyr - yr)) # backward
    if ((baseyr - yr) <  0) tmpy <- base * pi ^ (abs(baseyr - yr)) # forwward
    tmpy <- round(tmpy, 1)
    return(tmpy)
    }

############################################################
## manipulate all --- will gen NAs when data missing in   ##
## new secciones and some errors that will be fixed later ##
############################################################
manip <- eq2 # duplicate for manipulation
manip <- within(manip, {y2005 <- p18_2005; y2010 <- p18_2010; y2020 <- p18_2020;})     
#
# project 2005-2010
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)))
manip <- within(manip, {
    y1997 <- tmp.proj(baseyr = 2005, yr = 1997);
    y1998 <- tmp.proj(baseyr = 2005, yr = 1998);
    y1999 <- tmp.proj(baseyr = 2005, yr = 1999);
    y2000 <- tmp.proj(baseyr = 2005, yr = 2000);
    y2001 <- tmp.proj(baseyr = 2005, yr = 2001);
    y2002 <- tmp.proj(baseyr = 2005, yr = 2002);
    y2003 <- tmp.proj(baseyr = 2005, yr = 2003);
    y2004 <- tmp.proj(baseyr = 2005, yr = 2004);
    y2006 <- tmp.proj(baseyr = 2005, yr = 2006);
    y2007 <- tmp.proj(baseyr = 2005, yr = 2007);
    y2008 <- tmp.proj(baseyr = 2005, yr = 2008);
    y2009 <- tmp.proj(baseyr = 2005, yr = 2009);
})
# project 2010-2020
manip <- within(manip, pi <-  (y2020 / y2010) ^ (1 / (2020 - 2010)) );
manip <- within(manip, {
        y2011 <- tmp.proj(baseyr = 2010, yr = 2011);
        y2012 <- tmp.proj(baseyr = 2010, yr = 2012);
        y2013 <- tmp.proj(baseyr = 2010, yr = 2013);
        y2014 <- tmp.proj(baseyr = 2010, yr = 2014);
        y2015 <- tmp.proj(baseyr = 2010, yr = 2015);
        y2016 <- tmp.proj(baseyr = 2010, yr = 2016);
        y2017 <- tmp.proj(baseyr = 2010, yr = 2017);
        y2018 <- tmp.proj(baseyr = 2010, yr = 2018);
        y2019 <- tmp.proj(baseyr = 2010, yr = 2019);
        y2021 <- tmp.proj(baseyr = 2010, yr = 2021);
        y2022 <- tmp.proj(baseyr = 2010, yr = 2022);
})
manip <- within(manip, times.manip <- times.manip + 1)
#
# return manipulated data
eq2 <- manip
##################################################
## secciones that never changed: indicate manip ##
##################################################
sel <- which(is.na(manip$when) & manip$dpre05done==0 & manip$d0510done==0 & manip$d1020done==0 & manip$dpost20done==0)
length(sel)
manip <- eq3
manip[sel,] <- within(manip[sel,], {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
# return manipulated data
eq3 <- manip
rm(manip)

###############################################################################
## secciones modified up to 1997 will be ignored (analysis to start in 1997) ##
###############################################################################
sel <- which(eq3$when3 <= 1997)
length(sel) # is zero, ignore
sel <- which(eq3$when2 <= 1997)
length(sel) # is zero, ignore
sel <- which(eq3$when <= 1997)
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip

############################
## cases with when3 record #
############################
## sel2 <- which(!is.na(eq3$when3)) # two cases only
## eq2[sel2[1],]
# hand manipulation
sel <- which(eq2$edosecn==161976)
manip <- eq2[sel,]
vec <- c(1976,2675,2677)
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- "no.baja"
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.minus <- which(eq2$edosecn==162675) # targets w/o seccion (for use when seccion was dropped)
# aggregate split populations
manip$p18_2005b <- sum(eq2$p18_2005[vec])
manip$p18_2010b <- sum(eq2$p18_2010[vec]) # needed for backwards projection
manip <- within(manip, {y2005 <- p18_2005b; y2010 <- p18_2010b; y2020 <- p18_2020;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
# re-project where needed (targets unneeded before 2005)
if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
# return manipulation to data
manip$times.manip <- manip$times.manip + 1
eq2[sel,] <- manip
# fix proj in non-existing secciones and return manipulation to data
manip <- eq3[vec.minus,]
if (manip$when>1997) manip <- within(manip, y1997 <- NA)
manip <- within(manip, {
    action  <- "";      when <- NA;    orig.dest <- "";
})
eq3[vec.minus,] <- manip
#
# do when2 change
sel <- which(eq3$edosecn==161976)
manip <- eq2[sel,]
vec <- c(1976,2677)
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- "no.baja"
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.minus <- which(eq2$edosecn==162677)
# aggregate split populations
manip$p18_2005b <- sum(eq2$p18_2005[vec])
manip$p18_2010b <- sum(eq2$p18_2010[vec]) # needed for backwards projection
manip <- within(manip, {y2005 <- p18_2005b; y2010 <- p18_2010b; y2020 <- p18_2020;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
# re-project where needed (targets unneeded before 2005)
if (manip$when2>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
if (manip$when2>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
if (manip$when2>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
if (manip$when2>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
# restore un-manipulated 2005 in cases where when is below
if (manip$when<2005) manip <- within(manip, y2005 <- p18_2005)
# indicate manip and return to data
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[sel,] <- manip
# further indicate manip and return to data
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- action3; when  <- when3; orig.dest <- orig.dest3;
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq3[sel,] <- manip
# fix proj in non-existing secciones and return manipulation to data
manip <- eq3[vec.minus,]
if (manip$when>1997) manip <- within(manip, y1997 <- NA)
if (manip$when>1998) manip <- within(manip, y1998 <- NA)
if (manip$when>1999) manip <- within(manip, y1999 <- NA)
if (manip$when>2000) manip <- within(manip, y2000 <- NA)
if (manip$when>2001) manip <- within(manip, y2001 <- NA)
manip <- within(manip, {
    action  <- "";      when <- NA;    orig.dest <- "";
})
eq3[vec.minus,] <- manip
#
# item 2
# get pi
sel <- which(eq2$edosecn==192136)
manip <- eq2[sel,]
vec <- c(2136,2437,2809:2822)
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- "baja"
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec <- vec[-1]
# aggregate split populations
manip$p18_2020b <- sum(eq2$p18_2020[vec]) # needed for backwards projection
manip <- within(manip, {y2020 <- p18_2020b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2020 / y2010) ^ (1 / (2020 - 2010)) ); # rate of change
## # re-project where needed (targets unneeded before 2005)
## manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
## manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
## manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
## manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
## manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
## manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
## manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
## manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
tmp <- manip$pi # keep pi for orig.dest3
#manip <- within(manip, {y2020 <- when3 <- NA; action3 <- orig.dest3 <- ""}) # clean
# return manipulation to data
## manip$times.manip <- manip$times.manip + 1
eq2[sel,] <- manip
# fix proj in non-existing secciones and return manipulation to data
manip <- eq2[vec,]
manip$pi <- tmp
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2020 <- tmp.proj(baseyr = 2020, yr = 2020))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[vec,] <- manip
# indicate manip
manip <- eq3[vec,]
manip <- within(manip, {
    action  <- "";      when <- NA;    orig.dest <- "";
})
eq3[vec,] <- manip
# fix sección 2437 created in 2011 and return to data
manip <- eq2[vec[1],]
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
tmp <- manip$y2010 # retain 2010 pop
eq2[vec,] <- manip
# inidicate manip
manip <- eq3[vec,]
manip <- within(manip, y2010 <- NA)
eq3[vec,] <- manip
# fix sección 2136 and return to data
sel <- which(eq2$edosecn==192136)
manip <- eq2[sel,]
manip$y2010 <- manip$y2010 - tmp # subtract split sección 2437 pop in 2010 
# re-project where needed
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2010, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2010, yr = 2018))
# return manipulation to data
manip$y2010 <- manip$p18_2010 # restore pre-split pop in 2010
manip$times.manip <- manip$times.manip + 1
eq2[sel,] <- manip
# indicate manip
manip <- eq3[sel,]
manip <- within(manip, {y2020 <- when3 <- when2 <- NA; action3 <- orig.dest3 <- action2 <- orig.dest2 <- ""}) # clean
eq3[sel,] <- manip

############################
## cases with when2 record #
############################
table(eq3$action2)
# items where change1 occurred before 2010 census and change2 before/at 2020 = mode
sel <- which(eq3$action2=="split.to" & eq3$when2>2010 & eq3$when2 - eq3$when >= 10)
# monitor
eq2[sel,c(1,6:11)]
#
for (i in sel){
    #i <- sel[1]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest2)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    #manip$p18_2010b <- sum(eq2$p18_2010[vec])
    manip$p18_2020b <- sum(eq2$p18_2020[vec]) # needed for backwards projection
    manip <- within(manip, {y2010 <- p18_2010; y2020 <- p18_2020b;}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2020 / y2010) ^ (1 / (2020 - 2010)) ); # rate of change
    # re-project where needed
    if (manip$when2>2011) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
    if (manip$when2>2012) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
    if (manip$when2>2013) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
    if (manip$when2>2014) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
    if (manip$when2>2015) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
    if (manip$when2>2016) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
    if (manip$when2>2017) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
    if (manip$when2>2018) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
    if (manip$when2>2019) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
    if (manip$when2>2020) manip <- within(manip, y2020 <- tmp.proj(baseyr = 2020, yr = 2020))
    if (manip$when2>2021) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
    if (manip$when2>2022) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
    # restore un-manipulated 2020 in cases where when is below
    if (manip$when2<=2020) manip <- within(manip, y2020 <- p18_2020)
    # return manipulation to data
    tmp <- manip$pi # save to plug into split.from
    eq2[i,] <- manip
    # fix proj in non-existing secciones and return manipulation to data
    manip <- eq2[vec.minus,]
    manip <- within(manip, {y2020 <- p18_2020; pi <- tmp}) # plug pi
    if (manip$when[1]<2012) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
    if (manip$when[1]<2013) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
    if (manip$when[1]<2014) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
    if (manip$when[1]<2015) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
    if (manip$when[1]<2016) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
    if (manip$when[1]<2017) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
    if (manip$when[1]<2018) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
    if (manip$when[1]<2019) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
    if (manip$when[1]<2020) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
    if (manip$when[1]<2021) manip <- within(manip, y2020 <- tmp.proj(baseyr = 2020, yr = 2020))
    if (manip$when[1]<2022) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
    if (manip$when[1]<2023) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
    # indicate manipulation pt 1 and return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[vec.minus,] <- manip
    # indicate manipulation pt 2 and return to data
    manip <- eq3[vec.minus,]
    manip <- within(manip, {
        action <- "";      when <- NA;    orig.dest <- "";
    })
    eq3[vec.minus,] <- manip
    # indicate sección split.from and return to data
    manip <- eq3[i,] # duplicate obs for manipulation
    manip <- within(manip, {
        action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    })
    eq3[i,] <- manip
}

# items where change1 occurred before 2005 census and change2 at 2010
sel <- which(eq3$action2=="split.to" & eq3$when2==2010 & eq3$when2 - eq3$when >= 5)
# monitor
eq2[sel,c(1,6:11)]
#
manip <- eq2[sel,] # duplicate obs for manipulation
vec <- eval(parse(text = manip$orig.dest2)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.plus  <- union(vec, sel)   # seccion and its targets (in case vec excludes dropped seccion) 
vec.minus <- setdiff(vec, sel) # targets w/o seccion (for use when seccion was dropped)
# aggregate split populations
manip$p18_2010b <- sum(eq2$p18_2010[vec])
#manip$p18_2020b <- sum(eq2$p18_2020[vec]) # needed for backwards projection
manip <- within(manip, {y2005 <- p18_2005; y2010 <- p18_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
# re-project where needed
if (manip$when2>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2010, yr = 2002))
if (manip$when2>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2010, yr = 2003))
if (manip$when2>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2010, yr = 2004))
if (manip$when2>2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2010, yr = 2006))
if (manip$when2>2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2010, yr = 2007))
if (manip$when2>2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2010, yr = 2008))
if (manip$when2>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2010, yr = 2009))
# return manipulation to data
tmp <- manip$pi # save to plug into split.from
manip <- within(manip, {
    times.manip <- times.manip + 1
})
eq2[sel,] <- manip
# non-existing secciones need no further manipulation, indicate done and return to data
manip <- eq3[vec.minus,]
manip <- within(manip, {
    action <- "";      when <- NA;    orig.dest <- "";
})
eq3[vec.minus,] <- manip
#
# get vec to manipulate action1 in seccion 1913
manip <- eq3[sel,] # duplicate obs for manipulation
vec <- eval(parse(text = manip$orig.dest)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
vec <- 80000 + vec   # turn into edosecn
vec <- which(eq2$edosecn %in% vec) # turn to indices
# indicate previous manip in 2828 and return to data
manip <- within(manip, {
    action  <- "";    when  <- NA;   orig.dest  <- "";
    action2 <- "";    when2 <- NA;   orig.dest2 <- "";
})
eq3[sel,] <- manip # duplicate obs for manipulation --- sel can be reset
# get seccion 1913 for manipulation
sel <- vec # vec can be reset
manip <- eq2[sel,] # duplicate obs for manipulation
manip$pi <- tmp # plug last known pi
vec <- eval(parse(text = manip$orig.dest)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.plus  <- union(vec, sel)   # seccion and its targets (in case vec excludes dropped seccion) 
vec.minus <- setdiff(vec, sel) # targets w/o seccion (for use when seccion was dropped)
# aggregate split populations
manip$p18_2005b <- sum(eq2$p18_2005[vec])
#manip$p18_2020b <- sum(eq2$p18_2020[vec]) # needed for backwards projection
manip <- within(manip, {y2005 <- p18_2005b;}) # use aggregates to compute pi
# re-project where needed
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
manip <- within(manip, y2005 <- NA # 2005 no longer needed
# indicate manipulation pt 1 and return manipulation to data
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[sel,] <- manip # sel can be reset
# indicate manipulation pt 2 and return manipulation to data
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";    when  <- NA;   orig.dest  <- "";
    y2002 <- y2003 <- y2004 <- y2005 <- NA;
})
eq3[sel,] <- manip # sel can be reset
# finally, make pre 2002 pop NA in new secciones and indicate manipulation
sel <- vec
manip <- eq3[sel,]
manip <- within(manip, {
    y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- NA;
    action  <- "";    when  <- NA;   orig.dest  <- "";
})
eq3[sel,] <- manip # return to data

# items where change1 and change2 occurred between 2010 and 2020 censuses
sel <- which(eq3$action2=="split.to" & eq3$when2>2010 & eq3$when2 - eq3$when < 10)
# monitor
eq2[sel,c(1,6:11)]
# item 1
i <- which(eq2$edosecn==161976)
manip <- eq2[i,]
# for 2010 seccion 1976 pop only, already there
# for 2020 bunch of other pops
j <- which(eq2$edosecn %in% 162678:162699 | eq2$edosecn %in% c(162692,162703))
manip$p18_2020b <- sum(eq2$p18_2020[j])
manip <- within(manip, y2020 <- p18_2020b)
manip <- within(manip, pi <-  (y2020 / y2010) ^ (1 / (2020 - 2010)) ); # rate of change
# project
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
# keep piand return to data
tmp <- manip$pi
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[i,] <- manip
# inidacte manip
manip <- eq3[i,]
manip <- within(manip, {
    y2020 <- NA;
    action <- orig.dest <- "";   when  <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
eq3[i,] <- manip
# plug pi across the board and return to data before loop
sel <- which(eq2$edosecn %in% 162678:162699 | eq2$edosecn %in% c(162692,162703))
manip <- eq2[sel,]
manip$pi <- tmp
eq2[sel,] <- manip
# loop over items
for (i in sel){
    #i <- sel[1]
    manip <- eq2[i,]
    manip$y2020 <- manip$p18_2020
    # re-project where needed
    if (manip$when<2011) manip <- within(manip, y2010 <- tmp.proj(baseyr = 2020, yr = 2010))
    if (manip$when<2012) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
    if (manip$when<2013) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
    if (manip$when<2014) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
    if (manip$when<2015) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
    if (manip$when<2016) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
    if (manip$when<2017) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
    if (manip$when<2018) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
    if (manip$when<2019) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
    if (manip$when<2021) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
    if (manip$when<2022) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
    if (manip$when<2023) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
    # return manipulation to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[i,] <- manip
    # indicate manip and return to data
    manip <- eq3[i,]
    manip <- within(manip, {
        action <- orig.dest <- "";   when  <- NA;
        action2 <- orig.dest2 <- ""; when2 <- NA;
    })
    eq3[i,] <- manip
}
#
# item 2
i <- which(eq2$edosecn==230205)
manip <- eq2[i,]
# for 2010 seccion 1976 pop only, already there
# for 2020 bunch of other pops
j <- which(eq2$edosecn %in% c(230874,230876:230922,230975:231003))
manip$p18_2020b <- sum(eq2$p18_2020[j])
manip <- within(manip, y2020 <- p18_2020b)
manip <- within(manip, pi <-  (y2020 / y2010) ^ (1 / (2020 - 2010)) ); # rate of change
# project
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
# keep pi, indicate manip and return to data
tmp <- manip$pi
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[i,] <- manip
# indicate manip pt 2
manip <- eq3[i,]
manip <- within(manip, {
    y2020 <- NA;
    action <- orig.dest <- "";   when  <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
eq3[i,] <- manip
# fix seccion 875 pop 2020 and return to data before loop
sel <- which(eq2$edosecn==230875)
manip <- eq2[sel,]
j <- which(eq2$edosecn %in% c(230975:231003))
manip$p18_2020 <- sum(eq2$p18_2020[j]) # not use b to match other items in next block
eq2[sel,] <- manip
# plug pi across the board and return to ddata before block manip
sel <- which(eq2$edosecn %in% c(230874:230922,230975:231003))
manip <- eq2[sel,]
manip$pi <- tmp
eq2[sel,] <- manip
#
# 1st block of items
i <- which(eq2$edosecn %in% c(230874:230922))
manip <- eq2[i,]
manip$y2020 <- manip$p18_2020
# re-project where needed
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate manip pt1 and return to data 
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[i,] <- manip
# indicate manip pt2 and return to data 
manip <- eq3[i,]
# fix seccion 875
manip$y2020[which(manip$edosecn==230875)] <- manip$y2021[which(manip$edosecn==230875)] <- manip$y2022[which(manip$edosecn==230875)] <- NA
manip[2,]
manip <- within(manip, {
    action <- orig.dest <- "";   when  <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
eq3[i,] <- manip
#
# other block of split secciones
i <- which(eq2$edosecn %in% c(230975:231003))
manip <- eq2[i,]
manip$y2020 <- manip$p18_2020
# re-project where needed
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# indicate manip and return to data 
manip <- within(manip, {
    times.manip <- times.manip + 1;
})
eq2[i,] <- manip
# indicate manip and return to data 
manip <- eq3[i,]
manip <- within(manip, {
    action <- orig.dest <- "";   when  <- NA;
    action2 <- orig.dest2 <- ""; when2 <- NA;
})
eq3[i,] <- manip

# item where changes occurred before 2005 both
sel <- which(eq2$edosecn==190439)
manip <- eq2[sel,] # duplicate obs for manipulation
vec <- c(2124,2136:2165)
vec <- manip$edon*10000 + vec   # turn into edosecn
vec <- which(eq2$edosecn %in% vec) # turn to indices
# aggregate split populations
manip$p18_2005b <- sum(eq2$p18_2005[vec])
manip$p18_2010b <- sum(eq2$p18_2010[vec])
manip <- within(manip, {y2005 <- p18_2005b; y2010 <- p18_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
# re-project full pop up to 2001
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
# use lisnom 2003=12021 minus 11% = 10696 for 2002:2004
manip$y2005 <- 10696
# project 2002:2004
manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
# indicate manip pt1 and return to data
tmp <- manip$pi # save to plug into split.from
manip <- within(manip, {
    times.manip <- times.manip + 2
})
eq2[sel,] <- manip
# indicate manip pt2
manip <- eq3[sel,]
manip <- within(manip, {
    y2005 <- y2010 <- NA;
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
})
eq3[sel,] <- manip
#
# new secciones need no manipulation (only one is 2136, which cannot be manip bec chg in 2005, so 2005-10 trend used)
sel <- which(eq2$edosecn %in% c(192124,192136:192165))
manip <- eq3[sel,] # duplicate obs for manipulation
manip <- within(manip, {
    y1997 <- y1998 <- y1999 <- y2000 <- y2001 <- NA;
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
})
eq3[sel,] <- manip
# manip[5,]

# fix merged.to case
# item 1
sel <- which(eq2$edosecn==154308)
manip <- eq2[sel,] # duplicate obs for manipulation
vec <- c(4308,5931)
vec <- manip$edon*10000 + vec   # turn into edosecn
vec <- which(eq2$edosecn %in% vec) # turn to indices
# aggregate split populations
manip$p18_2005b <- sum(eq2$p18_2005[vec])
manip$p18_2010b <- sum(eq2$p18_2010[vec])
manip <- within(manip, {y2005 <- p18_2005b; y2010 <- p18_2010b;}) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
# re-project full pop up to 2001
manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
# restore 2005 and 2010 and return to data
manip <- within(manip, {y2005 <- p18_2005; y2010 <- p18_2010;}) 
eq2[sel,] <- manip
# indicate manip
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
})
eq3[sel,] <- manip
# item 2
sel <- which(eq2$edosecn==155931)
manip <- eq2[sel,] # duplicate obs for manipulation
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
# indicate manip pt1 and return to data
manip <- within(manip, {
    times.manip <- times.manip + 1
})
eq2[sel,] <- manip
# indicate manip and return to data
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
})
eq3[sel,] <- manip
# item 3
sel <- which(eq2$edosecn==155930)
manip <- eq2[sel,] # duplicate obs for manipulation
vec <- c(5930,5931)
vec <- manip$edon*10000 + vec   # turn into edosecn
vec <- which(eq2$edosecn %in% vec) # turn to indices
# aggregate split populations
manip$p18_2010b <- sum(eq2$p18_2010[vec])
manip <- within(manip, y2010 <- p18_2010b)
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
# re-project 2014-on
manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
# restore 2005 and 2010
manip <- within(manip, y2010 <- p18_2010)
# indicate manip pt1 and return to data
manip <- within(manip, {
    times.manip <- times.manip + 1
})
eq2[sel,] <- manip
# indicate manip pt2
manip <- eq3[sel,]
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
})
eq3[sel,] <- manip

#
# verify no more when2 remaining
sel <- which(!is.na(eq3$when2))
eq2[sel,c(1,6:11)]

# vectors picking census column that is not NA
#aorb05 <- apply(X=eq2[,c("p18_2005","p18_2005b","y2005")], MARGIN=1, FUN=function(X)
#    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
aorb10 <- apply(X=eq2[,c("p18_2010","p18_2010b","y2010")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
#aorb20 <- apply(X=eq2[,c("p18_2020","p18_2020b","y2020")], MARGIN=1, FUN=function(X)
#    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
#################################
## split secciones before 2005 ##
#################################
sel <- which(eq3$action=="split.to" & eq3$when > 1996 & eq3$when <  2005)
# add target secciones
for (i in sel){
    #i <- sel[15]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$p18_2005b <- with(eq2, sum(p18_2005[vec]))
    manip$p18_2010b <- with(eq2, sum(aorb10[vec])) # needed for backwards projection
    manip <- within(manip, {y2005 <- p18_2005b; y2010 <- p18_2010b;}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
    # re-project where needed (targets unneeded before 2005)
    if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
    if (manip$when>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
    if (manip$when>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
    if (manip$when>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
    if (manip$when>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
    if (manip$when>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
    if (manip$when>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
    if (manip$when>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
    # restore un-manipulated 2005 in cases where when is below
    if (manip$when<2005 & !is.na(manip$p18_2005)) manip <- within(manip, y2005 <- p18_2005) # if is.na then indicated in eq3
    if (manip$when<2010 & !is.na(manip$p18_2010)) manip <- within(manip, y2010 <- p18_2010) # if is.na then indicated in eq3
    # indicate manip pt1 and return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1
    })
    eq2[i,] <- manip
    tmp <- manip$case # plug below
    # fix proj for eliminated secciones
    manip <- eq3[i,]
    manip$case <- tmp
    if (manip$case=="baja") {
        if (eq2$when[i]<=1998) manip <- within(manip, y1998 <- NA) # use eq2$when bec eq3$when may be NA if prior manip
        if (eq2$when[i]<=1999) manip <- within(manip, y1999 <- NA)
        if (eq2$when[i]<=2000) manip <- within(manip, y2000 <- NA)
        if (eq2$when[i]<=2001) manip <- within(manip, y2001 <- NA)
        if (eq2$when[i]<=2002) manip <- within(manip, y2002 <- NA)
        if (eq2$when[i]<=2003) manip <- within(manip, y2003 <- NA)
        if (eq2$when[i]<=2004) manip <- within(manip, y2004 <- NA)
        if (eq2$when[i]<=2005) manip <- within(manip, y2005 <- NA)
        if (eq2$when[i]<=2006) manip <- within(manip, y2006 <- NA)
        if (eq2$when[i]<=2007) manip <- within(manip, y2007 <- NA)
        if (eq2$when[i]<=2008) manip <- within(manip, y2008 <- NA)
        if (eq2$when[i]<=2009) manip <- within(manip, y2009 <- NA)
        if (eq2$when[i]<=2010) manip <- within(manip, y2010 <- NA)
        if (eq2$when[i]<=2011) manip <- within(manip, y2011 <- NA)
        if (eq2$when[i]<=2012) manip <- within(manip, y2012 <- NA)
        if (eq2$when[i]<=2013) manip <- within(manip, y2013 <- NA)
        if (eq2$when[i]<=2014) manip <- within(manip, y2014 <- NA)
        if (eq2$when[i]<=2015) manip <- within(manip, y2015 <- NA)
        if (eq2$when[i]<=2016) manip <- within(manip, y2016 <- NA)
        if (eq2$when[i]<=2017) manip <- within(manip, y2017 <- NA)
        if (eq2$when[i]<=2018) manip <- within(manip, y2018 <- NA)
        if (eq2$when[i]<=2019) manip <- within(manip, y2019 <- NA)
        if (eq2$when[i]<=2020) manip <- within(manip, y2020 <- NA)
        if (eq2$when[i]<=2021) manip <- within(manip, y2021 <- NA)
        if (eq2$when[i]<=2022) manip <- within(manip, y2022 <- NA)
    }
    # indicate manip pt2
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    manip$case <- NULL
    eq3[i,] <- manip
    # fix proj in non-existing secciones and return manipulation to data
    manip <- eq3[vec.minus,]
    if (eq2$when[i]>1997) manip <- within(manip, y1997 <- NA) # use eq2$when bec eq3$when may be NA if prior manip
    if (eq2$when[i]>1998) manip <- within(manip, y1998 <- NA)
    if (eq2$when[i]>1999) manip <- within(manip, y1999 <- NA)
    if (eq2$when[i]>2000) manip <- within(manip, y2000 <- NA)
    if (eq2$when[i]>2001) manip <- within(manip, y2001 <- NA)
    if (eq2$when[i]>2002) manip <- within(manip, y2002 <- NA)
    if (eq2$when[i]>2003) manip <- within(manip, y2003 <- NA)
    if (eq2$when[i]>2004) manip <- within(manip, y2004 <- NA)
    if (eq2$when[i]>2005) manip <- within(manip, y2005 <- NA)
    # indicate manipulation and return to data
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[vec.minus,] <- manip
}

####################################################
## single change split.from secciones before 2005 ##
## only need NAs added where appropriate          ##
####################################################
sel <- which(eq3$action=="split.from" & eq3$when > 1996 & eq3$when <  2005)
manip <- eq3[sel,] # duplicate obs for manipulation
# add NAs
manip[manip$when>1997,] <- within(manip[manip$when>1997,], y1997 <- NA)
manip[manip$when>1998,] <- within(manip[manip$when>1998,], y1998 <- NA)
manip[manip$when>1999,] <- within(manip[manip$when>1999,], y1999 <- NA)
manip[manip$when>2000,] <- within(manip[manip$when>2000,], y2000 <- NA)
manip[manip$when>2001,] <- within(manip[manip$when>2001,], y2001 <- NA)
manip[manip$when>2002,] <- within(manip[manip$when>2002,], y2002 <- NA)
manip[manip$when>2003,] <- within(manip[manip$when>2003,], y2003 <- NA)
# indicate manipulation and return to data
manip <- within(manip, {
    action  <- "";      when  <- NA;    orig.dest  <- "";
})
eq3[sel,] <- manip
#
# verify what's left before 2005
with(eq3, table(pre05=when<2005, action, useNA = "always"))

# vectors picking census column that is not NA
aorb05 <- apply(X=eq2[,c("p18_2005","p18_2005b","y2005")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
aorb10 <- apply(X=eq2[,c("p18_2010","p18_2010b","y2010")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
#aorb20 <- apply(X=eq2[,c("p18_2020","p18_2020b","y2020")], MARGIN=1, FUN=function(X)
#    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
#######################################
## single change secciones 2005-2010 ##
#######################################
#############################
## split.to and split.from ##
#############################
sel <- which(eq3$action=="split.to" & eq3$when >= 2005 & eq3$when < 2010)
for (i in sel){
    #i <- sel[12]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$p18_2005b <- with(eq2, sum(aorb05[vec]))
    manip$p18_2010b <- with(eq2, sum(aorb10[vec]))
    #manip$p18_2020b <- with(eq2, sum(aorb20[vec]))
    if (manip$when==2005) manip <- within(manip, {y2005 <- p18_2005b; y2010 <- p18_2010b;}) # use aggregates to compute pi
    if (manip$when>2005)  manip <- within(manip, {y2005 <- p18_2005; y2010 <- p18_2010b;}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
    # re-project pre-2010 where needed
    if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
    if (manip$when>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
    if (manip$when>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
    if (manip$when>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
    if (manip$when>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
    if (manip$when>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
    if (manip$when>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
    if (manip$when>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
    if (manip$when>2005) manip <- within(manip, y2005 <- tmp.proj(baseyr = 2005, yr = 2005))
    if (manip$when>2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
    if (manip$when>2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
    if (manip$when>2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2005, yr = 2008))
    #if (manip$when>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
    # indicate manip pt1 and return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[i,] <- manip
    # indicate manip pt2
    manip <- eq3[i,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[i,] <- manip
    #
    # fix proj in non-existing secciones and return manipulation to data
    manip <- eq2[vec.minus,]
    if (eq2$when[i]<=1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2010, yr = 1997)) # use eq2$when bec eq3$when may be NA if prior manip
    if (eq2$when[i]<=1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2010, yr = 1998))
    if (eq2$when[i]<=1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2010, yr = 1999))
    if (eq2$when[i]<=2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2010, yr = 2000))
    if (eq2$when[i]<=2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2010, yr = 2001))
    if (eq2$when[i]<=2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2010, yr = 2002))
    if (eq2$when[i]<=2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2010, yr = 2003))
    if (eq2$when[i]<=2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2010, yr = 2004))
    if (eq2$when[i]<=2005) manip <- within(manip, y2005 <- tmp.proj(baseyr = 2010, yr = 2005))
    if (eq2$when[i]<=2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2010, yr = 2006))
    if (eq2$when[i]<=2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2010, yr = 2007))
    if (eq2$when[i]<=2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2010, yr = 2008))
    if (eq2$when[i]<=2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2010, yr = 2009))
    #if (eq2$when[i]<=2010) manip <- within(manip, y2010 <- tmp.proj(baseyr = 2010, yr = 2010))
    # return to data
    manip <- within(manip, {
        times.manip <- times.manip + 1;
    })
    eq2[vec.minus,] <- manip
    # indicate manip in new secciones and return to data
    manip <- eq3[vec.minus,]
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[vec.minus,] <- manip
}

###############
## merged.to ##
###############
sel <- which(eq3$action=="merged.to" & eq3$when >= 2005 & eq3$when < 2010)
#eq3$when[sel]
for (i in sel){
    #i <- sel[60]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$p18_2005b <- with(eq2, sum(aorb05[vec.plus]))
    manip$p18_2010b <- with(eq2, sum(aorb10[vec]))
    #manip$p18_2020b <- with(eq2, sum(aorb20[vec]))
    manip <- within(manip, {y2005 <- p18_2005b; y2010 <- p18_2010b;}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
    # recover original pop 2005 for projection
    manip <- within(manip, y2005 <- p18_2005)
    # re-project pre-2010 where needed
    if (manip$when>1997) manip <- within(manip, y1997 <- tmp.proj(baseyr = 2005, yr = 1997))
    if (manip$when>1998) manip <- within(manip, y1998 <- tmp.proj(baseyr = 2005, yr = 1998))
    if (manip$when>1999) manip <- within(manip, y1999 <- tmp.proj(baseyr = 2005, yr = 1999))
    if (manip$when>2000) manip <- within(manip, y2000 <- tmp.proj(baseyr = 2005, yr = 2000))
    if (manip$when>2001) manip <- within(manip, y2001 <- tmp.proj(baseyr = 2005, yr = 2001))
    if (manip$when>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
    if (manip$when>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
    if (manip$when>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
    if (manip$when>2005) manip <- within(manip, y2005 <- tmp.proj(baseyr = 2005, yr = 2005))
    if (manip$when>2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
    if (manip$when>2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
    if (manip$when>2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2005, yr = 2008))
    if (manip$when>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
    # return to data
    manip <- within(manip, times.manip <- times.manip + 1)
    eq2[i,] <- manip
    # indicate manipulation
    manip <- eq3[i,] 
    manip <- within(manip, {
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[i,] <- manip
}
# verify if any unmanip left
with(eq3, table(pre10=when<2010, action, useNA = "always"))

# 2010-20 here

with(eq3, table(pre20=when<2020, action, useNA = "always"))
x

# monitor
paste(manip$when, manip$when2, manip$when3)
paste(manip$action, manip$action2, manip$action3)
paste(manip$orig.dest, manip$orig.dest2, manip$orig.dest3)
x

    
# debug
with(eq2, table(times.manip, paste0(dpre05done, d0510done, d1020done, dpost20done)))
x



# groups 
g1 <- which(eq2$when < 2005 | eq2$when %in% 2006:2009) # project with 2005-2010 slope, define start point
g2 <- which(eq2$when %in% 2011:2019 | eq2$when > 2020) # project with 2010-2020 slope, define start point
g3 <- which(eq2==2005) # inspect
g4 <- which(eq2==2010)
g5 <- which(eq2==2020)

#
################################
## manipulate cases split.to  ##
################################
sel <- grep("split.to", eq2$action)
manip <- eq2[sel,]
#
# wrap routine in function
tmp.f <- function(){
    # process chg up to 2005: except those that survived the split, will be redundant (determined afterwards)
    sel <- which(manip$when <= 2005)
    #sel <- which(manip$edosecn == 10098) # debug
    #manip[sel,]                          # debug
    for (i in sel){
        #i <- sel[1]
        vec <- eval(parse(text = manip$orig.dest[i])) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
        vec <- manip$edon[i]*10000 + vec   # turn into edosecn
        vec <- which(eq2$edosecn %in% vec) # indices
        #manip[i,-c(2:5,8:14)]
        #if (manip$edosecn[i] %in% vec) {
            manip$p18_2005b[i] <- sum(eq2$p18_2005[vec])
            manip$p18_2010b[i] <- sum(eq2$p18_2010[vec]) # needed for backwards projection
        #}
    }
    # process chg between 2006 and 2010
    sel <- which(manip$when %in% 2005:2010) # 2005 will be redundant unless occurred after conteo2005 (ie pob18_2005 not NA)
    for (i in sel){
        #i <- sel[1]
        vec <- eval(parse(text = manip$orig.dest[i])) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
        vec <- manip$edon[i]*10000 + vec   # turn into edosecn
        vec <- which(eq2$edosecn %in% vec) # indices
        manip[i,-c(2:5,8:14)]
        manip$p18_2010b[i] <- sum(eq2$p18_2010[vec])
    }
    # process chg between 2011 and 2020
    sel <- which(manip$when %in% 2010:2020) # 2010 will be redundant unless occurred after censo2010 (ie pob18_2010 not NA)
    for (i in sel){
        #i <- sel[1]
        vec <- eval(parse(text = manip$orig.dest[i])) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
        vec <- manip$edon[i]*10000 + vec   # turn into edosecn
        vec <- which(eq2$edosecn %in% vec) # indices
        #manip[i,-c(2:5,8:14)]
        manip$p18_2020b[i] <- sum(eq2$p18_2020[vec])
    }
    return(manip)
}
# apply function
manip <- tmp.f()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]
#
# cases recorded in action2
sel <- grep("split.to", eq2$action2)
tmp <-   eq2[sel,]
tmp$action <- tmp$action2; tmp$when <- tmp$when2; tmp$orig.dest <- tmp$orig.dest2 # move action2 to action etc
manip <- tmp
# apply function
manip <- tmp.f()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]
#
# cases recorded in action3
sel <- grep("split.to", eq2$action3)
tmp <-   eq2[sel,]
tmp$action <- tmp$action3; tmp$when <- tmp$when3; tmp$orig.dest <- tmp$orig.dest3 # move action3 to action etc
manip <- tmp
# apply function
manip <- tmp.f()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]




################################
## manipulate cases merged.to ##
################################
sel <- grep("merged.to", eq2$action)
manip <- eq2[sel,]
#
# wrap routine in function
tmp.f2 <- function(){
    # process chg up to 2005: except those that survived the split, will be redundant (determined afterwards)
    sel <- which(manip$when <= 2005) 
    # none occurred before 2008
    #
    # process chg between 2005 and 2010
    sel <- which(manip$when %in% 2005:2010)
    for (i in sel){
        #i <- sel[3]
        vec <- eval(parse(text = manip$orig.dest[i])) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
        vec <- c(vec, manip$seccion[i]) # add current seccion to vector
        vec <- manip$edon[i]*10000 + vec   # turn into edosecn
        vec <- which(eq2$edosecn %in% vec) # indices
        #manip[i,-c(2:5,8:14)]
        manip$p18_2005b[i] <- sum(eq2$p18_2005[vec])
        manip$p18_2010b[i] <- sum(eq2$p18_2010[vec], na.rm=TRUE) # surviving seccion's pop for rate of chg
    }
    # process chg between 2010 and 2020 
    sel <- which(manip$when %in% 2010:2020)
    for (i in sel){
        #i <- sel[1]
        vec <- eval(parse(text = manip$orig.dest[i])) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
        vec <- c(vec, manip$seccion[i]) # add current seccion to vector
        vec <- manip$edon[i]*10000 + vec   # turn into edosecn
        vec <- which(eq2$edosecn %in% vec) # indices
        #manip[i,-c(2:5,8:14)]
        manip$p18_2010b[i] <- sum(eq2$p18_2010[vec])
        manip$p18_2020b[i] <- sum(eq2$p18_2020[vec], na.rm=TRUE) # surviving seccion's pop for rate of chg
    }
    return(manip)
}
# apply function
manip <- tmp.f2()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]
#
# cases recorded in action2
sel <- grep("merged.to", eq2$action2)
tmp <-   eq2[sel,]
tmp$action <- tmp$action2; tmp$when <- tmp$when2; tmp$orig.dest <- tmp$orig.dest2 # move action2 to action etc
manip <- tmp
# apply function
manip <- tmp.f2()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]
#
# cases recorded in action3
sel <- grep("merged.to", eq2$action3)
tmp <-   eq2[sel,]
tmp$action <- tmp$action3; tmp$when <- tmp$when3; tmp$orig.dest <- tmp$orig.dest3 # move action3 to action etc
manip <- tmp
# apply function
manip <- tmp.f2()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]




#########################################
## compute a-bs for linear projections ##
#########################################
eq2$b1020 <- eq2$a1020 <- eq2$b0510 <- eq2$a0510 <- NA

eq2 <- within(eq2, {
    b0510 <- (p18_2010 - p18_2005)/5;
    a0510 <- p18_2005 - 2005 * (p18_2010 - p18_2005)/5;
    b1020 <- (p18_2020 - p18_2010)/10;
    a1020 <- p18_2010 - 2010 * (p18_2020 - p18_2010)/10;
})

eq2 <- within(eq2, {
#    p18_21 <- a1020 + b1020*2021;
#    p18_20 <- a1020 + b1020*2020;
#    p18_19 <- a1020 + b1020*2019;
#    p18_18 <- a1020 + b1020*2018;
#    p18_17 <- a1020 + b1020*2017;
#    p18_16 <- a1020 + b1020*2016;
#    p18_15 <- a1020 + b1020*2015;
#    p18_14 <- a1020 + b1020*2014;
#    p18_13 <- a1020 + b1020*2013;
#    p18_12 <- a1020 + b1020*2012;
#    p18_11 <- a1020 + b1020*2011;
#    p18_10 <- a1020 + b1020*2010;
#    p18_09 <- a0510 + b0510*2009;
#    p18_08 <- a0510 + b0510*2008;
#    p18_07 <- a0510 + b0510*2007;
#    p18_06 <- a0510 + b0510*2006;
    p18_05 <- a0510 + b0510*2005;
    p18_04 <- a0510 + b0510*2004;
    p18_03 <- a0510 + b0510*2003;
    p18_02 <- a0510 + b0510*2002;
#    p18_01 <- a0510 + b0510*2001;
#    p18_00 <- a0510 + b0510*2000;
#    p18_99 <- a0510 + b0510*1999;
#    p18_98 <- a0510 + b0510*1998;
#    p18_97 <- a0510 + b0510*1997;
})


# indicate cases that need not or cannot be manipulated, change action to ""
sel <- with(eq2, which(action %in% c("lost","mun.chg","state.chg")))
if(length(sel)>0) {eq2$ddone <- 0;  eq2$ddone[sel] <- 1; eq2$ action[sel] <- eq2$ orig.dest[sel] <- ""; eq2$ when[sel] <- NA}
sel <- with(eq2, which(action2 %in% c("lost","mun.chg","state.chg")))
if(length(sel)>0) {                 eq2$ddone[sel] <- 1; eq2$action2[sel] <- eq2$orig.dest2[sel] <- ""; eq2$when2[sel] <- NA}
sel <- with(eq2, which(action3 %in% c("lost","mun.chg","state.chg")))
if(length(sel)>0) {                 eq2$ddone[sel] <- 1; eq2$action3[sel] <- eq2$orig.dest3[sel] <- ""; eq2$when3[sel] <- NA}
#
# cases that need no manipulation
sel <- with(eq2, which(action=="" & action2=="" & action3=="")) # cases that need not manipulation
eq2$ddone[sel] <- 1
# cases calling for special attention
sel <- with(eq2, which((action!="" & action2!="") | (action!="" & action3!="") | (action2!="" & action3!=""))) # dual chg
eq2$ddone[sel] <- 2

# cases w manipulated p18
sel <- with(eq2, which(ddone==0 & is.na(p18_2005b)==FALSE & action=="split.to" & when<2005)) #is.na(eq2$p18_2005)==TRUE)
eq2[sel,] <- within(eq2[sel,], {
    a0510 <- p18_2005b - 2005 * b0510 # change intercept for projection (using same slope)
})
eq2[sel,] <- within(eq2[sel,], {
    p18_04 <- a0510 + b0510*2004;
    p18_03 <- a0510 + b0510*2003;
    p18_02 <- a0510 + b0510*2002;
#    p18_01 <- a0510 + b0510*2001;
#    p18_00 <- a0510 + b0510*2000;
#    p18_99 <- a0510 + b0510*1999;
#    p18_98 <- a0510 + b0510*1998;
#    p18_97 <- a0510 + b0510*1997;
    ddone <- 1
})

table(eq2$ddone)
table(eq2$action[sel])
eq2[1,]
x



eq2[sel[1],]
eq2 <- within(eq2[sel,], {
    b0510 <- (p18_2010b - p18_2005b)/5;
#    a0510 <- p18_2005 - 2005 * (p18_2010 - p18_2005)/5;
})

# inspect
sel <- which(is.na(eq2$p18_2005b)==FALSE)#is.na(eq2$p18_2005)==TRUE)
sel <- which(eq2$action=="split.from")
sel <- which(eq2$action=="split.to")
table(is.na(eq2$p18_2005[sel]))
eq2[sel[1:10],c("edosecn","p18_2005","p18_2010","p18_2020","p18_2005b","p18_2010b","p18_2020b","action","orig.dest","when", "b0510","b1020")]
data.frame(eq2$p18_2005[sel], eq2$p18_2005b[sel], eq2$action[sel])
head(eq2)
x

#####################################################################################
## manipulate cases split.to second time to account for subsequent merged.to cases ##  NEED TO USE p18bs
#####################################################################################
sel <- grep("split.to", eq2$action)
manip <- eq2[sel,]
#
# apply function
manip <- tmp.f()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]
#
# cases recorded in action2
sel <- grep("split.to", eq2$action2)
tmp <-   eq2[sel,]
tmp$action <- tmp$action2; tmp$when <- tmp$when2; tmp$orig.dest <- tmp$orig.dest2 # move action2 to action etc
manip <- tmp
# apply function
manip <- tmp.f()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]
#
# cases recorded in action3
sel <- grep("split.to", eq2$action3)
tmp <-   eq2[sel,]
tmp$action <- tmp$action3; tmp$when <- tmp$when3; tmp$orig.dest <- tmp$orig.dest3 # move action3 to action etc
manip <- tmp
# apply function
manip <- tmp.f()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]

# manipulate merged to cases
sel <- grep("merged.to", eq2$action)
manip <- eq2[sel,]
#manip$orig.dest
#table(manip$when)

# wrap routine in function
tmp.f <- function(){
    # process chg up to 2005: except those that survived the split, will be redundant (determined afterwards)
    sel <- which(manip$when <= 2005) 
    # none occurred before 2008
    #
    # process chg between 2005 and 2010
    sel <- which(manip$when %in% 2005:2010)
    for (i in sel){
        #i <- sel[3]
        manip[sel,] <- within(manip[sel,], {
            b0510 <- (p18_2010b - p18_2005b)/5;
            a0510 <- p18_2005b - 2005 * (p18_2010b - p18_2005b)/5;
        })
        ## vec <- eval(parse(text = manip$orig.dest[i])) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
        ## vec <- c(vec, manip$seccion[i]) # add current seccion to vector
        ## vec <- manip$edon[i]*10000 + vec   # turn into edosecn
        ## vec <- which(eq2$edosecn %in% vec) # indices
        ## #manip[i,-c(2:5,8:14)]
        ## manip$p18_2005b[i] <- sum(eq2$p18_2005[vec])
        ## manip$p18_2010b[i] <- sum(eq2$p18_2010[vec], na.rm=TRUE) # surviving seccion's pop for rate of chg
    }
    # process chg between 2010 and 2020 
    sel <- which(manip$when %in% 2010:2020)
    for (i in sel){
        #i <- sel[1]
        manip[sel,] <- within(manip[sel,], {
            b1020 <- (p18_2020b - p18_2010b)/10;
            a1020 <- p18_2010b - 2010 * (p18_2020b - p18_2010b)/10;
        })
        ## vec <- eval(parse(text = manip$orig.dest[i])) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
        ## vec <- c(vec, manip$seccion[i]) # add current seccion to vector
        ## vec <- manip$edon[i]*10000 + vec   # turn into edosecn
        ## vec <- which(eq2$edosecn %in% vec) # indices
        ## #manip[i,-c(2:5,8:14)]
        ## manip$p18_2010b[i] <- sum(eq2$p18_2010[vec])
        ## manip$p18_2020b[i] <- sum(eq2$p18_2020[vec], na.rm=TRUE) # surviving seccion's pop for rate of chg
    }
    return(manip)
}
# apply function
manip <- tmp.f()
# return manip to eq2
sel <- which(eq2$edosecn %in% manip$edosecn)
eq2[sel, c("p18_2005b","p18_2010b","p18_2020b")] <- manip[, c("p18_2005b","p18_2010b","p18_2020b")]






