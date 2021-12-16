plot(2002:2010, manip[,26:34])

redo projection (linear, no constant needed)

20+            o
  |
  |
10+  o
  |--+-+-+-+-+-+-
     5 6     9 10 

increment = (20-10) / (10-5) = 10/5 = 2 by year
therefore p_6 = p_5 + 2 or p_6 = p_10 - 2*4 


---

# re-do vectors picking census column that is not NA
#aorb05 <- apply(X=eq2[,c("p18_2005","p18_2005b","y2005")], MARGIN=1, FUN=function(X)
#    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
aorb10 <- apply(X=eq2[,c("p18_2010","p18_2010b","y2010")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
aorb20 <- apply(X=eq2[,c("p18_2020","p18_2020b","y2020")], MARGIN=1, FUN=function(X)
    ifelse(!is.na(X[1]), X[1], ifelse(!is.na(X[2]), X[2], X[3]))) # try b if pop2010 is NA
##################################
## single change secciones 2010 ##
## resecc 2010 before census    ##
##################################
#############################
## split.to and split.from ##
#############################
sel <- which(eq3$action=="split.to" & eq3$when==2010)
## sel <- which(eq2$edosecn %in% c(10081,10594:10607))
## eq2$p18_2010[sel]
for (i in sel){
    #i <- sel[1]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$p18_2010b <- with(eq2, sum(aorb10[vec]))
    manip$p18_2020b <- with(eq2, sum(aorb20[vec]))
    #
    manip <- within(manip, {y2005 <- p18_2005; y2010 <- p18_2010b; y2020 <- p18_2020b}) # use aggregates to compute pi
    # 2005-10 needed since 2010 was missing in first pass
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
    if (manip$when>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
    # now pi for 2010-20
    manip <- within(manip, pi <-  (y2020 / y2010) ^ (1 / (2020 - 2010)) ); # rate of change
    tmp <- manip$pi # save pi for new secciones
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
    # 2010 chg need no manip, just indicate it's done
    manip <- eq3[vec.minus,]
    manip <- within(manip, {
        y1997 <- y1998 <- y1999 <- NA;
        y2000 <- y2001 <- y2002 <- y2003 <- y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- y2009 <- NA;
        action  <- "";      when  <- NA;    orig.dest  <- "";
    })
    eq3[vec.minus,] <- manip
}

######################
## post2010 pre2020 ##
######################
sel <- which(eq3$action=="split.to" & eq3$when>2010 & eq3$when<2020)
for (i in sel){
    #i <- sel[1]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest))
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    #manip$p18_2010b <- with(eq2, sum(aorb10[vec]))
    manip$p18_2020b <- with(eq2, sum(aorb20[vec]))
    manip <- within(manip, {y2020 <- p18_2020b}) # use aggregates to compute pi
    manip <- within(manip, pi <-  (y2020 / y2010) ^ (1 / (2020 - 2010)) ); # rate of change
    tmp <- manip$pi # save pi for new secciones
    if (manip$when>2011) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2010, yr = 2011))
    if (manip$when>2012) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2010, yr = 2012))
    if (manip$when>2013) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2010, yr = 2013))
    if (manip$when>2014) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2010, yr = 2014))
    if (manip$when>2015) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2010, yr = 2015))
    if (manip$when>2016) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2010, yr = 2016))
    if (manip$when>2017) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2010, yr = 2017))
    if (manip$when>2018) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2010, yr = 2018))
    if (manip$when>2019) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2010, yr = 2019))
    #if (manip$when>2021) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2010, yr = 2021))
    #if (manip$when>2022) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2010, yr = 2022))
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
    # 2010 chg need no manip, just indicate it's done
    manip <- eq2[vec.minus,]
    manip$pi <- tmp # paste agg's rate of change
    if (eq2$when[i]<=2011) manip <- within(manip, y2011 <- tmp.proj(baseyr = 2020, yr = 2011))
    if (eq2$when[i]<=2012) manip <- within(manip, y2012 <- tmp.proj(baseyr = 2020, yr = 2012))
    if (eq2$when[i]<=2013) manip <- within(manip, y2013 <- tmp.proj(baseyr = 2020, yr = 2013))
    if (eq2$when[i]<=2014) manip <- within(manip, y2014 <- tmp.proj(baseyr = 2020, yr = 2014))
    if (eq2$when[i]<=2015) manip <- within(manip, y2015 <- tmp.proj(baseyr = 2020, yr = 2015))
    if (eq2$when[i]<=2016) manip <- within(manip, y2016 <- tmp.proj(baseyr = 2020, yr = 2016))
    if (eq2$when[i]<=2017) manip <- within(manip, y2017 <- tmp.proj(baseyr = 2020, yr = 2017))
    if (eq2$when[i]<=2018) manip <- within(manip, y2018 <- tmp.proj(baseyr = 2020, yr = 2018))
    if (eq2$when[i]<=2019) manip <- within(manip, y2019 <- tmp.proj(baseyr = 2020, yr = 2019))
    if (eq2$when[i]<=2021) manip <- within(manip, y2021 <- tmp.proj(baseyr = 2020, yr = 2021))
    if (eq2$when[i]<=2022) manip <- within(manip, y2022 <- tmp.proj(baseyr = 2020, yr = 2022))
    manip[1,]
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

# Now check remaining split.from ??
with(eq3, table(pre20=when<2020, action, useNA = "always"))
x
###############
## merged.to ##
###############
sel <- which(eq3$action=="merged.to" & eq3$when==2010)
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
with(eq3, table(pre10=when<2020, action, useNA = "always"))





