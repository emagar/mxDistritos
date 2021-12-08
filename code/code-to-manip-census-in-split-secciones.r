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

# secciones that cannot be fixed for census data projection
#no.manip <- c(10487) # eg. 10487 created in 2004 with bits of three secciones (mun limits), then merged to 10378 in 2008
sel <- which(eq2$edosecn==10487);
eq2[sel,] <- within(eq2[sel,], {
    y2004 <- y2005 <- y2006 <- y2007 <- y2008 <- p18_2005; # given no way to get change, assign 2005 pop=31 along seccion's lifespan 2004:2008
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
    dpre05done  <- d0510done   <- d1020done   <- dpost20done <- 1
})
#eq2[sel,]

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
##################################
## secciones that never changed ##
##################################
sel <- which(is.na(manip$when) & manip$dpre05done==0 & manip$d0510done==0 & manip$d1020done==0 & manip$dpost20done==0)
length(sel)
manip[sel,] <- within(manip[sel,], {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
# return manipulated data
eq2 <- manip
rm(manip)

##############################
## municipio changes ignored #
##############################
sel <- which(eq2$action3 == "mun.chg")
length(sel) # is zero, so ignore
# action2
sel <- which(eq2$action2 == "mun.chg")
manip <- eq2[sel,]
manip <- within(manip, {
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
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
###############################################################################
## secciones modified up to 1997 will be ignored (analysis to start in 1997) ##
###############################################################################
sel <- which(eq2$when3 <= 1997)
length(sel) # is zero, ignore
sel <- which(eq2$when2 <= 1997)
length(sel) # is zero, ignore
sel <- which(eq2$when <= 1997)
manip <- eq2[sel,]
manip <- within(manip, {
    action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
    action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
})
eq2[sel,] <- manip
# record done
sel <- which(is.na(eq2$when) & (eq2$dpre05done==0 | eq2$d0510done==0 | eq2$d1020done==0 | eq2$dpost20done==0))
eq2[sel,] <- within(eq2[sel,], dpre05done  <- d0510done   <- d1020done   <- dpost20done <- 1)

1 [] manipulate when3 controlling when2 and when
2 [] start with split.to (should take care of split.from), then merged. Any missing?
For each:
    3 [] compute pct chg -> pi
    4 [] project backwards with appropriate baseline pop
    5 [] chg pre..done to 1
    6 [] return manipulation to dataset
    7 [] next
    8 [] check ddone status across the board
    9 [] check cases ddone==1 but NA

1 [x] select changes bef 2005
2 [x] take 1, subset it and its targets
3 [x] compute pct chg -> pi
4 [x] project backwards with appropriate baseline pop
5 [x] chg pre2005done to 1
6 [x] return manipulation to dataset
7 [x] loop
8 [] check ddone status across the board
9 [] check cases ddone==1 but NA

############################
## cases with when3 record #
############################
#sel <- which(!is.na(eq2$when3)) # one case only
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
manip <- eq2[vec.minus,]
if (manip$when>1997) manip <- within(manip, y1997 <- NA)
manip <- within(manip, {
    action  <- "";      when <- NA;    orig.dest <- "";
    #times.manip <- times.manip + 1;
})
eq2[vec.minus,] <- manip
#
# do when2 change
sel <- which(eq2$edosecn==161976)
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
# indicate what's done
manip <- within(manip, {
    action  <- action3; when  <- when3; orig.dest <- orig.dest3;
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    action3 <- "";      when3 <- NA;    orig.dest3 <- "";
    times.manip <- times.manip + 1;
})
# return to data
eq2[sel,] <- manip
# fix proj in non-existing secciones and return manipulation to data
manip <- eq2[vec.minus,]
if (manip$when>1997) manip <- within(manip, y1997 <- NA)
if (manip$when>1998) manip <- within(manip, y1998 <- NA)
if (manip$when>1999) manip <- within(manip, y1999 <- NA)
if (manip$when>2000) manip <- within(manip, y2000 <- NA)
if (manip$when>2001) manip <- within(manip, y2001 <- NA)
manip <- within(manip, {
    action  <- "";      when <- NA;    orig.dest <- "";
    #times.manip <- times.manip + 1;
})
eq2[vec.minus,] <- manip

############################
## cases with when2 record #
############################
sel <- which(!is.na(eq2$when2)) # five items only
# item 1 by hand
manip <- eq2[sel[1],]
vec <- eval(parse(text = manip$orig.dest2)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.plus  <- union(vec, sel[1])   # seccion and its targets (in case vec excludes dropped seccion) 
vec.minus <- setdiff(vec, sel[1]) # targets w/o seccion (for use when seccion was dropped)
# aggregate split populations
#manip$p18_2005b <- sum(eq2$p18_2005[vec])
manip$p18_2010b <- sum(eq2$p18_2010[vec]) # needed for backwards projection
manip <- within(manip, y2010 <- p18_2010b) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change
# re-project where needed
if (manip$when2>2002) manip <- within(manip, y2002 <- tmp.proj(baseyr = 2005, yr = 2002))
if (manip$when2>2003) manip <- within(manip, y2003 <- tmp.proj(baseyr = 2005, yr = 2003))
if (manip$when2>2004) manip <- within(manip, y2004 <- tmp.proj(baseyr = 2005, yr = 2004))
if (manip$when2>2006) manip <- within(manip, y2006 <- tmp.proj(baseyr = 2005, yr = 2006))
if (manip$when2>2007) manip <- within(manip, y2007 <- tmp.proj(baseyr = 2005, yr = 2007))
if (manip$when2>2008) manip <- within(manip, y2008 <- tmp.proj(baseyr = 2005, yr = 2008))
if (manip$when2>2009) manip <- within(manip, y2009 <- tmp.proj(baseyr = 2005, yr = 2009))
# indicate what's done
manip <- within(manip, {
    action2 <- "";      when2 <- NA;    orig.dest2 <- "";
    times.manip <- times.manip + 1;
})
# return manipulation to data
eq2[i,] <- manip

# items 4 and 5 by hand
manip <- eq2[sel[5],]
vec <- c(2136,2437)
vec <- manip$edon*10000 + vec   # turn into edosecn
manip$case <- "no.baja"
vec <- which(eq2$edosecn %in% vec) # turn to indices
vec.plus  <- union(vec, sel[5])   # seccion and its targets (in case vec excludes dropped seccion) 
vec.minus <- setdiff(vec, sel[5]) # targets w/o seccion (for use 
# aggregate split populations
#manip$p18_2005b <- sum(eq2$p18_2005[vec])
manip$p18_2020b <- sum(eq2$p18_2020[vec]) # needed for backwards projection
manip <- within(manip, y2010 <- p18_2010b) # use aggregates to compute pi
manip <- within(manip, pi <-  (y2010 / y2005) ^ (1 / (2010 - 2005)) ); # rate of change


x

paste(manip$when, manip$when2, manip$when3)
paste(manip$action, manip$action2, manip$action3)
paste(manip$orig.dest, manip$orig.dest2, manip$orig.dest3)
x


################################
## split secciones up to 2005 ##
################################
sel <- which(eq2$action=="split.to" & eq2$when > 1996 & eq2$when <  2005)
# add target secciones
for (i in sel){
    #i <- sel[3]
    manip <- eq2[i,] # duplicate obs for manipulation
    vec <- eval(parse(text = manip$orig.dest)) # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    vec <- manip$edon*10000 + vec   # turn into edosecn
    manip$case <- ifelse(manip$edosecn %in% vec, "no.baja", "baja") # did seccion survive the split? (baja means no) 
    vec <- which(eq2$edosecn %in% vec) # turn to indices
    vec.plus  <- union(vec, i)   # seccion and its targets (in case vec excludes dropped seccion) 
    vec.minus <- setdiff(vec, i) # targets w/o seccion (for use when seccion was dropped)
    # aggregate split populations
    manip$p18_2005b <- sum(eq2$p18_2005[vec])
    manip$p18_2010b <- sum(eq2$p18_2010[vec]) # needed for backwards projection
    manip <- within(manip, {y2005 <- p18_2005b; y2010 <- p18_2010b; y2020 <- p18_2020;}) # use aggregates to compute pi
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
    if (manip$when<2005) manip <- within(manip, y2005 <- p18_2005)
    # fix proj for eliminated secciones
    if (manip$case=="baja") {
        if (manip$when<1998) manip <- within(manip, y1997 <- NA)
        if (manip$when<1999) manip <- within(manip, y1998 <- NA)
        if (manip$when<2000) manip <- within(manip, y1999 <- NA)
        if (manip$when<2001) manip <- within(manip, y2000 <- NA)
        if (manip$when<2002) manip <- within(manip, y2001 <- NA)
        if (manip$when<2003) manip <- within(manip, y2002 <- NA)
        if (manip$when<2004) manip <- within(manip, y2003 <- NA)
        if (manip$when<2005) manip <- within(manip, y2004 <- NA)
    }
    # return manipulation to data
    eq2[i,] <- manip
    # fix proj in non-existing secciones and return manipulation to data
    manip <- eq2[vec.minus,]
    if (manip$when>1997) manip <- within(manip, y1997 <- NA)
    if (manip$when>1998) manip <- within(manip, y1998 <- NA)
    if (manip$when>1999) manip <- within(manip, y1999 <- NA)
    if (manip$when>2000) manip <- within(manip, y2000 <- NA)
    if (manip$when>2001) manip <- within(manip, y2001 <- NA)
    if (manip$when>2002) manip <- within(manip, y2002 <- NA)
    if (manip$when>2003) manip <- within(manip, y2003 <- NA)
    if (manip$when>2004) manip <- within(manip, y2004 <- NA)
    if (manip$when>2005) manip <- within(manip, y2005 <- NA)
    eq2[vec.minus,] <- manip
    # indicate manipulation
    manip <- eq2[vec.plus,]
    manip <- within(manip, {
        dpre05done <- 1;
        times.manip <- times.manip + 1;
        action  <- action2; when  <- when2; orig.dest  <- orig.dest2;
        action2 <- action3; when2 <- when3; orig.dest2 <- orig.dest3;
        action3 <- "";      when3 <- NA;    orig.dest3 <- "";
    })
    # indicate cases that are ready and return manipulation to data
    sel0 <- which(is.na(manip$when) & (manip$dpre05done==0 | manip$d0510done==0 | manip$d1020done==0 | manip$dpost20done==0))
    if (length(sel0)>0) manip[sel0,] <- within(manip[sel0,], dpre05done  <- d0510done   <- d1020done   <- dpost20done <- 1)
    eq2[vec.plus,] <- manip
}


    
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






