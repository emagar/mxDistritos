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

# secciones that cannot be fixed for census data projection
#no.manip <- c(10487) # eg. 10487 created in 2004 with bits of three secciones (mun limits), then merged to 10378 in 2008
sel <- which(eq2$edosecn==10487); eq2[sel,] <- within(eq2[sel,], {action <- "lost"; when <- NA; orig.dest <- ""})
#eq2[sel,]


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






