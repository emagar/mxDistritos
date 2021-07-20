
###################################################
## determine which muncipalities changed borders ##
###################################################
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

# select secciones that switched municipios
tmp <- tmp1 <- eq[eq$dmunchg==1, c("ife1991", "ife1994", "ife1997", "ife2000", "ife2003", "ife2006", "ife2009", "ife2012", "ife2015", "ife2018", "ife2021")]
dim(tmp)

chg1994 <- unique(c(tmp$ife1991[which(tmp$ife1991 != tmp$ife1994)],
                    tmp$ife1994[which(tmp$ife1991 != tmp$ife1994)]))
chg1994 <- chg1994[order(chg1994)]
#
chg1997 <- unique(c(tmp$ife1994[which(tmp$ife1994 != tmp$ife1997)],
                    tmp$ife1997[which(tmp$ife1994 != tmp$ife1997)]))
chg1997 <- chg1997[order(chg1997)]
#
chg2000 <- unique(c(tmp$ife1997[which(tmp$ife1997 != tmp$ife2000)],
                    tmp$ife2000[which(tmp$ife1997 != tmp$ife2000)]))
chg2000 <- chg2000[order(chg2000)]
#
chg2003 <- unique(c(tmp$ife2000[which(tmp$ife2000 != tmp$ife2003)],
                    tmp$ife2003[which(tmp$ife2000 != tmp$ife2003)]))
chg2003 <- chg2003[order(chg2003)]
#
chg2006 <- unique(c(tmp$ife2003[which(tmp$ife2003 != tmp$ife2006)],
                    tmp$ife2006[which(tmp$ife2003 != tmp$ife2006)]))
chg2006 <- chg2006[order(chg2006)]
#
chg2009 <- unique(c(tmp$ife2006[which(tmp$ife2006 != tmp$ife2009)],
                    tmp$ife2009[which(tmp$ife2006 != tmp$ife2009)]))
chg2009 <- chg2009[order(chg2009)]
#
chg2012 <- unique(c(tmp$ife2009[which(tmp$ife2009 != tmp$ife2012)],
                    tmp$ife2012[which(tmp$ife2009 != tmp$ife2012)]))
chg2012 <- chg2012[order(chg2012)]
#
chg2015 <- unique(c(tmp$ife2012[which(tmp$ife2012 != tmp$ife2015)],
                    tmp$ife2015[which(tmp$ife2012 != tmp$ife2015)]))
chg2015 <- chg2015[order(chg2015)]
#
chg2018 <- unique(c(tmp$ife2015[which(tmp$ife2015 != tmp$ife2018)],
                    tmp$ife2018[which(tmp$ife2015 != tmp$ife2018)]))
chg2018 <- chg2018[order(chg2018)]
#
chg2021 <- unique(c(tmp$ife2018[which(tmp$ife2018 != tmp$ife2021)],
                    tmp$ife2021[which(tmp$ife2018 != tmp$ife2021)]))
chg2021 <- chg2021[order(chg2021)]
#



