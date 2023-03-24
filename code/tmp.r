plot(2002:2010, manip[,26:34])

redo projection (linear, no constant needed)

20+            o
  |
  |
10+  o
  |--+-+-+-+-+-+-
     5 6     9 10 

increment = (20-10) / (10-5) = 10/5 = 2 by year
          = (p_20 - p_10) / (20 - 10)
therefore p_6 = p_5 + 2*1 or p_6 = p_10 - 2*4 





head(tmp)

colnames(d)[which(colnames(d)=="pri.pvem")] <- "pric"
colnames(d)[which(colnames(d)=="prd.pt.mc")] <- "prdc"




#####################################################################
## aggregate coalitions where present for correct winner assesment ##
#####################################################################
d_agg <- d # duplicate for manipulation
sel.r <- which(d_agg$dpric==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    pric <- pri + pric + pvem;
    pri <- pvem <- 0;
})
sel.r <- which(d_agg$dprdc==1)
d_agg[sel.r,] <- within(d_agg[sel.r,], {
    prdc <- prd + pt + mc + prdc;
    prd <- pt <- mc <- 0;
})
##############################################################
## split coalition votes according to contributions in unit ##
##############################################################
d_split <- d # duplicate for manipulation
d_split <- apportion_v(dat=d_split, members=c("pri","pvem"), joint="pric")
d_split <- apportion_v(dat=d_split, members=c("pt","conve"), joint="ptc")
#
v09_agg <- d_agg
v09_split <- d_split


