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


head(tmp18)
table(duplicated(pob18$edosecn))
table(duplicated(tmp18$edosecn))
table(duplicated(generic$edosecn), as.integer(generic$edosecn/10000))
sel <- which(duplicated(generic$edosecn)==TRUE)
table(generic$edosecn[sel])
head(generic)












