
case
seccion == 81
action == split.to
orig.dest == 597:607
when == 2014

0. subset eq2.manip with i and orig.dest lines
1. gen
sel <- which(seccion %in% eval(parse(text = manip$orig.dest[i])))
lo.merge[i] <-     pob2010[i]
hi.merge[i] <- sum(pob2020[sel])
#lo.split <- 
#hi.split <- 
2. gen slope with lo.merge hi.merge
3. pob2011 to pob2013  <-  slope from lo.merge
3. pob2014 to pob2019  <-  slope from hi.split
