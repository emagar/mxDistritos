

table(raw.2021$dpanc), raw.2021$edon)
table(raw.2021$dmorenac, raw.2021$edon)

table(v21d$dpanc)
table(v21d$dmorenac)

v21[1,]

sel <- grep("^4013", names(extendCoal))
tmp <- extendCoal
dim(tmp)
length(tmp)
tail(tmp)
sel <- which(tmp$seccion==5000 & tmp$edon==17)
tmp[sel,]
which(tmp$munchg>0)
tmp[sel]

extendCoal[[sel]]

            y12 y15
   a bcd      a
   b          a   b
   c          a   c
   d          a   d

            y12 y15
   abc d      a   d
   b          b   d
   c          c   d
   d          d





