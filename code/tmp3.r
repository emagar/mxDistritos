



tmp <- as.numeric(sub("^v([0-9]{2}).*", "\\1", rownames(pan)))
ifelse(tmp>89, tmp+1900, tmp+2000)

tmp <- ls(); tmp[grep("^v..m", tmp)]
tmp <- ls(); tmp[grep("extend", tmp)]

v94s[1,]

