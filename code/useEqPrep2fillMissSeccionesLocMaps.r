######################################################
######################################################
## SCRIPT CALLED FROM within STATE MAP PREP SCRIPTS ##
######################################################
######################################################

## MERGE TO tablaEquivalenciaSeccional TO GET FED MAP AND FILL MISSING SECCIONES
edon <- 6 # set a state number 1:32
edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
message("Working with ", edon, "=", edo[edon], sep = "")

d$edon <- NULL # drop edon column in locmap (d comes from state map script)

# may need to set temporary wd to find table file if directory structure has been re-organized
eq <- read.csv(file = "../../equivSecc/tablaEquivalenciasSeccionalesDesde1994.csv", stringsAsFactors = FALSE)
sel <- which(eq$edon==edon)
eq <- eq[sel,] # subset to chosen edon
dim(eq)
dim(d)
#eq$ord <- eq$dis1979 <- eq$dis1997 <- eq$dis2006 <- NULL
eq$ord <- NULL
colnames(eq) <- gsub(pattern = "dis", replacement = "disfed", colnames(eq))
# merge
eq <- merge(x = eq, y = d, by = c("seccion","munn"), all = TRUE)
dim(eq)
#table(duplicated(eq$seccion), useNA = "ifany")
eq <- eq[order(eq$seccion),]
tail(eq)
#
# invoke script to transfer missing info from secciones that dis/appeared from their parent/offspring 
source(file = "../../code/eqPrep.r", encoding = "utf-8", echo=TRUE)

eq$OBSERVACIONES <- eq$action <- eq$fr.to <- eq$orig.dest <- eq$when <- eq$color <- eq$coment <- NULL # drop columns with instructions
#
eq$disfed1997 <- eq$disfed2000 <- eq$disfed2006 <- eq$disfed2009 <- eq$disfed2012 <- eq$disfed2013 <- NULL # keep 1 column per federal map
eq <- rename.col(old = "disfed1994", new = "disfed1979", what = eq) # gets oldest version of each map (should no longer make difference atfer this script)
eq <- rename.col(old = "disfed2003", new = "disfed1997", what = eq) # gets oldest version of each map (should no longer make difference atfer this script)
eq <- rename.col(old = "disfed2015", new = "disfed2006", what = eq)
# colnames(eq) # inspect

# NOW BACK TO LOCAL MAP PREP SCRIPT
