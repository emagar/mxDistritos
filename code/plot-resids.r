##################
## party colors ##
##################
col.pan <-    rgb(.18,.24,.73, alpha = .2) # moderate blue
col.vxm <-    rgb(  0,.75,  1, alpha = .2) # deepskyblue
col.pri <-    rgb(.89,.17,.17, alpha = .2) # brightred
col.morena <- rgb(.55,.27,.07, alpha = .2) # saddlebrown
col.prd <-    rgb(  1,.84,  0, alpha = .2) # gold
col.pvem   <- rgb(  0,.80,  0, alpha = .2) # green 3
col.mc     <- rgb(.93,.46,  0, alpha = .2) # darkorange2
col.oth    <- rgb(.55,.55,.55, alpha = .2) # gray 59
#col.oth    <- rgb(.94, .5, .5, alpha = .2) # lightcoral
#19/255

############################################################
## ###################################################### ##
## ## Load data or use elec-data-for-maps to generate  ## ##
## ###################################################### ##
############################################################

#####################
## municipio-level ##
#####################
pty <- "PRI"
yr <- 2018
alfa <- "PRI" # what alpha in x-axis?
##
if (yr==2009) dat <- read.csv(file = paste(wd, "data/dipfed-municipio-vhat-2009.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2012) dat <- read.csv(file = paste(wd, "data/dipfed-municipio-vhat-2012.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2015) dat <- read.csv(file = paste(wd, "data/dipfed-municipio-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2018) dat <- read.csv(file = paste(wd, "data/dipfed-municipio-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2021) dat <- read.csv(file = paste(wd, "data/dipfed-municipio-vhat-2021.csv", sep = ""), stringsAsFactors = FALSE)
##
dat <- within(dat, {
    res.pan <- pan - vhat.pan;
    res.pri <- pri - vhat.pri;
    res.left <- left - vhat.left;
})
##
tit <- paste(pty, "en", yr, "(municipios)")
xl  <- ifelse(alfa=="PRI", "alpha del PRI", "alpha")
with(dat, {
    plot(x=c(0,1), y=c(-.85,.85), type="n", xlab = xl, ylab = "residual", axes = FALSE, main = tit)
    axis(1, at = seq(0,1,.2))
    axis(2, at = seq(-1,1,.2))
    abline(h=0, col = "gray")
    if (pty=="PAN"    & alfa=="PAN")    points(alphahat.pan,  res.pan,  col = col.pan,    pch = 19, cex = .6)
    if (pty=="PRI"    & alfa=="PRI")    points(alphahat.pri,  res.pri,  col = col.pri,    pch = 19, cex = .6)
    if (pty=="MORENA" & alfa=="MORENA") points(alphahat.left, res.left, col = col.morena, pch = 19, cex = .6)
    if (pty=="PRD"    & alfa=="PRD")    points(alphahat.left, res.left, col = col.prd,    pch = 19, cex = .6)
    if (pty=="PAN"    & alfa=="PRI")    points(alphahat.pri,  res.pan,  col = col.pan,    pch = 19, cex = .6)
    if (pty=="PRI"    & alfa=="PRI")    points(alphahat.pri,  res.pri,  col = col.pri,    pch = 19, cex = .6)
    if (pty=="MORENA" & alfa=="PRI")    points(alphahat.pri, res.left, col = col.morena, pch = 19, cex = .6)
    if (pty=="PRD"    & alfa=="PRI")    points(alphahat.pri, res.left, col = col.prd,    pch = 19, cex = .6)
})



####################
## distrito-level ##
####################
pty <- "PRI"
yr <- 1988
alfa <- "PRI" # what alpha in x-axis?
##
if (yr==1988) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-1988.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1991) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-1991.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1994) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-1994.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==1997) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-1997.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2000) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2000.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2003) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2003.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2006) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2006.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2009) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2009.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2012) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2012.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2015) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2015.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2018) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2018.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2021) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2021.csv", sep = ""), stringsAsFactors = FALSE)
if (yr==2024) dat <- read.csv(file = paste(wd, "data/dipfed-distrito-vhat-2024.csv", sep = ""), stringsAsFactors = FALSE)
##
dat <- within(dat, {
    res.pan <- pan - vhat.pan;
    res.pri <- pri - vhat.pri;
    res.left <- left - vhat.left;
})
##
tit <- paste(pty, "en", yr, "(distritos)")
xl  <- ifelse(alfa=="PRI", "alpha del PRI", "alpha")
with(dat, {
    plot(x=c(0,1), y=c(-.85,.85), type="n", xlab = xl, ylab = "residual", axes = FALSE, main = tit)
    axis(1, at = seq(0,1,.2))
    axis(2, at = seq(-1,1,.2))
    abline(h=0, col = "gray")
    if (pty=="PAN"    & alfa=="PAN")    points(alphahat.pan,  res.pan,  col = col.pan,    pch = 19, cex = .9)
    if (pty=="PRI"    & alfa=="PRI")    points(alphahat.pri,  res.pri,  col = col.pri,    pch = 19, cex = .9)
    if (pty=="MORENA" & alfa=="MORENA") points(alphahat.left, res.left, col = col.morena, pch = 19, cex = .9)
    if (pty=="PRD"    & alfa=="PRD")    points(alphahat.left, res.left, col = col.prd,    pch = 19, cex = .9)
    if (pty=="PAN"    & alfa=="PRI")    points(alphahat.pri,  res.pan,  col = col.pan,    pch = 19, cex = .9)
    if (pty=="PRI"    & alfa=="PRI")    points(alphahat.pri,  res.pri,  col = col.pri,    pch = 19, cex = .9)
    if (pty=="MORENA" & alfa=="PRI")    points(alphahat.pri,  res.left, col = col.morena, pch = 19, cex = .9)
    if (pty=="PRD"    & alfa=="PRI")    points(alphahat.pri,  res.left, col = col.prd,    pch = 19, cex = .9)
})



