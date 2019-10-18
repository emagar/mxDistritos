# Function transforming ternary into cartesian coordinates
# from https://stackoverflow.com/questions/11623602/shaded-triplot-in-r
tern2cart <- function(coord){
    coord[1] -> x
    coord[2] -> y
    coord[3] -> z
    x+y+z -> tot
    x/tot -> x
    y/tot -> y
    z/tot -> z
    (2*y + z)/(2*(x+y+z)) -> x1
    sqrt(3)*z/(2*(x+y+z)) -> y1
    return(c(x1,y1))
    }
# function to add vértice labels
add.lab <- function(labels){
    left  <- labels[2];
    right <- labels[1];
    up    <- labels[3];
    text(0,0,labels=left,pos=1)         # left
    text(1,0,labels=right,pos=1)        # right
    text(0.5,sqrt(3)/2,labels=up,pos=3) # up
}

la.ternera <- function(datos, color = rgb(.55,.27,.07, alpha = .2), cex.pts = .15, main = NA){
    # Prepare data: re-arrange so pan is lower right, pri lower left, morena is above 
    #                  left    right    up
    datos <- datos[,c("pri",   "pan",  "morena")] # subset
    #Then transformed into cartesian coordinates:
    datos <- t(apply(datos,1,tern2cart))
    # Draw the empty ternary diagram:
    par(mar=c(2.1, 2.1, 4.1, 2.1)) ## SETS B L U R MARGIN SIZES
    plot(NA, NA, xlim=c(0,1), ylim=c(0,sqrt(3)/2), asp=1, bty="n", axes=F, xlab="", ylab="", main = main)
    segments(0,0,0.5,sqrt(3)/2)
    segments(0.5,sqrt(3)/2,1,0)
    segments(1,0,0,0)
    # add vértice labels
    add.lab(c("PAN","PRI","Morena"))
    ## # add a grid:
    ## a <- seq(0.9,0.1,by=-0.1)
    ## b <- rep(0,9)
    ## c <- seq(0.1,0.9,by=0.1)
    ## grid <- data.frame(x=c(a, b, c, a, c, b),y=c(b, c, a, c, b, a),z=c(c, a, b, b, a, c))
    ## t(apply(grid,1,tern2cart)) -> grid.tern
    ## cbind(grid.tern[1:27,],grid.tern[28:54,]) -> grid
    ## apply(grid,1,function(x){segments(x0=x[1],y0=x[2],x1=x[3],y1=x[4],lty=2,col="grey80")})
    # or 50-50 to 33-33-33 lines instead
    a <- c(1,1,0)
    b <- c(1,0,1)
    c <- c(0,1,1)
    d <- c(1,1,1)
    grid <- data.frame(matrix(c(a,b,c,d),nrow=4,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:3){
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[4,1],y1=grid.tern[4,2],lty=2,col="grey10")
    }
    # Plot points:
    points(datos, pch = 20, cex = cex.pts, col = color)
    #
    ## # axis labels
    ## paste(seq(10,90,by=10),"%")->lab
    ## text(grid.tern[9:1,],paste(lab,"\n(PAN)"),col="grey80",cex=0.7, pos=2)
    ## text(grid.tern[18:10,],paste(lab,"\n(PRI)"),col="grey80",cex=0.7, pos=4)
    ## text(grid.tern[27:19,],paste(lab,"\n(Morena)"),col="grey80",cex=0.7, pos=1)
}


# party colors
col.pan <-    rgb(.18,.24,.73, alpha = .2) # moderate blue
col.pri <-    rgb(.89,.17,.17, alpha = .2) # bright red
col.morena <- rgb(.55,.27,.07, alpha = .2)


# triplot predicted
tmp <- extendCoal.2018[,c("pan", "pri", "morena")] # subset
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
la.ternera(datos = tmp, cex.pts = 2, color = tri.color, main = "Voto 2018 pronosticado")

# triplot observed
tmp <- extendCoal.2018[,c("vhat.pan", "vhat.pri", "vhat.morena")] # subset
colnames(tmp) <- c("pan", "pri", "morena")
tri.color <- apply(tmp, 1, which.max); names(tri.color) <- NULL
tri.color[tri.color==1] <- col.pan
tri.color[tri.color==2] <- col.pri
tri.color[tri.color==3] <- col.morena
la.ternera(datos = tmp, cex.pts = 2, main = "Voto 2018 observado")



