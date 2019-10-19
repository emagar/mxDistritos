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
    # or add 50-50 to 33-33-33 lines instead
    a <- c(1,1,0)
    b <- c(1,0,1)
    c <- c(0,1,1)
    d <- c(1,1,1)
    grid <- data.frame(matrix(c(a,b,c,d),nrow=4,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:3){
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[4,1],y1=grid.tern[4,2],lty=2,col="grey10")
    }
    # with 10 percent bands
    a <- c(55,45,0)
    b <- c(55,0,45)
    c <- c(40,30,30)
    grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:2){
        #i <- 1 # debug
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey60")
    }
    a <- c(45,55, 0)
    b <- c( 0,55,45)
    c <- c(30,40,30)
    grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:2){
        #i <- 1 # debug
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey60")
    }
    a <- c(45, 0,55)
    b <- c( 0,45,55)
    c <- c(30,30,40)
    grid <- data.frame(matrix(c(a,b,c),nrow=3,byrow=TRUE))
    grid.tern <- t(apply(grid,1,tern2cart))
    for (i in 1:2){
        #i <- 1 # debug
        segments(x0=grid.tern[i,1],y0=grid.tern[i,2],x1=grid.tern[3,1],y1=grid.tern[3,2],lty=2,col="grey60")
    }
