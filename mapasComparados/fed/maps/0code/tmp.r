x <- rnorm(100)  
y <- rbinom(100, 1, 0.5)

par(fig = c(0,1,0,1))

hist(x)  
par(fig = c(0.07, 0.5, 0.5, 1), new = T)  
boxplot(x ~ y)



gray
blues[5:7]
reds[5:7]
yellows[5:7]

gray
blue
red
yellow
green
brown

        # bastion legend for three parties
        clr <- data.frame(pan = blues[c(3,5,7)], pri = reds[c(3,5,7)], prd = yellows[c(2,4,6)], stringsAsFactors = FALSE)
        sz <- .75
        par(mar=c(0,0,0,0)) ## SETS B L U R MARGIN SIZES
#        par(bg = "white")
        plot(x = c(1,6), y = c(0,4.5), type = "n", axes = FALSE)
        polygon(x = c(1,1,6,6), y = c(4,5,5,4), border = "white", col = "white") # white background
        polygon(x = c(4,4,6,6), y = c(0,6,6,0), border = "white", col = "white") # white background
        for (r in 1:3){
            for (c in 1:3){
                polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0)+r, col = alpha(clr[r,c], .5), border = "white", lwd = 4)
            }
        }
        for (c in 1:3){
            polygon(x = c(0,0,1,1)+c, y = c(0,1,1,0), col = alpha(gray, .5), border = "white", lwd = 4)
        }
        text(x = 1.5, y = 4.2, label = "PAN", cex = sz)
        text(x = 2.5, y = 4.2, label = "PRI", cex = sz)
        text(x = 3.5, y = 4.2, label = "Left", cex = sz)
        text(x = 5,   y = 4.2, label = "won", cex = sz)
        text(x = 4.1, y = 3.5, label = "6 of 6", pos = 4, cex = sz)
        text(x = 4.1, y = 2.5, label = "5 of 6", pos = 4, cex = sz)
        text(x = 4.1, y = 1.5, label = "4 of 6", pos = 4, cex = sz)
        text(x = 4.1, y = 0.5, label = "fewer", pos = 4, cex = sz)



