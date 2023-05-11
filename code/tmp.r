        add1988 <- function(x){
            rbind(v88=x[1,], x) # repeat 1st row
            x$yr[1] <- 1988
            x[1,c(2:7,9:11)] <- NA # all NAs except ife
            }
        ## OJO: should replace NAs above with 1988 mun returns, if I have them
        tmp <- extendCoalm94 # duplicate for manipulation
        tmp <- lapply(extendCoalm94, add1988) # add row for 1991 to each data frame in list
        extendCoalm94 <- tmp


        add1991 <- function(x){
            rbind(v91=x[1,], x) # repeat 1st row
            x$yr[1] <- 1991
            x[1,c(2:7,9:11)] <- NA # all NAs except ife
            return(x)
        }
