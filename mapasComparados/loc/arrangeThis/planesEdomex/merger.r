 ## CONTINUE ANALYSIS IN R AFTER RUNNING STATA MERGER

rm(list=ls())
#workdir <- c("C:/Users/emagarm/Documents/Dropbox/data/elecs/redistrict/planesEdomex/")
#workdir <- c("D:/01/Dropbox/data/elecs/redistrict/planesEdomex")
workdir <- c("~/Dropbox/data/elecs/redistrict/planesEdomex")
setwd(workdir)

old <- read.csv("old.csv")
new.magar <- read.csv("new.magar.csv")
#new.esquivel <- read.csv("new.esquivel.csv")
#new.lego <- read.csv("newA.csv")

margin <- .06 ## DEFINE MARGIN CUTPOINT

tmp <- data.frame(pan=rep(NA,40),pric=rep(NA,40),prdc=rep(NA,40),panal=rep(NA,40))
## PARTY RANKS, WINNER, AND MARGINS
for (j in 1:40){
    tmp[j,] <- -rank(old[j,2:5],ties.method="random")+5
}
old$win <- rep(".",40); old$second <- rep(".",40); old$third <- rep(".",40); old$mg12 <- rep(NA,40); old$mg23 <- rep(NA,40);
for (j in 1:40){
    old$win[j] <- ifelse(tmp[j,1]==1, "pan",
                  ifelse(tmp[j,2]==1, "pric",
                  ifelse(tmp[j,3]==1, "prdc", "oth")))
    old$second[j] <- ifelse(tmp[j,1]==2, "pan",
                  ifelse(tmp[j,2]==2, "pric",
                  ifelse(tmp[j,3]==2, "prdc", "oth")))
    old$third[j] <- ifelse(tmp[j,1]==3, "pan",
                  ifelse(tmp[j,2]==3, "pric",
                  ifelse(tmp[j,3]==3, "prdc", "oth")))
}
first <- tmp; second <- tmp; third <- tmp
for (i in 1:ncol(tmp)){
    cond <- first[,i] > 1;
    first[cond,i] <- 0;
    cond <- second[,i]==1 | second[,i]>2 ;
    second[cond,i] <- 0;
    cond <- third[,i]<3 | third[,i]>3 ;
    third[cond,i] <- 0;
}
second <- second/2; third <- third/3
sh <- old[,2:5]/old$efec
old$mg12 <- round(apply(sh*first, 1, sum)  - apply(sh*second, 1, sum), digits=2)
old$mg23 <- round(apply(sh*second, 1, sum)  - apply(sh*third, 1, sum), digits=2)
#old$mg13 <- apply(sh*first, 1, sum)  - apply(sh*third, 1, sum)
old$dmgnal <- ifelse(old$mg12<=margin, 1, 0)
hh <- apply(sh^2, 1, sum); fi <- apply((sh*first)^2, 1, sum)
old$enp <- round(1+(hh-fi)/hh^2, digits=2) #Molinar
#old$enp <- round(1/hh, digits=2)           #Laakso+Taagepera
rm(hh)
old <- old[order(-old$mg12),]

new <- new.lego
#new <- new.magar
tmp <- data.frame(pan=rep(NA,40),pric=rep(NA,40),prdc=rep(NA,40),panal=rep(NA,40))
## PARTY RANKS, WINNER, AND MARGINS
for (j in 1:40){
    tmp[j,] <- -rank(new[j,2:5],ties.method="random")+5
}
new$win <- rep(".",40); new$second <- rep(".",40); new$third <- rep(".",40); new$mg12 <- rep(NA,40); new$mg23 <- rep(NA,40);
for (j in 1:40){
    new$win[j] <- ifelse(tmp[j,1]==1, "pan",
                  ifelse(tmp[j,2]==1, "pric",
                  ifelse(tmp[j,3]==1, "prdc", "oth")))
    new$second[j] <- ifelse(tmp[j,1]==2, "pan",
                  ifelse(tmp[j,2]==2, "pric",
                  ifelse(tmp[j,3]==2, "prdc", "oth")))
    new$third[j] <- ifelse(tmp[j,1]==3, "pan",
                  ifelse(tmp[j,2]==3, "pric",
                  ifelse(tmp[j,3]==3, "prdc", "oth")))
}
first <- tmp; second <- tmp; third <- tmp
for (i in 1:ncol(tmp)){
    cond <- first[,i] > 1;
    first[cond,i] <- 0;
    cond <- second[,i]==1 | second[,i]>2 ;
    second[cond,i] <- 0;
    cond <- third[,i]<3 | third[,i]>3 ;
    third[cond,i] <- 0;
}
second <- second/2; third <- third/3
sh <- new[,2:5]/new$efec
new$mg12 <- round(apply(sh*first, 1, sum)  - apply(sh*second, 1, sum), digits=2)
new$mg23 <- round(apply(sh*second, 1, sum)  - apply(sh*third, 1, sum), digits=2)
#new$mg13 <- apply(sh*first, 1, sum)  - apply(sh*third, 1, sum)
new$dmgnal <- ifelse(new$mg12<=margin, 1, 0)
hh <- apply(sh^2, 1, sum); fi <- apply((sh*first)^2, 1, sum)
new$enp <- round(1+(hh-fi)/hh^2, digits=2) #Molinar
#new$enp <- round(1/hh, digits=2)           #Laakso+Taagepera
rm(hh)
new <- new[order(-new$mg12),]
#new.magar <- new
new.lego <- new

comp <- function(plan1=old,plan2=old,long.print=FALSE)
  {print(paste("Plan 1:",deparse(substitute(plan1)), "(summary)"))
   cat("Districts won by\n")
   cat(paste("PAN","PRI","PRD","Oth","\n",sep="\t"))
   cat(paste(length(plan1$win[plan1$win=="pan"]), length(plan1$win[plan1$win=="pric"])+length(plan1$win[plan1$win=="pri"]), length(plan1$win[plan1$win=="prdc"]), length(plan1$win[plan1$win=="oth"]), sep="\t"))
   cat("\n\n")
   cat("Districts won by margin of\n")
   cat(paste("<10%","<8%","<6%","<4%","<2%","\n",sep="\t"))
   cat(paste(sum(ifelse(plan1$mg12<=.1, 1, 0)), sum(ifelse(plan1$mg12<=.08, 1, 0)), sum(ifelse(plan1$mg12<=.06, 1, 0)), sum(ifelse(plan1$mg12<=.04, 1, 0)), sum(ifelse(plan1$mg12<=.02, 1, 0)), sep="\t"))
#   cat(paste("Marginal (6%) districts:", sum(plan1$dmgnal)))
   cat("\n\n")
   print(paste("Plan 2:",deparse(substitute(plan2)), "(summary)"))
   cat("Districts won by\n")
   cat(paste("PAN","PRI","PRD","Oth","\n",sep="\t"))
   cat(paste(length(plan2$win[plan2$win=="pan"]), length(plan2$win[plan2$win=="pric"])+length(plan2$win[plan2$win=="pri"]), length(plan2$win[plan2$win=="prdc"]), length(plan2$win[plan2$win=="oth"]), sep="\t"))
   cat("\n\n")
   cat("Districts won by margin of\n")
   cat(paste("<10%","<8%","<6%","<4%","<2%","\n",sep="\t"))
   cat(paste(sum(ifelse(plan2$mg12<=.1, 1, 0)), sum(ifelse(plan2$mg12<=.08, 1, 0)), sum(ifelse(plan2$mg12<=.06, 1, 0)), sum(ifelse(plan2$mg12<=.04, 1, 0)), sum(ifelse(plan2$mg12<=.02, 1, 0)), sep="\t"))
#   cat(paste("Marginal (6%) districts:", sum(plan2$dmgnal)))
   cat("\n\n")
   if (long.print==TRUE) {
      print(paste("District results for plan 1:",deparse(substitute(plan1))))
      print(plan1)
      cat("\n")
      print(paste("District results for plan 2:",deparse(substitute(plan2))))
      print(plan2)
   } else {
       print("District by district description omitted (use option long.print=TRUE).")
   }
 }

comp(old,new.lego,long.print=TRUE)




