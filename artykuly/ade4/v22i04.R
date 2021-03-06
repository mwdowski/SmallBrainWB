###################################################
### chunk number 1: option
###################################################
options(prompt="R> ", continue=" ", width = 85) 


###################################################
### chunk number 5: ni1
###################################################
library(ade4)
apropos("dudi.")


###################################################
### chunk number 6: ni2
###################################################
args(as.dudi)


###################################################
### chunk number 7: ni3
###################################################
methods(class="dudi")


###################################################
### chunk number 8: ex1
###################################################
data(dunedata)
sapply(dunedata$envir,class)


###################################################
### chunk number 9: ex2
###################################################
dunedata$envir$use <- factor(dunedata$envir$use,ordered=FALSE)
summary(dunedata$envir)


###################################################
### chunk number 10: ex3
###################################################
dd1 <- dudi.hillsmith(dunedata$envir, scannf = FALSE,nf=2)
dd1


###################################################
### chunk number 11: ex4
###################################################
scatter.dudi(dd1)


