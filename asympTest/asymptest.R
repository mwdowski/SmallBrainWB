library(asympTest)

data(DIGdata)
attach(DIGdata)
x <- na.omit(DIABP[TRTMT==0])
y <- na.omit(DIABP[TRTMT==1])
c(length(x),length(y))
# wychodzi do0kładnie ile trzeba

var.test(DIABP ~ TRTMT, data = DIGdata, na.action = na.omit)
#woah, poza x/y zmienionym na DIABP/TRTMT jest dokładnie to samo

asymp.test(DIABP ~ TRTMT, data = DIGdata, na.action = na.omit, parameter = "dVar")
#tu 100% tak samo

n <- 1000
x <- runif(n, max = sqrt(12))
asymp.test(x, par = "var", alt = "gr", ref = 0.97)
#tu już jest random, więc są zmiany, nie podano seeda nawet

chisq.stat <- (n-1)*var(x)/0.97
pchisq(chisq.stat, n-1, lower.tail = F)
#same here, jak wyżej