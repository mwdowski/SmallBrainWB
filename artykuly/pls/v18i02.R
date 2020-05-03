###################################################
library("pls")


###################################################
data("yarn")
data("oliveoil")
data("gasoline")


###################################################
matplot(t(gasoline$NIR), type = "l", lty = 1, ylab = "log(1/R)", xaxt = "n")
ind <- pretty(seq(from = 900, to = 1700, by = 2))
ind <- ind[ind >= 900 & ind <= 1700]
ind <- (ind - 898) / 2
axis(1, ind, colnames(gasoline$NIR)[ind])


###################################################
gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]


###################################################
gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")


###################################################
summary(gas1)


###################################################
plot(RMSEP(gas1), legendpos = "topright")


###################################################
plot(gas1, ncomp = 2, asp = 1, line = TRUE)


###################################################
plot(gas1, plottype = "scores", comps = 1:3)


###################################################
explvar(gas1)


###################################################
plot(gas1, "loadings", comps = 1:2, legendpos = "topleft", labels = "numbers", xlab = "nm")
abline(h = 0)


###################################################
predict(gas1, ncomp = 2, newdata = gasTest)


###################################################
RMSEP(gas1, newdata = gasTest)


###################################################
dens1 <- plsr(density ~ NIR, ncomp = 5, data = yarn)


###################################################
dim(oliveoil$sensory)
plsr(sensory ~ chemical, data = oliveoil)


###################################################
trainind <- which(yarn$train == TRUE)
dens2 <- update(dens1, subset = trainind)


###################################################
dens3 <- update(dens1, ncomp = 10)


###################################################
olive1 <- plsr(sensory ~ chemical, scale = TRUE, data = oliveoil)


###################################################
gas2 <- plsr(octane ~ msc(NIR), ncomp = 10, data = gasTrain)


###################################################
predict(gas2, ncomp = 3, newdata = gasTest)


###################################################
gas2.cv <- crossval(gas2, segments = 10)
plot(MSEP(gas2.cv), legendpos='topright')
summary(gas2.cv, what = "validation")


###################################################
plot(gas1, plottype = "coef", ncomp=1:3, legendpos = "bottomleft", labels = "numbers", xlab = "nm")


###################################################
plot(gas1, plottype = "correlation")


###################################################
predict(gas1, ncomp = 2:3, newdata = gasTest[1:5,])


###################################################
predict(gas1, comps = 2, newdata = gasTest[1:5,])


###################################################
drop(predict(gas1, ncomp = 2:3, newdata = gasTest[1:5,]))


###################################################
predplot(gas1, ncomp = 2, newdata = gasTest, asp = 1, line = TRUE)


###################################################
pls.options()


###################################################
pls.options(plsralg = "oscorespls")


###################################################
pls.options("plsralg")
rm(.pls.Options)
pls.options("plsralg")


###################################################
X <- gasTrain$NIR
Y <- gasTrain$octane
ncomp <- 5
cvPreds <- matrix(nrow = nrow(X), ncol = ncomp)
for (i in 1:nrow(X)) {
    fit <- simpls.fit(X[-i,], Y[-i], ncomp = ncomp, stripped = TRUE)
    cvPreds[i,] <- (X[i,] - fit$Xmeans) %*% drop(fit$coefficients) + fit$Ymeans
}


###################################################
sqrt(colMeans((cvPreds - Y)^2))

