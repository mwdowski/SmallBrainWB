library(neuralnet)

nn <- neuralnet(
    case~age+parity+induced+spontaneous,
    data=infert, hidden=2, err.fct="ce",
    linear.output=FALSE)
nn # wypisuje tonę tekstu
nn$call
summary(nn)
#nie da się zrobić tak żeby pokazywało tak samo

nn$result.matrix

# mniej więcej to samo, pewno znowu seed
# a no i pokazywanie, zaokrąglanie, taki szajs jak zwykle

out <- cbind(nn$covariate, nn$net.result[[1]])
dimnames(out) <- list(NULL, c("age","parity","induced", "spontaneous","nn-output"))
head(out)
# nn-outputy są inne, może to seed, bo czasem jest dość mały błąd względny, ale czasem też jest duży

nn.bp <- neuralnet(
    case~age+parity+induced+spontaneous,
    data=infert, hidden=2, err.fct="ce",
    linear.output=FALSE,
    algorithm="backprop",
    learningrate=0.01)
nn.bp
#znwou tona tekstu, ale chyba wychodzi mniej więcej to samo

nn.nnet <- nnet(
    case~age+parity+induced+spontaneous,
    data=infert, size=2, entropy=T,
    abstol=0.01)

head(nn$generalized.weights[[1]])
# są inne, może to seed, bo czasem jest dość mały błąd względny, ale czasem też jest duży

plot(nn)
# wartości są albo dość dlaekie od oryginalnych, albo są bardzo bliskie, ale z minusem

par(mfrow=c(2,2))
gwplot(nn,selected.covariate="age",
         min=-2.5, max=5)
gwplot(nn,selected.covariate="parity",
         min=-2.5, max=5)
gwplot(nn,selected.covariate="induced",
         min=-2.5, max=5)
gwplot(nn,selected.covariate="spontaneous",
         min=-2.5, max=5)
# plot wygląda z grubsza tak samo, ale niekiedy widać odstające wartości

new.output <- compute(nn,
                      covariate=matrix(c(22,1,0,0,
                                         22,1,1,0,
                                         22,1,0,1,
                                         22,1,1,1),
                                       byrow=TRUE, ncol=4))
new.output$net.result
# wartości bardzo bliskie są oryginalnym

ci <- confidence.interval(nn.new, alpha=0.05)
ci$lower.ci
# opowiedzieli jakz robili nn.new, ale nie pokazALI KODU, ZATEM NIEREPRODUKOWALNY CHUNK, CND