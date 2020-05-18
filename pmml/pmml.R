library(pmml)
library(rpart)
data(iris)

my.rpart <- rpart(Species ~ ., data=iris)
my.rpart

pmml(my.rpart)

# zapis
# saveXML(pmml(my.rpart), file="my_rpart.xml")

library(kernlab)
audit <- read.csv(file("http://rattle.togaware.com/audit.csv"))
# halo zmiana nazwy kolumny w zbiorze audit Adjusted -> TARGET_Adjusted
#myksvm <- ksvm(as.factor(Adjusted) ~ ., data=audit[,c(2:10,13)], kernel = "rbfdot", prob.model=TRUE)
myksvm <- ksvm(as.factor(TARGET_Adjusted) ~ ., data=audit[,c(2:10,13)], kernel = "rbfdot", prob.model=TRUE)
pmml(myksvm, data=audit)

# imo reprodukowalne