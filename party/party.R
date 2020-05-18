library(party)

set.seed(42)

readingSkills.cf <- cforest(score ~ ., data = readingSkills, control = cforest_unbiased(mtry = 2, ntree = 50))
set.seed(42)
varimp(readingSkills.cf)
# prawie to samo, ale nie do końca - kwestia seeda znowu

set.seed(42)
varimp(readingSkills.cf, conditional = TRUE)
# prawie to samo, ale nie do końca - kwestia seeda znowu

