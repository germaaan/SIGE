setwd("~/proyectos/titanic-kaggle")

train=read.csv(file="train.csv", header=TRUE, sep=",", dec=".")
test=read.csv(file="test.csv", header=TRUE, sep=",", dec=".")

# Mujeres y niños primero
summary(train$Sex)
summary(train$Age)

prop.table(table(train$Sex, train$Survived),1)

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Age < 20.12 ] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "solution.csv", row.names = FALSE)
