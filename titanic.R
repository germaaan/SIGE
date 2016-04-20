install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

setwd("~/proyectos/titanic-kaggle")

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv(file="train.csv", header=TRUE, sep=",", dec=".")
test <- read.csv(file="test.csv", header=TRUE, sep=",", dec=".")

# Mujeres y niños primero
summary(train$Sex)
summary(train$Age)

prop.table(table(train$Sex, train$Survived),1)

# test$Survived <- 0
# test$Survived[test$Sex == 'female'] <- 1
# test$Survived[test$Age < 20.12 ] <- 1

modelo <- Survived ~ Sex + Age
ajuste <- rpart(modelo, data=train, method="class")

fancyRpartPlot(ajuste)

prediccion <- predict(ajuste, test, type = "class")

resultado <- data.frame(PassengerId = test$PassengerId, Survived = prediccion)
write.csv(resultado, file = "solution.csv", row.names = FALSE)
