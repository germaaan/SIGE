# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
# install.packages('randomForest')

# library(rpart)
# library(rattle)
# library(rpart.plot)
# library(RColorBrewer)
library(randomForest)

set.seed(343)

train <- read.csv(paste(getwd(), "/train.csv", sep=""), header=TRUE, sep=",", dec=".")
test <- read.csv(paste(getwd(), "/test.csv", sep=""), header=TRUE, sep=",", dec=".")

# Mujeres y niños primero
summary(train$Sex)
summary(train$Age)

prop.table(table(train$Sex, train$Survived),1)

# test$Survived <- 0
# test$Survived[test$Sex == 'female'] <- 1
# test$Survived[test$Age < 20.12 ] <- 1

complete.cases(train)
x <- train[complete.cases(train), ]
complete.cases(x)

modelo <- as.factor(Survived) ~ Sex + Age
# ajuste <- rpart(modelo, data=train, method="class")
ajuste <- randomForest(modelo, data=x, ntree=5000)
tab <- table(predict(ajuste), x$Survived)
sum(diag(tab))/sum(tab)

# fancyRpartPlot(ajuste)

# prediccion <- predict(ajuste, test, type = "class")
prediccion <- predict(ajuste, test)

resultado <- data.frame(PassengerId = test$PassengerId, Survived = prediccion)
write.csv(resultado, file = "solution.csv", row.names = FALSE)