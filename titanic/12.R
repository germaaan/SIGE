# install.packages('randomForest')
# install.packages('gbm')
# install.packages('caret')

library(rpart)
library(randomForest)
library(gbm)
library(caret)


# Inicializamos semilla fija para que no aparezca un resulta nuevo cada vez
set.seed(343)

# Cargamos los archivos con los datos
train <- read.csv(paste(getwd(), "/train.csv", sep=""), header=TRUE, sep=",", dec=".")
test <- read.csv(paste(getwd(), "/test.csv", sep=""), header=TRUE, sep=",", dec=".")

# HIPÓTESIS: "Mujeres y niños primero"
# Comprobar pasajeros por sexo y edad
# pruebas=train
# summary(pruebas$Sex)
# summary(pruebas$Age)

# Probabilidad de que sobrevivan por sexo y edad
# prop.table(table(pruebas$Sex, pruebas$Survived), 1)
# pruebas$Infante=ifelse(pruebas$Age < 18, "Niño", "Adulto")
# prop.table(table(pruebas$Infante, pruebas$Survived), 1)

# REGLA: Si es mujer sobrevive, si es niño sobrevive
# test$Survived <- 0
# test$Survived[test$Sex == 'female'] <- 1
# test$Survived[test$Age < 18 ] <- 1

# Predecir edades perdidas en función del resto de datos relevantes
test$Survived <- NA
total <- rbind(train, test)

total$TamFamilia <- total$SibSp + total$Parch + 1

#summary(total$Age)

#modeloEdad <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, 
#                data=total[!is.na(total$Age),], method="anova")

#total$Age[is.na(total$Age)] <- predict(modeloEdad, total[is.na(total$Age),])
#summary(total$Age)

total$Fare[is.na(total$Fare)] <- 0
summary(total$Fare)
total$NivelTarifa=ifelse(total$Fare >= 31.280, "Muy alta", 
                         ifelse(total$Fare >= 14.450, "Alta", 
                                ifelse(total$Fare >= 7.896, "Normal", "Baja")))
total$NivelTarifa=as.factor(total$NivelTarifa)
summary(total$NivelTarifa)

modeloEdad <- gbm(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                  data=total[!is.na(total$Age),], n.trees = 10000)

total$Age[is.na(total$Age)] <- predict(modeloEdad, total, n.trees=10000)[is.na(total$Age)]
summary(total$Age)

total$NivelEdad=ifelse(total$Age >= 36.5, "Muy alta", 
                       ifelse(total$Age >= 28.00, "Alta", 
                              ifelse(total$Age >= 22.00, "Normal", "Baja")))
total$NivelEdad=as.factor(total$NivelEdad)
summary(total$NivelEdad)

total$Survived  <- as.factor(total$Survived )

train <- total[1:891,]
test <- total[892:1309,]

# Definimos modelo
modelo <- Survived ~ Sex + Age + TamFamilia + NivelTarifa + NivelEdad


# Predicción de la supervivencia mediante Rpart
# ajuste <- rpart(modelo, data=train, method="class")
# prediccion <- predict(ajuste, test, type = "class")

# Predicción de la supervivencia mediante RandomForest
# ajuste <- randomForest(modelo, data=train, importance=TRUE, ntree=5000)
# prediccion <- predict(ajuste, test)

# Predicción de la supervivencia mediante Boosting
#ajuste <- gbm(modelo, data = train, distribution = "adaboost", n.trees = 5000)
#prediccion <- predict(ajuste, test, n.trees=5000, type="response")
#density(prediccion)
#prediccion <- ifelse(prediccion<0.4797,0,1)
#as.factor(prediccion)
#as.factor(test$Survived)
#postResample(testPred, testing$Class)
#postResample(as.factor(prediccion), as.factor(test$Survived))

#ajuste = train(modelo, data=train, method="gbm", distribution="adaboost", verbose=FALSE, tuneGrid=data.frame(.n.trees=5000, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))

ajuste <- randomForest(modelo, data=train, importance=TRUE, ntree=5000)
prediccion <- predict(ajuste, test)
tab <- table(predict(ajuste), train$Survived)
sum(diag(tab))/sum(tab)

resultado <- data.frame(PassengerId = test$PassengerId, Survived = prediccion)
write.csv(resultado, file = "solution.csv", row.names = FALSE)