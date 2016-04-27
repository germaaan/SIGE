# install.packages('randomForest')
# install.packages('gbm')

library(rpart)
library(randomForest)
library(gbm)

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

modeloEdad <- gbm(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + TamFamilia,
           data=total[!is.na(total$Age),], n.trees = 10000)

total$Age[is.na(total$Age)] <- predict(modeloEdad, total, n.trees=10000)[is.na(total$Age)]
summary(total$Age)

train <- total[1:891,]
test <- total[892:1309,]

# Definimos modelo
modelo <- Survived ~ Sex + Age + Fare + Embarked + TamFamilia

# Predicción de la supervivencia mediante Rpart
# ajuste <- rpart(modelo, data=train, method="class")
# prediccion <- predict(ajuste, test, type = "class")

# Predicción de la supervivencia mediante RandomForest
# ajuste <- randomForest(modelo, data=train, importance=TRUE, ntree=5000)
# prediccion <- predict(ajuste, test)

# Predicción de la supervivencia mediante Boosting
ajuste <- gbm(modelo, data = train, distribution = "adaboost", n.trees = 10000)
prediccion <- predict(ajuste, test, n.trees=10000, type="response")
density(prediccion)
prediccion <- ifelse(prediccion<0.4925,0,1)

resultado <- data.frame(PassengerId = test$PassengerId, Survived = prediccion)
write.csv(resultado, file = "solution.csv", row.names = FALSE)
