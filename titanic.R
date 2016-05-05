# install.packages('randomForest')
# install.packages('gbm')
# install.packages('party')

library(rpart)
library(randomForest)
library(party)
library(gbm)
library(stringr)

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
quantile(total$Fare, c(.33, .66))
#total$NivelTarifa=ifelse(total$Fare >= 31.280, "Muy alta", 
#                         ifelse(total$Fare >= 14.450, "Alta", 
#                                ifelse(total$Fare >= 7.896, "Normal", "Baja")))
total$NivelTarifa=ifelse(total$Fare >= 26.000, "Alta", 
                         ifelse(total$Fare >= 8.5167, "Media", "Baja"))

total$NivelTarifa=as.factor(total$NivelTarifa)
summary(total$NivelTarifa)

modeloEdad <- gbm(Age ~ Pclass + Sex + SibSp + Parch + NivelTarifa + Embarked + TamFamilia,
           data=total[!is.na(total$Age),], n.trees = 5000)

total$Age[is.na(total$Age)] <- predict(modeloEdad, total, n.trees=5000)[is.na(total$Age)]
summary(total$Age)

total$NivelEdad=ifelse(total$Age >= 36.5, "Muy alta", 
                         ifelse(total$Age >= 28.00, "Alta", 
                                ifelse(total$Age >= 22.00, "Normal", "Baja")))
total$NivelEdad=as.factor(total$NivelEdad)
summary(total$NivelEdad)

total$Name <- as.character(total$Name)
total$Titulo <- sapply(total$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
total$Titulo <- sub(' ', '', total$Titulo)
total$Titulo[total$Titulo %in% c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir')] <- 'Caballeros'
total$Titulo[total$Titulo %in% c('Dona', 'Jonkheer', 'Lady', 'Miss', 'Mlle', 'Mme', 'Ms','the Countess')] <- 'Damas'
total$Titulo <- factor(total$Titulo)
summary(total$Titulo)

total$Familia <- as.factor(sapply(total$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]}))

summary(total$Familia)
total$Familia <- as.character(total$Familia)
total$Familia=as.factor(ifelse(total$TamFamilia <= 3, 'COMUN', total$Familia))
summary(total$Familia)

summary(total$Embarked)
which(total$Embarked == '')
total$Embarked[c(62,830)] = "S"
total$Embarked <- factor(total$Embarked)
summary(total$Embarked)

summary(total)

train <- total[1:891,]
test <- total[892:1309,]

# Definimos modelo
#modelo <- as.factor(Survived) ~ Sex + Age + TamFamilia + NivelTarifa
modelo <- as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + TamFamilia + Titulo

# Predicción de la supervivencia mediante Rpart
# ajuste <- rpart(modelo, data=train, method="class")
# prediccion <- predict(ajuste, test, type = "class")

# Predicción de la supervivencia mediante RandomForest
 ajuste <- randomForest(modelo, data=train, importance=TRUE, ntree=2000)
 prediccion <- predict(ajuste, test)
 
# cforest
 #ajuste <- cforest(modelo, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
# prediccion <- predict(ajuste, test, OOB=TRUE, type = "response")

# Predicción de la supervivencia mediante Boosting
# ajuste <- gbm(modelo, data = train, distribution = "adaboost", n.trees = 2000)
# prediccion <- predict(ajuste, test, n.trees=2000, type="response")
# density(prediccion)
# prediccion <- ifelse(prediccion<9940,0,1)

resultado <- data.frame(PassengerId = test$PassengerId, Survived = prediccion)
write.csv(resultado, file = "solution.csv", row.names = FALSE)
