# install.packages('party')
# install.packages('stringr')

library(rpart)
library(party)
library(stringr)

# Inicializamos semilla fija para que no aparezca un resulta nuevo cada vez
set.seed(343)

# Cargamos los archivos con los datos
train <- read.csv(paste(getwd(), "/train.csv", sep=""), header=TRUE, sep=",", dec=".")
test <- read.csv(paste(getwd(), "/test.csv", sep=""), header=TRUE, sep=",", dec=".")

test$Survived <- NA

# Juntamos todos los datos para la creación/predicción de las variables
total <- rbind(train, test)

# No dejamos tarifas vacías
summary(total$Fare)
total$Fare[is.na(total$Fare)] <- 0
summary(total$Fare)

# No dejamos puertos de embarque vacíos
summary(total$Embarked)
which(total$Embarked == '')
total$Embarked[c(62,830)] = "S"
total$Embarked <- factor(total$Embarked)
summary(total$Embarked)

# Crear variables con el tamaño de la familia
total$TamFamilia <- total$SibSp + total$Parch + 1

# Extraer títulos del nombre de los pasajeros
total$Name <- as.character(total$Name)
total$Titulo <- sapply(total$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
total$Titulo <- sub(' ', '', total$Titulo)
total$Titulo[total$Titulo %in% c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir')] <- 'Caballeros'
total$Titulo[total$Titulo %in% c('Dona', 'Jonkheer', 'Lady', 'Miss', 'Mlle', 'Mme', 'Ms','the Countess')] <- 'Damas'
total$Titulo <- factor(total$Titulo)
summary(total$Titulo)

# Predecir edades perdidas en función del resto de datos relevantes
summary(total$Age)
modeloEdad <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Titulo + TamFamilia, 
                    data=total[!is.na(total$Age),], method="anova")

total$Age[is.na(total$Age)] <- predict(modeloEdad, total[is.na(total$Age),])
summary(total$Age)

summary(total)

# Volvemos a separar los datos en sus respectivos conjuntos de entrenamiento y validación
train <- total[1:891,]
test <- total[892:1309,]

# Definimos modelo
modelo <- as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Titulo + TamFamilia

# Realizamos el entrenamiento
ajuste <- cforest(modelo, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
# Realizamos la predicción
prediccion <- predict(ajuste, test, OOB=TRUE, type = "response")

# Clasificación cruzada
tab <- table(predict(ajuste), train$Survived)
# Calculo del porcentaje de aciertos
sum(diag(tab))/sum(tab)

# Escribimos los resultados
resultado <- data.frame(PassengerId = test$PassengerId, Survived = prediccion)
write.csv(resultado, file = "solution.csv", row.names = FALSE)