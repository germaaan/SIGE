# install.packages("randomForest")
# install.packages("unbalanced")
# install.packages("DMwR")

library(plyr)
library(caret)
library(randomForest)
library(rpart)
library(unbalanced)
library(DMwR)

# Inicializamos semilla fija para que no aparezca un resulta nuevo cada vez
set.seed(343)

# Cargamos los archivos con los datos
train <- read.csv("/home/germaaan/proyectos/titanic-kaggle/animal/train.csv", header=TRUE, sep=",")
test <- read.csv("/home/germaaan/proyectos/titanic-kaggle/animal/test.csv", header=TRUE, sep=",")

test$ID <- as.character(test$ID)
names(train)[1] <- "ID"

test$OutcomeType <- ""
test$OutcomeSubtype <- ""

total <- rbind(train, test)[, c(4, 6, 7, 8, 9, 10)]

#completar y convertir a numero
#summary(total$AgeuponOutcome)
total$AgeuponOutcome <- as.character(total$AgeuponOutcome)
total$ValorTiempo=sapply(total$AgeuponOutcome, function(x) strsplit(x, split=' ')[[1]][1])
total$UnidadTiempo=sapply(total$AgeuponOutcome, function(x) strsplit(x, split=' ')[[1]][2])
total$UnidadTiempo=gsub('s', '', total$UnidadTiempo)
total$UnidadTiempo=as.factor(total$UnidadTiempo)
total$ValorTiempo=as.numeric(total$ValorTiempo)
multiplicador=ifelse(total$UnidadTiempo == 'day', 1, ifelse(total$UnidadTiempo == 'week', 7, ifelse(total$UnidadTiempo == 'month', 30, ifelse(total$UnidadTiempo == 'year', 365, NA))))
total$Edad=multiplicador * total$ValorTiempo
total$Edad[total$Edad == 0] <- NA
summary(total$Edad)
#probar a predecir sexo
total$SexuponOutcome[total$SexuponOutcome == ""] <- "Unknown"
total$SexuponOutcome <- factor(total$SexuponOutcome)

modeloEdad <- rpart(Edad ~ AnimalType + SexuponOutcome + Breed + Color, 
                    data=total[!is.na(total$Edad),], method="anova")
total$Edad[is.na(total$Edad)] <- predict(modeloEdad, total[is.na(total$Edad),])

summary(total$AnimalType)
summary(total$Edad)
summary(total$SexuponOutcome)
summary(total$Breed)
summary(total$Color)

total$Color[grepl("^Agouti*", total$Color)] <- "Agouti"
total$Color[grepl("^Apricot*", total$Color)] <- "Apricot"
total$Color[grepl("^Black*", total$Color)] <- "Black"
total$Color[grepl("^Brown*", total$Color)] <- "Brown"
total$Color[grepl("^White*", total$Color)] <- "White"
total$Color[grepl("^Blue*", total$Color)] <- "Blue"

prop.table(table(total$Color))

# clustering
#discretizar categoricos para prediccion
#pasar todo a años
#predecir las edades que faltan

#edad promedio en vez de predecir

# Volvemos a separar los datos en sus respectivos conjuntos de entrenamiento y validación
train <- total[1:nrow(train), c(1, 2, 3, 5, 6, 9)]
test <- total[(nrow(train)+1):nrow(total), c(1, 2, 3, 5, 6, 9)]

modelo <- OutcomeType ~ AnimalType + Edad + SexuponOutcome + Breed + Color
ajuste <- randomForest(modelo, data=train, importance=TRUE, mtry = 3, n.trees = 2000)

#prediccion <- predict(ajuste, test, type="vote")


