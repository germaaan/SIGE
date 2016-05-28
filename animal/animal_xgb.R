library(caret)
library(xgboost)
library(rpart)

# Inicializamos semilla fija para que no aparezca un resulta nuevo cada vez
set.seed(343)

# Cargamos los archivos con los datos
train <- read.csv("/home/germaaan/proyectos/titanic-kaggle/animal/train.csv", header=TRUE, sep=",")
test <- read.csv("/home/germaaan/proyectos/titanic-kaggle/animal/test.csv", header=TRUE, sep=",")

# Ambos ID iguales
test$ID <- as.character(test$ID)
names(train)[1] <- "ID"

# Añadir clases faltantes en test
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

#probar a predecir sexo
total$SexuponOutcome[total$SexuponOutcome == ""] <- "Unknown"
total$SexuponOutcome <- factor(total$SexuponOutcome)

modeloEdad <- rpart(Edad ~ AnimalType + SexuponOutcome + Breed + Color, 
                    data=total[!is.na(total$Edad),], method="anova")
total$Edad[is.na(total$Edad)] <- predict(modeloEdad, total[is.na(total$Edad),])

Breed.table = data.frame(table(total$Breed))
Breed.table = Breed.table[order(-Breed.table$Freq),]
noChange1 <- Breed.table$Var1[1:30]
total$newFactorBreed <- (ifelse(total$Breed %in% noChange1, total$Breed, "Other"))
total$newFactorBreed <- factor(total$newFactorBreed)

total$Color <- as.character(total$Color)
total$Color[grepl("^Agouti*", total$Color)] <- "Agouti"
total$Color[grepl("^Apricot*", total$Color)] <- "Apricot"
total$Color[grepl("^Black*", total$Color)] <- "Black"
total$Color[grepl("^Blue*", total$Color)] <- "Blue"
total$Color[grepl("^Brown*", total$Color)] <- "Brown"
total$Color[grepl("^Buff*", total$Color)] <- "Buff"
total$Color[grepl("^Calico*", total$Color)] <- "Calico"
total$Color[grepl("^Chocolate*", total$Color)] <- "Chocolate"
total$Color[grepl("^Cream*", total$Color)] <- "Cream"
total$Color[grepl("^Fawn*", total$Color)] <- "Fawn"
total$Color[grepl("^Flame Point*", total$Color)] <- "Flame Point"
total$Color[grepl("^Gold*", total$Color)] <- "Gold"
total$Color[grepl("^Gray*", total$Color)] <- "Gray"
total$Color[grepl("^Lilac Point*", total$Color)] <- "Lilac Point"
total$Color[grepl("^Liver*", total$Color)] <- "Liver"
total$Color[grepl("^Lynx Point*", total$Color)] <- "Lynx Point"
total$Color[grepl("^Orange*", total$Color)] <- "Orange"
total$Color[grepl("^Red*", total$Color)] <- "Red"
total$Color[grepl("^Ruddy*", total$Color)] <- "Ruddy"
total$Color[grepl("^Sable*", total$Color)] <- "Sable"
total$Color[grepl("^Seal Point*", total$Color)] <- "Seal Point"
total$Color[grepl("^Silver*", total$Color)] <- "Silver"
total$Color[grepl("^Tan*", total$Color)] <- "Tan"
total$Color[grepl("^Torbie*", total$Color)] <- "Torbie"
total$Color[grepl("^Tortie*", total$Color)] <- "Tortie"
total$Color[grepl("^Tricolor*", total$Color)] <- "Tricolor"
total$Color[grepl("^White*", total$Color)] <- "White"
total$Color[grepl("^Yellow*", total$Color)] <- "Yellow"
total$Color <- factor(total$Color)

summary(total$AnimalType)
summary(total$Edad)
summary(total$SexuponOutcome)
summary(total$newFactorBreed)
summary(total$Color)


# clustering
#discretizar categoricos para prediccion
#pasar todo a años
#predecir las edades que faltan

#edad promedio en vez de predecir

# Volvemos a separar los datos en sus respectivos conjuntos de entrenamiento y validación
train <- droplevels(total[1:nrow(train), c(1, 2, 3, 6, 9, 10)])
test <- droplevels(total[(nrow(train)+1):nrow(total), c(1, 2, 3, 6, 9, 10)])

inTrain <- createDataPartition(y=train$OutcomeType, p=.6, list=FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]

clases <- OutcomeType ~ AnimalType + Edad + SexuponOutcome + newFactorBreed + Color

control <- trainControl(
  method="cv",
  number=5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all", 
  classProbs = TRUE
#  allowParallel=TRUE
)

tuning <- expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  colsample_bytree = 0.7,
  min_child_weight = 1
)

modelo <- train(clases, data=training, method="xgbTree", trControl=control, tuneGrid=tuning)
modelo

validacion <- predict(modelo, testing)
confusionMatrix(testing$OutcomeType, validacion)

prediccion <- predict(modelo, test)

id <- seq(1, 11456)
submission <- data.frame(ID=id, prediccion)
submission$prediccion <- as.character(submission$prediccion)
submission$Adoption <- 0
submission$Died <- 0
submission$Euthanasia <- 0
submission$Return_to_owner <- 0
submission$Transfer <- 0

submission$Adoption[submission$prediccion == "Adoption"] <- 1
submission$Died[submission$prediccion == "Died"] <- 1
submission$Euthanasia[submission$prediccion == "Euthanasia"] <- 1
submission$Return_to_owner[submission$prediccion == "Return_to_owner"] <- 1
submission$Transfer[submission$prediccion == "Transfer"] <- 1
submission$prediccion <- NULL

write.csv(submission, file = "animal_solution.csv", row.names = F)
