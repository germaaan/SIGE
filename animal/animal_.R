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

total <- rbind(train, test)

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

tablaRazas = data.frame(table(total$Breed))
tablaRazas = tablaRazas[order(-tablaRazas$Freq),]
topRazas <- tablaRazas$Var1[1:30]
total$Raza <- (ifelse(total$Breed %in% topRazas, total$Breed, "Otro"))
total$Raza <- factor(total$Raza)

summary(total$AnimalType)
summary(total$Edad)
summary(total$SexuponOutcome)
summary(total$Raza)
summary(total$Color)


# clustering
#discretizar categoricos para prediccion
#pasar todo a años
#predecir las edades que faltan

#edad promedio en vez de predecir

total$OutcomeType <- as.numeric(total$OutcomeType) - 1
total$AnimalType <- as.numeric(total$AnimalType) - 1
total$SexuponOutcome <- as.numeric(total$SexuponOutcome) - 1
total$Color <- as.numeric(total$Color) - 1
total$Raza <- as.numeric(total$Raza) - 1

# Volvemos a separar los datos en sus respectivos conjuntos de entrenamiento y validación
train <- droplevels(total[1:nrow(train), c(4, 6, 7, 10, 13, 14)])
test <- droplevels(total[(nrow(train)+1):nrow(total), c(4, 6, 7, 10, 13, 14)])
test <- as.matrix(test)

# Crear particiones para entrenamiento y validacion
in.train <- createDataPartition(y=train$OutcomeType, p=0.80, list=FALSE)
in.train <- in.train[1:21383]

# Crear el vector con las clases a predecir
train.y <- train$OutcomeType

# Crear matriz con los valores
train.x <- train
train.x$OutcomeType <- NULL
train.x <- as.matrix(train.x)
train.x <- matrix(data = train.x, nrow = nrow(train.x), ncol = ncol(train.x))

# Parametros para la prediccion
xg.param <- list("objective" = "multi:softprob",
                 'eval_metric' = "mlogloss",
                 'num_class' = 5,
                 'eta' = 0.005,
                 'gamma' = 0.5,
                 'max.depth' = 10,
                 'min_child_weight' = 4,
                 'subsample' = 0.9,
                 'colsample_bytree' = 0.8,
                 'nthread' = 3)

# Cross-Validation
xgb.fit.cv <- xgb.cv(param = xg.param, data = train.x, label = train.y, 
                     nfold = 5, nrounds = 1000)

cv.min <- min(xgb.fit.cv$test.mlogloss.mean)
cv.min.rounds <- which(xgb.fit.cv$test.mlogloss.mean == min(xgb.fit.cv$test.mlogloss.mean)) 

cv.rounds <- round(mean(which(xgb.fit.cv$test.mlogloss.mean == min(xgb.fit.cv$test.mlogloss.mean))))  

# Ajustar modelo en el conjunto de validacion
xgb.fit <- xgboost(param = xg.param, data = train.x[-in.train, ], 
                   label = train.y[-in.train], nrounds = cv.rounds)

# Ajustar modelo en el conjunto de entrenamiento completo
xgb.fit <- xgboost(param = xg.param, data = train.x, label = train.y, nrounds = cv.rounds)

# Prediccion
xgb.pred <- predict(xgb.fit, test)

test <- matrix(data = as.numeric(test), nrow = nrow(test), ncol = ncol(test))

xgb.pred <- predict(xgb.fit, test)
xgb.pred <- t(matrix(xgb.pred, nrow = 5, ncol = length(xgb.pred)/5))
xgb.pred <- data.frame(1:nrow(xgb.pred), xgb.pred)
names(xgb.pred) <- c("ID", "Adoption", "Died", "Euthanasia", "Return_to_owner", "Transfer")
write.csv(xgb.pred, file='animal_solution.csv', quote=FALSE, row.names=FALSE)
