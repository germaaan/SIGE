library(dplyr)
library(rpart)
library(caret)
library(xgboost)


# Inicializamos semilla fija para que no aparezca un resulta nuevo cada vez
set.seed(343)

# Cargamos los archivos con los datos
train <- read.csv("/home/germaaan/proyectos/titanic-kaggle/animal/train.csv", stringsAsFactors=FALSE)
test <- read.csv("/home/germaaan/proyectos/titanic-kaggle/animal/test.csv", stringsAsFactors=FALSE)

# Ambos ID iguales
colnames(train)[1] <- "ID"
test$ID <- as.character(test$ID)

# Unimos ambos conjuntos
todo <- bind_rows(train, test)

# Clasificamos perros por tener o no tener nombre
todo$Name <- ifelse(todo$Name=="", 0, 1)

# Clasificamos por sexo
todo$SexuponOutcome[todo$SexuponOutcome == ''] <- "Unknown"
todo$Sexo <- ifelse(grepl('Male', todo$SexuponOutcome), 'Macho',
                   ifelse(grepl('Unknown', todo$SexuponOutcome), 'Desconocido', 'Hembra'))
# Añadimos si el animal está castrado, esterilizado o intacto
#todo$Estado <- sapply(todo$SexuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])

# Simplificamos las razas y diferenciamos si es mestizo
todo$Raza <- sapply(todo$Breed, function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))
todo$Mestizo <- ifelse(grepl('Mix', todo$Breed), 1, 0)

# Simplificamos los colores
todo$Color <- sapply(todo$Color, function(x) strsplit(x, split = '/| ')[[1]][1])

# Procesamos edad para homogeneizarla
todo$ValorTiempo <- sapply(todo$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])
todo$ValorTiempo  <- as.numeric(todo$ValorTiempo)
todo$UnidadTiempo <- gsub('s', '', sapply(todo$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2]))
todo$UnidadTiempo <- as.factor(todo$UnidadTiempo)

calculo <- ifelse(todo$UnidadTiempo == 'day', 1,
                  ifelse(todo$UnidadTiempo == 'week', 7,
                         ifelse(todo$UnidadTiempo == 'month', 30,
                                ifelse(todo$UnidadTiempo == 'year', 365, NA))))

todo$Edad <- todo$ValorTiempo * calculo

# Predecimos edades que faltantes
modeloEdad <- rpart(Edad ~ Name + AnimalType + Sexo + Edad + Raza + Mestizo + Color, 
                 data = todo[!is.na(todo$Edad), ], 
                 method = 'anova')

# Impute predicted age values where missing using "predict"
todo$Edad[is.na(todo$Edad)] <- predict(modeloEdad, todo[is.na(todo$Edad), ])

todo$OutcomeType <- as.numeric(factor(todo$OutcomeType)) - 1
todo$Name <- as.numeric(factor(todo$Name)) - 1
todo$AnimalType <- as.numeric(factor(todo$AnimalType)) - 1
todo$Sexo <- as.numeric(factor(todo$Sexo)) - 1
summary(todo$Edad)
todo$Raza <- as.numeric(factor(todo$Raza)) - 1
todo$Mestizo <- as.numeric(factor(todo$Mestizo)) - 1
todo$Color <- as.numeric(factor(todo$Color)) - 1

# clustering

# Volvemos a separar los datos en sus respectivos conjuntos de entrenamiento y validación
train <- todo[1:nrow(train), c(4, 2, 6, 11, 16, 12, 13, 10)]

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
test <- todo[(nrow(train)+1):nrow(todo), c(2, 6, 11, 16, 12, 13, 10)]
test <- as.matrix(test)
test <- matrix(data = as.numeric(test), nrow = nrow(test), ncol = ncol(test))

xgb.pred <- predict(xgb.fit, test)
xgb.pred <- t(matrix(xgb.pred, nrow = 5, ncol = length(xgb.pred)/5))
xgb.pred <- data.frame(1:nrow(xgb.pred), xgb.pred)
names(xgb.pred) <- c("ID", "Adoption", "Died", "Euthanasia", "Return_to_owner", "Transfer")
write.csv(xgb.pred, file='animal_solution.csv', quote=FALSE, row.names=FALSE)
