library(dplyr)
library(rpart)
library(caret)
library(xgboost)
library(lubridate)
library(Ckmeans.1d.dp)


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

# Añadimos aspectos referentes a la fecha
todo$Hora <- hour(todo$DateTime)
todo$Horario <- ifelse(todo$Hora >= 6 & todo$Hora < 14, "Mañana", 
                       ifelse(todo$Hora >= 14 & todo$Hora < 22, "Tarde", "Noche"))

todo$Dia <- weekdays(as.Date(todo$DateTime))
todo$Laborable[todo$Dia == "lunes"] <- 1
todo$Laborable[todo$Dia == "martes"] <- 1
todo$Laborable[todo$Dia == "miércoles"] <- 1
todo$Laborable[todo$Dia == "jueves"] <- 1
todo$Laborable[todo$Dia == "viernes"] <- 1
todo$Laborable[todo$Dia == "sábado"] <- 0
todo$Laborable[todo$Dia == "domingo"] <- 0

todo$Mes <- month(todo$DateTime)
todo$Estacion <- ifelse(todo$Mes == 4 | todo$Mes == 5 | todo$Mes == 6, "Primavera", 
                        ifelse(todo$Mes == 7 | todo$Mes == 8 | todo$Mes == 9, "Verano",
                               ifelse(todo$Mes == 10 | todo$Mes == 11 | todo$Mes == 12, "Otoño",
                                      "Invierno")))

# Clasificamos por sexo
todo$SexuponOutcome[todo$SexuponOutcome == ""] <- "Unknown"
todo$Sexo <- ifelse(grepl("Male", todo$SexuponOutcome), "Macho",
                   ifelse(grepl("Unknown", todo$SexuponOutcome), "Desconocido", "Hembra"))
# Añadimos si el animal está castrado, esterilizado o intacto
todo$Estado <- sapply(todo$SexuponOutcome, function(x) strsplit(x, split=" ")[[1]][1])

# Simplificamos las razas y diferenciamos si es mestizo
todo$Raza <- sapply(todo$Breed, function(x) gsub(" Mix", "", strsplit(x, split="/")[[1]][1]))
todo$Mestizo <- ifelse(grepl("Mix", todo$Breed), 1, 0)

# Simplificamos los colores
todo$Color <- sapply(todo$Color, function(x) strsplit(x, split="/| ")[[1]][1])

# Procesamos edad para homogeneizarla
todo$ValorTiempo <- sapply(todo$AgeuponOutcome, function(x) strsplit(x, split=" ")[[1]][1])
todo$ValorTiempo  <- as.numeric(todo$ValorTiempo)
todo$UnidadTiempo <- gsub("s", "", sapply(todo$AgeuponOutcome, function(x) strsplit(x, split=" ")[[1]][2]))
todo$UnidadTiempo <- as.factor(todo$UnidadTiempo)

calculo <- ifelse(todo$UnidadTiempo == "day", 1,
                  ifelse(todo$UnidadTiempo == "week", 7,
                         ifelse(todo$UnidadTiempo == "month", 30,
                                ifelse(todo$UnidadTiempo == "year", 365, NA))))

todo$Edad <- todo$ValorTiempo * calculo

# Predecimos edades que faltantes
modeloEdad <- rpart(Edad ~ Name + AnimalType + Sexo + Estado + Edad + Raza + Mestizo + Color, 
                 data=todo[!is.na(todo$Edad), ], 
                 method="anova")

# Impute predicted age values where missing using "predict"
todo$Edad[is.na(todo$Edad)] <- predict(modeloEdad, todo[is.na(todo$Edad), ])

todo$OutcomeType <- as.numeric(factor(todo$OutcomeType)) - 1
todo$Name <- as.numeric(factor(todo$Name)) - 1
todo$Horario <- as.numeric(factor(todo$Horario)) - 1
todo$Laborable <- as.numeric(factor(todo$Laborable)) - 1
todo$Estacion <- as.numeric(factor(todo$Estacion)) - 1
todo$AnimalType <- as.numeric(factor(todo$AnimalType)) - 1
todo$Sexo <- as.numeric(factor(todo$Sexo)) - 1
todo$Estado <- as.numeric(factor(todo$Estado)) - 1
summary(todo$Edad)
todo$Raza <- as.numeric(factor(todo$Raza)) - 1
todo$Mestizo <- as.numeric(factor(todo$Mestizo)) - 1
todo$Color <- as.numeric(factor(todo$Color)) - 1

# Volvemos a separar los datos en sus respectivos conjuntos de entrenamiento y validación
train <- todo[1:nrow(train), c(4, 2, 12, 14, 16, 6, 17, 18, 23, 19, 20, 10)]
#train <- todo[1:nrow(train), c(4, 2, 6, 18, 23, 19, 10)]

# Crear particiones para entrenamiento y validacion
in.train <- createDataPartition(y=train$OutcomeType, p=0.80, list=FALSE)
in.train <- in.train[1:21383]

# Crear el vector con las clases a predecir
train.y <- train$OutcomeType

# Crear matriz con los valores
train.x <- train
train.x$OutcomeType <- NULL
train.x <- as.matrix(train.x)
train.x <- matrix(data=train.x, nrow=nrow(train.x), ncol=ncol(train.x))

# Parametros para la prediccion

best_param = list()
best_logloss = Inf
best_logloss_index = 0

for (iter in 1:10) {
  parametros <- list("objective"="multi:softprob",
                   "eval_metric"="mlogloss",
                   "num_class"=5,
                   "max_depth"=sample(6:10, 1),
                   "eta"=runif(1, .01, .3),
                   "gamma"=runif(1, 0.0, 0.2),
                   "subsample"=runif(1, .6, .9),
                   "colsample_bytree"=runif(1, .5, .8),
                   "min_child_weight"=sample(1:40, 1),
                   "max_delta_step"=sample(1:10, 1),
                   "nthread"=6)
  
  # Cross-Validation
  ajuste.cv <- xgb.cv(param=parametros, data=train.x, label=train.y, nfold=5, nrounds=2000,
                      maximize=FALSE)
  
  min_logloss = min(ajuste.cv[, test.mlogloss.mean])
  min_logloss_index = which.min(ajuste.cv[, test.mlogloss.mean])
  
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_param = parametros
  }
}

#num_rondas <- round(mean(which(ajuste.cv$test.mlogloss.mean == min(ajuste.cv$test.mlogloss.mean))))  
num_rondas = best_logloss_index

# Ajustar modelo en el conjunto de validacion
validacion <- xgboost(param=best_param, data=train.x[-in.train, ], label=train.y[-in.train], 
                      nrounds=num_rondas, nthread=6)

# Ajustar modelo en el conjunto de entrenamiento completo
ajuste <- xgboost(param=best_param, data=train.x, label=train.y, nrounds=num_rondas, nthread=6)

# Comprobar la importancia de las distintas variables en el ajuste
# nombres <- dimnames(train.x)[[2]]
# importancia <- xgb.importance(nombres, model=ajuste)
# importancia$Feature[importancia$Feature == 0] <- "Name"
# importancia$Feature[importancia$Feature == 1] <- "Horario"
# importancia$Feature[importancia$Feature == 2] <- "Laborable"
# importancia$Feature[importancia$Feature == 3] <- "Estacion"
# importancia$Feature[importancia$Feature == 4] <- "AnimalType"
# importancia$Feature[importancia$Feature == 5] <- "Sexo"
# importancia$Feature[importancia$Feature == 6] <- "Estado"
# importancia$Feature[importancia$Feature == 7] <- "Edad"
# importancia$Feature[importancia$Feature == 8] <- "Raza"
# importancia$Feature[importancia$Feature == 9] <- "Mestizo"
# importancia$Feature[importancia$Feature == 10] <- "Color"
# xgb.plot.importance(importancia[1:11,])

# Prediccion
test <- todo[(nrow(train)+1):nrow(todo), c(2, 12, 14, 16, 6, 17, 18, 23, 19, 20, 10)]
test <- as.matrix(test)
test <- matrix(data=as.numeric(test), nrow=nrow(test), ncol=ncol(test))

prediccion <- predict(ajuste, test)
prediccion <- t(matrix(prediccion, nrow=5, ncol=length(prediccion)/5))
prediccion <- data.frame(1:nrow(prediccion), prediccion)
names(prediccion) <- c("ID", "Adoption", "Died", "Euthanasia", "Return_to_owner", "Transfer")
write.csv(prediccion, file="animal_solution.csv", quote=FALSE, row.names=FALSE)
