library(caret)
library(xgboost)
library(rpart)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(randomForest)


# Inicializamos semilla fija para que no aparezca un resulta nuevo cada vez
set.seed(343)

# Cargamos los archivos con los datos
train <- read.csv('/home/germaaan/proyectos/titanic-kaggle/animal/train.csv', stringsAsFactors=FALSE)
test <- read.csv('/home/germaaan/proyectos/titanic-kaggle/animal/test.csv', stringsAsFactors=FALSE)

train %>% head()
test %>% head()

colnames(train)[1] <- 'ID'
test$ID <- as.character(test$ID)

full <- bind_rows(train, test)

# Get the time value:
full$TimeValue <- sapply(full$AgeuponOutcome,  
                         function(x) strsplit(x, split = ' ')[[1]][1])

# Now get the unit of time:
full$UnitofTime <- sapply(full$AgeuponOutcome,  
                          function(x) strsplit(x, split = ' ')[[1]][2])

# Fortunately any "s" marks the plural, so we can just pull them all out
full$UnitofTime <- gsub('s', '', full$UnitofTime)

full$TimeValue  <- as.numeric(full$TimeValue)
full$UnitofTime <- as.factor(full$UnitofTime)

# Make a multiplier vector
multiplier <- ifelse(full$UnitofTime == 'day', 1,
                     ifelse(full$UnitofTime == 'week', 7,
                            ifelse(full$UnitofTime == 'month', 30, # Close enough
                                   ifelse(full$UnitofTime == 'year', 365, NA))))

# Apply our multiplier
full$AgeinDays <- full$TimeValue * multiplier

# Replace blank names with "Nameless"
full$Name <- ifelse(nchar(full$Name)==0, 'Nameless', full$Name)

# Make a name v. no name variable
full$HasName[full$Name == 'Nameless'] <- 0
full$HasName[full$Name != 'Nameless'] <- 1

# Replace blank sex with most common
full$SexuponOutcome <- ifelse(nchar(full$SexuponOutcome)==0, 
                              'Spayed Female', full$SexuponOutcome)

# Extract time variables from date (uses the "lubridate" package)
full$Hour    <- hour(full$DateTime)
full$Weekday <- wday(full$DateTime)
full$Month   <- month(full$DateTime)
full$Year    <- year(full$DateTime)

# Time of day may also be useful
full$TimeofDay <- ifelse(full$Hour > 5 & full$Hour < 11, 'morning',
                         ifelse(full$Hour > 10 & full$Hour < 16, 'midday',
                                ifelse(full$Hour > 15 & full$Hour < 20, 'lateday', 'night')))

# Put factor levels into the order we want
full$TimeofDay <- factor(full$TimeofDay, 
                         levels = c('morning', 'midday',
                                    'lateday', 'night'))

summary(full$AgeinDays)

# Use "grepl" to look for "Mix"
full$IsMix <- ifelse(grepl('Mix', full$Breed), 1, 0)

# Split on "/" and remove " Mix" to simplify Breed
full$SimpleBreed <- sapply(full$Breed, 
                           function(x) gsub(' Mix', '', 
                                            strsplit(x, split = '/')[[1]][1]))

# Use strsplit to grab the first color
full$SimpleColor <- sapply(full$Color, 
                           function(x) strsplit(x, split = '/| ')[[1]][1])

levels(factor(full$SimpleColor))

# Use "grepl" to look for "Intact"
full$Intact <- ifelse(grepl('Intact', full$SexuponOutcome), 1,
                      ifelse(grepl('Unknown', full$SexuponOutcome), 'Unknown', 0))

# Use "grepl" to look for sex
full$Sex <- ifelse(grepl('Male', full$SexuponOutcome), 'Male',
                   ifelse(grepl('Unknown', full$Sex), 'Unknown', 'Female'))

# Use rpart to predict the missing age values
age_fit <- rpart(AgeinDays ~ AnimalType + Sex + Intact + SimpleBreed + HasName, 
                 data = full[!is.na(full$AgeinDays), ], 
                 method = 'anova')

# Impute predicted age values where missing using "predict"
full$AgeinDays[is.na(full$AgeinDays)] <- predict(age_fit, full[is.na(full$AgeinDays), ])

# All gone? Yes.
sum(is.na(full$AgeinDays))

# Use the age variable to make a puppy/kitten variable
full$Lifestage[full$AgeinDays < 365] <- 'baby'
full$Lifestage[full$AgeinDays >= 365] <- 'adult'

full$Lifestage <- factor(full$Lifestage)

factorVars <- c('Name','OutcomeType','OutcomeSubtype','AnimalType',
                'SexuponOutcome','AgeuponOutcome','SimpleBreed','SimpleColor',
                'HasName','IsMix','Intact','Sex','TimeofDay','Lifestage')

full[factorVars] <- lapply(full[factorVars], function(x) as.factor(x))

class(train$OutcomeType)

# clustering
#discretizar categoricos para prediccion
#pasar todo a años
#predecir las edades que faltan

#edad promedio en vez de predecir

full$OutcomeType <- as.numeric(full$OutcomeType) - 1
full$AnimalType <- as.numeric(full$AnimalType) - 1
full$Intact <- as.numeric(full$Intact) - 1
full$HasName <- as.numeric(full$HasName) - 1
full$TimeofDay <- as.numeric(full$TimeofDay) - 1
full$SimpleColor <- as.numeric(full$SimpleColor) - 1
full$IsMix <- as.numeric(full$IsMix) - 1
full$Sex <- as.numeric(full$Sex) - 1

# Volvemos a separar los datos en sus respectivos conjuntos de entrenamiento y validación
train <- full[1:nrow(train), c(4, 6, 13, 23, 14, 15, 16, 19, 22, 20, 24, 17)]

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
test <- full[(nrow(train)+1):nrow(full), c(6, 13, 23, 14, 15, 16, 19, 22, 20, 24, 17)]
test <- as.matrix(test)
test <- matrix(data = as.numeric(test), nrow = nrow(test), ncol = ncol(test))

xgb.pred <- predict(xgb.fit, test)
xgb.pred <- t(matrix(xgb.pred, nrow = 5, ncol = length(xgb.pred)/5))
xgb.pred <- data.frame(1:nrow(xgb.pred), xgb.pred)
names(xgb.pred) <- c("ID", "Adoption", "Died", "Euthanasia", "Return_to_owner", "Transfer")
write.csv(xgb.pred, file='animal_solution.csv', quote=FALSE, row.names=FALSE)
