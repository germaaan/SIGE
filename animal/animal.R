# install.packages("randomForest")

library(randomForest)

# Inicializamos semilla fija para que no aparezca un resulta nuevo cada vez
set.seed(343)

# Cargamos los archivos con los datos
train <- read.csv("/home/germaaan/proyectos/titanic-kaggle/animal/train.csv", header=TRUE, sep=",")
test <- read.csv("/home/germaaan/proyectos/titanic-kaggle/animal/test.csv", header=TRUE, sep=",")

test$ID <- as.character(test$ID)
names(train)[1] <- "ID"

test$OutcomeType <- ""
test$OutcomeSubtype <- ""

total <- rbind(train, test)

summary(total$AnimalType)
#completar y convertir a numero
summary(total$AgeuponOutcome)
total$AgeuponOutcome <- as.character(total$AgeuponOutcome)
total$ValorTiempo = sapply(total$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])
total$UnidadTiempo = sapply(total$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][2])
total$UnidadTiempo = gsub('s', '', total$UnidadTiempo)
total$UnidadTiempo = as.factor(total$UnidadTiempo)
total$ValorTiempo = as.numeric(total$ValorTiempo)
multiplicador = ifelse(total$UnidadTiempo == 'day', 1, ifelse(total$UnidadTiempo == 'week', 7, ifelse(total$UnidadTiempo == 'month', 30, ifelse(total$UnidadTiempo == 'year', 365, NA))))
total$Edad = multiplicador * total$ValorTiempo
total$Edad[total$Edad == 0] <- NA
summary(total$Edad)
#probar a predecir sexo
total$SexuponOutcome[total$SexuponOutcome == ""] <- "Unknown"
total$SexuponOutcome <- factor(total$SexuponOutcome)
summary(total$SexuponOutcome)
summary(total$Breed)
summary(total$Color)

# clustering
#discretizar categoricos para prediccion
#pasar todo a años
#predecir las edades que faltan