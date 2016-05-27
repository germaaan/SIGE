# install.packages("randomForest")
# install.packages("unbalanced")
# install.packages("DMwR")

#library(plyr)
library(caret)
library(randomForest)
library(rpart)
#library(unbalanced)
#library(DMwR)

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

total$Breed <- as.character(total$Breed)
total$Breed[grepl("^Abyssinian*", total$Breed)] <- "Abyssinian"
total$Breed[grepl("^Affenpinscher*", total$Breed)] <- "Affenpinscher"
total$Breed[grepl("^Afghan Hound*", total$Breed)] <- "Afghan Hound"
total$Breed[grepl("^Airedale Terrier*", total$Breed)] <- "Airedale Terrier"
total$Breed[grepl("^Akita*", total$Breed)] <- "Akita"
total$Breed[grepl("^Alaskan Husky*", total$Breed)] <- "Alaskan Husky"
total$Breed[grepl("^Alaskan Husky*", total$Breed)] <- "Alaskan Husky"
total$Breed[grepl("^Alaskan Malamute*", total$Breed)] <- "Alaskan Malamute"
total$Breed[grepl("^American Bulldog*", total$Breed)] <- "American Bulldog"
total$Breed[grepl("^American Eskimo*", total$Breed)] <- "American Eskimo"
total$Breed[grepl("^American Eskimo*", total$Breed)] <- "American Eskimo"
total$Breed[grepl("^American Foxhound*", total$Breed)] <- "American Foxhound"
total$Breed[grepl("^American Foxhound*", total$Breed)] <- "American Foxhound"
total$Breed[grepl("^American Pit Bull Terrier*", total$Breed)] <- "American Pit Bull Terrier"
total$Breed[grepl("^American Shorthair*", total$Breed)] <- "American Shorthair"
total$Breed[grepl("^American Staffordshire Terrier*", total$Breed)] <- "American Staffordshire Terrier"
total$Breed[grepl("^American Wirehair*", total$Breed)] <- "American Wirehair"
total$Breed[grepl("^Anatol Shepherd*", total$Breed)] <- "Anatol Shepherd"
total$Breed[grepl("^Angora*", total$Breed)] <- "Angora"
total$Breed[grepl("^Australian Cattle Dog*", total$Breed)] <- "Australian Cattle Dog"
total$Breed[grepl("^Australian Kelpie*", total$Breed)] <- "Australian Kelpie"
total$Breed[grepl("^Australian Shepherd*", total$Breed)] <- "Australian Shepherd"
total$Breed[grepl("^Australian Terrier*", total$Breed)] <- "Australian Terrier"
total$Breed[grepl("^Balinese*", total$Breed)] <- "Balinese"
total$Breed[grepl("^Basenji*", total$Breed)] <- "Basenji"
total$Breed[grepl("^Basset Hound*", total$Breed)] <- "Basset Hound"
total$Breed[grepl("^Beagle*", total$Breed)] <- "Beagle"
total$Breed[grepl("^Bearded Collie*", total$Breed)] <- "Bearded Collie"
total$Breed[grepl("^Beauceron*", total$Breed)] <- "Beauceron"
total$Breed[grepl("^Beauceron*", total$Breed)] <- "Beauceron"
total$Breed[grepl("^Bedlington Terr*", total$Breed)] <- "Bedlington Terr"
total$Breed[grepl("^Belgian Malinois*", total$Breed)] <- "Belgian Malinois"
total$Breed[grepl("^Belgian Sheepdog*", total$Breed)] <- "Belgian Sheepdog"
total$Breed[grepl("^Belgian Sheepdog*", total$Breed)] <- "Belgian Sheepdog"
total$Breed[grepl("^Belgian Tervuren*", total$Breed)] <- "Belgian Tervuren"
total$Breed[grepl("^Belgian Tervuren*", total$Breed)] <- "Belgian Tervuren"
total$Breed[grepl("^Bengal*", total$Breed)] <- "Bengal"
total$Breed[grepl("^Bengal*", total$Breed)] <- "Bengal"
total$Breed[grepl("^Bernese Mountain Dog*", total$Breed)] <- "Bernese Mountain Dog"
total$Breed[grepl("^Bernese Mountain Dog*", total$Breed)] <- "Bernese Mountain Dog"
total$Breed[grepl("^Bichon Frise*", total$Breed)] <- "Bichon Frise"
total$Breed[grepl("^Black*", total$Breed)] <- "Black"
total$Breed[grepl("^Bloodhound*", total$Breed)] <- "Bloodhound"
total$Breed[grepl("^Bloodhound*", total$Breed)] <- "Bloodhound"
total$Breed[grepl("^Blue Lacy*", total$Breed)] <- "Blue Lacy"
total$Breed[grepl("^Bluetick Hound*", total$Breed)] <- "Bluetick Hound"
total$Breed[grepl("^Boerboel*", total$Breed)] <- "Boerboel"
total$Breed[grepl("^Boerboel*", total$Breed)] <- "Boerboel"
total$Breed[grepl("^Bombay*", total$Breed)] <- "Bombay"
total$Breed[grepl("^Border Collie*", total$Breed)] <- "Border Collie"
total$Breed[grepl("^Border Terrier*", total$Breed)] <- "Border Terrier"
total$Breed[grepl("^Boston Terrier*", total$Breed)] <- "Boston Terrier"
total$Breed[grepl("^Boxer*", total$Breed)] <- "Boxer"
total$Breed[grepl("^Boykin Span*", total$Breed)] <- "Boykin Span"
total$Breed[grepl("^British Shorthair*", total$Breed)] <- "British Shorthair"
total$Breed[grepl("^British Shorthair*", total$Breed)] <- "British Shorthair"
total$Breed[grepl("^Brittany*", total$Breed)] <- "Brittany"
total$Breed[grepl("^Bruss Griffon*", total$Breed)] <- "Bruss Griffon"
total$Breed[grepl("^Bulldog*", total$Breed)] <- "Bulldog"
total$Breed[grepl("^Bullmastiff*", total$Breed)] <- "Bullmastiff"
total$Breed[grepl("^Bullmastiff*", total$Breed)] <- "Bullmastiff"
total$Breed[grepl("^Bull Terrier*", total$Breed)] <- "Bull Terrier"
total$Breed[grepl("^Burmese*", total$Breed)] <- "Burmese"
total$Breed[grepl("^Cairn Terrier*", total$Breed)] <- "Cairn Terrier"
total$Breed[grepl("^Canaan Dog*", total$Breed)] <- "Canaan Dog"
total$Breed[grepl("^Canaan Dog*", total$Breed)] <- "Canaan Dog"
total$Breed[grepl("^Cane Corso*", total$Breed)] <- "Cane Corso"
total$Breed[grepl("^Cane Corso*", total$Breed)] <- "Cane Corso"
total$Breed[grepl("^Cardigan Welsh Corgi*", total$Breed)] <- "Cardigan Welsh Corgi"
total$Breed[grepl("^Carolina Dog*", total$Breed)] <- "Carolina Dog"
total$Breed[grepl("^Carolina Dog*", total$Breed)] <- "Carolina Dog"
total$Breed[grepl("^Catahoula*", total$Breed)] <- "Catahoula"
total$Breed[grepl("^Cavalier Span*", total$Breed)] <- "Cavalier Span"
total$Breed[grepl("^Chartreux*", total$Breed)] <- "Chartreux"
total$Breed[grepl("^Chesa Bay Retr*", total$Breed)] <- "Chesa Bay Retr"
total$Breed[grepl("^Chihuahua Longhair*", total$Breed)] <- "Chihuahua Longhair"
total$Breed[grepl("^Chihuahua Shorthair*", total$Breed)] <- "Chihuahua Shorthair"
total$Breed[grepl("^Chinese Crested*", total$Breed)] <- "Chinese Crested"
total$Breed[grepl("^Chinese Crested*", total$Breed)] <- "Chinese Crested"
total$Breed[grepl("^Chinese Sharpei*", total$Breed)] <- "Chinese Sharpei"
total$Breed[grepl("^Chow Chow*", total$Breed)] <- "Chow Chow"
total$Breed[grepl("^Cocker Spaniel*", total$Breed)] <- "Cocker Spaniel"
total$Breed[grepl("^Collie Rough*", total$Breed)] <- "Collie Rough"
total$Breed[grepl("^Collie Smooth*", total$Breed)] <- "Collie Smooth"
total$Breed[grepl("^Cornish Rex*", total$Breed)] <- "Cornish Rex"
total$Breed[grepl("^Coton De Tulear*", total$Breed)] <- "Coton De Tulear"
total$Breed[grepl("^Cymric*", total$Breed)] <- "Cymric"
total$Breed[grepl("^Dachshund*", total$Breed)] <- "Dachshund"
total$Breed[grepl("^Dalmatian*", total$Breed)] <- "Dalmatian"
total$Breed[grepl("^Dandie Dinmont*", total$Breed)] <- "Dandie Dinmont"
total$Breed[grepl("^Devon Rex*", total$Breed)] <- "Devon Rex"
total$Breed[grepl("^Devon Rex*", total$Breed)] <- "Devon Rex"
total$Breed[grepl("^Doberman Pinsch*", total$Breed)] <- "Doberman Pinsch"
total$Breed[grepl("^Dogo Argentino*", total$Breed)] <- "Dogo Argentino"
total$Breed[grepl("^Dogo Argentino*", total$Breed)] <- "Dogo Argentino"
total$Breed[grepl("^Dogue De Bordeaux*", total$Breed)] <- "Dogue De Bordeaux"
total$Breed[grepl("^Domestic Longhair*", total$Breed)] <- "Domestic Longhair"
total$Breed[grepl("^Domestic Longhair*", total$Breed)] <- "Domestic Longhair"
total$Breed[grepl("^Domestic Medium Hair*", total$Breed)] <- "Domestic Medium Hair"
total$Breed[grepl("^Domestic Medium Hair*", total$Breed)] <- "Domestic Medium Hair"
total$Breed[grepl("^Domestic Shorthair*", total$Breed)] <- "Domestic Shorthair"
total$Breed[grepl("^Domestic Shorthair*", total$Breed)] <- "Domestic Shorthair"
total$Breed[grepl("^Dutch Shepherd*", total$Breed)] <- "Dutch Shepherd"
total$Breed[grepl("^English Bulldog*", total$Breed)] <- "English Bulldog"
total$Breed[grepl("^English Cocker Spaniel*", total$Breed)] <- "English Cocker Spaniel"
total$Breed[grepl("^English Coonhound*", total$Breed)] <- "English Coonhound"
total$Breed[grepl("^English Coonhound*", total$Breed)] <- "English Coonhound"
total$Breed[grepl("^English Foxhound*", total$Breed)] <- "English Foxhound"
total$Breed[grepl("^English Foxhound*", total$Breed)] <- "English Foxhound"
total$Breed[grepl("^English Pointer*", total$Breed)] <- "English Pointer"
total$Breed[grepl("^English Setter*", total$Breed)] <- "English Setter"
total$Breed[grepl("^English Shepherd*", total$Breed)] <- "English Shepherd"
total$Breed[grepl("^English Shepherd*", total$Breed)] <- "English Shepherd"
total$Breed[grepl("^English Springer Spaniel*", total$Breed)] <- "English Springer Spaniel"
total$Breed[grepl("^English Springer Spaniel*", total$Breed)] <- "English Springer Spaniel"
total$Breed[grepl("^Eng Toy Spaniel*", total$Breed)] <- "Eng Toy Spaniel"
total$Breed[grepl("^Entlebucher*", total$Breed)] <- "Entlebucher"
total$Breed[grepl("^Exotic Shorthair*", total$Breed)] <- "Exotic Shorthair"
total$Breed[grepl("^Feist*", total$Breed)] <- "Feist"
total$Breed[grepl("^Field Spaniel*", total$Breed)] <- "Field Spaniel"
total$Breed[grepl("^Finnish Spitz*", total$Breed)] <- "Finnish Spitz"
total$Breed[grepl("^Flat Coat Retriever*", total$Breed)] <- "Flat Coat Retriever"
total$Breed[grepl("^French Bulldog*", total$Breed)] <- "French Bulldog"
total$Breed[grepl("^German Pinscher*", total$Breed)] <- "German Pinscher"
total$Breed[grepl("^German Shepherd*", total$Breed)] <- "German Shepherd"
total$Breed[grepl("^German Shorthair Pointer*", total$Breed)] <- "German Shorthair Pointer"
total$Breed[grepl("^German Wirehaired Pointer*", total$Breed)] <- "German Wirehaired Pointer"
total$Breed[grepl("^Glen Of Imaal*", total$Breed)] <- "Glen Of Imaal"
total$Breed[grepl("^Golden Retriever*", total$Breed)] <- "Golden Retriever"
total$Breed[grepl("^Gordon Setter*", total$Breed)] <- "Gordon Setter"
total$Breed[grepl("^Great Dane*", total$Breed)] <- "Great Dane"
total$Breed[grepl("^Greater Swiss Mountain Dog*", total$Breed)] <- "Greater Swiss Mountain Dog"
total$Breed[grepl("^Great Pyrenees*", total$Breed)] <- "Great Pyrenees"
total$Breed[grepl("^Greyhound*", total$Breed)] <- "Greyhound"
total$Breed[grepl("^Harrier*", total$Breed)] <- "Harrier"
total$Breed[grepl("^Havana Brown*", total$Breed)] <- "Havana Brown"
total$Breed[grepl("^Havanese*", total$Breed)] <- "Havanese"
total$Breed[grepl("^Himalayan*", total$Breed)] <- "Himalayan"
total$Breed[grepl("^Hovawart*", total$Breed)] <- "Hovawart"
total$Breed[grepl("^Ibizan Hound*", total$Breed)] <- "Ibizan Hound"
total$Breed[grepl("^Irish Setter*", total$Breed)] <- "Irish Setter"
total$Breed[grepl("^Irish Terrier*", total$Breed)] <- "Irish Terrier"
total$Breed[grepl("^Irish Wolfhound*", total$Breed)] <- "Irish Wolfhound"
total$Breed[grepl("^Italian Greyhound*", total$Breed)] <- "Italian Greyhound"
total$Breed[grepl("^Jack Russell Terrier*", total$Breed)] <- "Jack Russell Terrier"
total$Breed[grepl("^Japanese Bobtail*", total$Breed)] <- "Japanese Bobtail"
total$Breed[grepl("^Japanese Chin*", total$Breed)] <- "Japanese Chin"
total$Breed[grepl("^Javanese*", total$Breed)] <- "Javanese"
total$Breed[grepl("^Jindo*", total$Breed)] <- "Jindo"
total$Breed[grepl("^Keeshond*", total$Breed)] <- "Keeshond"
total$Breed[grepl("^Kuvasz*", total$Breed)] <- "Kuvasz"
total$Breed[grepl("^Labrador Retriever*", total$Breed)] <- "Labrador Retriever"
total$Breed[grepl("^Landseer*", total$Breed)] <- "Landseer"
total$Breed[grepl("^Leonberger*", total$Breed)] <- "Leonberger"
total$Breed[grepl("^Lhasa Apso*", total$Breed)] <- "Lhasa Apso"
total$Breed[grepl("^Lowchen*", total$Breed)] <- "Lowchen"
total$Breed[grepl("^Maine Coon*", total$Breed)] <- "Maine Coon"
total$Breed[grepl("^Maltese*", total$Breed)] <- "Maltese"
total$Breed[grepl("^Manx*", total$Breed)] <- "Manx"
total$Breed[grepl("^Mastiff*", total$Breed)] <- "Mastiff"
total$Breed[grepl("^Mexican Hairless*", total$Breed)] <- "Mexican Hairless"
total$Breed[grepl("^Miniature Pinscher*", total$Breed)] <- "Miniature Pinscher"
total$Breed[grepl("^Miniature Poodle*", total$Breed)] <- "Miniature Poodle"
total$Breed[grepl("^Miniature Schnauzer*", total$Breed)] <- "Miniature Schnauzer"
total$Breed[grepl("^Munchkin Longhair*", total$Breed)] <- "Munchkin Longhair"
total$Breed[grepl("^Munchkin Shorthair*", total$Breed)] <- "Munchkin Shorthair"
total$Breed[grepl("^Neapolitan Mastiff*", total$Breed)] <- "Neapolitan Mastiff"
total$Breed[grepl("^Newfoundland*", total$Breed)] <- "Newfoundland"
total$Breed[grepl("^Norfolk Terrier*", total$Breed)] <- "Norfolk Terrier"
total$Breed[grepl("^Norwegian Elkhound*", total$Breed)] <- "Norwegian Elkhound"
total$Breed[grepl("^Norwegian Forest Cat*", total$Breed)] <- "Norwegian Forest Cat"
total$Breed[grepl("^Norwich Terrier*", total$Breed)] <- "Norwich Terrier"
total$Breed[grepl("^Nova Scotia Duck Tolling Retriever*", total$Breed)] <- "Nova Scotia Duck Tolling Retriever"
total$Breed[grepl("^Ocicat*", total$Breed)] <- "Ocicat"
total$Breed[grepl("^Old English Bulldog*", total$Breed)] <- "Old English Bulldog"
total$Breed[grepl("^Old English Sheepdog*", total$Breed)] <- "Old English Sheepdog"
total$Breed[grepl("^Oriental Sh*", total$Breed)] <- "Oriental Sh"
total$Breed[grepl("^Otterhound*", total$Breed)] <- "Otterhound"
total$Breed[grepl("^Papillon*", total$Breed)] <- "Papillon"
total$Breed[grepl("^Parson Russell Terrier*", total$Breed)] <- "Parson Russell Terrier"
total$Breed[grepl("^Patterdale Terr*", total$Breed)] <- "Patterdale Terr"
total$Breed[grepl("^Pbgv*", total$Breed)] <- "Pbgv"
total$Breed[grepl("^Pekingese*", total$Breed)] <- "Pekingese"
total$Breed[grepl("^Pembroke Welsh Corgi*", total$Breed)] <- "Pembroke Welsh Corgi"
total$Breed[grepl("^Persian*", total$Breed)] <- "Persian"
total$Breed[grepl("^Pharaoh Hound*", total$Breed)] <- "Pharaoh Hound"
total$Breed[grepl("^Picardy Sheepdog*", total$Breed)] <- "Picardy Sheepdog"
total$Breed[grepl("^Pit Bull*", total$Breed)] <- "Pit Bull"
total$Breed[grepl("^Pixiebob Shorthair*", total$Breed)] <- "Pixiebob Shorthair"
total$Breed[grepl("^Plott Hound*", total$Breed)] <- "Plott Hound"
total$Breed[grepl("^Podengo Pequeno*", total$Breed)] <- "Podengo Pequeno"
total$Breed[grepl("^Pointer*", total$Breed)] <- "Pointer"
total$Breed[grepl("^Pomeranian*", total$Breed)] <- "Pomeranian"
total$Breed[grepl("^Port Water Dog*", total$Breed)] <- "Port Water Dog"
total$Breed[grepl("^Presa Canario*", total$Breed)] <- "Presa Canario"
total$Breed[grepl("^Pug*", total$Breed)] <- "Pug"
total$Breed[grepl("^Queensland Heeler*", total$Breed)] <- "Queensland Heeler"
total$Breed[grepl("^Ragdoll*", total$Breed)] <- "Ragdoll"
total$Breed[grepl("^Rat Terrier*", total$Breed)] <- "Rat Terrier"
total$Breed[grepl("^Redbone Hound*", total$Breed)] <- "Redbone Hound"
total$Breed[grepl("^Rhod Ridgeback*", total$Breed)] <- "Rhod Ridgeback"
total$Breed[grepl("^Rottweiler*", total$Breed)] <- "Rottweiler"
total$Breed[grepl("^Russian Blue*", total$Breed)] <- "Russian Blue"
total$Breed[grepl("^Saluki*", total$Breed)] <- "Saluki"
total$Breed[grepl("^Samoyed*", total$Breed)] <- "Samoyed"
total$Breed[grepl("^Schipperke*", total$Breed)] <- "Schipperke"
total$Breed[grepl("^Schnauzer Giant*", total$Breed)] <- "Schnauzer Giant"
total$Breed[grepl("^Scottish Fold*", total$Breed)] <- "Scottish Fold"
total$Breed[grepl("^Scottish Terrier*", total$Breed)] <- "Scottish Terrier"
total$Breed[grepl("^Sealyham Terr*", total$Breed)] <- "Sealyham Terr"
total$Breed[grepl("^Shetland Sheepdog*", total$Breed)] <- "Shetland Sheepdog"
total$Breed[grepl("^Shiba Inu*", total$Breed)] <- "Shiba Inu"
total$Breed[grepl("^Shih Tzu*", total$Breed)] <- "Shih Tzu"
total$Breed[grepl("^Siamese*", total$Breed)] <- "Siamese"
total$Breed[grepl("^Siberian Husky*", total$Breed)] <- "Siberian Husky"
total$Breed[grepl("^Silky Terrier*", total$Breed)] <- "Silky Terrier"
total$Breed[grepl("^Skye Terrier*", total$Breed)] <- "Skye Terrier"
total$Breed[grepl("^Smooth Fox Terrier*", total$Breed)] <- "Smooth Fox Terrier"
total$Breed[grepl("^Snowshoe*", total$Breed)] <- "Snowshoe"
total$Breed[grepl("^Soft Coated Wheaten Terrier*", total$Breed)] <- "Soft Coated Wheaten Terrier"
total$Breed[grepl("^Spanish Mastiff*", total$Breed)] <- "Spanish Mastiff"
total$Breed[grepl("^Sphynx*", total$Breed)] <- "Sphynx"
total$Breed[grepl("^Spinone Italiano*", total$Breed)] <- "Spinone Italiano"
total$Breed[grepl("^Staffordshire*", total$Breed)] <- "Staffordshire"
total$Breed[grepl("^Standard Poodle*", total$Breed)] <- "Standard Poodle"
total$Breed[grepl("^Standard Schnauzer*", total$Breed)] <- "Standard Schnauzer"
total$Breed[grepl("^St. Bernard Rough Coat*", total$Breed)] <- "St. Bernard Rough Coat"
total$Breed[grepl("^St. Bernard Smooth Coat*", total$Breed)] <- "St. Bernard Smooth Coat"
total$Breed[grepl("^Swedish Vallhund*", total$Breed)] <- "Swedish Vallhund"
total$Breed[grepl("^Swiss Hound*", total$Breed)] <- "Swiss Hound"
total$Breed[grepl("^Tibetan Spaniel*", total$Breed)] <- "Tibetan Spaniel"
total$Breed[grepl("^Tibetan Terrier*", total$Breed)] <- "Tibetan Terrier"
total$Breed[grepl("^Tonkinese*", total$Breed)] <- "Tonkinese"
total$Breed[grepl("^Toy Fox Terrier*", total$Breed)] <- "Toy Fox Terrier"
total$Breed[grepl("^Toy Poodle*", total$Breed)] <- "Toy Poodle"
total$Breed[grepl("^Treeing Cur*", total$Breed)] <- "Treeing Cur"
total$Breed[grepl("^Treeing Tennesse Brindle*", total$Breed)] <- "Treeing Tennesse Brindle"
total$Breed[grepl("^Treeing Walker Coonhound*", total$Breed)] <- "Treeing Walker Coonhound"
total$Breed[grepl("^Turkish Angora*", total$Breed)] <- "Turkish Angora"
total$Breed[grepl("^Turkish Van*", total$Breed)] <- "Turkish Van"
total$Breed[grepl("^Unknown*", total$Breed)] <- "Unknown"
total$Breed[grepl("^Vizsla*", total$Breed)] <- "Vizsla"
total$Breed[grepl("^Weimaraner*", total$Breed)] <- "Weimaraner"
total$Breed[grepl("^Welsh Springer Spaniel*", total$Breed)] <- "Welsh Springer Spaniel"
total$Breed[grepl("^Welsh Terrier*", total$Breed)] <- "Welsh Terrier"
total$Breed[grepl("^West Highland*", total$Breed)] <- "West Highland"
total$Breed[grepl("^Whippet*", total$Breed)] <- "Whippet"
total$Breed[grepl("^Wirehaired Pointing Griffon*", total$Breed)] <- "Wirehaired Pointing Griffon"
total$Breed[grepl("^Wire Hair Fox Terrier*", total$Breed)] <- "Wire Hair Fox Terrier"
total$Breed[grepl("^Yorkshire Terrier*", total$Breed)] <- "Yorkshire Terrier"
total$Breed <- factor(total$Breed)

tablaRazas = data.frame(table(total$Breed))
tablaRazas = tablaRazas[order(-tablaRazas$Freq),]
topRazas <- factor(tablaRazas$Var1[1:30])

total$Raza <- as.character(total$Breed)
for (i in 1:length(total$Breed)){
  if (!is.element(total$Breed[i], topRazas))
    total$Raza[i] = "OTRO"
}
total$Raza <- factor(total$Raza)

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
summary(total$Raza)
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

clases <- OutcomeType ~ AnimalType + Edad + SexuponOutcome + Raza + Color

modelo <- train(clases, data=training, method="parRF", trControl=trainControl(method="cv", number=5))
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
