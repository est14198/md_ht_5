# Christopher Sandoval 13660
# Maria Fernanda Estrada 14198

# Paquetes y librerias
install.packages("caret")
install.packages("e1071")
install.packages("klaR")
library(caret)
library(e1071)
library(klaR)

# Datos de entrenamiento filtrados
data_training <- read.csv("train.csv", stringsAsFactors = FALSE)
data_training$Class <- as.factor(ifelse(data_training$SalePrice >= 270000, "Cara", ifelse(data_training$SalePrice >= 195000, "Intermedia", "Economica")))
data_training_filtered <- data_training[, c(2,19,20,35,45,48,52,71,82)]

# Modelo Naive Bayes
modelo<-naiveBayes(Class~.,data=data_training_filtered)
modelo
# Aplicacion modelo Naive Bayes a prueba
data_test <- read.csv("test.csv", stringsAsFactors = FALSE)
data_test_filtered <- data_test[, c(2,19,20,35,45,48,52,71)]
data_sample <- read.csv("sample_submission.csv", stringsAsFactors = FALSE)
data_sample$Class <- as.factor(ifelse(data_sample$SalePrice >= 270000, "Cara", ifelse(data_sample$SalePrice >= 195000, "Intermedia", "Economica")))
data_test_filtered$Class <- data_sample$Class
data_test_filtered <- na.omit(data_test_filtered)
# Prediccion Naive Bayes
predBayes<-predict(modelo, newdata = data_test_filtered[1:8])
confusionMatrix(predBayes,data_test_filtered$Class)

# Todos los datos juntos para ingresar al modelo de validacion cruzada
data_filtered <- rbind(data_training_filtered, data_test_filtered)

# Modelo Validacion cruzada
ct<-trainControl(method = "cv",data_training_filtered,number=10, verboseIter=T)
modeloCaret<-train(Class~.,data=data_filtered,method="nb",trControl = ct)
prediccionCaret<-predict(modeloCaret,newdata = data_test_filtered)
confusionMatrix(prediccionCaret,data_test_filtered$Class)
summary(prediccionCaret)
