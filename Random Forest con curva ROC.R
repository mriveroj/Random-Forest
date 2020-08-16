library(reshape)
library(randomForest)
library(caret)
library(ROCR)
library(pROC)
#lectura de datos
datos <- read.csv(file = "NBADATOS.csv", sep=";")
#Observamos la estructura de los datos
str(datos)
#Convertimos la variable dependiente a factor (Y)
datos$Y<- factor(datos$Y)
#Convertimos una de las variables independientes a factor
datos$Tipo.de.partido<- factor(datos$Tipo.de.partido)
#Partición de los datos
#Hacemos la semilla para que la división de los datos sea la misma
set.seed(2020)
#Datos de entrenamiento (80% de los datos)
train <- createDataPartition(datos$Y, p=0.8, list = F)
#Creación del modelo
attach(datos)
fit <- randomForest(Y~PosicionEquipo+PosicionRival+PuntosAbajo+Tipo.de.partido, data = train,
                    ntree=600,importance=TRUE,maxnodes=10,mtry=25)

plot(fit,main = "Random Forest's error rates", 
     legend=c(x=500, y=0.20, legend=c("Resilient","No resilient")),
     fill=c("red","green"),cex=0.1,text.front=4, bg='grey')

#Importancia de las variables
#Creamos un objeto con las "importancias" de las variables
importancia=data.frame(importance(fit))
#install.packages("reshape")
importancia<-sort_df(importancia,vars='MeanDecreaseGini')
importancia
varImpPlot(fit)

#Predicción
pred <- predict(fit, datos[-train,])
#Matriz de confusión
table(datos[-train, "Y"],pred, dnn=c("Real","Pred"))
#Acurracy
#Especificidad
#Sensibilidad
probs <- predict(fit,datos[-train,], type = "prob")
head(probs)
pred <- prediction(probs[,2], datos[-train,"Y"])

#Forma 1: Paquete ROC
#tpr: true positive rate 
#fpr: false positive rate
per <- performance(pred,"tpr","fpr")
#Calculando el AUC
perf_AUC <- performance(pred,"auc")
AUC=perf_AUC@y.values[[1]]
#Graficando ROC
plot(per, main="ROC curve")
text(0.5,0.5, paste("AUC=",format(AUC,digits = 5,scientific = F)))

#Forma 2:
#caret y pROC
data.test <- datos[-train,]
library(pROC)
result.roc <- roc(data.test$Y,probs[,2])
plot(result.roc , print.thres="best", print..thres.best.method =
       "closest.topleft")
result.coord <- coords(result.roc,"best",best.method =
                         "closest.topleft", ret=c("threshold","accurracy"))
#Para obtener el accurracy y teta de corte
print(result.coord)
