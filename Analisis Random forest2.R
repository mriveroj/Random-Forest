#Leemos el archivo csv.
NBA<- read.csv(file="NBADATOS.csv",sep=";")
NBA$Tipo.de.partido <- factor(NBA$Tipo.de.partido)
#Dividimos el conjunto de datos en training y test.
set.seed(1234)
ind <- sample(2, nrow(NBA),replace= TRUE,prob=c(0.8,0.2))
train <- NBA[ind==1,]
test <- NBA[ind==2,]

#RANDOM FOREST
library(randomForest)
#Para observar la estructura de los datos
str(train)
train$Y<-factor(train$Y)
#Como la variable dependiente ya es un factor, no es necesario hacer la transformaci?n.
#creando el modelo usando la funci?n "randomForest"

model<-randomForest(Y~PosicionEquipo+PosicionRival+PuntosAbajo+Tipo.de.partido, data = train,ntree=500,importance=TRUE,maxnodes=10,mtry=25)
model
#Type of random forest: classification
#Number of trees: 500

#Creamos un objeto con las "importancias" de las variables
importancia=data.frame(importance(model))
#install.packages("reshape")
library(reshape)
importancia<-sort_df(importancia,vars='MeanDecreaseGini')
importancia
varImpPlot(model)

model<-randomForest(Y~PosicionRival+PosicionEquipo+PuntosAbajo, data = train,ntree=500,importance=TRUE,maxnodes=10,mtry=25)
model

plot(model,main = "Random Forest's error rates", 
     legend=c(x=500, y=0.20, legend=c("Resilient","No resilient")),
     fill=c("red","green"),cex=0.1,text.front=4, bg='grey')
plot
model

#Predicción y matrix de confusión para el test:
pred <- predict(model,test,type="response")
table(test$Y,pred)

probs <- predict(fit,test, type = "prob")
head(probs)
pred <- prediction(probs[,2], test$Y)

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
library(pROC)
result.roc <- roc(test,probs[,2])
plot(result.roc , print.thres="best", print..thres.best.method =
       "closest.topleft", main= "ROC curve")
result.coord <- coords(result.roc,"best",best.method =
                         "closest.topleft", ret=c("threshold","accurracy"))
#Para obtener teta de corte
print(result.coord)

