if (!require(caret)) install.packages("caret", dependencies = TRUE)
library(caret) 
if (!require(e1071)) install.packages("e1071", dependencies = TRUE)
library(e1071) 
if (!require(rpart)) install.packages("rpart", dependencies = TRUE)
library(rpart) 
if (!require(randomForest)) install.packages("randomForest", dependencies = TRUE)
library(randomForest) 


data(iris)


set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]


set.seed(123)
tree_model <- rpart(Species ~ ., data = trainData, method = "class")
tree_pred <- predict(tree_model, testData, type = "class")


set.seed(123)
svm_model <- svm(Species ~ ., data = trainData, kernel = "linear")
svm_pred <- predict(svm_model, testData)


set.seed(123)
rf_model <- randomForest(Species ~ ., data = trainData, ntree = 100)
rf_pred <- predict(rf_model, testData)


calcular_metricas <- function(actual, predicted) {
  cm <- confusionMatrix(predicted, actual)
  accuracy <- cm$overall["Accuracy"]
  by_class <- cm$byClass
  precision <- mean(by_class[,"Precision"], na.rm = TRUE)
  recall <- mean(by_class[,"Recall"], na.rm = TRUE)
  f1_score <- mean(by_class[,"F1"], na.rm = TRUE)
  specificity <- mean(by_class[,"Specificity"], na.rm = TRUE)
  list(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1_Score = f1_score,
    Specificity = specificity
  )
}


metricas_tree <- calcular_metricas(testData$Species, tree_pred)
metricas_svm <- calcular_metricas(testData$Species, svm_pred)
metricas_rf <- calcular_metricas(testData$Species, rf_pred)


resultados <- data.frame(
  Modelo = c("Árbol de Decisión", "SVM", "Bosque Aleatorio"),
  Precisión_Global = c(metricas_tree$Accuracy, metricas_svm$Accuracy, metricas_rf$Accuracy),
  Precisión = c(metricas_tree$Precision, metricas_svm$Precision, metricas_rf$Precision),
  Sensibilidad = c(metricas_tree$Recall, metricas_svm$Recall, metricas_rf$Recall),
  Puntaje_F1 = c(metricas_tree$F1_Score, metricas_svm$F1_Score, metricas_rf$F1_Score),
  Especificidad = c(metricas_tree$Specificity, metricas_svm$Specificity, metricas_rf$Specificity)
)

print("Resultados de evaluación de los modelos:")
print(resultados)


mejor_modelo <- resultados[which.max(resultados$Puntaje_F1), ]
cat("\nEl mejor modelo basado en el Puntaje F1 es:", mejor_modelo$Modelo, 
    "\nPrecisión Global:", mejor_modelo$Precisión_Global, 
    "\nPuntaje F1:", mejor_modelo$Puntaje_F1, 
    "\nPrecisión (Precision):", mejor_modelo$Precisión,
    "\nSensibilidad (Recall):", mejor_modelo$Sensibilidad,
    "\nEspecificidad (Specificity):", mejor_modelo$Especificidad, "\n")
