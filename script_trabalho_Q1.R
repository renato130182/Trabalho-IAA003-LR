library(caret)
library(mlbench)

#Carregando os dados
data("Satellite")

#criando as partições
set.seed(7)
indices<-createDataPartition(Satellite$classes,p=0.8,list = F)
treino<-Satellite[indices,c(17,18,19,20,37)]
teste<-Satellite[-indices,c(17,18,19,20,37)]

#Treinar RF, SVM e RNA com a base de Treino
print("Treinando modelos..")
print("Treinando rf")
rf <- train(classes~., data=treino, method="rf")
print("Treinando svm")
svm <- train(classes~., data=treino, method="svmRadial")
print("Treinando rna")
rna <- train(classes~., data=treino, method="nnet",trace=FALSE)

#Aplicar modelos treinados na base de Teste
predict.rf <- predict(rf, teste)
predict.svm <- predict(svm, teste)
predict.rna <- predict(rna, teste)

# Criar as matrizes de confusão e comparar os resultados
print("Matriz de confusão RF")
print(confusionMatrix(predict.rf, teste$classes))
print("Matriz de confusão SVM")
print(confusionMatrix(predict.svm, teste$classes))
print("Matriz de confusão RNA")
print(confusionMatrix(predict.rna, teste$classes))

