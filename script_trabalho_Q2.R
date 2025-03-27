library("caret")
library(RSNNS)
#Carregabdo a base de Dados
df <- read.csv("http://www.razer.net.br/datasets/Volumes.csv",sep = ";", dec = ",")

#Eliminando a coluna NR
df$NR<- NULL

#Criando as Partições 80/20
set.seed(7)
indices <- indices <- createDataPartition(df$VOL,p=0.80, list=FALSE)
treino <- df[indices,]
teste <- df[-indices,]

print("Treinando os modelos...")
#Treinar um modelo Random Forest
print("Random Forest")
rf <- caret::train(VOL~., data=treino,method="rf")

#Treinar um modelo SVM
print("SVM Radial")
svm <- caret::train(VOL~., data=treino,method="svmRadial")

#Treinar um modelo RNA (NNET) 
print("RNA")
rna <- caret::train(VOL~., data=treino,method="nnet")

#Treinar mmodelo Alométrico
alom <- nls(VOL ~ b0 + b1*DAP*DAP*HT, df, start=list(b0=0.5, b1=0.5))

#predições
predicoes.svm <- predict(svm, teste)
predicoes.rf <- predict(rf, teste)
predicoes.rna <- predict(rna, teste)
predicoes.alom <- predict(alom, teste)

coefR2 <- function(y_real, y_pred){
  residual <- sum((y_real - y_pred)^2)  # Soma dos quadrados dos resíduos
  total <- sum((y_real - mean(y_real))^2)  # Soma dos quadrados totais
  r2 <- 1 - (residual / total)  # Fórmula do R²
  return(r2)
}

coefSYX <- function(y_real,y_pred,percent=F){
  n <- length(y_real)
  residual <- sum((y_real - y_pred)^2)
  syx <- sqrt(residual / (n-2))
  if(percent){
    syx <- (syx / mean(y_real)) * 100  # Cálculo do Syx %
  }
  return(syx)
}

#Validando resultados RF
result.rf.r2 <- coefR2(teste$VOL,predicoes.rf)
result.rf.syx <- coefSYX(teste$VOL,predicoes.rf)
result.rf.syxPerc <- coefSYX(teste$VOL,predicoes.rf,percent = T)

#validando resultados SVM
result.svm.r2 <- coefR2(teste$VOL,predicoes.svm)
result.svm.syx <- coefSYX(teste$VOL,predicoes.svm)
result.svm.syxPerc <- coefSYX(teste$VOL,predicoes.svm,percent = T)

#validando resultados RNA
result.rna.r2 <- coefR2(teste$VOL,predicoes.rna)
result.rna.syx <- coefSYX(teste$VOL,predicoes.rna)
result.rna.syxPerc <- coefSYX(teste$VOL,predicoes.rna,percent = T)

#validando resultados SVM
result.alom.r2 <- coefR2(teste$VOL,predicoes.alom)
result.alom.syx <- coefSYX(teste$VOL,predicoes.alom)
result.alom.syxPerc <- coefSYX(teste$VOL,predicoes.alom,percent = T)

cat("RF-> R²: ", result.rf.r2,"Syx: ", result.rf.syx, "Syx%",result.rf.syxPerc,"\n" )
cat("SVM-> R²: ", result.svm.r2,"Syx: ", result.svm.syx, "Syx%",result.svm.syxPerc,"\n" )
cat("RNA-> R²: ", result.rna.r2,"Syx: ", result.rna.syx, "Syx%",result.rna.syxPerc,"\n" )
cat("Alometrico-> R²: ", result.alom.r2,"Syx: ", result.alom.syx, "Syx%",result.alom.syxPerc,"\n" )
