baseline_models <- function(dados, coluna_classe) {
  dados=as.data.frame(dados)
  #colnames(dados)[coluna_classe] <- "Classe"
  #dados$Classe <- as.factor(dados$Classe)
  coluna_classe=ncol(dados)
  dados[,coluna_classe]=as.factor(dados[,coluna_classe])
  colnames(dados)[coluna_classe] <- "Classe"
  preditores <- dados[, -coluna_classe]
  resposta <- dados$Classe
  
  preditores_norm <- as.data.frame(scale(preditores))
  dados_norm <- cbind(preditores_norm, Classe = resposta)
  
  n <- nrow(dados_norm)
  indices <- sample(1:n, size = 0.8 * n)
  
  treino <- dados_norm[indices, ]
  teste <- dados_norm[-indices, ]
  
  x_treino <- treino[, -ncol(treino),drop=FALSE]
  y_treino <- treino$Classe
  x_teste <- teste[, -ncol(teste),drop=FALSE]
  y_teste <- teste$Classe
  
  # KNN
  knn_pred <- knn(train = x_treino, test = x_teste, cl = y_treino, k = 3)
  acuracia_knn <- mean(knn_pred == y_teste)
  
  # SVM
  svm_model <- svm(Classe ~ ., data = treino, kernel = "radial")
  svm_pred <- predict(svm_model, newdata = teste)
  acuracia_svm <- mean(svm_pred == y_teste)
  
  # Árvore
  arvore_modelo <- rpart(Classe ~ ., data = treino, method = "class")
  arvore_pred <- predict(arvore_modelo, newdata = teste, type = "class")
  acuracia_arvore <- mean(arvore_pred == y_teste)
  
  return(c(KNN = acuracia_knn, SVM = acuracia_svm, Arvore = acuracia_arvore))
}