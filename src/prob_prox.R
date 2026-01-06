# Função prob_prox sem vazamento
# Autor: Luiz Ledo Mota Melo Junior
# Objetivo: calcular priori por classe no conjunto alvo (validação/teste)
# com base na distância Euclidiana para o conjunto de treino

prob_prox <- function(x_treino, y_treino, x_alvo, k = 5) {
  # Número de classes únicas
  todas_classes <- sort(unique(y_treino))
  n_classes <- length(todas_classes)
  
  # Matriz de proporções: linhas = observações alvo, colunas = classes
  proporcoes <- matrix(0, nrow = nrow(x_alvo), ncol = n_classes)
  colnames(proporcoes) <- todas_classes
  
  # Função de distância euclidiana
  distancia <- function(v1, v2) sqrt(sum((v1 - v2)^2))
  
  # Para cada ponto em x_alvo (validação o
  for (i in 1:nrow(x_alvo)) {
    ponto_teste <- as.numeric(x_alvo[i, ])
    
    # Calcula distâncias para todos os pontos do treino
    distancias <- apply(x_treino, 1, function(linha) {
      distancia(ponto_teste, as.numeric(linha))
    })
    
    # Seleciona os k vizinhos mais próximos
    idx_vizinhos <- order(distancias)[1:k]
    vizinhos <- y_treino[idx_vizinhos]
    
    # Conta e calcula proporções das classes
    contagem <- table(factor(vizinhos, levels = todas_classes))
    proporcoes[i, ] <- prop.table(contagem)
  }
  
  return(proporcoes)
}
