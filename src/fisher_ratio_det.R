fisher_ratio_det <- function(C, M, U, selected_features, eps = 1e-8) {
  c <- nrow(C)
  Pi <- colSums(U) / nrow(U)
  C0 <- as.vector(Pi %*% C[, selected_features, drop = FALSE])
  
  B <- matrix(0, length(selected_features), length(selected_features))
  W <- matrix(0, length(selected_features), length(selected_features))
  
  for (i in 1:c) {
    # Entre classes
    diff <- C0 - C[i, selected_features]
    B <- B + Pi[i] * tcrossprod(diff)
    
    # Dentro da classe
    F <- M[, selected_features, i]
    F <- diag(diag(F))
    W <- W + Pi[i] * F
  }
  
  # Só diagonais (igual ao seu código original)
  B <- diag(diag(B))
  W <- diag(diag(W))
  
  # regularização
  detB <- det(B + eps * diag(nrow(B)))
  detW <- det(W + eps * diag(nrow(W)))
  
  ratio <- detB / detW
  return(as.numeric(ratio))
}