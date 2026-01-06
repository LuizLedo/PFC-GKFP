evaluate_acc <- function(x_sub, y_sub, C_sub, M_sub, R, usar_CI = FALSE, alpha = 0.01, p_CI = 0.5,
                         classes_unicas = sort(unique(y_sub)), correcao_labels = TRUE) {
  # espera-se que fuzeval retorne matrix n_obs x n_rules (ou parecido)
  beta_sub <- fuzeval(as.matrix(x_sub), C_sub, M_sub)
  # Remove linhas com NaN ou Inf
  linhas_invalidas <- which(apply(beta_sub, 1, function(x) any(is.na(x) | is.infinite(x))))
  if (length(linhas_invalidas) > 0) {
  beta_sub <- beta_sub[-linhas_invalidas, , drop = FALSE]
  x_sub <- x_sub[-linhas_invalidas, , drop = FALSE]
  y_sub <- y_sub[-linhas_invalidas]
 }

  
  resultado_prelevant <- if (!usar_CI) {
    PrelevantAlpha(beta_sub, y_sub, R, alpha, M_sub)
  } else {
    PrelevantCI(beta_sub, y_sub, R, p_CI, M_sub)
  }
  P <- resultado_prelevant$P
  
  n <- nrow(beta_sub)
  preds <- numeric(n)
  for (i in seq_len(n)) {
    probs <- as.numeric(P %*% beta_sub[i, ])
    preds[i] <- classes_unicas[which.max(probs)]
  }
  if (correcao_labels) preds <- corrigir_permutacao_labels(preds, y_sub)
  acc <- mean(preds == y_sub)
  return(acc)
}
