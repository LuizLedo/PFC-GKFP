fisher_selection_loop <- function(x_treino, y_treino, x_valid, y_valid, R,
                                  usar_CI = FALSE, alpha = 0.001, p_CI = 0.5,
                                  min_features = 2, m = 2, verbose = TRUE) {
  # Treina uma única vez o supervised para obter V (C), M e U 
  sup <- GKFP1(as.matrix(x_treino), R, m)
  C_full <- sup$V
  M_full <- sup$M
  U_full <- if (!is.null(sup$U)) sup$U else matrix(1, nrow = nrow(x_treino), ncol = ncol(x_treino))
  
  features <- seq_len(ncol(x_treino))
  classes_unicas <- sort(unique(y_treino))
  
  # acurácia inicial (com todas as features)
  current_acc <- evaluate_acc(x_treino[, features, drop = FALSE], y_treino,
                              C_full[, features, drop = FALSE],
                              M_full[features, features, , drop = FALSE],
                              R, usar_CI, alpha, p_CI, classes_unicas, correcao_labels = TRUE)
  best_acc <- current_acc
  best_features <- features
  print(best_acc)
  if (verbose) cat("Início seleção: features =", length(features), "acc =", current_acc, "\n")
  
  repeat {
    if (length(features) <= min_features) break
    
    # calcula razão Fisher se remover cada feature (subconjunto sem essa feature)
    ratios <- sapply(features, function(f) {
      sel <- setdiff(features, f)
      fisher_ratio_det(C_full, M_full, U_full, sel)
    })

    # escolhe a feature cuja remoção resulta no menor ratio (piora menor contribuição)
    pos_remove <- which.max(ratios)
    feat_remove <- features[pos_remove]
    new_features <- features[features != feat_remove]
    print(pos_remove)
    print(feat_remove)
    # constrói C e M reduzidos
    C_new <- C_full[, new_features, drop = FALSE]
    M_new <- M_full[new_features, new_features, , drop = FALSE]
    
    # avalia acurácia usando validação (atenção: usamos valid para decidir seleção)
    new_acc <- evaluate_acc(x_valid[, new_features, drop = FALSE], y_valid,
                            C_new, M_new, R, usar_CI, alpha, p_CI,
                            classes_unicas, correcao_labels = TRUE)
   print(new_acc)
   print(new_features)


    if (verbose) cat("Removendo feature", feat_remove, 
                     "-> new_acc =", sprintf("%.4f", new_acc),
                     " (current_acc =", sprintf("%.4f", current_acc), ")\n")
   print(pos_remove)
print(feat_remove)
    cat("\n--- Iteração ---\n")
  cat("Removendo feature:", feat_remove, " (posição:", pos_remove, ")\n")
  cat("Features restantes:", new_features, "\n")
  cat("Acurácia nova:", new_acc, " | Atual:", current_acc, "\n")
    # se a remoção não reduzir a acurácia, aceita; caso contrário, para
    if (new_acc + 1e-10 >= current_acc) {
      features <- new_features
      current_acc <- new_acc
      best_acc <- current_acc
      best_features <- features
    } else {
      break
    }
  }
  
  if (verbose) cat("Seleção final: features =", length(best_features), "acc =", best_acc, "\n")
  
  return(list(features = best_features, acc = best_acc,
              C = C_full[, best_features, drop = FALSE],
              M = M_full[best_features, best_features, , drop = FALSE]))
}



