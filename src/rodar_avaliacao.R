rodar_avaliacao <- function(dados, P, prob, R, n_rodadas=1,
                            usar_CI = FALSE,
                            usar_GKFP = FALSE, otimiza_zeta = FALSE,
                            usar_predold = TRUE, usar_GA = FALSE,
                            metodo = c("PCA", "FISHER"), min_features = 2,
                            m = 2, verbose = FALSE,
                            alphas = seq(0.1, 1.0, by = 0.1),
                            pCIs   = seq(0.5, 2.0, by = 0.1),
                            # --- grades para o grid interno de m e zeta ---
                            m_grid = c(2),
                            zeta_grid = c(1e-6, 0.5, 1.0)) {

  metodo <- match.arg(metodo)

  oracle_obs <- numeric(n_rodadas)
  acc_best_valid <- numeric(n_rodadas)
  best_on_test <- numeric(n_rodadas)

  grid_vals <- if (!usar_CI) alphas else pCIs
  grid_name <- if (!usar_CI) "alpha" else "pCI"

  for (i in seq_len(n_rodadas)) {

    # ------------------------------
    # Pré-processamento (mantive como você fez)
    # ------------------------------
    if (metodo == "PCA") {
      ACP <- prcomp(dados[, -ncol(dados)], scale. = TRUE)
      dados_proc <- cbind(ACP$x[, 1:P, drop = FALSE], Classe = dados[, ncol(dados)])
      dados_proc <- as.data.frame(dados_proc)
    } else {
      x_scaled <- scale(dados[, -ncol(dados)])
      dados_proc <- as.data.frame(cbind(x_scaled, Classe = dados[, ncol(dados)]))
    }

    # ------------------------------
    # Split 50/25/25
    # ------------------------------
    idx <- sample.int(nrow(dados_proc))
    n <- length(idx)
    n_treino <- floor(0.5 * n)
    n_valid  <- floor(0.25 * n)

    idx_treino <- idx[1:n_treino]
    idx_valid  <- idx[(n_treino + 1):(n_treino + n_valid)]
    idx_teste  <- idx[(n_treino + n_valid + 1):n]

    x_treino <- as.matrix(dados_proc[idx_treino, -ncol(dados_proc)])
    y_treino <- dados_proc[idx_treino, "Classe"]
    x_valid  <- as.matrix(dados_proc[idx_valid,  -ncol(dados_proc)])
    y_valid  <- dados_proc[idx_valid,  "Classe"]
    x_teste  <- as.matrix(dados_proc[idx_teste,  -ncol(dados_proc)])
    y_teste  <- dados_proc[idx_teste,  "Classe"]

    # ------------------------------
    # Seleção de features (FISHER)
    # ------------------------------
    if (metodo == "FISHER") {
      sel <- fisher_selection_loop(
        x_treino = x_treino, y_treino = y_treino,
        x_valid  = x_valid,  y_valid  = y_valid,
        R = R, usar_CI = usar_CI, alpha = 0.001, p_CI = 0.5,
        min_features = min_features, m = m,
        verbose = verbose
      )
      feats <- sel$features
      x_treino <- x_treino[, feats, drop = FALSE]
      x_valid  <- x_valid[,  feats, drop = FALSE]
      x_teste  <- x_teste[,  feats, drop = FALSE]
    }

    # ------------------------------
    # Varredura alpha/pCI (sem GA)
    # ------------------------------
    preds_list <- vector("list", length(grid_vals))
    accs_teste <- numeric(length(grid_vals))
    accs_valid_proxy <- numeric(length(grid_vals))
    yteste_ref <- NULL

    for (k in seq_along(grid_vals)) {
      v <- grid_vals[k]

      res <- otimizar_parametros(
        x_treino = x_treino, y_treino = y_treino,
        x_valid  = x_valid,  y_valid  = y_valid,
        x_teste  = x_teste,  y_teste  = y_teste,
        R = R, prob = prob,
        valid_idx = idx_valid, teste_idx = idx_teste,
        usar_CI = usar_CI,
        usar_GKFP = usar_GKFP,
        otimiza_zeta = otimiza_zeta,
        usar_predold = usar_predold,
        usar_GA = FALSE, # removido
        alpha_fixo = if (!usar_CI) v else NULL,
        pCI_fixo   = if (usar_CI)  v else NULL,
        m_grid = m_grid,
        zeta_grid = zeta_grid
      )

      preds_list[[k]] <- res$pred_teste
      yteste_ref <- res$y_teste

      accs_teste[k] <- mean(res$pred_teste == yteste_ref)
      accs_valid_proxy[k] <- max(sapply(res$historico, function(h) h$acuracia))
    }

    # Oráculo por observação
    acertos_mat <- sapply(seq_along(preds_list), function(k) preds_list[[k]] == yteste_ref)
    oracle_obs[i] <- mean(apply(acertos_mat, 1, any))

    best_on_test[i] <- max(accs_teste)

    k_best_valid <- which.max(accs_valid_proxy)
    acc_best_valid[i] <- accs_teste[k_best_valid]

    if (verbose) {
      cat("\nRodada", i, "\n",
          "grid =", grid_name, "\n",
          "oracle_obs =", oracle_obs[i], "\n",
          "best_on_test =", best_on_test[i], "\n",
          "acc_teste(escolhido_valid) =", acc_best_valid[i], "\n")
    }
  }

  return(list(
    media_oracle_obs = mean(oracle_obs),
    sd_oracle_obs    = sd(oracle_obs),

    media_best_on_test = mean(best_on_test),
    sd_best_on_test    = sd(best_on_test),

    media_acc_teste_escolhido_valid = mean(acc_best_valid),
    sd_acc_teste_escolhido_valid    = sd(acc_best_valid),

    grid_name = grid_name,
    grid_vals = grid_vals
  ))
}




