otimizar_parametros <- function(x_treino, y_treino, x_valid, y_valid, x_teste, y_teste,
                                R, prob, valid_idx, teste_idx,
                                usar_CI = FALSE,
                                usar_GKFP = FALSE,
                                otimiza_zeta = FALSE,
                                correcao_labels = TRUE,
                                usar_predold = TRUE,
                                usar_GA = FALSE,  # ignorado agora
                                maxiter = 3, popSize = 10, # ignorados agora
                                alpha_fixo = NULL,
                                pCI_fixo = NULL,
                                # ---- NOVOS: grades do grid search ----
                                m_grid = c(2),
                                zeta_grid = c(1e-6, 0.5, 1.0)) {

  historico_avaliacoes <- list()
  classes_unicas <- sort(unique(y_treino))

  # -----------------------------
  # Predição vetorizada (bem mais rápida que o for)
  # P: R x R, beta_mat: n x R
  # -----------------------------
  pred_com_P <- function(P, beta_mat, prob1_mat = NULL) {
    scores <- beta_mat %*% t(P)  # n x R
    if (!usar_predold && !is.null(prob1_mat)) {
      scores <- scores * prob1_mat
    }
    classes_unicas[max.col(scores, ties.method = "first")]
  }

  # -----------------------------
  # Helper: avalia uma combinação (m, zeta) com alpha/pCI fixo
  # -----------------------------
  avalia_combo <- function(m_otim, zeta_otim) {

    if (!usar_CI) {
      if (is.null(alpha_fixo)) stop("alpha_fixo deve ser fornecido quando usar_CI=FALSE (você varre alphas fora).")
      alpha_otim <- alpha_fixo
      p_CI_otim  <- 0
    } else {
      if (is.null(pCI_fixo)) stop("pCI_fixo deve ser fornecido quando usar_CI=TRUE (você varre pCIs fora).")
      p_CI_otim  <- pCI_fixo
      alpha_otim <- 0
    }

    # --- Ajuste C e M no TREINO ---
    if (!usar_GKFP) {
      sup <- supervised(as.matrix(x_treino), c = R, m = m_otim)
      C_otim <- sup$V
      M_otim <- sup$M
    } else {
      gk <- GKFP1(as.matrix(x_treino), c = R, m = m_otim, zeta = zeta_otim)
      C_otim <- gk$V
      M_otim <- gk$M
    }

    # --- Beta na validação ---
    beta_valid <- fuzeval(x_valid, C_otim, M_otim)

    # linhas inválidas mais rápido que apply:
    linhas_invalidas <- which(rowSums(!is.finite(beta_valid)) > 0)
    if (length(linhas_invalidas) > 0) {
      beta_valid2 <- beta_valid[-linhas_invalidas, , drop = FALSE]
      x_valid2 <- x_valid[-linhas_invalidas, , drop = FALSE]
      y_valid2 <- y_valid[-linhas_invalidas]
    } else {
      beta_valid2 <- beta_valid; x_valid2 <- x_valid; y_valid2 <- y_valid
    }

    # --- Matriz P ---
    pre <- if (!usar_CI) {
      PrelevantAlpha(beta_valid2, y_valid2, R, alpha_otim, M_otim)
    } else {
      PrelevantCI(beta_valid2, y_valid2, R, p_CI_otim, M_otim)
    }
    Pmat <- pre$P

    # --- Prob prox + predição ---
    prob1 <- prob_prox(x_treino, y_treino, x_valid2, k = 5)
    pred <- pred_com_P(Pmat, beta_valid2, prob1)

    if (correcao_labels) pred <- corrigir_permutacao_labels(pred, y_valid2)
    acc <- mean(pred == y_valid2)

    list(acc = acc, C = C_otim, M = M_otim, P = Pmat)
  }

  # -----------------------------
  # GRID SEARCH (sem GA)
  # -----------------------------
  if (!otimiza_zeta) zeta_grid <- 0

  melhor_acc <- -Inf
  melhor_obj <- NULL

  for (m_otim in m_grid) {
    for (zeta_otim in zeta_grid) {
      out <- avalia_combo(m_otim, zeta_otim)

      historico_avaliacoes[[length(historico_avaliacoes) + 1]] <- list(
        parametros = c(m = m_otim, zeta = zeta_otim,
                       alpha = if (!usar_CI) alpha_fixo else NA_real_,
                       pCI   = if (usar_CI)  pCI_fixo else NA_real_),
        C = out$C, M = out$M, P = out$P,
        acuracia = out$acc
      )

      if (out$acc > melhor_acc) {
        melhor_acc <- out$acc
        melhor_obj <- out
      }
    }
  }

  # pega melhores C,M,P
  C <- melhor_obj$C
  M <- melhor_obj$M
  Pmat <- melhor_obj$P

  # -----------------------------
  # Predição no TESTE
  # -----------------------------
  beta_teste <- fuzeval(x_teste, C, M)
  linhas_invalidas <- which(rowSums(!is.finite(beta_teste)) > 0)
  if (length(linhas_invalidas) > 0) {
    beta_teste2 <- beta_teste[-linhas_invalidas, , drop = FALSE]
    x_teste2 <- x_teste[-linhas_invalidas, , drop = FALSE]
    y_teste2 <- y_teste[-linhas_invalidas]
  } else {
    beta_teste2 <- beta_teste; x_teste2 <- x_teste; y_teste2 <- y_teste
  }

  prob1_t <- prob_prox(x_treino, y_treino, x_teste2, k = 5)
  pred_teste <- pred_com_P(Pmat, beta_teste2, prob1_t)

  if (correcao_labels) pred_teste <- corrigir_permutacao_labels(pred_teste, y_teste2)
  acuracia <- mean(pred_teste == y_teste2)

  return(list(
    ga_resultado = NULL,  # removido
    historico = historico_avaliacoes,
    C = C, M = M, P = Pmat,
    acuracia = acuracia,
    pred_teste = pred_teste,
    y_teste = y_teste2
  ))
}

