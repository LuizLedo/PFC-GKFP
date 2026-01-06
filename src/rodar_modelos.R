rodar_modelos <- function(dados, P, R,
                          usar_predold = TRUE,
                          prob = NULL,
                          n_rodadas = 1,
                          alphas = seq(0.1, 1.0, by = 0.1),
                          pCIs   = seq(0.5, 2.0, by = 0.5),
                          verbose = FALSE) {

  modelos <- list(

   
    # =========================
    # GKFP
    # =========================
    "GKFP_ACP_CI" = list(
      usar_GKFP = TRUE,
      otimiza_zeta = TRUE,
      usar_CI = TRUE,
      usar_GA = TRUE,
      metodo = "PCA"
    ),

    "GKFP_ACP_AC" = list(
      usar_GKFP = TRUE,
      otimiza_zeta = TRUE,
      usar_CI = FALSE,
      usar_GA = TRUE,
      metodo = "PCA"
    )

    

    
  )

  resultados <- list()

  for (nome_modelo in names(modelos)) {

    params <- modelos[[nome_modelo]]

    cat("\n=============================\n")
    cat("Rodando:", nome_modelo, "\n")
    cat("=============================\n")

    res <- rodar_avaliacao(
      dados = dados,
      P = P,
      prob = prob,
      R = R,
      n_rodadas = n_rodadas,
      usar_CI = params$usar_CI,
      usar_GKFP = params$usar_GKFP,
      otimiza_zeta = params$otimiza_zeta,
      usar_predold = usar_predold,
      usar_GA = params$usar_GA,
      metodo = params$metodo,
      alphas = alphas,
      pCIs   = pCIs,
      verbose = verbose
    )

    # =========================
    # Métricas salvas
    # =========================
    resultados[[paste0(nome_modelo, "_oracle_media")]] <-
      res$media_oracle_obs
    resultados[[paste0(nome_modelo, "_oracle_sd")]] <-
      res$sd_oracle_obs

    resultados[[paste0(nome_modelo, "_best_on_test_media")]] <-
      res$media_best_on_test
    resultados[[paste0(nome_modelo, "_best_on_test_sd")]] <-
      res$sd_best_on_test

    resultados[[paste0(nome_modelo, "_valid_choice_media")]] <-
      res$media_acc_teste_escolhido_valid
    resultados[[paste0(nome_modelo, "_valid_choice_sd")]] <-
      res$sd_acc_teste_escolhido_valid
  }

  return(resultados)
}

