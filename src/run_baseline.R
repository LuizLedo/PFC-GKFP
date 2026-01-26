run_baseline <- function(datasets, n_repeticoes = 10, seed = 123, usar_pca = TRUE) {

  set.seed(seed)
  resultados <- list()

  for (nome_dataset in names(datasets)) {
    cat("\nRodando para dataset:", nome_dataset, "\n")

    dados <- datasets[[nome_dataset]]

    if (usar_pca) {
      dados <- pca_2pc_prcomp(dados)
    }

    coluna_classe <- ncol(dados)

    resultados_temp <- matrix(NA, nrow = 3, ncol = n_repeticoes,
                              dimnames = list(c("KNN", "SVM", "Arvore"), NULL))

    for (i in 1:n_repeticoes) {
      res <- tryCatch({
        baseline_models(dados, coluna_classe)
      }, error = function(e) {
        cat("  -> Erro em", nome_dataset, "rodada", i, ":", conditionMessage(e), "\n")
        return(c(KNN = NA, SVM = NA, Arvore = NA))
      })

      resultados_temp[, i] <- c(res["KNN"], res["SVM"], res["Arvore"])
    }

    medias  <- rowMeans(resultados_temp, na.rm = TRUE)
    desvios <- apply(resultados_temp, 1, sd, na.rm = TRUE)

    resultados[[nome_dataset]] <- data.frame(
      Modelo = c("KNN", "SVM", "Arvore"),
      Media  = round(medias, 4),
      Desvio = round(desvios, 4),
      row.names = NULL
    )
  }

  # Montar matrizes
  nomes_modelos  <- c("KNN", "SVM", "Arvore")
  nomes_datasets <- names(resultados)

  matriz_media <- matrix(NA, nrow = length(nomes_modelos), ncol = length(nomes_datasets),
                         dimnames = list(nomes_modelos, nomes_datasets))

  matriz_desvio <- matrix(NA, nrow = length(nomes_modelos), ncol = length(nomes_datasets),
                          dimnames = list(nomes_modelos, nomes_datasets))

  for (nm in nomes_datasets) {
    res <- resultados[[nm]]
    matriz_media[, nm]  <- res$Media
    matriz_desvio[, nm] <- res$Desvio
  }

  return(list(
    resultados = resultados,
    matriz_media = matriz_media,
    matriz_desvio = matriz_desvio
  ))
}
