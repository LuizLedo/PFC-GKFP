inicio_total <- Sys.time()

source("carregar_datasets.R")

library(parallel)
library(dplyr)

# =========================
# 0) CRIA O CLUSTER
# =========================
ncores <- max(1, detectCores() - 1)
cl <- makeCluster(ncores)

# =========================
# 1) PACOTES NOS WORKERS
# =========================
clusterEvalQ(cl, {
  library(dplyr)
  library(GA)
  library(MASS)
 library(clue)
 library(dplyr)
     # se você usa GA::ga dentro
  NULL
})

# =========================
# 2) EXPORTA OBJETOS/FUNÇÕES
# =========================
clusterExport(
  cl,
  varlist = c(
    "datasets", "info_datasets",
    "rodar_modelos", "rodar_avaliacao", "otimizar_parametros","fisher_ratio_det",
    "fisher_selection_loop", "supervised", "GKFP1", "fuzeval",
    "PrelevantAlpha", "PrelevantCI", "prob_prox","evaluate_acc", "corrigir_permutacao_labels"
  ),
  envir = environment()
)

# =========================
# 3) RODA EM PARALELO
# =========================
todos_resultados <- parLapply(cl, names(info_datasets), function(nome_dataset) {

  inicio_ds <- Sys.time()

  out <- tryCatch({

    dados <- datasets[[nome_dataset]]
    cfg   <- info_datasets[[nome_dataset]]

    R <- cfg$R
    R<- 2*R
    usar_predold <- cfg$usar_pred
    P <- 2

    set.seed(abs(as.integer(sum(utf8ToInt(nome_dataset)))))

    resultados <- rodar_modelos(
      dados = dados,
      P = P,
      R = R,
      usar_predold = usar_predold,
      prob = NULL,
      n_rodadas = 1,
      verbose = FALSE
    )

    linha <- unlist(resultados)
    df <- data.frame(dataset = nome_dataset, t(linha), row.names = NULL)
    df

  }, error = function(e) {

    calls_txt <- paste(sapply(sys.calls(), deparse), collapse = " -> ")
    data.frame(
      dataset = nome_dataset,
      erro = paste0("ERRO: ", conditionMessage(e)),
      calls = calls_txt,
      row.names = NULL
    )
  })

  out$tempo_s <- round(difftime(Sys.time(), inicio_ds, units = "secs"), 3)
  out
})

# =========================
# 4) FECHA O CLUSTER
# =========================
stopCluster(cl)

# =========================
# 5) JUNTA RESULTADOS
# =========================
tabela_final <- bind_rows(todos_resultados) %>%
  arrange(dataset)

tempo_total <- round(difftime(Sys.time(), inicio_total, units = "secs"), 3)

cat("=== Tempo total de execução:", tempo_total, "s ===\n")
print(tabela_final)

