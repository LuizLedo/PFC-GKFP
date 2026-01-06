inicio_total <- Sys.time()

# =========================
# Load all source functions
# =========================
files <- list.files("src", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(files, source))

library(parallel)
library(dplyr)

# =========================
# 0) CREATE CLUSTER
# =========================
ncores <- max(1, parallel::detectCores() - 1)
cl <- makeCluster(ncores)
on.exit(stopCluster(cl), add = TRUE)

# =========================
# 1) LOAD PACKAGES ON WORKERS
# =========================
clusterEvalQ(cl, {
  library(dplyr)
  library(GA)
  library(MASS)
  library(clue)
  NULL
})

# =========================
# 2) EXPORT OBJECTS / FUNCTIONS
# =========================
clusterExport(
  cl,
  varlist = c(
    "datasets", "info_datasets",
    "rodar_modelos", "rodar_avaliacao", "otimizar_parametros",
    "fisher_ratio_det", "fisher_selection_loop",
    "supervised", "GKFP1", "fuzeval",
    "PrelevantAlpha", "PrelevantCI",
    "prob_prox", "evaluate_acc",
    "corrigir_permutacao_labels"
  ),
  envir = environment()
)

# =========================
# 3) RUN EXPERIMENTS
# =========================
todos_resultados <- parLapply(cl, names(info_datasets), function(nome_dataset) {

  inicio_ds <- Sys.time()

  out <- tryCatch({

    dados <- datasets[[nome_dataset]]
    cfg   <- info_datasets[[nome_dataset]]

    R <- 2 * cfg$R
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
    data.frame(dataset = nome_dataset, t(linha), row.names = NULL)

  }, error = function(e) {

    data.frame(
      dataset = nome_dataset,
      erro = conditionMessage(e),
      row.names = NULL
    )
  })

  out$tempo_s <- round(difftime(Sys.time(), inicio_ds, units = "secs"), 3)
  out
})

# =========================
# 4) COLLECT RESULTS
# =========================
tabela_final <- bind_rows(todos_resultados) %>%
  arrange(dataset)

tempo_total <- round(difftime(Sys.time(), inicio_total, units = "secs"), 3)

cat("=== Total execution time:", tempo_total, "seconds ===\n")
print(tabela_final)
