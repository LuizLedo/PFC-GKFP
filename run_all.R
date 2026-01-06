
files <- list.files("src", pattern = "\\.R$", full.names = TRUE)
invisible(lapply(files, source))

# neste ponto, datasets e info_datasets já existem no ambiente
stopifnot(exists("datasets"), exists("info_datasets"))




# =========================================================
# PFC-GKFP - Main script (DUAL: parallel with safe fallback)
# =========================================================

inicio_total <- Sys.time()

# -------------------------
# Load source files
# -------------------------
source("src/carregar_datasets.R")
source("src/prob_prox.R")
source("src/supervised.R")
source("src/GKFP1.R")
source("src/fuzeval.R")
source("src/PrelevantAlpha.R")
source("src/PrelevantCI.R")
source("src/corrigir_permutacao_labels.R")
source("src/otimizar_parametros.R")
source("src/rodar_avaliacao.R")
source("src/rodar_modelos.R")
source("src/fisher_ratio_det.R")
source("src/evaluate_acc.R")
source("src/fisher_selection_loop.R")

library(dplyr)
library(parallel)

stopifnot(exists("datasets"), exists("info_datasets"))

# -------------------------
# Runner (one dataset)
# -------------------------
runner <- function(nome_dataset) {

  inicio_ds <- Sys.time()
  cat(">> Starting dataset:", nome_dataset, "\n")
  flush.console()

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

  tempo_ds <- round(difftime(Sys.time(), inicio_ds, units = "secs"), 3)
  cat("<< Finished dataset:", nome_dataset, "| Time:", tempo_ds, "seconds\n")
  flush.console()

  out$tempo_s <- tempo_ds
  out
}

# -------------------------
# Try parallel (protected)
# -------------------------
usar_paralelo <- FALSE
cl <- NULL

# Windows/RStudio tends to be unstable with many cores; keep it small
ncores <- 2

cl <- tryCatch(
  makeCluster(ncores, type = "PSOCK", outfile = ""),
  error = function(e) NULL
)

if (!is.null(cl)) {
  usar_paralelo <- TRUE
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)

  # Load packages on workers
  ok_eval <- tryCatch({
    clusterEvalQ(cl, {
      library(dplyr)
      library(GA)
      library(MASS)
      library(clue)
      NULL
    })
    TRUE
  }, error = function(e) FALSE)

  # Export required objects/functions
  ok_export <- FALSE
  if (ok_eval) {
    ok_export <- tryCatch({
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
      TRUE
    }, error = function(e) FALSE)
  }

  if (!ok_eval || !ok_export) {
    cat("WARNING: Parallel setup failed. Running sequentially.\n")
    usar_paralelo <- FALSE
    try(stopCluster(cl), silent = TRUE)
    cl <- NULL
  }
} else {
  cat("WARNING: Could not create cluster. Running sequentially.\n")
}

# -------------------------
# Run (parallel or sequential)
# -------------------------
if (usar_paralelo) {
  todos_resultados <- parLapply(cl, names(info_datasets), runner)
} else {
  todos_resultados <- lapply(names(info_datasets), runner)
}

# -------------------------
# Collect results
# -------------------------
tabela_final <- bind_rows(todos_resultados) %>%
  arrange(dataset)

tempo_total <- round(difftime(Sys.time(), inicio_total, units = "secs"), 3)

cat("=== Total execution time:", tempo_total, "seconds ===\n")
print(tabela_final)

