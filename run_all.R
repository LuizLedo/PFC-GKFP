
# =========================================================
# PFC-GKFP - Main script (SEQUENTIAL, STABLE)
# =========================================================
require(dplyr)
require(clue)
library(dplyr)
library(clue)
inicio_total <- Sys.time()

# -------------------------
# Load source files (ONCE)
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



# Sanity check
stopifnot(exists("datasets"), exists("info_datasets"))

# -------------------------
# Runner (one dataset)
# -------------------------
runner <- function(nome_dataset) {

  inicio_ds <- Sys.time()
  message(">> Starting dataset: ", nome_dataset)

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
      n_rodadas = 10,
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
  message("<< Finished dataset: ", nome_dataset,
          " | Time: ", tempo_ds, " seconds")

  out$tempo_s <- tempo_ds
  out
}

# -------------------------
# Run sequentially
# -------------------------
todos_resultados <- lapply(names(info_datasets), runner)

# -------------------------
# Collect results
# -------------------------
tabela_final <- bind_rows(todos_resultados) %>%
  arrange(dataset)

tempo_total <- round(difftime(Sys.time(), inicio_total, units = "secs"), 3)

message("=== Total execution time: ", tempo_total, " seconds ===")
print(tabela_final)
