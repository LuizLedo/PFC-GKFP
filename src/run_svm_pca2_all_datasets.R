# ============================================================
# run_svm_pca2_all_datasets.R
# SVM (e1071) + PCA(2 PCs) under TVT 50/25/25 with validation selection
# Runs ALL datasets in `datasets` from src/carregar_datasets.R
# Repetitions: 10 (seeds 1:10)
# Simple hyperparameter selection on validation:
#   kernel = radial, grid over cost and gamma
# Prints acc_test_mean and acc_test_sd in console
# Saves CSV in results/
# ============================================================

suppressPackageStartupMessages({
  library(e1071)
  library(dplyr)
})

# -----------------------------
# Helpers
# -----------------------------
acc <- function(y_true, y_pred) mean(y_true == y_pred)

make_tvt_split <- function(y, seed, p_train = 0.50, p_val = 0.25) {
  set.seed(seed)
  n <- length(y)
  idx <- sample.int(n)

  n_train <- floor(p_train * n)
  n_val   <- floor(p_val   * n)

  idx_train <- idx[1:n_train]
  idx_val   <- idx[(n_train + 1):(n_train + n_val)]
  idx_test  <- idx[(n_train + n_val + 1):n]

  list(train = idx_train, val = idx_val, test = idx_test)
}

# ------------------------------------------------------------
# Leakage-free PCA(2) pipeline
# ------------------------------------------------------------
prep_pca2_from_train <- function(dados_train) {
  dados_train <- as.data.frame(dados_train)

  ytr <- as.factor(dados_train[, ncol(dados_train)])
  Xtr <- dados_train[, -ncol(dados_train), drop = FALSE]

  # char -> factor
  Xtr[] <- lapply(Xtr, function(col) if (is.character(col)) as.factor(col) else col)

  # one-hot on TRAIN
  mm_train <- model.matrix(~ . - 1, data = Xtr)

  # remove zero-variance cols (TRAIN only)
  sds <- apply(mm_train, 2, sd, na.rm = TRUE)
  keep_cols <- names(sds)[sds > 0]
  mm_train <- mm_train[, keep_cols, drop = FALSE]

  # remove NA rows (TRAIN)
  ok_tr <- complete.cases(mm_train) & !is.na(ytr)
  mm_train <- mm_train[ok_tr, , drop = FALSE]
  ytr <- ytr[ok_tr]

  # PCA on TRAIN
  pca <- prcomp(mm_train, center = TRUE, scale. = TRUE)

  list(pca = pca, keep_cols = keep_cols)
}

apply_pca2 <- function(dados_any, pca_obj) {
  dados_any <- as.data.frame(dados_any)

  y <- as.factor(dados_any[, ncol(dados_any)])
  X <- dados_any[, -ncol(dados_any), drop = FALSE]

  # char -> factor
  X[] <- lapply(X, function(col) if (is.character(col)) as.factor(col) else col)

  mm <- model.matrix(~ . - 1, data = X)

  # align cols to TRAIN (missing -> 0, extra -> drop)
  keep <- pca_obj$keep_cols
  missing <- setdiff(keep, colnames(mm))
  if (length(missing) > 0) {
    mm <- cbind(mm, matrix(0, nrow = nrow(mm), ncol = length(missing),
                           dimnames = list(NULL, missing)))
  }
  mm <- mm[, keep, drop = FALSE]

  # remove NA rows
  ok <- complete.cases(mm) & !is.na(y)
  mm <- mm[ok, , drop = FALSE]
  y  <- y[ok]

  pcs <- predict(pca_obj$pca, newdata = mm)[, 1:2, drop = FALSE]

  list(PCs = pcs, y = y)
}

# ------------------------------------------------------------
# SVM (radial)
# ------------------------------------------------------------
fit_svm <- function(X_train, y_train, cost, gamma) {
  df <- data.frame(PC1 = X_train[, 1], PC2 = X_train[, 2], y = y_train)
  e1071::svm(
    y ~ .,
    data   = df,
    kernel = "radial",
    cost   = cost,
    gamma  = gamma,
    scale  = FALSE,
    probability = FALSE
  )
}

predict_svm <- function(model, X_pred) {
  df <- data.frame(PC1 = X_pred[, 1], PC2 = X_pred[, 2])
  predict(model, df)
}

run_svm_pca2_tvt <- function(dados, split,
                             cost_grid = c(0.1, 1, 10),
                             gamma_grid = c(0.1, 1, 10)) {
  tr <- split$train
  va <- split$val
  te <- split$test

  dados_tr <- dados[tr, , drop = FALSE]
  dados_va <- dados[va, , drop = FALSE]
  dados_te <- dados[te, , drop = FALSE]

  # Fit PCA on TRAIN only
  pca_obj <- prep_pca2_from_train(dados_tr)

  # Apply PCA to TRAIN/VAL/TEST
  tr_p <- apply_pca2(dados_tr, pca_obj)
  va_p <- apply_pca2(dados_va, pca_obj)
  te_p <- apply_pca2(dados_te, pca_obj)

  Xtr <- tr_p$PCs; ytr <- tr_p$y
  Xva <- va_p$PCs; yva <- va_p$y
  Xte <- te_p$PCs; yte <- te_p$y

  # Validation grid search
  best_acc <- -Inf
  best_cost <- NA
  best_gamma <- NA

  for (C in cost_grid) {
    for (g in gamma_grid) {
      model <- fit_svm(Xtr, ytr, cost = C, gamma = g)
      yhat_va <- predict_svm(model, Xva)
      a <- acc(yva, yhat_va)

      if (a > best_acc) {
        best_acc <- a
        best_cost <- C
        best_gamma <- g
      }
    }
  }

  # Refit on TRAIN+VAL (same PCA trained on TRAIN)
  Xtrva <- rbind(Xtr, Xva)
  ytrva <- factor(c(as.character(ytr), as.character(yva)), levels = levels(ytr))

  model_best <- fit_svm(Xtrva, ytrva, cost = best_cost, gamma = best_gamma)
  yhat_te <- predict_svm(model_best, Xte)

  list(
    best_cost = best_cost,
    best_gamma = best_gamma,
    acc_val = best_acc,
    acc_test = acc(yte, yhat_te)
  )
}

# ============================================================
# Extractor: get a data.frame where last col is class
# ============================================================
to_dados_df <- function(obj) {
  # Case 1: list(X,y)
  if (is.list(obj) && all(c("X", "y") %in% names(obj))) {
    return(data.frame(obj$X, Classe = obj$y))
  }

  # Case 2: list(data=..., class/target=...)
  if (is.list(obj) && ("data" %in% names(obj))) {
    X <- obj$data
    yname <- intersect(names(obj), c("y", "class", "Class", "target", "label"))
    if (length(yname) >= 1) {
      y <- obj[[yname[1]]]
      return(data.frame(X, Classe = y))
    }
  }

  # Case 3: data.frame/matrix
  if (is.data.frame(obj) || is.matrix(obj)) {
    df <- as.data.frame(obj)
    return(df)
  }

  stop("Unknown dataset format.")
}

# ============================================================
# MAIN: run all datasets
# ============================================================
run_svm_pca2_all <- function(datasets,
                             seeds = 1:10,
                             cost_grid = c(0.1, 1, 10),
                             gamma_grid = c(0.1, 1, 10),
                             out_dir = "results") {
  dir.create(out_dir, showWarnings = FALSE)

  raw <- data.frame(
    dataset = character(),
    seed = integer(),
    best_cost = numeric(),
    best_gamma = numeric(),
    acc_val = numeric(),
    acc_test = numeric(),
    stringsAsFactors = FALSE
  )

  failed <- character()

  for (dname in names(datasets)) {
    cat("\n====================================\n")
    cat("Dataset:", dname, "\n")
    cat("====================================\n")

    obj <- datasets[[dname]]

    dados <- tryCatch(to_dados_df(obj), error = function(e) NULL)
    if (is.null(dados)) {
      cat("  [FAIL] could not convert dataset:", dname, "\n")
      failed <- c(failed, dname)
      next
    }

    dados <- as.data.frame(dados)
    y_all <- as.factor(dados[, ncol(dados)])

    for (s in seeds) {
      split <- make_tvt_split(y_all, seed = s)

      res <- tryCatch(
        run_svm_pca2_tvt(dados, split, cost_grid, gamma_grid),
        error = function(e) NULL
      )

      if (is.null(res)) {
        cat("  [FAIL] seed", s, "for dataset", dname, "\n")
        raw <- rbind(raw, data.frame(
          dataset = dname, seed = s,
          best_cost = NA, best_gamma = NA,
          acc_val = NA, acc_test = NA
        ))
      } else {
        raw <- rbind(raw, data.frame(
          dataset = dname, seed = s,
          best_cost = res$best_cost,
          best_gamma = res$best_gamma,
          acc_val = res$acc_val,
          acc_test = res$acc_test
        ))
      }
    }
  }

  summary <- raw %>%
    group_by(dataset) %>%
    summarise(
      acc_val_mean  = mean(acc_val, na.rm = TRUE),
      acc_val_sd    = sd(acc_val, na.rm = TRUE),
      acc_test_mean = mean(acc_test, na.rm = TRUE),
      acc_test_sd   = sd(acc_test, na.rm = TRUE),
      best_cost_mode  = suppressWarnings(as.numeric(names(sort(table(best_cost), decreasing = TRUE)[1]))),
      best_gamma_mode = suppressWarnings(as.numeric(names(sort(table(best_gamma), decreasing = TRUE)[1]))),
      .groups = "drop"
    ) %>%
    arrange(desc(acc_test_mean))

  # SAVE CSVs
  write.csv(raw, file.path(out_dir, "svm_pca2_all_raw.csv"), row.names = FALSE)
  write.csv(summary, file.path(out_dir, "svm_pca2_all_summary.csv"), row.names = FALSE)

  if (length(failed) > 0) {
    writeLines(failed, con = file.path(out_dir, "svm_pca2_failed_datasets.txt"))
    cat("\nFailed datasets saved at:", file.path(out_dir, "svm_pca2_failed_datasets.txt"), "\n")
  }

  # PRINT requested columns in console
  cat("\n=============================\n")
  cat("SVM SUMMARY (acc_test_mean / sd)\n")
  cat("=============================\n")

  print(
    summary %>%
      select(dataset, acc_test_mean, acc_test_sd) %>%
      mutate(
        acc_test_mean = round(acc_test_mean, 4),
        acc_test_sd   = round(acc_test_sd, 4)
      )
  )

  cat("\nSaved results:\n")
  cat(" -", file.path(out_dir, "svm_pca2_all_raw.csv"), "\n")
  cat(" -", file.path(out_dir, "svm_pca2_all_summary.csv"), "\n")

  invisible(list(raw = raw, summary = summary, failed = failed))
}
rm(list = ls())
cat("Starting run_svm_main.R ...\n")

source("src/carregar_datasets.R")
stopifnot(exists("datasets"))

source("src/run_svm_pca2_all_datasets.R")  # só define funções

run_svm_pca2_all(
  datasets = datasets,
  seeds = 1:10,
  cost_grid = c(0.1, 1, 10),
  gamma_grid = c(0.1, 1, 10),
  out_dir = "results"
)
