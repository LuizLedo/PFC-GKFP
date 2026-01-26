# run_tree_tvt_all_datasets.R
# Decision Tree (rpart) under TVT 50/25/25 with validation selection
# Repetitions: seeds 1:10
# Saves CSV in results/

suppressPackageStartupMessages({
  library(rpart)
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

  # Case 3: data.frame/matrix already
  if (is.data.frame(obj) || is.matrix(obj)) {
    return(as.data.frame(obj))
  }

  stop("Unknown dataset format.")
}

# -----------------------------
# Fit / Predict (rpart)
# -----------------------------
fit_tree <- function(dados_train, cp, maxdepth, minsplit) {
  dados_train <- as.data.frame(dados_train)

  yname <- names(dados_train)[ncol(dados_train)]
  dados_train[[yname]] <- as.factor(dados_train[[yname]])

  # make characters into factors (rpart handles factors)
  for (j in seq_len(ncol(dados_train) - 1)) {
    if (is.character(dados_train[[j]])) dados_train[[j]] <- as.factor(dados_train[[j]])
  }

  # remove NAs
  ok <- complete.cases(dados_train)
  dados_train <- dados_train[ok, , drop = FALSE]

  rpart::rpart(
    formula = as.formula(paste0(yname, " ~ .")),
    data = dados_train,
    method = "class",
    control = rpart.control(
      cp = cp,
      maxdepth = maxdepth,
      minsplit = minsplit,
      xval = 0
    )
  )
}

predict_tree <- function(model, dados_pred) {
  dados_pred <- as.data.frame(dados_pred)
  yname <- names(dados_pred)[ncol(dados_pred)]

  # ensure same type handling
  for (j in seq_len(ncol(dados_pred) - 1)) {
    if (is.character(dados_pred[[j]])) dados_pred[[j]] <- as.factor(dados_pred[[j]])
  }

  # remove NAs (keep y aligned)
  ok <- complete.cases(dados_pred)
  dados_pred <- dados_pred[ok, , drop = FALSE]

  probs <- predict(model, newdata = dados_pred, type = "prob")
  pred  <- colnames(probs)[max.col(probs, ties.method = "first")]
  list(pred = factor(pred, levels = colnames(probs)),
       y = as.factor(dados_pred[[yname]]))
}

run_tree_tvt <- function(dados, split,
                         cp_grid = c(0.001, 0.01, 0.05),
                         maxdepth_grid = c(3, 5, 10),
                         minsplit_grid = c(5, 10, 20)) {
  tr <- split$train
  va <- split$val
  te <- split$test

  dados_tr <- dados[tr, , drop = FALSE]
  dados_va <- dados[va, , drop = FALSE]
  dados_te <- dados[te, , drop = FALSE]

  # grid search on validation
  best_acc <- -Inf
  best <- list(cp = NA, maxdepth = NA, minsplit = NA)

  for (cp in cp_grid) {
    for (md in maxdepth_grid) {
      for (ms in minsplit_grid) {
        model <- fit_tree(dados_tr, cp = cp, maxdepth = md, minsplit = ms)

        pva <- predict_tree(model, dados_va)
        a <- acc(pva$y, pva$pred)

        if (a > best_acc) {
          best_acc <- a
          best$cp <- cp; best$maxdepth <- md; best$minsplit <- ms
        }
      }
    }
  }

  # refit on TRAIN + VAL with best hyperparams
  dados_trva <- rbind(dados_tr, dados_va)
  model_best <- fit_tree(dados_trva, cp = best$cp, maxdepth = best$maxdepth, minsplit = best$minsplit)

  pte <- predict_tree(model_best, dados_te)
  acc_test <- acc(pte$y, pte$pred)

  list(
    best_cp = best$cp,
    best_maxdepth = best$maxdepth,
    best_minsplit = best$minsplit,
    acc_val = best_acc,
    acc_test = acc_test
  )
}

# ============================================================
# MAIN: run all datasets
# ============================================================
run_tree_all <- function(datasets,
                         seeds = 1:10,
                         cp_grid = c(0.001, 0.01, 0.05),
                         maxdepth_grid = c(3, 5, 10),
                         minsplit_grid = c(5, 10, 20),
                         out_dir = "results") {
  dir.create(out_dir, showWarnings = FALSE)

  raw <- data.frame(
    dataset = character(),
    seed = integer(),
    best_cp = numeric(),
    best_maxdepth = integer(),
    best_minsplit = integer(),
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
    dados[[ncol(dados)]] <- as.factor(dados[[ncol(dados)]])

    for (s in seeds) {
      split <- make_tvt_split(dados[[ncol(dados)]], seed = s)

      res <- tryCatch(
        run_tree_tvt(dados, split, cp_grid, maxdepth_grid, minsplit_grid),
        error = function(e) NULL
      )

      if (is.null(res)) {
        cat("  [FAIL] seed", s, "for dataset", dname, "\n")
        raw <- rbind(raw, data.frame(
          dataset = dname, seed = s,
          best_cp = NA, best_maxdepth = NA, best_minsplit = NA,
          acc_val = NA, acc_test = NA
        ))
      } else {
        raw <- rbind(raw, data.frame(
          dataset = dname, seed = s,
          best_cp = res$best_cp,
          best_maxdepth = res$best_maxdepth,
          best_minsplit = res$best_minsplit,
          acc_val = res$acc_val,
          acc_test = res$acc_test
        ))
      }
    }
  }

  # mode helper
  mode1 <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    tab <- table(x)
    as.numeric(names(tab)[which.max(tab)])
  }

  summary <- raw %>%
    group_by(dataset) %>%
    summarise(
      acc_val_mean  = mean(acc_val, na.rm = TRUE),
      acc_val_sd    = sd(acc_val, na.rm = TRUE),
      acc_test_mean = mean(acc_test, na.rm = TRUE),
      acc_test_sd   = sd(acc_test, na.rm = TRUE),
      best_cp_mode = mode1(best_cp),
      best_maxdepth_mode = mode1(best_maxdepth),
      best_minsplit_mode = mode1(best_minsplit),
      .groups = "drop"
    ) %>%
    arrange(desc(acc_test_mean))

  write.csv(raw, file.path(out_dir, "tree_all_raw.csv"), row.names = FALSE)
  write.csv(summary, file.path(out_dir, "tree_all_summary.csv"), row.names = FALSE)

  if (length(failed) > 0) {
    writeLines(failed, con = file.path(out_dir, "tree_failed_datasets.txt"))
    cat("\nFailed datasets saved at:", file.path(out_dir, "tree_failed_datasets.txt"), "\n")
  }

  cat("\n=============================\n")
  cat("TREE SUMMARY (acc_test_mean / sd)\n")
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
  cat(" -", file.path(out_dir, "tree_all_raw.csv"), "\n")
  cat(" -", file.path(out_dir, "tree_all_summary.csv"), "\n")

  invisible(list(raw = raw, summary = summary, failed = failed))
}

# -----------------------------
# RUN
# -----------------------------
cat("Starting run_tree_tvt_all_datasets.R ...\n")

source("src/carregar_datasets.R")
stopifnot(exists("datasets"))

run_tree_all(
  datasets = datasets,
  seeds = 1:10,
  cp_grid = c(0.001, 0.01, 0.05),
  maxdepth_grid = c(3, 5, 10),
  minsplit_grid = c(5, 10, 20),
  out_dir = "results"
)
