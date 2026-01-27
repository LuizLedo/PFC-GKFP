# run_all_baseline.R
library(ggplot2)
library(lattice)
library(class)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
# ============================================================
# run_all_baselines_main.R
# Builds final table: rows = datasets, columns = classifiers
# Values shown as mean ± sd (test accuracy)
# ============================================================

rm(list = ls())
cat("Starting run_all_baselines_main.R ...\n")

suppressPackageStartupMessages({
  library(dplyr)
})

# ------------------------------------------------------------
# 1) Load datasets and functions
# ------------------------------------------------------------
source("src/carregar_datasets.R")
stopifnot(exists("datasets"))

source("src/run_knn_pca2_all_datasets.R")
source("src/run_svm_pca2_all_datasets.R")
source("src/run_tree_tvt_all_datasets.R")

dir.create("results", showWarnings = FALSE)

# ------------------------------------------------------------
# 2) Run classifiers
# ------------------------------------------------------------

cat("\nRunning KNN...\n")
run_knn_pca2_all(
  datasets = datasets,
  seeds = 1:10,
  k_grid = c(1,3,5,7,9),
  out_dir = "results"
)

cat("\nRunning SVM...\n")
run_svm_pca2_all(
  datasets = datasets,
  seeds = 1:10,
  cost_grid = c(0.1, 1, 10),
  gamma_grid = c(0.1, 1),
  out_dir = "results"
)

cat("\nRunning Decision Tree...\n")
run_dt_pca2_all(
  datasets = datasets,
  seeds = 1:10,
  cp_grid = c(0.001, 0.01, 0.05, 0.1),
  out_dir = "results"
)

# ------------------------------------------------------------
# 3) Read summaries
# ------------------------------------------------------------

read_summary <- function(path, tag) {
  df <- read.csv(path, stringsAsFactors = FALSE)
  df %>%
    select(dataset, acc_test_mean, acc_test_sd) %>%
    rename(
      !!paste0(tag, "_mean") := acc_test_mean,
      !!paste0(tag, "_sd")   := acc_test_sd
    )
}

knn_sum  <- read_summary("results/knn_pca2_all_summary.csv",  "knn")
svm_sum  <- read_summary("results/svm_pca2_all_summary.csv",  "svm")
tree_sum <- read_summary("results/dt_pca2_all_summary.csv",   "dt")

# ------------------------------------------------------------
# 4) Merge and format (mean ± sd)
# ------------------------------------------------------------

final_table <- knn_sum %>%
  full_join(svm_sum,  by = "dataset") %>%
  full_join(tree_sum, by = "dataset") %>%
  arrange(dataset)

final_table_fmt <- final_table %>%
  mutate(
    KNN = sprintf("%.4f ± %.4f", knn_mean, knn_sd),
    SVM = sprintf("%.4f ± %.4f", svm_mean, svm_sd),
    DT  = sprintf("%.4f ± %.4f",  dt_mean,  dt_sd)
  ) %>%
  select(dataset, KNN, SVM, DT)

# ------------------------------------------------------------
# 5) Print results
# ------------------------------------------------------------

cat("\n========================================\n")
cat("FINAL TEST ACCURACY (mean ± sd)\n")
cat("========================================\n")
print(final_table_fmt)

# ------------------------------------------------------------
# 6) Save final table
# ------------------------------------------------------------

write.csv(
  final_table_fmt,
  "results/final_baselines_accuracy_mean_sd.csv",
  row.names = FALSE
)

cat("\nSaved:\n")
cat(" - results/final_baselines_accuracy_mean_sd.csv\n")
