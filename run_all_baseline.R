# run_all_baselines_main.R
# Builds final table: rows = datasets, columns = classifiers
# Values shown as mean ± sd (test accuracy)

rm(list = ls())
cat("Starting run_all_baselines_main.R ...\n")

suppressPackageStartupMessages({
  library(dplyr)
})

dir.create("results", showWarnings = FALSE)

# ------------------------------------------------------------
# 1) Load datasets
# ------------------------------------------------------------
source("src/carregar_datasets.R")
stopifnot(exists("datasets"))

# ------------------------------------------------------------
# 2) Run baselines (each script already runs 10 seeds internally)
# ------------------------------------------------------------
source("src/run_knn_pca2_all_datasets.R")
source("src/run_svm_pca2_all_datasets.R")
source("src/run_tree_tvt_all_datasets.R")   # saves tree_all_summary.csv

# ------------------------------------------------------------
# 3) Read summaries
# ------------------------------------------------------------
read_summary <- function(path, tag) {
  if (!file.exists(path)) stop("Summary file not found: ", path)

  df <- read.csv(path, stringsAsFactors = FALSE)

  # expected columns: dataset, acc_test_mean, acc_test_sd
  stopifnot(all(c("dataset", "acc_test_mean", "acc_test_sd") %in% names(df)))

  df %>%
    select(dataset, acc_test_mean, acc_test_sd) %>%
    rename(
      !!paste0(tag, "_mean") := acc_test_mean,
      !!paste0(tag, "_sd")   := acc_test_sd
    )
}

knn_sum  <- read_summary("results/knn_pca2_all_summary.csv", "knn")
svm_sum  <- read_summary("results/svm_pca2_all_summary.csv", "svm")
tree_sum <- read_summary("results/tree_all_summary.csv",    "dt")  # <- CORRIGIDO

# ------------------------------------------------------------
# 4) Merge and format (mean ± sd)
# ------------------------------------------------------------
final_table <- knn_sum %>%
  full_join(svm_sum,  by = "dataset") %>%
  full_join(tree_sum, by = "dataset") %>%
  arrange(dataset)

fmt_mean_sd <- function(mu, sd) {
  ifelse(is.na(mu) | is.na(sd), NA_character_, sprintf("%.4f \u00B1 %.4f", mu, sd))
}

final_table_fmt <- final_table %>%
  mutate(
    KNN = fmt_mean_sd(knn_mean, knn_sd),
    SVM = fmt_mean_sd(svm_mean, svm_sd),
    DT  = fmt_mean_sd(dt_mean,  dt_sd)
  ) %>%
  select(dataset, KNN, SVM, DT)

# ------------------------------------------------------------
# 5) Print results
# ------------------------------------------------------------
cat("\n========================================\n")
cat("FINAL TEST ACCURACY (mean \u00B1 sd)\n")
cat("========================================\n")
print(final_table_fmt, row.names = FALSE)

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
