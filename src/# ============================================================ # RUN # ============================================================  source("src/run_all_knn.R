# ============================================================
# RUN
# ============================================================

source("src/carregar_datasets.R")
if (!exists("datasets")) stop("Object 'datasets' not found after sourcing carregar_datasets.R.")
source("src/run_knn_pca2_all.R")
run_knn_pca2_all(
  datasets = datasets,
  seeds = 1:10,
  k_grid = c(1, 3, 5, 7, 9),
  out_dir = "results"
)
