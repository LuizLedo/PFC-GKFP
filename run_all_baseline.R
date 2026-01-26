# run_all_baseline.R

# 1) Carrega funções
source("carregar_dataset.R")      # cria datasets (ou função carregar_datasets())
source("pca_2pc_prcomp.R")        # sua função PCA
source("baseline_models.R")       # sua baseline_models()
source("run_baseline.R")          # sua função run_baseline()

# 2) Rodar
out <- run_baseline(datasets, n_repeticoes = 10, seed = 123, usar_pca = TRUE)

# 3) Mostrar resultados
print(out$matriz_media)
print(out$matriz_desvio)
