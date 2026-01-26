pca_2pc_prcomp <- function(dados) {
  dados <- as.data.frame(dados)

  # classe na última coluna
  y <- as.factor(dados[, ncol(dados)])
  X <- dados[, -ncol(dados), drop = FALSE]

  # char -> factor
  X[] <- lapply(X, function(col) {
    if (is.character(col)) as.factor(col) else col
  })

  # one-hot para categóricas
  X_mm <- model.matrix(~ . - 1, data = X)

  # remove variância zero
  sds <- apply(X_mm, 2, sd, na.rm = TRUE)
  X_mm <- X_mm[, sds > 0, drop = FALSE]

  # remove NA
  ok <- complete.cases(X_mm) & !is.na(y)
  X_mm <- X_mm[ok, , drop = FALSE]
  y    <- y[ok]

  # PCA com scale=TRUE
  pca <- prcomp(X_mm, center = TRUE, scale. = TRUE)

  PCs <- pca$x[, 1:2, drop = FALSE]

  dados_pca <- data.frame(
    PC1 = PCs[, 1],
    PC2 = PCs[, 2],
    Classe = y
  )

  return(dados_pca)
}
