  PrelevantCI <- function(beta1, label, R, p_CI, M) {
  nrows <- nrow(beta1)
  ncols <- 1
  p=p_CI
  CI <- matrix(0, nrow = nrows, ncol = ncols)
  C <- max(label)
  Nx <- nrow(beta1)
  nx <- ncol(beta1)
  P <- matrix(0, nrow = C, ncol = R)
  Vcard <- numeric(R)
  w <- numeric(R)
  
  for (j in 1:R) {
    aux <- cbind(beta1[, j], label)
    
    for (k in 1:nrows) {
      if (aux[k, 1] <= 0.5) {
        CI[k, 1] <- 2^(p - 1) * aux[k, 1]^p
      } else {
        CI[k, 1] <- 1 - (2^(p - 1) * (1 - aux[k, 1])^p)
      }
    }
    
    aux2 <- cbind(CI[, 1], label)
    Vcard[j] <- sum(aux2[, 1])
    
    for (i in 1:C) {
      aux1 <- which(aux2[, 2] == i)
      vero <- aux2[aux1, 1]
      if (Vcard[j] == 0) {
        P[i, j] <- 0
      } else {
        P[i, j] <- sum(vero) / Vcard[j]
      }
    }
  }
  
  for (j in 1:R) {
    w[j] <- (Vcard[j] / Nx) * sqrt(det(M[, , j])) / ((2 * pi)^(nx / 2))
  }
  
  if (max(w) > 0) {
    w <- w / max(w)
  }
  
  for (j in 1:R) {
    if (w[j] < 0.001) {
      w[j] <- 0
    }
  }
  
  return(list(P = P, w = w))
}