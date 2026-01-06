#Supervised
#X(dados) tem que ser na forma de matriz
#require(MASS)
supervised <- function(X, c, m = 2, max.iter = 100, epsilon = 1e-5) {
  n <- nrow(X)
  p <- ncol(X)
  
  # Inicialização aleatória da matriz de pertinência
  U <- matrix(runif(n * c), nrow = n, ncol = c)
  U <- U / rowSums(U)
  
  # Inicializar variáveisX
  iter <- 0
  obj_func_old <- 0
  
  repeat {

    iter <- iter + 1
    
    # Atualizar centroides
    U_m <- U^m
    V <- t(U_m) %*% X / colSums(U_m)
    # Pesos de cada classe
    Pi <- colSums(U) / nrow(U)
  

    # Atualizar matrizes A_k (covariância normalizada)
    A_list <- vector("list", c)
    for (k in 1:c) {
      x_diff <- sweep(X, 2, V[k, ])
      S_k <- matrix(0, p, p)
      for (i in 1:n) {
        S_k <- S_k + U_m[i, k] * (x_diff[i, ] %*% t(x_diff[i, ]))
      }
      A_k <- S_k / sum(U_m[, k])
      A_k= diag(diag(A_k)) + diag(rep(1e-6, p))
      A_list[[k]] <- A_k
    }
    
    
    A_array <- array(NA, dim = c(p, p, c))
    for (k in 1:c) {
      A_array[, , k] <- A_list[[k]]
    }

    # Atualizar distâncias
    D <- matrix(0, n, c)
    for (k in 1:c) {
      A_inv <- solve(A_list[[k]])
      for (i in 1:n) {
        diff <- X[i, ] - V[k, ]
        #D[i, k] <-(t(diff) %*% (A_inv) %*% diff)-log(det(solve(A_inv))) - log( Pi[k])
         D[i, k] <-(t(diff) %*% ginv(A_inv) %*% diff)-log(Pi[k])
      }
    }
   
    # Atualizar U
    for (i in 1:n) {
      for (k in 1:c) {
        denom <- sum((D[i, k] / D[i, ])^(1 / (m - 1)))
        U[i, k] <- 1 / denom
      }
    }
    
    # Função objetivo
    obj_func <- sum(U_m * D)
    
    if (abs(obj_func - obj_func_old) < epsilon || iter >= max.iter) break
    obj_func_old <- obj_func
  }
  
  return(list(U = U, V = V, M =  A_array, iter = iter, obj = obj_func))
}
