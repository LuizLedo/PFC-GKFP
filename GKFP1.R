
# Luiz Ledo Mota Melo Junior
# Quando zeta=0 temos o GK
# X dados em matriz 


GKFP1 <- function(X, c, m = 2,zeta=0,focal = "media",FP_custom = NULL, max.iter = 100, epsilon = 1e-5) {
  n <- nrow(X)
  p <- ncol(X)


 # Definir ponto focal
  FP <- switch(focal,
               "media" = colMeans(X),
               "extremo" = {
                 media <- colMeans(X)
                 distancias <- apply(X, 1, function(x) sum((x - media)^2))
                 X[which.max(distancias), ]
               },
               "custom" = {
                 if (is.null(FP_custom)) stop("For 'custom' focal, provide FP_custom.")
                 FP_custom
               },
               stop("Opção inválida para 'focal'. Use 'media', 'extremo' ou 'custom'.")
  )

 
  # Inicialização aleatória da matriz de pertinência
  U <- matrix(runif(n * c), nrow = n, ncol = c)
  U <- U / rowSums(U)
  
  # Inicializar variáveis
  iter <- 0
  obj_func_old <- 0
  
  repeat {
    iter <- iter + 1
    
    # Atualizar centroides
    U_m <- U^m
    V <- (t(U_m) %*% X+ zeta * FP)  / (colSums(U_m)+ zeta)
    
    # Atualizar matrizes A_k (covariância normalizada)
    A_list <- vector("list", c)
    F_list <- vector("list", c)

    for (k in 1:c) {
        
      x_diff <- sweep(X, 2, V[k, ])
      x_diff_FP<-sweep(t(FP), 2, V[k, ])
     
      S_k <- matrix(0, p, p)
      for (i in 1:n) {
        S_k <- S_k + U_m[i, k] * ((x_diff[i, ]) %*% t(x_diff[i, ]))
      }
      S_k =S_k+ zeta*(t(x_diff_FP) %*% (x_diff_FP))

      A_k <- S_k / sum(U_m[, k])
      A_k= diag(diag(A_k)) 
     epsilon <- 1e-5
     A_k <- A_k + diag(epsilon, ncol(A_k)) 
      F_k= diag(diag(A_k)) 
      det_S <-(det(A_k))
      A_k <- (det_S^(-1 / p)) * A_k  # Normalização GK
      A_list[[k]] <- A_k
      F_list[[k]] <- F_k

    }


    A_array <- array(NA, dim = c(p, p, c))
    F_array <- array(NA, dim = c(p, p, c))

    for (k in 1:c) {
      A_array[, , k] <- A_list[[k]]
      F_array[, , k] <- F_list[[k]]

    }
    
    # Atualizar distâncias
    D <- matrix(0, n, c)
    for (k in 1:c) {
      A_inv <- solve(A_list[[k]])
      for (i in 1:n) {
        diff <- X[i, ] - V[k, ]
        D[i, k] <- t(diff) %*% A_inv %*% diff
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
  
  return(list(U = U, V = V, F=F_array, M =  A_array, iter = iter, obj = obj_func))
}


