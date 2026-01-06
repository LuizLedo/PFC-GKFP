#Luiz Ledo Mota Melo Junior 
# Beta= Grau de ativação da Regra
# Depende de Centroide e Matriz de Covariância Fuzzy



fuzeval <- function(x, C, M) {
      c <- nrow(C)  
      n <- ncol(C)  
      Nx <- nrow(x) 
      beta <- matrix(0, nrow = Nx, ncol = c) 
      
      for (j in 1:c) {
        invM <- solve(diag(diag(M[,,j])))
               for (i in 1:Nx) {
          diff <- as.matrix(x[i,] - C[j,])
          beta[i,j] <- exp(-0.5 * t(diff) %*% invM %*% diff)
        }
      }
      beta <- beta / rowSums(beta)
      return(beta)
    }
    
    