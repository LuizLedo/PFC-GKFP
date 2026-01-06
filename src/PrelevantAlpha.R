  
    #Luiz Ledo Mota Melo Junior 
    #P[i,j]= Prob da classe i na regra j
    # w  confiança Regra j 
    #alpha: alpha_corte
    PrelevantAlpha <- function(beta1, label, R, alpha, M) {
      C_labels <- max(label)
      Nx <- nrow(beta1)
      nx <- ncol(beta1)
      P <- matrix(0, nrow = C_labels, ncol = R)
      Vcard <- rep(0, R)
      w <- rep(0, R)
      
      for (j in 1:R) {
        aux <- cbind(beta1[,j], label)
        pos <- which(aux[,1] >= alpha)
        Vcard[j] <- length(pos)
        aux2 <- aux[pos, , drop = FALSE]
        for (i in 1:C_labels) {
          if (Vcard[j] == 0) {
            P[i, j] <- 0
          } else {
            P[i, j] <- sum(aux2[,2] == i) / Vcard[j]
          }
        }
      }
      for (j in 1:R) {
        w[j] <- (Vcard[j] / Nx) * sqrt(det(M[,,j])) / ((2 * pi)^(nx / 2))
      }
      if (max(w) > 0) w <- w / max(w)
      w[w < 0.001] <- 0
      return(list(P = P, w = w))
    }
