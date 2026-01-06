

#Correção dos labels
#em algoritmos de clustering (k-means, FKM, etc.), 
#porque os clusters gerados não têm ordem fixa — 
#por exemplo, o cluster 1 pode corresponder à classe “B” e 
#o cluster 2 à classe “A”
# Carregar  library(clue)
corrigir_permutacao_labels <- function(predicoes, y_real) {
      tab <- table(predicoes, y_real)
      if (nrow(tab) != ncol(tab)) {
        tamanho <- max(nrow(tab), ncol(tab))
        tab_aux <- matrix(0, nrow = tamanho, ncol = tamanho)
        tab_aux[1:nrow(tab), 1:ncol(tab)] <- tab
        tab <- tab_aux
      }
      assoc <- solve_LSAP(tab, maximum = TRUE)
      mapeamento <- as.integer(assoc)
      predicoes_corrigidas <- sapply(predicoes, function(p) {
        if (p <= length(mapeamento)) mapeamento[p] else p
      })
      return(predicoes_corrigidas)
    }
