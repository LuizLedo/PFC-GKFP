# ==== Funções auxiliares ====
limpar_strings <- function(dados) {
  dados[] <- lapply(dados, function(x) if (is.character(x)) trimws(x) else x)
  return(dados)
}

amostrar <- function(dados, n) {
  if (nrow(dados) > n) {
    set.seed(123)
    dados <- dados[sample(nrow(dados), n), ]
  }
  return(dados)
}

# ==== Carregamento dos datasets ====
datasets <- list(
  "Adult"      = {
    dados=read.table("Adult.txt", sep = ",")
    dados=dados[,c(8,9,10,15)]
    },

  "Banana"     = read.table("Banana.txt"),
  "Blood"      = read.table("Blood.txt"),
  
  "CTG"        = {
    dados <- read.table("CTG.txt")
    dados=dados[,c(22,5,2,23)]
    dados
  },
  
  "Diabetes"   = {
    dados <- read.table("Diabetes.txt", head = FALSE)
    dados[, 9] <- ifelse(dados[, 9] == 1, 2, 1)
    dados=dados[,c(2,6,8,9)]
    dados
  },
  
  "Ecoli"      = {
  dados=read.table("Ecoli.txt", sep = "", head = FALSE)
  dados$V8 <- factor(dados$V8,
                     levels = c(1,2,3,4,5,6,7,8),
                     labels = c("Alta", "Alta", "Baixa", "Baixa", "Média", "Média", "Baixa", "Média"))
dados
},
  
  "Faults"     = {
    dados <- read.table("Faults.txt", sep = ",")
    dados <- limpar_strings(dados)
    dados <- dados[, c(5,8,18,6,23,22,28)]
    amostrar(dados, 500)
  },
  
  "German"     = read.table("German.txt"),
  "Glass"      = read.table("Glass.txt"),
  "Haberman"   = {
    dados=read.table("Haberman.txt")
    dados= dados[,c(3,1,4)]
   },


  "Heart"      = {
   dados=read.table("Heart.txt", sep = ",")
   dados=dados[,c(13,12,3,8,14)]
    },
   
  "ILPD"       = read.table("ILPD.txt", sep = ","),
  
  "Ionosphere" = {
    dados <- read.table("Ionosphere.txt", sep = ",", head = FALSE)
    dados <- dados[, -2]
    dados
  },
  
  "Laryngeal1" = read.table("Laryngeal1.txt", sep = ","),
  "Laryngeal3" = read.table("Laryngeal3.txt", sep = ","),
  "Lithuanian" = read.table("Lithuanian.txt"),
  "Liver"={

# 1) Leitura
Liver <- read.table("Liver.txt", header = TRUE, stringsAsFactors = FALSE)

# 2) Renomear colunas conforme BUPA clássico
names(Liver) <- c("mcv","alkphos","sgpt","sgot","gammagt","drinks","target")

# 3) Log1p nas enzimas
Liver_trans <- Liver
Liver_trans$alkphos <- log1p(Liver_trans$alkphos)
Liver_trans$sgpt    <- log1p(Liver_trans$sgpt)
Liver_trans$sgot    <- log1p(Liver_trans$sgot)
Liver_trans$gammagt <- log1p(Liver_trans$gammagt)

# 4) Criar variáveis derivadas (razões clínicas)
Liver_trans$ast_alt <- Liver_trans$sgot / pmax(Liver_trans$sgpt, 1e-6)
Liver_trans$ggt_alk <- Liver_trans$gammagt / pmax(Liver_trans$alkphos, 1e-6)

# 5) Reordenar para garantir que o target esteja no fim
Liver_completo <- Liver_trans[, c("mcv","alkphos","sgpt","sgot",
                                  "gammagt","drinks","ast_alt","ggt_alk","target")]

},

  "Mammo"      = read.table("Mammo.txt", sep = ","),
  
  "Magic"      = {
    dados <- read.table("Magic.txt", sep = ",", strip.white = FALSE)
    dados <- limpar_strings(dados)
    amostrar(dados, 300)
  },
  
  "Monk"       = {
    dados <- read.table("Monk.txt", head = FALSE)
    dados[, 7] <- ifelse(dados[, 7] == 1, 2, 1)
    dados=dados[,c(2,5,7)] 
    dados
  },
  
  "Phoneme"    = {
    dados <- read.table("Phoneme.txt", sep = ",", strip.white = FALSE)
    dados <- limpar_strings(dados)
    amostrar(dados, 500)
  },
  
  "Segmentation" = {
    dados <- read.table("Segmentation.txt", sep = "", strip.white = FALSE)
    dados <- limpar_strings(dados)
    dados <- dados[, c(11,13,17,19,20)]
    amostrar(dados, 500)
  },
  
  "Sonar"      = {
    dados <- read.table("Sonar.txt", sep = ",", strip.white = FALSE)
    dados <- dados[, c(11, 10, 9, 12, 13, 8, 61)]
    limpar_strings(dados)
  },
  
  "Thyroid"    = read.table("Thyroid.txt", sep = ",", strip.white = FALSE),
  "Vehicle"    = read.table("Vehicle.txt", sep = ",", strip.white = FALSE),
  
 "Vertebral" ={
   Vertebral= read.table("Vertebral.txt", sep = ",", strip.white = FALSE)

# Supondo que Vertebral já está carregado com colunas V1..V7
# Onde V1..V6 são preditores e V7 é o target

# 1) Separar preditores e target
X <- Vertebral[, 1:6]
y <- Vertebral[, 7, drop = FALSE]

# 2) Construir as features importantes segundo glmnet
X_sel <- data.frame(
  V2       = X$V2,
  V4       = X$V4,
  V5       = X$V5,
  V6       = X$V6,
  V5_2     = X$V5^2,
  V2_x_V4  = X$V2 * X$V4,
  V2_x_V5  = X$V2 * X$V5,
  V2_x_V6  = X$V2 * X$V6,
  V3_x_V4  = X$V3 * X$V4,
  V3_x_V5  = X$V3 * X$V5,
  V3_x_V6  = X$V3 * X$V6,
  V5_x_V6  = X$V5 * X$V6
)

# 3) Conjunto final com target na última coluna
Vertebral_sel <- cbind(X_sel, target = y)

},
 "WBC"        = read.table("WBC.txt", sep = ","),
  
  "WDVG"       = {
    dados <- read.table("WDVG.txt", sep = ",", strip.white = FALSE)
    dados <- dados[, c(6, 11, 9, 15, 12, 10, 5, 22)]
    dados <- limpar_strings(dados)
    amostrar(dados, 500)
  },
  
  "Weaning"    = {
    dados <- read.table("Weaning.txt", sep = ",", strip.white = FALSE)
    dados <- dados[, c(6, 7, 13, 14, 18)]
    limpar_strings(dados)
  },
  
  "Wine"       = read.table("Wine.txt", sep = ",")
)

# ==== Metadados dos datasets ====
info_datasets <- list(
  Adult      = list(R = 2, usar_pred = TRUE),
  Banana     = list(R = 2, usar_pred = TRUE),
  Blood      = list(R = 2, usar_pred = TRUE),
  CTG        = list(R = 3, usar_pred = TRUE),
  Diabetes   = list(R = 2, usar_pred = TRUE),
  Ecoli      = list(R = 6, usar_pred = TRUE),
  Faults     = list(R = 7, usar_pred = TRUE),
  German     = list(R = 2, usar_pred = TRUE),
  Glass      = list(R = 6, usar_pred = TRUE),
  Haberman   = list(R = 2, usar_pred = TRUE),
  Heart      = list(R = 4, usar_pred = TRUE),
  ILPD       = list(R = 2, usar_pred = TRUE),
  Ionosphere = list(R = 2, usar_pred = TRUE),
  Laryngeal1 = list(R = 2, usar_pred = TRUE),
  Laryngeal3 = list(R = 3, usar_pred = TRUE),
  Lithuanian = list(R = 2, usar_pred = TRUE),
  Liver      = list(R = 2, usar_pred = TRUE),
  Magic      = list(R = 2, usar_pred = TRUE),
  Mammo      = list(R = 2, usar_pred = TRUE),
  Monk       = list(R = 2, usar_pred = TRUE),
  Phoneme    = list(R = 2, usar_pred = TRUE),
  Segmentation = list(R = 7, usar_pred =TRUE),
  Sonar      = list(R = 2, usar_pred = TRUE),
  Thyroid    = list(R = 2, usar_pred = TRUE),
  Vehicle    = list(R = 4, usar_pred = TRUE),
  Vertebral  = list(R = 2, usar_pred = TRUE),
  WBC        = list(R = 2, usar_pred = TRUE),
  WDVG       = list(R = 3, usar_pred = TRUE),
  Weaning    = list(R = 2, usar_pred = TRUE),
  Wine       = list(R = 3, usar_pred = TRUE)
)
