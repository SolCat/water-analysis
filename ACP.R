acpn = function(data_in, round_precision = 3){
  
  result = list()
  #Filtre
  result$raw_data = na.omit(data_in[sapply(data_in, is.numeric)])
  result$matrix = as.matrix(result$raw_data, rownames.force = NA)
  num_row = nrow(result$raw_data)
  num_col = ncol(result$raw_data)
  
  #Moyenne
  result$mean = apply(result$matrix, 2, mean)
  result$meanmatrix = t(matrix( rep(x=result$mean, num_row), nrow = num_col, byrow = FALSE))

  #Centrage avec scale
  result$autocen = scale(result$matrix, center = TRUE, scale = FALSE)
  #Centrage/Reduction avec scale
  result$autocenred = scale(result$matrix, center = TRUE, scale = TRUE)

  #Centrage sans scale
  result$cen = result$matrix - result$meanmatrix

  #Variance/co-variance
  result$var = t(result$cen) %*% result$cen / (num_row - 1)
    
  #Centrage/Reduction sans scale
  result$stddev = diag(sapply(diag(as.matrix(result$var)), function(x){1/sqrt(x)}))
  result$cenred = result$cen %*% result$stddev
  
  #Correlation
  result$cor = t(result$cenred) %*% result$cenred / (num_row - 1)
  
  #Valeurs propres
  eigen_full = eigen(result$cor)
  result$eigen$values = eigen_full$values[eigen_full$values>1.0]
  result$eigen$vectors = eigen_full$vectors[,1:length(result$eigen$values)]
  
  #Composantes principales
  result$comp = result$cenred %*% result$eigen$vectors
  
  #Rapport de l'inertie
  result$inertiaratio = acp_eau$eigen$values / num_col
  
  #Inertie cumulée
  result$inertiasum = round(sum(result$inertiaratio),3)*100

  #Contribution relative des individus
  result$qlti = (result$comp)^2
  for (i in 1:nrow(result$qlti)) {
    for (k in 1:ncol(result$qlti)) {
      result$qlti[i,k] = result$qlti[i,k] / (sum(result$comp[i,]^2))
    }
  }
  result$qtli = round(result$qlti, round_precision)
  
  #Contribution absolue des individus
  result$ctri = (result$comp)^2
  for (i in 1:nrow(result$ctri)) {
    for (k in 1:ncol(result$ctri)) {
      result$ctri[i,k] = result$ctri[i,k] / (ncol(result$ctri)*result$eigen$values[k])
    }
  }
  result$ctri = round(result$ctri, round_precision)
  
  #Matrice des coordonnées de variables
  D = diag(sapply(as.matrix(result$eigen$values), sqrt))
  result$coords = result$eigen$vectors %*% D
  
  #Contribution relative des variables
  result$qltv = (result$coords)^2
  for (j in 1:nrow(result$qltv)) {
    for (k in 1:ncol(result$qltv)) {
      result$qltv[j,k] = result$qltv[j,k] / (sum(result$coords[j,]^2))
    }
  }
  result$qtlv = round(result$qltv, round_precision)
  
  #Contribution absolue des variables
  result$ctri = (result$comp)^2
  for (i in 1:nrow(result$ctri)) {
    for (k in 1:ncol(result$ctri)) {
      result$ctri[i,k] = result$ctri[i,k] / (ncol(result$ctri)*result$eigen$values[k])
    }
  }
  result$ctri = round(result$ctri, round_precision)
  
  #Retour
  return(result)
}

library(ade4)

eaux <- read.table("./Eaux2018 FM.txt", header=TRUE, sep="\t")
acp_eau = acpn(eaux)


#eaux_actives <- filter(eaux, Pays== "France")
#dudi.pca(df = acp_eau$raw_data, scannf = FALSE, nf = 3)
