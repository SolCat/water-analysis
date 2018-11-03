acpn = function(data_in){
  
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
  result$eigen = eigen(result$cor)
  
  #Retour
  return(result)
}

library(ade4)

eaux <- read.table("./Eaux2018 FM.txt", header=TRUE, sep="\t")
acp_eau = acpn(eaux)


#eaux_actives <- filter(eaux, Pays== "France")
#dudi.pca(df = acp_eau$raw_data, scannf = FALSE, nf = 2)
