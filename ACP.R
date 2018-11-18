library(plotrix)
library(ade4)

acpn = function(data_in, round_precision = 3){  
  result = list()
  
  result$source = data_in
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
  result$inertiaratio = result$eigen$values / num_col
  
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
  
  #Cercle de corrélation
  apply(combn(1:length(result$eigen$values), 2), 2, function(combi){
    axis1 = result$coords[,combi[1]]
    axis2 = result$coords[,combi[2]]
    par(mfrow=c(1,1))
    plot(axis1, axis2, xlim=c(-1,1), ylim=c(-1,1), asp = 1, type = "n", xlab = sprintf("Axe %d",combi[1]), ylab = sprintf("Axe %d",combi[2]), main = sprintf("Cercle des corrélations dans le plan %d-%d", combi[1], combi[2]) )
    abline(h=0,v=0)
    draw.circle(x = 0, y = 0, radius = c(1,1))
    arrows(0, 0, axis1, axis2, length = 0.10, code = 2, col = gray(0.6))
    text(x=axis1, y=axis2, labels = colnames(result$raw_data), font=2) 
  })
  
  #Retour
  return(result)
}

addIndiv = function(acp, indiv){
  return( t(scale(as.matrix(na.omit(indiv[sapply(indiv, is.numeric)]), rownames.force = NA), center = TRUE, scale = TRUE) %*% acp$eigen$vectors ))
}


waters <- read.table("./Eaux2018 FM.txt", header=TRUE, sep="\t")
active.waters <- waters[waters$Pays %in% "France",] 
supp.waters <- waters[waters$Pays %in% "Maroc",] 
acp_waters = acpn(active.waters)

supps = t(addIndiv(acp_waters, supp.waters))

water_type = c()
for(i in rownames(acp_waters$comp)){
  type = acp_waters$source$Nature[strtoi(i)]
  v = 0
  print(type)
  if(is.na(type)){}
  else if(type == "plat"){
    v=1
  }else if(type == "gaz"){
    v=2
  }
  water_type = c(water_type, v)
}

plot(acp_waters$comp[,1], acp_waters$comp[,2], xlab = "Axe 1", ylab = "Axe 2", col=water_type)
legend(x = "bottomright",legend=unique(water_type),col=1:length(water_type),pch=1)

#eaux_actives <- filter(eaux, Pays== "France")
dudi = dudi.pca(df = acp_waters$raw_data, scannf = FALSE, nf = 3)

acp_waters$matrix[setdiff(c(1:nrow(acp_waters$matrix)), unique(unlist(apply(acp_waters$matrix, 2, function(x){which(x %in% boxplot.stats(x)$out)})))),]

