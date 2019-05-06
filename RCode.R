# Importation du jeu de données
waters <- read.table("/home/sol/Téléchargements/Eaux2018 FM.txt", header=TRUE, sep="\t")
attach(waters)

# 1. Analyse univariée et bivariée
## 1.1. Etude du jeu de données
head(waters)
dim(waters) # 95 lignes, 12 variables
names(waters)
lapply(waters, class) # 3 variables qualitatives ; 9 variables quantitatives
active.waters <- waters[waters$Pays %in% "France",]
nrow(active.waters) # 84 obs. actives
supp.waters <- waters[waters$Pays %in% "Maroc",]

nrow(supp.waters) # 11 obs. supplémentaires
num.variables <- waters[sapply(waters, is.numeric)] # variables quantitatives
cat.variables <- waters[sapply(waters, is.factor)] # variables qualitatives

## 1.2. Analyse exploratoire des données
### 1.2.1. Recherche des données manquantes/corrompues
colSums(is.na(waters))

### 1.2.2. Analyse unidimensionnelle : variables qualitatives
still.waters <- subset(waters, waters$Nature=='plat', select=Ca:PH)
gaz.waters <- subset(waters[sapply(waters, is.numeric)], waters$Nature=='gaz')
summary(still.waters)
summary(gaz.waters)
colMeans(still.waters, na.rm=TRUE)
colMeans(gaz.waters, na.rm=TRUE)
table(waters$Nature, waters$Pays) # Table de contingence des eaux (par nature et pays)
par(mfrow=c(1,1))
mosaicplot(~waters$Pays+waters$Nature, data=waters, main="Répartition des eaux par Nature et Pays", color=TRUE)

### 1.2.3. Analyse unidimensionnelle : variables quantitatives
summary(num.variables)
boxplot(num.variables, main="Boîtes à moustaches des variables quantitatives du jeu de données")
# Etude de la variable PH
summary(waters$PH)
par(mfrow=c(1,2))
boxplot(waters$PH, main="Boîte à moustache de la variable PH")
hist(waters$PH, prob=TRUE, main="Histogramme et densité de probabilité du PH", xlab="PH")
curve(dnorm(x, mean=mean(waters$PH, na.rm=TRUE), sd=sd(waters$PH, na.rm=TRUE)), add=TRUE, col="red")

# Etude de la variable NO3
summary(waters$NO3)
par(mfrow=c(1,2))
boxplot(waters$NO3, main="Boîte à moustache de la variable NO3")
hist(waters$NO3, prob=TRUE, main="Histogramme et densité de probabilité du NO3", xlab="NO3")
curve(dnorm(x, mean=mean(waters$NO3, na.rm=TRUE), sd=sd(waters$NO3, na.rm=TRUE)), add=TRUE, col="red")

# Etude des variables Cl, K, SO4, et HCO3
par(mfrow=c(2,2))
hist(waters$Cl, prob=TRUE, main="Histogramme et densité de probabilité du Cl", xlab="Cl")
curve(dnorm(x, mean=mean(waters$Cl, na.rm=TRUE), sd=sd(waters$Cl, na.rm=TRUE)), add=TRUE, col="red")
hist(waters$K, prob=TRUE, main="Histogramme et densité de probabilité du K", xlab="K")
curve(dnorm(x, mean=mean(waters$K, na.rm=TRUE), sd=sd(waters$K, na.rm=TRUE)), add=TRUE, col="red")
hist(waters$SO4, prob=TRUE, main="Histogramme et densité de probabilité du SO4", xlab="SO4")
curve(dnorm(x, mean=mean(waters$SO4, na.rm=TRUE), sd=sd(waters$SO4, na.rm=TRUE)), add=TRUE, col="red")
hist(waters$HCO3, prob=TRUE, main="Histogramme et densité de probabilité du HCO3", xlab="HCO3")
curve(dnorm(x, mean=mean(waters$HCO3, na.rm=TRUE), sd=sd(waters$HCO3, na.rm=TRUE)), add=TRUE, col="red")

### 1.2.4. Recherche des valeurs extrêmes/outliers
boxplot.stats(waters$SO4)$out
which(waters$SO4 %in% boxplot.stats(waters$SO4)$out)

boxplot.stats(waters$K)$out
which(waters$K %in% boxplot.stats(waters$K)$out)
boxplot.stats(waters$Na)$out
which(waters$Na %in% boxplot.stats(waters$Na)$out)
boxplot.stats(waters$Cl)$out
which(waters$Cl %in% boxplot.stats(waters$Cl)$out)

### 1.2.5. Analyse bidimensionnelle
plot(num.variables, main="Nuage de points croisant deux à deux les variables du jeu de données")
round(cor(num.variables, use="complete.obs"),3)

# 2. Analyse multivariée
## 2.1. ACP sur le premier jeu de données
complete.waters <- na.omit(waters)
dim(complete.waters)
active.waters <- complete.waters[complete.waters$Pays %in% "France",]
acp.variables <- active.waters[sapply(active.waters, is.numeric)] # On ne conserve que les obs. actives

# [ACP via la fonction "dudi.pca"]
library(ade4)
auto.acp <- dudi.pca(df = acp.variables, center = TRUE, scale = TRUE, scannf = FALSE, nf = 9)

### 2.1.1. Sélection des axes et plans retenus
round(auto.acp$eig,3)
round(cumsum(100*auto.acp$eig/sum(auto.acp$eig)), 3)

### 2.1.2. Projection des variables et observations dans un plan donné

# Analyse des variables
var.inertie <- inertia.dudi(auto.acp, col.inertia=TRUE)
round(auto.acp$co,3)
F <- round(auto.acp$li,2) # Matrice des composantes principales
var.ctr <- var.inertie$col.abs/100 # (Contribution des variables en %)
var.qlt <- var.inertie$col.re/100 # (Qualité des variables en %)
par(mfrow=c(1,1))
s.corcircle(auto.acp$co, xax=1, yax=2) ; mtext("Cercle des corrélations dans le plan 1-2")
s.corcircle(auto.acp$co, xax=1, yax=3) ; mtext("Cercle des corrélations dans le plan 1-3")
s.corcircle(auto.acp$co, xax=2, yax=3) ; mtext("Cercle des corrélations dans le plan 2-3")

# Analyse des observations
round(auto.acp$l1, 3)
obs.inertie <- inertia.dudi(auto.acp, row.inertia=TRUE)
obs.ctr <- obs.inertie$row.abs/100 # (Contribution des observations en %)
obs.qlt <- obs.inertie$row.re/100 # (Qualité des observations en %)
abs(round(obs.qlt,4)[1]+round(obs.qlt,4)[2])
abs(round(obs.qlt,4)[1])+abs(round(obs.qlt,4)[3])
abs(round(obs.qlt,4)[2])+abs(round(obs.qlt,4)[3])
s.label(auto.acp$li,xax=1,yax=2, sub = "Projection des observations dans le plan 1-2")
s.label(auto.acp$li,xax=1,yax=3, sub = "Projection des observations dans le plan 1-3")
s.label(auto.acp$li,xax=2,yax=3, sub = "Projection des observations dans le plan 2-3")

### 2.1.3. Analyse des observations supplémentaires
supp.waters
med.PH <- median(waters$PH, na.rm =TRUE)
supp.waters[5,11] <- med.PH # On substitue les valeurs de PH manquantes par la médiane du PH (7)
supp.waters[6,11] <- med.PH
supp.waters[7,11] <- med.PH
supp.waters[8,11] <- med.PH
new.supp.waters <- na.omit(supp.waters)
cl1 <- auto.acp$li[,1]
cl2 <- auto.acp$li[,2]
cl3 <- auto.acp$li[,3]
ligsup <- suprow(auto.acp,new.supp.waters[3:11])
ligsup$lisup # Coordonnées des individus supplémentaires
csup1 <- ligsup$lisup[,1]
csup2 <- ligsup$lisup[,2]
csup3 <- ligsup$lisup[,3]

# Graphe des observations supplémentaires
plot(cl1, cl2, type="n", main="Projection des individus supplémentaires dans le plan 1-2", xlim=c(-2,2),ylim=c(-1,1))
abline(h=0,v=0)
text(csup1, csup2, row.names(ligsup$lisup), col="red", cex=1.2)

# Graphe des observations actives et supplémentaires - plan 1-2
plot(cl1, cl2, type="n", main="Projection des individus actifs (noirs) et supplémentaires (rouges) dans le plan 1-2", xlim=c(-2,2))
abline(h=0,v=0)
text(cl1, cl2, row.names(auto.acp$li))
text(csup1, csup2, row.names(ligsup$lisup), col="red", cex=1.2)

# Graphe des observations actives et supplémentaires - plan 1-3
plot(cl1, cl3, type="n", main="Projection des individus actifs (noirs) et supplémentaires (rouges) dans le plan 1-3", xlim=c(-2,2))
abline(h=0,v=0)
text(cl1, cl3, row.names(auto.acp$li))
text(csup1, csup3, row.names(ligsup$lisup), col="red", cex=1.2)

# Graphe des observations actives et supplémentaires - plan 2-3
plot(cl2, cl3, type="n", main="Projection des individus actifs (noirs) et supplémentaires (rouges) dans le plan 2-3", xlim=c(-2,2))
abline(h=0,v=0)
text(cl2, cl3, row.names(auto.acp$li))
text(csup2, csup3, row.names(ligsup$lisup), col="red", cex=1.2)

## 2.2. ACP sur le deuxième jeu de données
library(DMwR)
new.waters <- knnImputation(waters, k=10, scale=T, meth="weighAvg", distData=NULL) # On fixe k à 10 voisins
new.active.waters <- new.waters[new.waters$Pays %in% "France",]

# 2.2.1. Sélection des axes et plans retenus
acp.variables <- new.active.waters[sapply(new.active.waters, is.numeric)] # On ne conserve que les obs. actives
new.auto.acp <- dudi.pca(df = acp.variables, center = TRUE, scale = TRUE, scannf = FALSE, nf = 9)
round(new.auto.acp$eig,3)
round(cumsum(100*new.auto.acp$eig/sum(new.auto.acp$eig)), 3)

# 2.2.2. Projection des variables et observations dans un plan donné
var.inertie <- inertia.dudi(new.auto.acp, col.inertia=TRUE)
round(new.auto.acp$co,3)
F <- round(new.auto.acp$li,2) # Matrice des composantes principales
var.ctr <- var.inertie$new.col.abs/100 # (Contribution des variables en %)
var.qlt <- var.inertie$new.col.re/100 # (Qualité des variables en %)
s.corcircle(new.auto.acp$co, xax=1, yax=2) ; mtext("Cercle des corrélations dans le plan 1-2")
s.corcircle(new.auto.acp$co, xax=1, yax=3) ; mtext("Cercle des corrélations dans le plan 1-3")
s.corcircle(new.auto.acp$co, xax=2, yax=3) ; mtext("Cercle des corrélations dans le plan 2-3")

# Analyse des observations
round(new.auto.acp$l1, 3)
new.obs.inertie <- inertia.dudi(new.auto.acp, row.inertia=TRUE)
new.obs.ctr <- new.obs.inertie$row.abs/100 # (Contribution des observations en %)
new.obs.qlt <- new.obs.inertie$row.re/100 # (Qualité des observations en %)
outliers <- sort(unique(which(is.na(active.waters), arr.ind = TRUE)[,1]))
round(new.obs.qlt[,1:3],3)
s.label(auto.acp$li,xax=1,yax=2, sub = "Projection des observations dans le plan 1-2")
s.label(auto.acp$li,xax=1,yax=3, sub = "Projection des observations dans le plan 1-3")
s.label(auto.acp$li,xax=2,yax=3, sub = "Projection des observations dans le plan 2-3")

# 2.3. ACP sur le troisième jeu de données
data_in = waters[waters$Pays %in% "France",]
active.waters = na.omit(data_in[sapply(data_in, is.numeric)])
num_outliers = unique(unlist(apply(active.waters, 2, function(x){which(x %in% boxplot.stats(x)$out)})))
outliers = active.waters[setdiff(c(1:nrow(active.waters)), num_outliers),]
without_outliers = active.waters[num_outliers,]
auto.acp = dudi.pca(df = without_outliers, scannf = FALSE, nf = 3)

# 2.3.1. Sélection des axes et plans retenus
round(auto.acp$eig,3)
round(cumsum(100*auto.acp$eig/sum(auto.acp$eig)), 3)

# 2.3.2. Projection des variables et observations dans un plan donné
# Analyse des variables
var.inertie <- inertia.dudi(auto.acp, col.inertia=TRUE)
round(auto.acp$co,3)
F <- round(auto.acp$li,2) # Matrice des composantes principales
var.ctr <- var.inertie$new.col.abs/100 # (Contribution des variables en %)
var.qlt <- var.inertie$new.col.re/100 # (Qualité des variables en %)
s.corcircle(new.auto.acp$co, xax=1, yax=2) ; mtext("Cercle des corrélations dans le plan 1-2")
s.corcircle(new.auto.acp$co, xax=1, yax=3) ; mtext("Cercle des corrélations dans le plan 1-3")
s.corcircle(new.auto.acp$co, xax=2, yax=3) ; mtext("Cercle des corrélations dans le plan 2-3")

# Analyse des observations
obs.inertie <- inertia.dudi(auto.acp, row.inertia=TRUE)
obs.ctr <- obs.inertie$row.abs/100
obs.qlt <- obs.inertie$row.re/100
s.label(auto.acp$li,xax=1,yax=2); mtext("Projection des observations dans le plan 1-2")
s.label(auto.acp$li,xax=1,yax=3); mtext("Projection des observations dans le plan 1-3")
s.label(auto.acp$li,xax=2,yax=3); mtext("Projection des observations dans le plan 2-3")

### 2.3.3. Analyse des observations supplémentaires
axe1 <- auto.acp$li[,1]
axe2 <- auto.acp$li[,2]
axe3 <- auto.acp$li[,3]
ligsup <- suprow(auto.acp, outliers)
acpcos2sup <- ligsup$lisup^2/apply(ligsup$lisup^2, 1, sum)
round(acpcos2sup[,1:3]*1000)
csup1 <- ligsup$lisup[,1]
csup2 <- ligsup$lisup[,2]
plot(axe1, axe2, type="n", main="Individus actifs et supplémentaires dans le plan 1-2", xlim=c(-2,2))
abline(h=0,v=0)
text(axe1, axe2, row.names(auto.acp$li))
text(csup1, csup2, row.names(ligsup$lisup), col="red", cex=1.2)

# Graphe des observations actives et supplémentaires - plan 1-3
csup3 <- ligsup$lisup[,3]
plot(axe1, axe3, type="n", main="Individus actifs et supplémentaires dans le plan 1-3", xlim=c(-2,2))
abline(h=0,v=0)
text(axe1, axe3, row.names(auto.acp$li))
text(csup1, csup3, row.names(ligsup$lisup), col="red", cex=1.2)

# Graphe des observations actives et supplémentaires - plan 2-3
plot(axe2, axe3, type="n", main="Individus actifs et supplémentaires dans le plan 2-3", xlim=c(-2,2))
abline(h=0,v=0)
text(axe2, axe3, row.names(auto.acp$li))
text(csup2, csup3, row.names(ligsup$lisup), col="red", cex=1.2)
ACP “manuelle”
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
plot(main="Répartition des eaux par nature dans le plan 1-2", acp_waters$comp[,1], acp_waters$comp[,2], xlab = "Axe 1", ylab = "Axe 2", col=water_type)
legend(x = "bottomright",legend=unique(water_type),col=1:length(water_type),pch=1)
plot(main="Répartition des eaux par nature dans le plan 1-3", acp_waters$comp[,1], acp_waters$comp[,3], xlab = "Axe 1", ylab = "Axe 3", col=water_type)
legend(x = "bottomright",legend=unique(water_type),col=1:length(water_type),pch=1)
plot(main="Répartition des eaux par nature dans le plan 2-3", acp_waters$comp[,2], acp_waters$comp[,3], xlab = "Axe 2", ylab = "Axe 3", col=water_type)
legend(x = "bottomright",legend=unique(water_type),col=1:length(water_type),pch=1)
#eaux_actives <- filter(eaux, Pays== "France")
dudi = dudi.pca(df = acp_waters$raw_data, scannf = FALSE, nf = 3)
acp_waters$matrix[setdiff(c(1:nrow(acp_waters$matrix)), unique(unlist(apply(acp_waters$matrix, 2, function(x){which(x %in% boxplot.stats(x)$out)})))),]

# Classification non supervisée (K-Means)
waters <- read.table("./Eaux2018 FM.txt", header=TRUE, sep="\t")
data = na.omit(waters[sapply(waters, is.numeric)])
library(ade4)
set.seed(3)
classi = kmeans(data, 5)
values = sapply(2:10, function(k){
curr = kmeans(data, k)
curr$tot.withinss/curr$totss*100
})
plot(2:10,values, type="b", xlab="Nombre de clusters", ylab = "Variance expliquée")
acp = dudi.pca(data, scannf = FALSE, nf=3)
plot(acp$li[,1], acp$li[,2], xlab = "Axe 1", ylab = "Axe 2", type="n")
text(x=acp$li[,1], y=acp$li[,2], labels = row.names(acp$li), col=unname(classi$cluster))
library(RJSONIO)
out_l = list()
for (k in names(classi$cluster)) {
out_l[[k]] = list("k"=classi$cluster[[k]], "nom"=toString(waters$Nom[strtoi(k)]))
}
write(toJSON(out_l), file="./map/classif.json")
library(rgl)
open3d()
plot3d(acp$li[,1], acp$li[,2], acp$li[,3], "Axe 1", "Axe 2", "Axe 3",col=unname(classi$cluster), type = "s", size=0.2)
text3d(acp$li[,1], acp$li[,2], acp$li[,3], texts=row.names(acp$li) ,adj = 0, col=unname(classi$cluster)