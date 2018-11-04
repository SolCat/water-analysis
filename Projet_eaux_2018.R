##############
# SCRIPT ACP #
##############

# Importation du jeu de données #
waters <- read.table("/home/sol/Téléchargements/Eaux2018 FM.txt", header=TRUE, sep="\t")
attach(waters)

# 1. Analyse univariée et bivariée #
## 1.1. Etude du jeu de données ##
head(waters)
dim(waters)  # 95 lignes, 12 variables
names(waters)
lapply(waters, class) # 3 variables qualitatives ; 9 variables quantitatives

active.waters <- waters[waters$Pays %in% "France",] 
nrow(active.waters) # 84 obs. actives
supp.waters <- waters[waters$Pays %in% "Maroc",] 
nrow(supp.waters) # 11 obs. supplémentaires

num.variables <- waters[sapply(waters, is.numeric)] # variables quantitatives
cat.variables <- waters[sapply(waters, is.factor)] # variables qualitatives

## 1.2. Analyse exploratoire des données ##
### 1.2.1. Recherche des données manquantes/corrompues ###
colSums(is.na(waters))

### 1.2.2. Analyse unidimensionnelle : variables quantitatives ###
summary(num.variables)
boxplot(num.variables)

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

### 1.2.3. Recherche des valeurs extrêmes/outliers ###
boxplot.stats(waters$SO4)$out
which(waters$SO4 %in% boxplot.stats(waters$SO4)$out)
boxplot.stats(waters$K)$out
which(waters$K %in% boxplot.stats(waters$K)$out)
boxplot.stats(waters$Na)$out
which(waters$Na %in% boxplot.stats(waters$Na)$out)
boxplot.stats(waters$Cl)$out
which(waters$Cl %in% boxplot.stats(waters$Cl)$out)

### 1.2.4. Analyse unidimensionnelle : variables qualitatives ###
still.waters <- subset(waters, waters$Nature=='plat', select=Ca:PH)
gaz.waters <- subset(waters[sapply(waters, is.numeric)], waters$Nature=='gaz')

summary(still.waters)
summary(gaz.waters)
colMeans(still.waters, na.rm=TRUE)
colMeans(gaz.waters, na.rm=TRUE)
table(waters$Nature, waters$Pays) # Table de contingence des eaux (par nature et pays)
mosaicplot(~waters$Pays+waters$Nature, data=waters, main="Répartition des eaux par Nature et Pays", color=TRUE)

### 1.2.5. Analyse bidimensionnelle ###
plot(num.variables)
round(cor(num.variables, use="complete.obs"),3)

# 2. Analyse multivariée #
## 2.1. ACP sur le premier jeu de données ##

complete.waters <- na.omit(waters)
dim(complete.waters)
active.waters <- complete.waters[complete.waters$Pays %in% "France",] 
acp.variables <- active.waters[sapply(active.waters, is.numeric)] # On ne conserve que les obs. actives

#[ACP via la fonction "dudi.pca"]
library(ade4)
auto.acp <- dudi.pca(df = acp.variables, center = TRUE, scale = TRUE, scannf = FALSE, nf = 9)

### 2.1.1. Sélection des axes et plans retenus ###
round(auto.acp$eig,3)
round(cumsum(100*auto.acp$eig/sum(auto.acp$eig)), 3)

### 2.1.2. Projection des variables et observations dans un plan donné ###
# Analyse des variables
var.inertie <- inertia.dudi(acp, col.inertia=TRUE)
round(auto.acp$co,3)
F <- round(acp$li,2) # Matrice des composantes principales
var.ctr <- var.inertie$col.abs/100 # (Contribution des variables en %)
var.qlt <- var.inertie$col.re/100 # (Qualité des variables en %)

s.corcircle(auto.acp$co, xax=1, yax=2) # Cercle des corrélations dans le plan 1-2
s.corcircle(auto.acp$co, xax=1, yax=3) # Cercle des corrélations dans le plan 1-3
s.corcircle(auto.acp$co, xax=2, yax=3) # Cercle des corrélations dans le plan 2-3

# Analyse des individus
round(auto.acp$l1, 3) # Analyse des observations
obs.inertie <- inertia.dudi(acp, row.inertia=TRUE)
obs.ctr <- obs.inertie$row.abs/100 # (Contribution des observations en %)
obs.qlt <- obs.inertie$row.re/100 # (Qualité des observations en %)

abs(round(obs.qlt,4)[1]+round(obs.qlt,4)[2])
abs(round(obs.qlt,4)[1])+abs(round(obs.qlt,4)[3])
abs(round(obs.qlt,4)[2])+abs(round(obs.qlt,4)[3])

s.label(auto.acp$li,xax=1,yax=2) # Observations dans le plan 1-2
s.label(auto.acp$li,xax=1,yax=3) # Observations dans le plan 1-3
s.label(auto.acp$li,xax=2,yax=3) # Observations dans le plan 2-3

### 2.1.3. Analyse des observations supplémentaires ###
supp.waters

supp.waters[5,11] <- 7 # On substitue les valeurs de PH manquantes par la médiane du PH (7)
supp.waters[6,11] <- 7
supp.waters[7,11] <- 7
supp.waters[8,11] <- 7

new.supp.waters <- na.omit(supp.waters)

cl1 <- auto.acp$li[,1]
cl2 <- auto.acp$li[,2]
cl3 <- auto.acp$li[,3]

# Graphe des observations actives
s.label(auto.acp$li, sub="ACP plan 1-2 : observations actives") 

# Graphe des observations supplémentaires
ligsup <- suprow(auto.acp,new.supp.waters[3:11])
ligsup$lisup # Coordonnées des individus supplémentaires

plot(cl1, cl2, type="n", main="Individus supplémentaires", xlim=c(-2,2),ylim=c(-1,1))
abline(h=0,v=0)
text(csup1, csup2, row.names(ligsup$lisup), col="red", cex=1.2)

# Graphe des observations actives et supplémentaires - plan 1-2
csup1 <- ligsup$lisup[,1]
csup2 <- ligsup$lisup[,2]
plot(cl1, cl2, type="n", main="Individus actifs et supplémentaires dans le plan 1-2", xlim=c(-2,2))
abline(h=0,v=0)
text(cl1, cl2, row.names(auto.acp$li),)
text(csup1, csup2, row.names(ligsup$lisup), col="red", cex=1.2)
# Graphe des observations actives et supplémentaires - plan 1-3
csup3 <- ligsup$lisup[,3]
plot(cl1, cl3, type="n", main="Individus actifs et supplémentaires dans le plan 1-3", xlim=c(-2,2))
abline(h=0,v=0)
text(cl1, cl3, row.names(auto.acp$li),)
text(csup1, csup3, row.names(ligsup$lisup), col="red", cex=1.2)
# Graphe des observations actives et supplémentaires - plan 2-3
plot(cl2, cl3, type="n", main="Individus actifs et supplémentaires dans le plan 2-3", xlim=c(-2,2))
abline(h=0,v=0)
text(cl2, cl3, row.names(auto.acp$li),)
text(csup2, csup3, row.names(ligsup$lisup), col="red", cex=1.2)

labsup <-c("Sisi", "Sidi", "Ain Sa", "Oulmès", "Ain So", "Ain A", "Ain I", "Ain C", "Bahia", "Ciel", "Mazine") 
s.arrow(auto.acp[1:5]$l1, sub="Graphe ACP Eaux actives et supplémentaires", possub="bottomright", ylim=c(-1,2))
s.label(suprow(auto.acp, new.supp.waters[,3:11])$lisup, label=labsup, add.plot=TRUE, clab=1)
