#-----------------#
# Projet Eaux1819 
#-----------------#

waters <- read.table("./Eaux2018 FM.txt", header=TRUE, sep="\t")
attach(waters)

# --- I. DESCRIPTION ET ANALYSE DE DONNEES UNIVARIEES ET BIVARIEES --- #
head(waters)
# Composition du jeu de données
dim(waters) 
# -> 95 lines (waters) ; 12 columns (variables)

# Catégories/Variables étudiées
names(waters) # -> Nom de l'eau (1), Nature de l'eau (1), Composants minéraux/PHP (9), Provenance (1)
lapply(waters, class) # 3 variables qualitatives ; 9 variables quantitatives
num_variables <- waters[sapply(waters, is.numeric)]
cat_variables <- waters[sapply(waters, is.factor)]

# Etude/Recherche des données manquantes/corrompues
colSums(is.na(waters))
describe(waters$PH) 
describe(waters$NO3)
# -> Variables N03 et PH comptabilisent 19 valeurs manquantes -> à éliminer 
# Suppression des objets/lignes enregistrant des NA via na.omit()

# --- ANALYSE UNIVARIEE --- #
summary(waters) # Résumés numériques 
table(waters$Nature) # Effectifs par nature d'eau
summary(waters$Nature)
summary(waters$Pays)
library(Hmisc)
describe(waters$Pays)
pie(table(waters$Pays))
pie(summary(waters$Nature))
# mosaicplot(~waters$Pays+waters$Nature, data=waters, main="Répartition des eaux par Nature et Pays", color=TRUE)
# -> Disparité en termes de provenance des eaux (11,6% eaux marocaines VS 88,4% eaux françaises)
# -> Disparité en termes de nature (27 eaux gazeuses VS 65 eaux plates).

# --- ANALYSE BIVARIEE --- #
# Résumés numériques par nature d'eau (plate, gazeuse)
plate_waters <- subset(waters, waters$Nature=='plat', select=Ca:PH)
gaz_waters <- subset(waters[sapply(waters, is.numeric)], waters$Nature=='gaz')

# Pour une revue complète, on peut utiliser la fonction summary
summary(plate_waters)
summary(gaz_waters)
colMeans(plate_waters, na.rm=TRUE) # Moyenne uniquement
colMeans(gaz_waters, na.rm=TRUE)
# -> Les eaux gazeuses sont en moyenne plus riches que les eaux plates, tous minéraux confondus (à l'exception des Nitrates)
# -> Les eaux plates ont un PH légèrement supérieur aux eaux gazeuses (7.12 VS 6.34)

boxplot(waters$Na ~ waters$Nature, main="Boxplots des compositions en Sodium (Na) par nature d'eau",
        ylab="Sodium (en mg/L)", col=c("lightyellow","lightblue"))

table(waters$Nature, waters$Pays) # table de contingence (2 vars. qualitatives)
# -> Les eaux gazeuses proviennent toutes de France

plot(num_variables)
cor(waters$HCO3, waters$Na, use="complete.obs")
cor(waters$K, waters$Na, use="complete.obs")
# -> Existence d'une forte corrélation linéaire entre Na/HCO3 (95%) et Na/K (92.8%)
# -> Le PH ne semble être corrélé linéairement avec une autre autre variable.
# -> Matrice des corrélations ?

# Faire le lien entre l'analyse univariée/bivariée -> multivariée
# > Trouver une problématique
# Méthodologie pas à pas

# --- II. DESCRIPTIONS MULTIVARIEES --- #
# ACP #
library(ade4)
cleared_variables = na.omit(num_variables)
# Classification non supervisée #
# Classification supervisée #
