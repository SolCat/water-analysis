eaux <- read.table("/home/sol/Téléchargements/Eaux2018 FM.txt", header=TRUE, sep="\t")
head(eaux)
eaux_actives <- filter(eaux, Pays== "France")
#waters.mat <- as.matrix(eaux)

par(mfrow=c(1,2))
boxplot(waters$NO3, main="Boîte à moustache de la variable NO3")
hist(waters$NO3, prob=TRUE, main="Histogramme et densité de probabilité du NO3", xlab="NO3")
curve(dnorm(x, mean=mean(waters$NO3, na.rm=TRUE), sd=sd(waters$NO3, na.rm=TRUE)), add=TRUE, col="red")

num_variables <- waters[sapply(waters, is.numeric)]
plot(num_variables)
cor(num_variables, use="complete.obs")

round(cor(num_variables, use="complete.obs"),3)
new_waters <- na.omit(new_waters)
active_waters <- new_waters[new_waters$Pays %in% "France",] 
new_num_variables <- active_waters[sapply(active_waters, is.numeric)]
library(ade4)
dudi.pca(df = water_dataset_1, center = TRUE, scale = TRUE, scannf = FALSE, nf = 9)
inertie <- inertia.dudi(waters_acp_1, col.inertia = TRUE)
round(waters_acp_1$co,3$=)
round(cumsum(100*waters_acp_1$eig/sum(waters_acp_1$eig)), 3)
s.corcircle(waters_acp_1$co, xax=1, yax=2)
s.corcircle(waters_acp_1$co, xax=1, yax=3)
s.corcircle(waters_acp_1$co, xax=2, yax=3)
