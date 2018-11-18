library(ade4)

waters <- read.table("./Eaux2018 FM.txt", header=TRUE, sep="\t")
data_in = waters[waters$Pays %in% "France",]
active.waters = na.omit(data_in[sapply(data_in, is.numeric)]) 

num_outliers = unique(unlist(apply(active.waters, 2, function(x){which(x %in% boxplot.stats(x)$out)})))
outliers = active.waters[setdiff(c(1:nrow(active.waters)), num_outliers),]
without_outliers = active.waters[num_outliers,]

auto.acp = dudi.pca(df = without_outliers, scannf = FALSE, nf = 3)

round(auto.acp$eig,3)
round(cumsum(100*auto.acp$eig/sum(auto.acp$eig)), 3)

round(auto.acp$co,3)

s.corcircle(auto.acp$co, xax=1, yax=2); mtext("Cercle des corrélations dans le plan 1-2")
s.corcircle(auto.acp$co, xax=1, yax=3); mtext("Cercle des corrélations dans le plan 1-3")
s.corcircle(auto.acp$co, xax=2, yax=3); mtext("Cercle des corrélations dans le plan 2-3")

third = apply(auto.acp$co, 1, function(x){sqrt(sum(x*x))})

s.label(auto.acp$li,xax=1,yax=2); mtext("Projection des observations dans le plan 1-2") 
s.label(auto.acp$li,xax=1,yax=3); mtext("Projection des observations dans le plan 1-3") 
s.label(auto.acp$li,xax=2,yax=3); mtext("Projection des observations dans le plan 2-3") 

obs.inertie <- inertia.dudi(auto.acp, row.inertia=TRUE)
obs.ctr <- obs.inertie$row.abs/100 
obs.qlt <- obs.inertie$row.re/100

sort(round(apply(obs.ctr, 1, function(x){sqrt(sum(x*x))}), 3), decreasing = TRUE)
sort(round(apply(obs.qlt, 1, function(x){sqrt(sum(x*x))}), 3), decreasing = TRUE)

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

#compare
waters <- read.table("./Eaux2018 FM.txt", header=TRUE, sep="\t")
data_in = waters[waters$Pays %in% "France",]
active.waters = na.omit(data_in[sapply(data_in, is.numeric)]) 

auto.acp = dudi.pca(df = active.waters, scannf = FALSE, nf = 3)
first = apply(auto.acp$co, 1, function(x){sqrt(sum(x*x))})
round(mean(third - first), 4)


