

waters <- read.table("./Eaux2018 FM.txt", header=TRUE, sep="\t")
data = na.omit(waters[sapply(waters, is.numeric)])
library(ade4)
library(ggplot2)

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
  out_l[[k]] = list("k"=classi$cluster[[k]], "city"=toString(waters$Nom[strtoi(k)]))
}
write(toJSON(out_l), file="./map/classif.json")

library(rgl)
open3d()
plot3d(acp$li[,1], acp$li[,2], acp$li[,3], "Axe 1", "Axe 2", "Axe 3",col=unname(classi$cluster), type = "s", size=0.2)
text3d(acp$li[,1], acp$li[,2], acp$li[,3], texts=row.names(acp$li) ,adj = 0, col=unname(classi$cluster), family="serif", font=5, cex=1)
