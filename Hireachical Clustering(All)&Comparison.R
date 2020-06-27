#read data
library(readxl)
df <- read_excel("Normalized-label+property.xlsx")
df2 <- read_excel("Normalized-label+property+Tetracore.xlsx")

#convert data
m=as.matrix(df[, 1])
v=as.vector(m)
df<-df[, -1]
row.names(df) <- v

m=as.matrix(df2[, 1])
v=as.vector(m)
df2<-df2[, -1]
row.names(df2) <- v

#clustering
d <- dist(df, method = "euclidean")
d2 <- dist(df2, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
hc2 <- hclust(d2, method = "complete" )
plot(hc1, hang = -1, xlab = "Category", ylab = "Distance", main = "Clustering Dendrogram (Label+Property)")
plot(hc2, hang = -1, xlab = "Category", ylab = "Distance", main = "Clustering Dendrogram (Overall)")

#Dendrogram comparison
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
tanglegram(dend1, dend2)
mtext("Dendrogram Comparison", at=7, line=+4, cex=1.2)