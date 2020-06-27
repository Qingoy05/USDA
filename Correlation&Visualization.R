#Calculate correlation coefficient
res <- cor(df)
round(res, 2)

#data output
m<-round(res, 2)
m<-as.data.frame(m)
write.table(m,"coefficent.csv",sep=",")

#visualization
library(corrplot)
corrplot(res, tl.cex = 0.7, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
mtext("Correlation Matrix", at=9, line=-0.8, cex=1)