#read data
df<-read_excel("file.xlsl")

#convert data
m=as.matrix(df[, 1])
v=as.vector(m)
df<-df[, -1]
row.names(df) <- v

#Normalization(column)
df<-t(apply(df,2,funcion(x)(x-min(x)/max(x)-min(x))))
df<-t(df)

#data output
df<-as.data.frame(df)
write_excel_csv(df,path = "Normalizaed data.csv")
