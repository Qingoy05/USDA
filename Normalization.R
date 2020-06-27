#read data
df<-read.csv("file.csv")

#Normalization(column)
df<-apply(df,2,funcion(x)(x-min(x)/max(x)-min(x)))

#data output
df<-as.data.frame(df)
write_excel_csv(df,path = "Normalizaed data.csv")