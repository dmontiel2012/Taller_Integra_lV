library(xlsx)

datos <- data.frame(read.xlsx("C:/Users/JCFunk/Downloads/Colegios Temuco.xlsx", sheetName = "Hoja1"))
datos
write.csv(datos, file="C:/Users/JCFunk/Downloads/escuelas.csv")



datos <- data.frame(read.xlsx("C:/Users/JCFunk/Downloads/dato1.xlsx", sheetName = "Hoja1"))
datos

