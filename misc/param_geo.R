### Preparación del archivo que recoge la relación entre cada gasolinera y sus 5 más cercanas

library(XLConnect)
library(plyr)
library(data.table)

library(sp)
library(rgeos)

setwd("~/_PFM")
df <- readWorksheetFromFile("data/preciosEESS_es.xls", sheet=1, startRow = 5)
df$cod <- as.numeric(substr(df$"Código.postal", 0, 2))

# limpiamos los registros invalidos 
df$Longitud <- as.numeric(gsub(",", ".", df$Longitud))
df$Latitud <- as.numeric(gsub(",", ".", df$Latitud))

df <- df[!is.na(df$Longitud), ]
df <- df[!is.na(df$Latitud), ]

df$Precio.gasolina.95 <- as.numeric(gsub(",", ".", df$Precio.gasolina.95))
df <- df[!is.na(df$Precio.gasolina.95), ]

mapaG <- df[, c("Longitud", "Latitud", "cod")]
names(mapaG) <- c("long", "lat","cod")

#####
set3 <- mapaG[,c("long", "lat")]
set3sp <- SpatialPoints(set3)

#### definitivo
d <- gDistance(set3sp, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2:6])

# Aqui se guardan las más cercanas. El nombre de la columna es el número de la gasolinera. Y los registros las cercanas
# Se buscan en set3. el numero de fila de set3 es el numero de fila de df
min.d[,2439]

save(min.d, file = "data/gasoC")

# modelo de pruebas

set3[7455,]
df[7455,7]


# Pruebas con gasolineras del modelo que sí tienen datos. con ellas se efectúa regresiones lineales de prueba para ver calidad de contenido

min.d[,2431] #5630
#selectedZip <- as.data.frame(dfp[which(dfp$Fecha_Ini == dia & dfp$zipp == 2439),c("Gasolina.95.sin.plomo", "Gasóleo.A", "Gasolina.98.ultimate")] )
selected0 <- as.data.frame(dfp[which(dfp$zipp == 2442),c("Gasóleo.A", "Fecha_Ini")] )
selected1 <- as.data.frame(dfp[which(dfp$zipp == 2446),c("Gasóleo.A", "Fecha_Ini")] )
selected2 <- as.data.frame(dfp[which(dfp$zipp == 2434),c("Gasóleo.A", "Fecha_Ini")] )
selected3 <- as.data.frame(dfp[which(dfp$zipp == 2443),c("Gasóleo.A", "Fecha_Ini")] )
selected4 <- as.data.frame(dfp[which(dfp$zipp == 2445),c("Gasóleo.A", "Fecha_Ini")] )
selected <- merge(selected0, selected2, by = "Fecha_Ini")
selected <- merge(selected, selected3, by = "Fecha_Ini")
# selected <- merge(selected, selected4, by = "Fecha_Ini")


min.d[,5181] #5630
#selectedZip <- as.data.frame(dfp[which(dfp$Fecha_Ini == dia & dfp$zipp == 2439),c("Gasolina.95.sin.plomo", "Gasóleo.A", "Gasolina.98.ultimate")] )
selected0 <- as.data.frame(dfp[which(dfp$zipp == 5183),c("Gasóleo.A", "Fecha_Ini")] )
selected1 <- as.data.frame(dfp[which(dfp$zipp == 5182),c("Gasóleo.A", "Fecha_Ini")] )
selected2 <- as.data.frame(dfp[which(dfp$zipp == 5180),c("Gasóleo.A", "Fecha_Ini")] )
selected3 <- as.data.frame(dfp[which(dfp$zipp == 5695),c("Gasóleo.A", "Fecha_Ini")] )
selected4 <- as.data.frame(dfp[which(dfp$zipp == 5699),c("Gasóleo.A", "Fecha_Ini")] )

selected <- merge(selected0, selected1, by = "Fecha_Ini")
selected <- merge(selected, selected2, by = "Fecha_Ini")
selected <- merge(selected, selected3, by = "Fecha_Ini")
print (nrow(selected))

colnames(selected) <- c("fecha", "original", "p1", "p2", "p3")
plot(selected)

modeloTG=lm(original ~ p1+ p2+ p3, data = selected)
summary(modeloTG)
plot(modeloTG)
modeloTG=lm(original ~ p1, data = selected)
pp<-summary(modeloTG)
modeloTG=lm(original ~ p2, data = selected)
summary(modeloTG)
modeloTG=lm(original ~ p3, data = selected)
summary(modeloTG)

residuos <- rstandard(modeloTG)
valores.ajustados <- fitted(modeloTG)
plot(valores.ajustados, residuos)
qqnorm(residuos)
qqline(residuos)

plot(creditosG$litros, creditosG$pre1, xlab = "Edad", ylab = "Grasas")
abline(modeloTG)

pairs(modeloTG)

plot(creditosG$precio, creditosG$pre1, xlab = "Edad", ylab = "Grasas")
plot(creditos$income, creditos$Rating, xlab = "Edad", ylab = "Grasas")
abline(modeloTG)

nuevasG <- seq(1245, 1275)
predict(modeloTG, nuevasG)

