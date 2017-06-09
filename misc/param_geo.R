library(XLConnect)
library(plyr)
library(data.table)

library(sp)
library(rgeos)

setwd("~/_PFM")      # ruta relativa
df <- readWorksheetFromFile("data/preciosEESS_es.xls", sheet=1, startRow = 5)
df$cod <- as.numeric(substr(df$"Código.postal", 0, 2))

# limpiamos los registros invalidos

#formateamos 
df$Longitud <- as.numeric(gsub(",", ".", df$Longitud))
df$Latitud <- as.numeric(gsub(",", ".", df$Latitud))

df <- df[!is.na(df$Longitud), ]
df <- df[!is.na(df$Latitud), ]

df$Precio.gasolina.95 <- as.numeric(gsub(",", ".", df$Precio.gasolina.95)) # QUITAR LUEGO !!!
df <- df[!is.na(df$Precio.gasolina.95), ]

mapaG <- df[, c("Longitud", "Latitud", "cod")]
names(mapaG) <- c("long", "lat","cod")

#####
set3 <- mapaG[,c("long", "lat")]
set3sp <- SpatialPoints(set3)

#### definitivo
d <- gDistance(set3sp, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2:6])

#aqui se guardan las más cercanas. El nombre de la columna es el número de la gasolinera. Y los registros las cercanas
#se buscan en set3. el numero de fila de set3 es el numero de fila de df
min.d[,2439]

#save(min.d, file = "data/gasoC")

set3[7455,]
df[7455,7]




min.d[,2439]
#selectedZip <- as.data.frame(dfp[which(dfp$Fecha_Ini == dia & dfp$zipp == 2439),c("Gasolina.95.sin.plomo", "Gasóleo.A", "Gasolina.98.ultimate")] )
selected1 <- as.data.frame(dfp[which(dfp$zipp == 2448),c("Gasóleo.A", "Fecha_Ini")] )
selected2 <- as.data.frame(dfp[which(dfp$zipp == 2436),c("Gasóleo.A", "Fecha_Ini")] )
selected3 <- as.data.frame(dfp[which(dfp$zipp == 2433),c("Gasóleo.A", "Fecha_Ini")] )

selected <- merge(selected1, selected2, by = "Fecha_Ini")
selected <- merge(selected, selected3, by = "Fecha_Ini")

plot(selected)

modeloTG=lm(Gasóleo.A ~ Gasóleo.A.x, data = selected)
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


