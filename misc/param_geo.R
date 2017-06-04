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

save(min.d, file = "data/gasoC")

set3[7455,]
df[7455,7]

