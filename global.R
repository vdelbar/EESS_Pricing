library(shiny)
library(leaflet)

library(RColorBrewer)

options(java.parameters = "-Xmx4g" )
library(XLConnect)
library(plyr)
library(dplyr)
library(data.table)

library(foreign)
library(ggplot2)
library(psych)

#setwd("~/_PFM") 

#dfp <- readWorksheetFromFile("data/PreciosGasolinas2017.xlsx", sheet=2)
#dfp[,"zipp"] <-""

df <- readWorksheetFromFile("data/preciosEESS_es.xls", sheet=1, startRow = 5)

load(file="data/gasoC") #cargamos las cercanias
load(file="data/preciosG")

df$cod <- as.integer(substr(df$"Código.postal", 0, 2))
df$Precio.gasolina.95 <- as.numeric(gsub(",", ".", df$Precio.gasolina.95))
df$Precio.gasolina.98 <- as.numeric(gsub(",", ".", df$Precio.gasolina.98))
df$Precio.gasóleo.A <- as.numeric(gsub(",", ".", df$Precio.gasóleo.A))

df$Longitud <- as.numeric(gsub(",", ".", df$Longitud))
df$Latitud <- as.numeric(gsub(",", ".", df$Latitud))

df <- df[!is.na(df$Longitud), ]
df <- df[!is.na(df$Latitud), ]
df <- df[!is.na(df$Precio.gasolina.95), ]

codN <- as.factor(df[,"Provincia"])
codN <- as.list(levels(codN))
codN <- c("TODAS", codN)
codP <- list(1,2,3,4,33,5,6,7,8,9,10,11,39,12,51,13,14,15,16,17,18,19,20,21,22,23,24,25,27,28,29,52,30,31,32,35,34,36,26,37,38,40,41,42,43,44,45,46,47,48,49,50)

rotN <- as.factor(df[,"Rótulo"])
rotN <- as.list(levels(rotN))
rotN <- c("TODAS", rotN)

z<-as.integer(count(df))

allG <- readRDS("data/gaso.rds")

allG[1:(z), "G95"] <- as.double(df[,"Precio.gasolina.95"]) * 1000
allG[,"G95"] <-gsub(",","",allG[,"G95"])
allG[,"G95"] <-as.integer(allG[,"G95"])

allG[1:(z), "G98"] <- as.double(df[,"Precio.gasolina.98"]) * 1000
allG[,"G98"] <-gsub(",","",allG[,"G98"])
allG[,"G98"] <-as.integer(allG[,"G98"])

allG[1:(z), "GA"] <- as.double(df[,"Precio.gasóleo.A"]) * 1000
allG[,"GA"] <-gsub(",","",allG[,"GA"])
allG[,"GA"] <-as.integer(allG[,"GA"])

allG[1:z, "latitude"] <- as.numeric(df[,"Latitud"])
allG[1:z, "longitude"] <- as.numeric(df[,"Longitud"])
allG[1:z, "cod"] <- as.numeric(df[,"cod"])
allG[1:z, "rot"] <- df[,"Rótulo"]
allG[1:z, "dir"] <- df[,"Dirección"]

allG <- allG[1:z,]

for(i in 1:nrow(allG)) {
  allG[i,"codeG"] <- i
}

allG$college <- allG$college * 100
allG$zipcode <- formatC(allG$zipcode, width=5, format="d", flag="0")
#row.names(allG) <- allG$zipcode

#allG<-allG[1:20,]
p <- "vacio"
cleantable <- allG %>%
  select(
    Zipcode = zipcode,
    Superzip = superzip,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )

#for(i in 1:nrow(allG)) {
#  row <- allG[i,]

#  dfp[which(dfp$Direccion == row[1,"dir"]),"zipp"] <- row[1,"codeG"]
#}

#save(dfp, file = "data/preciosG")

