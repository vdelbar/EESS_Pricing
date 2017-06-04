

setwd("~/precios")
dir()

ld <- list.dirs('/home/vic/precios', recursive = TRUE)
for (i in ld) {
  lf <- list.files(i)
  for (j in lf) {
    # Procesar el archivo j
    #cat('Directorio', i, 'archivo', j, '\n')

    df <- readWorksheetFromFile(j, sheet=1, startRow = 5)
    df2 <- readWorksheetFromFile(j, sheet=1, startRow = 2)
    
    c<- c(names(df2)[2],',', df[1659,9],',', df[9300,9],',', df[4112,9],',', df[4090,9],',', df[5533,9])

    print(c)
  }
}

for(i in 1:nrow(temp)) { #nrow(temp)
  row <- temp[i,]
  
  if (row[1,"zipp"] == 5630)
  {
    print ("dentro5630")
  }

  allG[which(allG$codeG == row[1,"zipp"]),c("G95", "GA", "G98")] <- row[1,c("Gasolina.95.sin.plomo", "Gasóleo.A", "Gasolina.98.ultimate")] * 1000
}

