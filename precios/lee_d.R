

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

