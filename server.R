library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

Gdata <- allG

function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%  addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%  setView(lng = -0.2, lat = 40.36, zoom = 6)
  })
  
  # solo lo hace cuando zoom
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(Gdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(Gdata, latitude >= latRng[1] & latitude <= latRng[2] & longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  output$PGasolina <- renderPlot({
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    hist(zipsInBounds()$G95/1000,
         main = "Precio Gasolina 95",
         xlab = "Euros",
         xlim = range(allG$G95/1000),
         col = '#00DD00',
         border = 'white')
  })
  
  output$PGasoleo <- renderPlot({
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    hist(zipsInBounds()$GA/1000,
         main = "Precio Gasoleo A",
         xlab = "Euros",
         xlim = range(allG$GA/1000),
         col = '#0FFD00',
         border = 'white')
  })
  
  observe({
    ### Para personalizar que tipos de datos se toman en pantalla.
    
    sizeBy <- "G95"
    colorBy <- "G95"
    
    rotChoice <- input$rotulos
    dia <- as.character(input$date)
    
    if (dia != Sys.Date()) {
      temp <- as.data.frame(dfp[which(dfp$Fecha_Ini == dia),c("Gasolina.95.sin.plomo", "Gasóleo.A", "Gasolina.98.ultimate","zipp")] )
      for(i in 1:nrow(temp)) { #nrow(temp)
        row <- temp[i,]
        
        Gdata[which(Gdata$codeG == row[1,"zipp"]),c("G95", "GA", "G98")] <- row[1,c("Gasolina.95.sin.plomo", "Gasóleo.A", "Gasolina.98.ultimate")] * 1000
      }
    }
    
    colChoice <- match(input$provincias, codN)
    colChoice <- colChoice - 1
    
    if (input$provincias != "TODAS") {
      Gdata <- Gdata[Gdata$cod == codP[colChoice],]      
      
    } else{}
    
    if (input$rotulos != "TODAS") {
      Gdata <- Gdata[Gdata$rot == rotChoice,]      
      
    } else{}    
    
    colorData <- Gdata[[colorBy]]
    pal <- colorBin("magma", colorData, 7, pretty = FALSE)
    radius <- Gdata[[sizeBy]] / max(Gdata[[sizeBy]]) * 120

    leafletProxy("map", data = Gdata) %>%
      clearShapes() %>% addCircles(~longitude, ~latitude, radius=radius, layerId=~codeG, stroke=FALSE, fillOpacity=0.5, fillColor=pal(colorData)) 
  })
  
  showZipcodePopup <- function(codeG, lat, lng) {
    dia <- as.character(input$date)

    if (dia != Sys.Date()) {
      p <- allG[allG$codeG == codeG, c("codeG", "rot")]
      selectedZip <- as.data.frame(dfp[which(dfp$Fecha_Ini == dia & dfp$zipp == p$codeG),c("Gasolina.95.sin.plomo", "Gasóleo.A", "Gasolina.98.ultimate")] )

    } else  {
      selectedZip <- Gdata[Gdata$codeG == codeG,]
    }
    
    print (selectedZip$codeG)
    print (min.d[,selectedZip$codeG])
    
    selected1 <- as.data.frame(dfp[which(dfp$zipp == min.d[1,selectedZip$codeG]),c("Gasóleo.A", "Fecha_Ini")] )
    selected2 <- as.data.frame(dfp[which(dfp$zipp == min.d[2,selectedZip$codeG]),c("Gasóleo.A", "Fecha_Ini")] )
    selected3 <- as.data.frame(dfp[which(dfp$zipp == min.d[3,selectedZip$codeG]),c("Gasóleo.A", "Fecha_Ini")] )
    selected4 <- as.data.frame(dfp[which(dfp$zipp == min.d[4,selectedZip$codeG]),c("Gasóleo.A", "Fecha_Ini")] )
    #selected5 <- as.data.frame(dfp[which(dfp$zipp == min.d[5,selectedZip$codeG]),c("Gasóleo.A", "Fecha_Ini")] )
        
    selected <- merge(selected1, selected2, by = "Fecha_Ini")
    selected <- merge(selected, selected3, by = "Fecha_Ini")
    selected <- merge(selected, selected4, by = "Fecha_Ini")
    #selected <- merge(selected, selected5, by = "Fecha_Ini"
    
    if (nrow(selected) != 0 & nrow(selected1) != 0 & nrow(selected2) != 0 & nrow(selected3) != 0 & nrow(selected4) != 0) {
      colnames(selected) <- c("fecha", "original", "p1", "p2", "p3")
      modeloTG=lm(original ~ p1+p2+p3, data = selected)

      ### Punto en el que se hace la regresión del modelo. Dada la casuística la variedad y la poca calidad de la información recopilada,
      ### se opta limitar la búsqueda a las 4 más cercanas y mostrar unicamente el indicador r2 resultado de la misma al considerar
      ### que el objetivo inicial del proyecto no se ha podido cumplir dad la dificultad de la obtención de los datos.
      ### Evidentemente y despues del trabajo realizado, queda abierta la puerta a una nueva versión de la aplicación en la que 
      ### se puedan introducir los litros de cada EESS y realizar la regresión en base a dichos litros y a los precios de las EESS cercanas.
      
      if (dia != Sys.Date()) {
        content <- as.character(tagList(
          tags$h4("Rótulo:", selectedZip$rot),
          tags$br(),
          sprintf("R-square: %s", summary(modeloTG)[8]), tags$br(),        
          sprintf("Precio gasolina 95: %s", selectedZip$Gasolina.95.sin.plomo), tags$br(),
          sprintf("Precio gasolina 98: %s", selectedZip$Gasolina.98.ultimate), tags$br(),
          sprintf("Precio Diesel: %s", selectedZip$Gasóleo.A) ))
      } else  {
        content <- as.character(tagList(
          tags$h4("Rótulo:", selectedZip$rot),
          tags$br(),
          sprintf("R-square: %s", summary(modeloTG)[8]), tags$br(),        
          sprintf("Precio gasolina 95: %s", selectedZip$G95 / 1000), tags$br(),
          sprintf("Precio gasolina 98: %s", selectedZip$G98 / 1000), tags$br(),
          sprintf("Precio Diesel: %s", selectedZip$GA / 1000) ))
      }
      
    } else
    {
      if (dia != Sys.Date()) {
        content <- as.character(tagList(
          tags$h4("Rótulo:", selectedZip$rot),
          tags$h4("No hay datos significativos"),
          sprintf("Precio gasolina 95: %s", selectedZip$Gasolina.95.sin.plomo), tags$br(),
          sprintf("Precio gasolina 98: %s", selectedZip$Gasolina.98.ultimate), tags$br(),
          sprintf("Precio Diesel: %s", selectedZip$Gasóleo.A) ))
      } else  {
        content <- as.character(tagList(
          tags$h4("Rótulo:", selectedZip$rot),
          tags$h4("No hay datos significativos"),
          sprintf("Precio gasolina 95: %s", selectedZip$G95 / 1000), tags$br(),
          sprintf("Precio gasolina 98: %s", selectedZip$G98 / 1000), tags$br(),
          sprintf("Precio Diesel: %s", selectedZip$GA / 1000) ))
      }
    }
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = codeG)
  }
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
}
