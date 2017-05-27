library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

Gdata <- allG
#Gdata <- Gdata[order(Gdata$centile),]

function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%  addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%  setView(lng = -0.2, lat = 40.36, zoom = 6)
  })
  
  # A reactive expression that returns the set of zips that are in bounds right now. solo lo hace cuando zoom
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(Gdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(Gdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allG$centile, breaks = 20)$breaks
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    hist(zipsInBounds()$centile,
         breaks = centileBreaks,
         main = "SuperZIP score (visible zips)",
         xlab = "Percentile",
         xlim = range(allG$centile),
         col = '#00DD00',
         border = 'white')
  })
  
  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
    
    print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allG$college), ylim = range(allG$income)))
  })
  
  # This observer is responsible for maintaining the circles and legend,according to the variables the user has chosen to map to color and size.
  observe({
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
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode, stroke=FALSE, fillOpacity=0.5, fillColor=pal(colorData)) 
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    dia <- as.character(input$date)
    print(dia)

    if (dia != Sys.Date()) {
      p <- allG[allG$zipcode == zipcode, c("codeG", "rot")]

      selectedZip <- as.data.frame(dfp[which(dfp$Fecha_Ini == dia & dfp$zipp == p$codeG),c("Gasolina.95.sin.plomo", "Gasóleo.A", "Gasolina.98.ultimate")] )
      content <- as.character(tagList(
        tags$h4("Rótulo:", selectedZip$rot),
        #tags$strong(HTML(sprintf("%s", selectedZip$codeG))), 
        tags$br(),
        sprintf("Precio gasolina 95: %s", selectedZip$Gasolina.95.sin.plomo), tags$br(),
        sprintf("Precio gasolina 98: %s", selectedZip$Gasolina.98.ultimate), tags$br(),
        sprintf("Precio Diesel: %s", selectedZip$Gasóleo.A) ))
    } else  {
      selectedZip <- Gdata[Gdata$zipcode == zipcode,]
      content <- as.character(tagList(
        tags$h4("Rótulo:", selectedZip$rot),
        tags$strong(HTML(sprintf("%s", selectedZip$codeG))), 
        tags$br(),
        sprintf("Precio gasolina 95: %s", selectedZip$G95 / 1000), tags$br(),
        sprintf("Precio gasolina 98: %s", selectedZip$G98 / 1000), tags$br(),
        sprintf("Precio Diesel: %s", selectedZip$GA / 1000) ))
    }

    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    print (event)
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
}
