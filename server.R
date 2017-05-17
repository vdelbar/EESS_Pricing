library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

zipdata <- allG
zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -0.2, lat = 40.36, zoom = 6)
  })
  
  # A reactive expression that returns the set of zips that are in bounds right now. solo lo hace cuando zoom
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
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
    colorBy <- "adultpop"
    sizeBy <- "adultpop"

    rotChoice <- input$rotulos
    
    colChoice <- match(input$provincias, codN)
    colChoice <- colChoice - 1
    
    if (input$provincias != "TODAS") {
      zipdata <- zipdata[zipdata$cod == codP[colChoice],]      
      
    } else{}
    
    if (input$rotulos != "TODAS") {
      print(rotChoice)
      zipdata <- zipdata[zipdata$rot == rotChoice,]      
      
    } else{}    
    
    colorData <- zipdata[[colorBy]]
    pal <- colorBin("magma", colorData, 7, pretty = FALSE)
    radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 100
      
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) 
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allG[allG$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Rótulo:", selectedZip$rot),
      tags$strong(HTML(sprintf("%s %s",
                               selectedZip$codeG, min.d[,selectedZip$codeG]
      ))), tags$br(),
      sprintf("Precio gasolina 95: %s", selectedZip$adultpop / 1000), tags$br(),
      sprintf("Precio gasolina 98: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Precio Diesel: %s", selectedZip$adultpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  
}

