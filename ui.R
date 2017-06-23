library(leaflet)
#getwd()         
#setwd("R/ZipG")      # ORIGINAL

navbarPage("Precios combustible", id="nav",
           
           tabPanel("Mapa",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = 500,

                                      selectInput("provincias","Provincias", codN, multiple=FALSE),
                                      selectInput("rotulos","Rotulos", rotN, multiple=FALSE),  
                                      dateInput("date",label = h5("Fecha precio"),value = Sys.Date()),
                                      
                                      #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                      #                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      #),
                                      
                                      plotOutput("histCentile", height = 250)
                                      #plotOutput("scatterCollegeIncome", height = 25)
                        )
                    )
           ),
           
           conditionalPanel("false", icon("crosshair"))
)

