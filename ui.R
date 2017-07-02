library(leaflet)
navbarPage("Precios combustible", id="nav",
           tabPanel("Mapa",
                    div(class="outer",
                        tags$head(
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        leafletOutput("map", width="100%", height="100%"),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = 650,
                                      selectInput("provincias","Provincias", codN, multiple=FALSE),
                                      selectInput("rotulos","Rotulos", rotN, multiple=FALSE),  
                                      dateInput("date",label = h5("Fecha precio"),value = Sys.Date()),
                                      plotOutput("PGasolina", height = 200),
                                      plotOutput("PGasoleo", height = 200)
                        )
                    )
           ),
           
           conditionalPanel("false", icon("crosshair"))
)
