library(shiny)
library(leaflet)
## leafletOutput se utiliza en el lado de la interfaz de usuario para mostrar el mapa .



shinyUI(fluidPage(
  
  navbarPage("Taller IV", id="nav",
             
             tabPanel("Mapa",
                      div(class="outer"),
                      
                      tags$head(
                        # Include our custom CSS
                        includeCSS("styles.css"),
                        includeScript("gomap.js")
                      ),
                      
                      absolutePanel(id = "controls2", class = "panel panel-default",fixed = FALSE,
                                    draggable = FALSE, top = 60, left = NULL, right = "auto", bottom = "auto",
                                    width = 600, height = 560,
                                    
                                    leafletOutput("mymap",width = 600, height = 560),
                                    actionButton("button", "General Estudiantes"),
                                    actionButton("button1", "General Polgonos"),
                                    downloadButton("descarga", "Descargar")
                                    
                                    
                                    
                                    ),
                      
                      
                      #leafletOutput("mymap",width = 500, height = 500),
                      
                      absolutePanel(id = "controls", class = "panel panel-default",fixed = FALSE,
                                    draggable = FALSE, top = 60, left = "780", right = NULL, bottom = "auto",
                                    width = 600, height = 680,
                                    h2("Graficos"),
                                    wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                    
                                    
                                    plotOutput("plot", height=300),
                                    plotOutput("plot2", height=300),
                                    plotOutput("plot3", height=300),
                                    plotOutput("plot4", height=300)
                                    #plotOutput("plot5", height=300)
                                    )),
                      tags$div(id="cite",
                               'Prueba Texto ', tags$em('Proyecto: Taller de integracion 4'), ' Por Juan Carlos Vergara (Taller 4, 2015).'
                      )
                      
             ),
             tabPanel("Datos",
             
             hr(),
             DT::dataTableOutput("prueba")
  ),
  
  tabPanel("Resultados",
  hr(),
  sidebarPanel(
    radioButtons("result", "Distribucion:",
                 c("Escuelas" = "esc",
                   "Estudiantes" = "est"
                   )),
    br(),
    
    sliderInput("n", 
                "Numero Iteraciones:", 
                value = 500,
                min = 1, 
                max = 1000)
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Plot", plotOutput("plot6")),
                tabPanel("Tabla", tableOutput("table")),
                tabPanel("Distanias", tableOutput("tableDist"))
    ))
  
  ),
    
  conditionalPanel("false", icon("crosshair"))

  
  )
))


