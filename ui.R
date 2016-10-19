library(shiny)
library(leaflet)
## leafletOutput se utiliza en el lado de la interfaz de usuario para mostrar el mapa .

shinyUI(fluidPage(
  leafletOutput("mymap")
  
))
