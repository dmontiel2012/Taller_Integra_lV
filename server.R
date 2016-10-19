library(shiny)
library(leaflet)
library(xlsx)
library(htmltools)

datos <- data.frame(read.xlsx("C:/Users/JCFunk/Downloads/Colegios Temuco.xlsx", sheetName = "Hoja1"))
datos

## renderLeaflet() se utiliza del lado del servidor para hacer el mapa

shinyServer(function(input, output) {
  output$mymap <- renderLeaflet({
    # definir el objeto de mapa
    
    leaflet(datos) %>% addTiles() %>%
      addMarkers(~Longitud, ~Latitud, popup = ~htmlEscape(Nombres))
    
  })
  
})
