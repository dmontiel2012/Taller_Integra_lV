library(xlsx)
library(leaflet)
library(htmltools)

datos <- data.frame(read.xlsx("C:/Users/JCFunk/Downloads/Colegios Temuco.xlsx", sheetName = "Hoja1"))
datos

leaflet(datos) %>% addTiles() %>%
  addMarkers(~Longitud, ~Latitud, popup = ~htmlEscape(Nombres))