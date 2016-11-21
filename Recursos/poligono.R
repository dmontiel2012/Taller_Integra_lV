library(xlsx)

datos <- data.frame(read.xlsx("C:/Users/JCFunk/Downloads/ColegiosTemuco.xlsx", sheetName = "Hoja1"))
datos
write.csv(datos, file="C:/Users/JCFunk/Downloads/escuelasFinal.csv")



datos <- data.frame(read.xlsx("C:/Users/JCFunk/Downloads/dato1.xlsx", sheetName = "Hoja1"))
datos

datos <- data.frame(read.csv("C:/Users/JCFunk/Downloads/escuelasFinal.csv"))






#############################################################################################################


library(leaflet)


lat1 <- -38.70891
lat2 <- -38.75835
lat3 <- -38.74019
lon1 <- -72.62492
lon2 <- -72.62233
lon3 <- -72.65103  

lat=c(lat1,lat2,lat3)
long=c(lon1,lon2,lon3)

leaflet(datos) %>% addTiles() %>% 
setView(lng = -72.5845, lat = -38.7338, zoom = 12) %>%
addPolygons(~long,~lat)
#addMarkers(~long,~lat)
"addCircles(datos,lng=long,lat=lat)

  
  




  
  leaflet(datos) %>% addTiles() %>% 
    setView(lng = -72.5845, lat = -38.7338, zoom = 12) %>%
    #addCircleMarkers(lng = -(runif(500,72.5,72.8)), -(runif(500,38.6,38.76)),color="BLUE")%>%
    #addPolygons(~Longitud, ~Latitud)%>%
    addCircles(~Longitud, ~Latitud, popup = ~htmlEscape(Nombres))
  
  
  