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
