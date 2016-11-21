
############################################################################################################
############################################################################################################

library(shiny)
library(leaflet)
library(htmltools)
library(rsconnect)
rsconnect::deployApp('C:/Users/JCFunk/Documents/GitHub/Taller_Integra_lV')


datos <- data.frame(read.csv("C:/Users/JCFunk/Documents/GitHub/Taller_Integra_lV/Datos/escuelasFinal.csv"))


## renderLeaflet() se utiliza del lado del servidor para hacer el mapa

shinyServer(function(input, output,session) {
  output$mymap <- renderLeaflet({
    # definir el objeto de mapa
    
    leaflet(datos) %>% addTiles() %>% 
      setView(lng = -72.5845, lat = -38.7338, zoom = 12) %>%
      #addCircleMarkers(lng = -(runif(500,72.5,72.8)), -(runif(500,38.6,38.76)), popup = ~htmlEscape("Dario Montiel"),color="RED")%>%
      addMarkers(~Longitud, ~Latitud, popup = ~htmlEscape(Nombres))

  })
  
  observeEvent(input$button, {
    
    output$mymap <- renderLeaflet({
      # definir el objeto de mapa
      
      leaflet(datos) %>% addTiles() %>% 
        setView(lng = -72.5845, lat = -38.7338, zoom = 12) %>%
        addCircleMarkers(lng = -(runif(500,72.5,72.8)), -(runif(500,38.6,38.76)), popup = ~htmlEscape("Dario Montiel"),color="RED")%>%
        addMarkers(~Longitud, ~Latitud, popup = ~htmlEscape(Nombres))
      
    })
    
  })
  
  output$plot <- renderPlot({
    input$newplot
    
    ggplot()+
      geom_line(aes(x=t,y=vmincostos))+
      geom_line(aes(x=t,y=vmaxcostos),col="red")+
      geom_line(aes(x=t,y=vmediancostos),col="blue")+
      geom_hline(yintercept=calCosto(masCerca,alpha),col="black",lty=2)
    
  })
  
  output$plot2 <- renderPlot({
    input$newplot
    
    ggplot()+
      geom_line(aes(x=t,y=minS),col="darkgreen")+
      geom_hline(yintercept=S(masCerca),col="black",lty=2)
    
  })
  
  output$plot3 <- renderPlot({
    input$newplot
    
    ggplot()+
      geom_line(aes(x=t,y=vmeanCostCupo),col="darkgreen")+
      geom_hline(yintercept=costCupo(masCerca),col="black",lty=2)
    
  })
  
  output$plot4 <- renderPlot({
    input$newplot
    
    ggplot()+
      geom_line(aes(x=t,y=vmeanDist),col="darkgreen")+
      geom_hline(yintercept=meanDist(masCerca),col="black",lty=2)
    
  })
  
  output$plot5 <- renderPlot({
    input$newplot
   
  

  })
  
  output$prueba <- DT::renderDataTable({
    
    datos <- read.csv("C:/Users/JCFunk/Documents/GitHub/Taller_Integra_lV/Datos/escuelasFinal.csv")
    datos
    
  })
  
})

