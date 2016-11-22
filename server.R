
############################################################################################################
############################################################################################################

library(shiny)
library(leaflet)
library(htmltools)


datos <- data.frame(read.csv("Datos/escuelasFinal.csv"))

####################################################################################################

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

###################################################################################################



## renderLeaflet() se utiliza del lado del servidor para hacer el mapa

shinyServer(function(input, output,session) {
  output$mymap <- renderLeaflet({
    # definir el objeto de mapa
    
    leaflet(datos) %>% addTiles() %>% 
      setView(lng = -72.5845, lat = -38.7338, zoom = 12) %>%
      addCircles(~Longitud, ~Latitud, popup = ~htmlEscape(Nombres))

  })
  
  observeEvent(input$button, {
    
    output$mymap <- renderLeaflet({
      # definir el objeto de mapa
      cx<-  genEst(nest,nesc,posEsc[1:nesc,],cupo[1:nesc],dist95=0.006)
      leaflet(datos) %>% addTiles() %>% 
        setView(lng = -72.5845, lat = -38.7338, zoom = 12) %>%
        #addPolygons(lng=-lon1, lat= -lat1 , popup = ~htmlEscape("Dario Montiel"),color="RED")%>%
        addCircleMarkers(radius = 1.5, lng =cx[1:nest,2],cx[1:nest,1], popup = ~htmlEscape("sss"),color="RED")%>%
        addCircles(~Longitud, ~Latitud, popup = ~htmlEscape(Nombres))
      
    })
    
  })
  
  observeEvent(input$button1, {
    
    output$mymap <- renderLeaflet({
      # definir el objeto de mapa
      cx<-  genEst(nest,nesc,posEsc[1:nesc,],cupo[1:nesc],dist95=0.006)
      leaflet(datos) %>% addTiles() %>% 
        setView(lng = -72.5845, lat = -38.7338, zoom = 12) %>%
        #addPolygons(lng=-lon1, lat= -lat1 , popup = ~htmlEscape("Dario Montiel"),color="RED")%>%
        addCircleMarkers(radius = 1.5, lng =cx[1:nest,2],cx[1:nest,1], popup = ~htmlEscape("sss"),color="RED")%>%
        addPolygons(~long,~lat,color="Green")%>%
        addCircles(~Longitud, ~Latitud, popup = ~htmlEscape(Nombres))
      
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
      geom_hline(yintercept=S(masCerca,nesc,vuln,nest,P),col="black",lty=2)
    
  })
  
  output$plot3 <- renderPlot({
    input$newplot
    
    ggplot()+
      geom_line(aes(x=t,y=vmeanCostCupo),col="darkgreen")+
      geom_hline(yintercept=costCupo(masCerca,nesc,cupo),col="black",lty=2)
    
  })
  
  output$plot4 <- renderPlot({
    input$newplot
    
    ggplot()+
      geom_line(aes(x=t,y=vmeanDist),col="darkgreen")+
      geom_hline(yintercept=meanDist(masCerca,dist),col="black",lty=2)
    
  })
  
  output$plot6 <- renderPlot({
    result <- input$result
    n <- input$n
    
    hist(data(), 
         main=paste('resultadoPrueba', result, '(', n, ')', sep=''))
  })
  
  output$table <- renderTable({
    data.frame(x=data())
  })
  
  
  data <- reactive({
    result <- switch(input$result,
                     esc = rnorm,
                     est = runif)
    result(input$n)
  })
  
  output$tableDist <- renderTable({
    DistAluEsc
  })
  
  output$descarga <- downloadHandler(
    filename = function() {
      paste("SinNombre-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(datos, file)
    }
  )
  
  
  output$prueba <- DT::renderDataTable({
    
    datos <- read.csv("Datos/escuelasFinal.csv")
    datos
    
  })
  
})

