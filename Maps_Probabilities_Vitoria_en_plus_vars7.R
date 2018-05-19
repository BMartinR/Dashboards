library(leaflet)
library(shiny)
library(tidyverse)
library(nnet)
library(rgdal)
library(RColorBrewer)

# Great circle distance function
# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

ui <- fluidPage(
  titlePanel("Mode Probability Explorer
             Click coordinates in the maps to create profiles"),
  
  mainPanel(
    h3("Click in the map and select age and sex to create profiles"), br()),
  
  fluidRow(
        column(width = 6,
               fluidRow(
               column(width = 6,
                      sliderInput("AgeProfile1",
                           "Age Profile 1 (decades):",
                           min = 2,
                           max = 9,
                           value = 3,
                           step = 0.1)),
               column(width = 6,
                      radioButtons("SexProfile1",
                            "Sex Profile 1:",
                            choices = list("Male" = 0, "Female" = 1),
                            selected = 1))
               ),
               plotOutput("Profile1Plot", height = 200),
               leafletOutput("map1", height= 400),
               verbatimTextOutput("coordinates1")
               ),
        column(width = 6,
               fluidRow(
                 column(width = 6,
                        sliderInput("AgeProfile2",
                           "Age Profile 2 (decades):",
                           min = 2,
                           max = 9,
                           value = 3,
                           step = 0.1)),
                 column(width = 6,
                        radioButtons("SexProfile2",
                            "Sex Profile 2:",
                            choices = list("Male" = 0, "Female" = 1),
                            selected = 1))
                 ),
               plotOutput("Profile2Plot", height = 200),
               leafletOutput("map2", height= 400),
               verbatimTextOutput("coordinates2")
              
               )
  )
  )

server <- function(input, output, session){

  load("model.Rda")
   

  
  ## Interactive Map ###########################################
  # Create the map
  output$map1 <- renderLeaflet({
    Fabrics <- readOGR(dsn = ".", layer = "Fabrics1")
    ogrInfo(".", layer = "Fabrics1")
    Fabrics <- spTransform(Fabrics, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    pal <- colorFactor(
      palette = "Set2",
      domain = Fabrics$Fabric)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -2.667, lat = 42.85, zoom = 13) %>%
      addScaleBar(position="bottomright") %>%
      addPolygons(data=Fabrics,weight=5,layerId=Fabrics$Fabric, group = "Typologies of Urban Fabrics", color = ~pal(Fabric)) %>%
      addLayersControl(
        overlayGroups = c("Typologies of Urban Fabrics"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend("topright", pal = pal, values = Fabrics$Fabric)
    
  })
  
  
  # Observe mouse clicks and add Marks
  # source: https://stackoverflow.com/questions/34348737/r-leaflet-how-to-click-on-map-and-add-a-circle
  observeEvent(input$map1_shape_click, {
    #If present, clear the previous markers
    leafletProxy("map1") %>% clearMarkers()
    ## Get the click info 
    click1 <- input$map1_shape_click
    c1lat <- click1$lat
    c1lng <- click1$lng
    c1Fabric <- click1$id
    if (is.null(click1))
      return()
   
   ## Add mark where you click
      
    leafletProxy('map1') %>% # use the proxy to save computation
      addMarkers(lng=c1lng, lat=c1lat)
    
    ##Write coordinates
    c1lnground <- round(c1lng, digits= 4)
    c1latround <- round(c1lat, digits= 4)
    output$coordinates1 <- renderText({
      paste0("Longitude=", c1lnground, "\nLatitude=", c1latround, "\nUnits: ", "degrees", "\n", "WGS84","\nType of Urban Fabric: ", c1Fabric )
    })
  
    ## Construct the interactive plot with the click coordinates clat clong
    
     load("model.Rda")
    output$Profile1Plot <- renderPlot({
      # Definir perfil
      Edad.p <- input$AgeProfile1
      Sexo.p <- as.numeric(input$SexProfile1)
      Edad2.p <- Edad.p^2
      Lat.p <- c1lat
      Lon.p <- c1lng
      Distancia.p <- gcd.slc(Lon.p, Lat.p, -2.6724, 42.8488)
      EdadLat2.p <- Edad.p * Lat.p^2
      EdadLat.p <- Edad.p * Lat.p
      EdadLatLon.p <- Edad.p * Lat.p * Lon.p
      EdadLon.p <- Edad.p * Lon.p
      EdadLon2.p <- Edad.p * Lon.p^2
      EdadDist.p <- Edad.p * Distancia.p
      Edad2Lat2.p <- Edad2.p * Lat.p^2
      Edad2Lat.p <- Edad2.p * Lat.p
      Edad2LatLon.p <- Edad2.p * Lat.p * Lon.p
      Edad2Lon.p <- Edad2.p * Lon.p
      Edad2Lon2.p <- Edad2.p * Lon.p^2
      Edad2Dist.p <- Edad2.p * Distancia.p
      SexoLat2.p <- Sexo.p * Lat.p^2
      SexoLat.p <- Sexo.p * Lat.p
      SexoLatLon.p <- Sexo.p * Lat.p * Lon.p
      SexoLon.p <- Sexo.p * Lon.p
      SexoLon2.p <- Sexo.p * Lon.p^2
      SexoDist.p <- Sexo.p * Distancia.p
      Employed.p <- 1 
      EmployLat2.p <- Lat.p^2
      EmployLat.p <- Lat.p
      EmployLatLon.p <- Lat.p * Lon.p
      EmployLon.p <- Lon.p
      EmployLon2.p <- Lon.p^2
      EmployDist.p <- Distancia.p
      Unemployed.p <- 0
      UnemployLat2.p <- 0 
      UnemployLat.p <- 0
      UnemployLatLon.p <- 0 
      UnemployLon.p <- 0
      UnemployLon2.p <- 0
      UnemployDist.p <- 0
      Homemaker.p <- 0
      HomeLat2.p <- 0
      HomeLat.p <- 0
      HomeLatLon.p <- 0 
      HomeLon.p <- 0
      HomeLon2.p <- 0
      HomeDist.p <- 0
      Retired.p <- 0
      RetirLat2.p <- 0
      RetirLat.p <- 0
      RetirLatLon.p <- 0
      RetirLon.p <- 0
      RetirLon2.p <- 0
      RetirDist.p <- 0
      Industrial.p <- if_else(c1Fabric == 'Central Fabric', 0,
                              if_else(c1Fabric == 'Industrial Fabric', 1 , 0))
      Medium.p <- if_else(c1Fabric == 'Central Fabric', 0,
                          if_else(c1Fabric == 'Medium Fabrics', 1 , 0))
      New.p <- if_else(c1Fabric == 'Central Fabric', 0,
                       if_else(c1Fabric == 'New Urban Sectors', 1 , 0))
      Residential.p <- if_else(c1Fabric == 'Central Fabric', 0,
                               if_else(c1Fabric == 'Residential Fabrics', 1 , 0))
      Other.p <- if_else(c1Fabric == 'Central Fabric', 0,
                         if_else(c1Fabric == 'Rural', 1 , 0))
      
      perfil = data.frame(Edad = Edad.p, 
                          EdadLat2 = EdadLat2.p, 
                          EdadLat = EdadLat.p, 
                          EdadLatLon = EdadLatLon.p,
                          EdadLon = EdadLon.p, 
                          EdadLon2 = EdadLon2.p,
                          EdadDist = EdadDist.p,
                          Edad2 = Edad2.p, 
                          Edad2Lat2 = Edad2Lat2.p, 
                          Edad2Lat = Edad2Lat.p, 
                          Edad2LatLon = Edad2LatLon.p, 
                          Edad2Lon = Edad2Lon.p, 
                          Edad2Lon2 = Edad2Lon2.p,
                          Edad2Dist = EdadDist.p,
                          Sexo = Sexo.p, 
                          SexoLat2 = SexoLat2.p, 
                          SexoLat = SexoLat.p, 
                          SexoLatLon = SexoLatLon.p, 
                          SexoLon = SexoLon.p, 
                          SexoLon2 = SexoLon2.p,
                          SexoDist = SexoDist.p,
                          Employed = Employed.p,
                          EmployLat2 = EmployLat2.p,
                          EmployLat = EmployLat.p,
                          EmployLatLon = EmployLatLon.p,
                          EmployLon = EmployLon.p, 
                          EmployLon2 = EmployLon2.p,
                          EmployDist = EmployDist.p,
                          Unemployed = Unemployed.p,
                          UnemployLat2 = UnemployLat2.p, 
                          UnemployLat = UnemployLat.p,
                          UnemployLatLon = UnemployLatLon.p, 
                          UnemployLon = UnemployLon.p,
                          UnemployLon2 = UnemployLon2.p,
                          UnemployDist = UnemployDist.p,
                          Homemaker =  Homemaker.p,
                          HomeLat2 = HomeLat2.p,
                          HomeLat = HomeLat.p,
                          HomeLatLon = HomeLatLon.p, 
                          HomeLon = HomeLon.p,
                          HomeLon2 = HomeLon2.p,
                          HomeDist = HomeDist.p,
                          Retired = Retired.p,
                          RetirLat2 = RetirLat2.p,
                          RetirLat = RetirLat.p,
                          RetirLatLon = RetirLatLon.p,
                          RetirLon = RetirLon.p,
                          RetirLon2 = RetirLon2.p,
                          RetirDist = RetirDist.p,
                          Industrial = Industrial.p, 
                          Medium = Medium.p,
                          New = New.p,
                          Residential = Residential.p,
                          Other = Other.p
                          )
      
      # Calcula probabilidades
      p.fit <- predict(model, newdata = perfil, type = "probs")
      probabilities <- data.frame(Mode = names(p.fit), Probability = p.fit)
      
      # Crear el grafico de barras
      ggplot (data = probabilities) + 
        geom_bar(aes (x = Mode, 
                      y = Probability, fill = Mode), 
                 stat = "identity", 
                 position = "dodge")+
        geom_text (aes(x = Mode,
                     y = Probability,
                     label= sprintf("%0.3f", round(Probability, digits = 4), vjust= -0.3, size=3.5)))
      
   
       })
   
  })
  
  output$map2 <- renderLeaflet({
    Fabrics <- readOGR(dsn = ".", layer = "Fabrics1")
    ogrInfo(".", layer = "Fabrics1")
    Fabrics <- spTransform(Fabrics, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    pal <- colorFactor(
      palette = "Set2",
      domain = Fabrics$Fabric)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -2.667, lat = 42.85, zoom = 13) %>%
      addScaleBar(position="bottomright") %>%
      addPolygons(data=Fabrics,weight=5,layerId=Fabrics$Fabric, group = "Typologies of Urban Fabrics", color = ~pal(Fabric)) %>%
      addLayersControl(
        overlayGroups = c("Typologies of Urban Fabrics"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend("topright", pal = pal, values = Fabrics$Fabric)
  })
  
  # Observe mouse clicks and add Markers
  # source: https://stackoverflow.com/questions/34348737/r-leaflet-how-to-click-on-map-and-add-a-circle
  observeEvent(input$map2_shape_click, {
    #If present, clear the previous markers
    leafletProxy("map2") %>% clearMarkers()
    ## Get the click info 
    click2 <- input$map2_shape_click
    c2lat <- click2$lat
    c2lng <- click2$lng
    c2Fabric <- click2$id
       if (is.null(click2))
      return()
    
    ## Add mark where you click
    
    leafletProxy('map2') %>% # use the proxy to save computation
      addMarkers(lng=c2lng, lat=c2lat)
    
    ##Write coordinates
    c2lnground <- round(c2lng, digits= 4)
    c2latround <- round(c2lat, digits= 4)
    output$coordinates2 <- renderText({
      paste0("Longitude=", c2lnground, "\nLatitude=", c2latround,"\nUnits: ", "degrees", "\n", "WGS84","\nType of Urban Fabric: ", c2Fabric) 
             
    })
    
        ## Construct the interactive plot with the click coordinates clat clong
  
  
  output$Profile2Plot <- renderPlot({
    # Definir perfil
    Edad.p <- input$AgeProfile2
    Sexo.p <- as.numeric(input$SexProfile2)
    Edad2.p <- Edad.p^2
    Lat.p <- c2lat
    Lon.p <- c2lng
    Distancia.p <- gcd.slc(Lon.p, Lat.p, -2.6724, 42.8488)
    EdadLat2.p <- Edad.p * Lat.p^2
    EdadLat.p <- Edad.p * Lat.p
    EdadLatLon.p <- Edad.p * Lat.p * Lon.p
    EdadLon.p <- Edad.p * Lon.p
    EdadLon2.p <- Edad.p * Lon.p^2
    EdadDist.p <- Edad.p * Distancia.p
    Edad2Lat2.p <- Edad2.p * Lat.p^2
    Edad2Lat.p <- Edad2.p * Lat.p
    Edad2LatLon.p <- Edad2.p * Lat.p * Lon.p
    Edad2Lon.p <- Edad2.p * Lon.p
    Edad2Lon2.p <- Edad2.p * Lon.p^2
    Edad2Dist.p <- Edad2.p * Distancia.p
    SexoLat2.p <- Sexo.p * Lat.p^2
    SexoLat.p <- Sexo.p * Lat.p
    SexoLatLon.p <- Sexo.p * Lat.p * Lon.p
    SexoLon.p <- Sexo.p * Lon.p
    SexoLon2.p <- Sexo.p * Lon.p^2
    SexoDist.p <- Sexo.p * Distancia.p
    Employed.p <- 1 
    EmployLat2.p <- Lat.p^2
    EmployLat.p <- Lat.p
    EmployLatLon.p <- Lat.p * Lon.p
    EmployLon.p <- Lon.p
    EmployLon2.p <- Lon.p^2
    EmployDist.p <- Distancia.p
    Unemployed.p <- 0
    UnemployLat2.p <- 0 
    UnemployLat.p <- 0
    UnemployLatLon.p <- 0 
    UnemployLon.p <- 0
    UnemployLon2.p <- 0
    UnemployDist.p <- 0
    Homemaker.p <- 0
    HomeLat2.p <- 0
    HomeLat.p <- 0
    HomeLatLon.p <- 0 
    HomeLon.p <- 0
    HomeLon2.p <- 0
    HomeDist.p <- 0
    Retired.p <- 0
    RetirLat2.p <- 0
    RetirLat.p <- 0
    RetirLatLon.p <- 0
    RetirLon.p <- 0
    RetirLon2.p <- 0
    RetirDist.p <- 0
    Industrial.p <- if_else(c2Fabric == 'Central Fabric', 0,
                            if_else(c2Fabric == 'Industrial Fabric', 1 , 0))
    Medium.p <- if_else(c2Fabric == 'Central Fabric', 0,
                        if_else(c2Fabric == 'Medium Fabrics', 1 , 0))
    New.p <- if_else(c2Fabric == 'Central Fabric', 0,
                     if_else(c2Fabric == 'New Urban Sectors', 1 , 0))
    Residential.p <- if_else(c2Fabric == 'Central Fabric', 0,
                             if_else(c2Fabric == 'Residential Fabrics', 1 , 0))
    Other.p <- if_else(c2Fabric == 'Central Fabric', 0,
                       if_else(c2Fabric == 'Rural', 1 , 0))
    perfil = data.frame(Edad = Edad.p, 
                        EdadLat2 = EdadLat2.p, 
                        EdadLat = EdadLat.p, 
                        EdadLatLon = EdadLatLon.p,
                        EdadLon = EdadLon.p, 
                        EdadLon2 = EdadLon2.p,
                        EdadDist = EdadDist.p,
                        Edad2 = Edad2.p, 
                        Edad2Lat2 = Edad2Lat2.p, 
                        Edad2Lat = Edad2Lat.p, 
                        Edad2LatLon = Edad2LatLon.p, 
                        Edad2Lon = Edad2Lon.p, 
                        Edad2Lon2 = Edad2Lon2.p,
                        Edad2Dist = EdadDist.p,
                        Sexo = Sexo.p, 
                        SexoLat2 = SexoLat2.p, 
                        SexoLat = SexoLat.p, 
                        SexoLatLon = SexoLatLon.p, 
                        SexoLon = SexoLon.p, 
                        SexoLon2 = SexoLon2.p,
                        SexoDist = SexoDist.p,
                        Employed = Employed.p,
                        EmployLat2 = EmployLat2.p,
                        EmployLat = EmployLat.p,
                        EmployLatLon = EmployLatLon.p,
                        EmployLon = EmployLon.p, 
                        EmployLon2 = EmployLon2.p,
                        EmployDist = EmployDist.p,
                        Unemployed = Unemployed.p,
                        UnemployLat2 = UnemployLat2.p, 
                        UnemployLat = UnemployLat.p,
                        UnemployLatLon = UnemployLatLon.p, 
                        UnemployLon = UnemployLon.p,
                        UnemployLon2 = UnemployLon2.p,
                        UnemployDist = UnemployDist.p,
                        Homemaker =  Homemaker.p,
                        HomeLat2 = HomeLat2.p,
                        HomeLat = HomeLat.p,
                        HomeLatLon = HomeLatLon.p, 
                        HomeLon = HomeLon.p,
                        HomeLon2 = HomeLon2.p,
                        HomeDist = HomeDist.p,
                        Retired = Retired.p,
                        RetirLat2 = RetirLat2.p,
                        RetirLat = RetirLat.p,
                        RetirLatLon = RetirLatLon.p,
                        RetirLon = RetirLon.p,
                        RetirLon2 = RetirLon2.p,
                        RetirDist = RetirDist.p,
                        Industrial = Industrial.p, 
                        Medium = Medium.p,
                        New = New.p,
                        Residential = Residential.p,
                        Other = Other.p
                        )

    # Calcula probabilidades
    p.fit <- predict(model, newdata = perfil, type = "probs")
    probabilities <- data.frame(Mode = names(p.fit), Probability = p.fit)
    
    
    # Crear el grafico de barras
    ggplot (data = probabilities) +
      geom_bar(aes (x = Mode,
                    y = Probability, fill = Mode),
               stat = "identity",
               position = "dodge") +
      # labs( x = "Mode\n") +
      # scale_x_discrete(labels = c("Car-driver", "Car-passenger", "Bicycle", "Walking", "Bus")) +
      # scale_fill_discrete(name="Mode",
      #                     breaks=c("Car-driver", "Car-passenger", "Bicycle", "Walking", "Bus"),
      #                     labels=c("Car-driver", "Car-passenger", "Bicycle", "Walking", "Bus"))
      geom_text (aes(x = Mode,
              y = Probability,
              label= sprintf("%0.3f", round(Probability, digits = 4), vjust= -0.3, size=3.5)))
        })
    
  })
}

shinyApp(ui = ui, server = server)


