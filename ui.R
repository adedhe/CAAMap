library(shiny)
library(leaflet)
library(RColorBrewer)

#myData<-read.csv('/Users/abhishekdedhe/Documents/CMU/General/Archives/Map/PuneCAAProtests.csv')
myData<-read.csv('PuneCAAProtests.csv')
myData$Latitude <- jitter(myData$Latitude, factor = 0.00002)
myData$Longitude <- jitter(myData$Longitude, factor = 0.00002)

popup00 <- paste0("Number of participants: ", myData$Turnout, "<br>", "Date: ", myData$Date)
popup01 <- paste0("<br>", "Location: ", myData$Location, "<br>", "Viewpoint: ", myData$Type)
popup1<- paste0(popup00,popup01 )

ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                sliderInput("Turnout", "Participation", min(read.csv('PuneCAAProtests.csv')$Turnout), max(read.csv('PuneCAAProtests.csv')$Turnout),
                            value = range(read.csv('PuneCAAProtests.csv')$Turnout), step = 20
                ),
                selectInput("Type", "Viewpoint",c("Pro-CAA" = "Pro","Anti-CAA" = "Anti")
                )#,
               #checkboxInput("legend", "Show legend", TRUE)
   )
)

server <- function(input, output) {
  
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    myData[myData$Turnout >= input$range[1] & myData$Turnout <= input$range[2],]
  })
  # filteredData <- reactive({
  #   quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  # })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  # colorpal <- reactive({
    #colorNumeric("Blues", domain = read.csv('/Users/abhishekdedhe/Documents/CMU/General/Archives/Map/PuneCAAProtests.csv')$Type)
    # colorNumeric("Blues", domain = c(1,2))
#  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
  #   leaflet(quakes) %>% addTiles() %>%
  #     fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  # })
    leaflet(myData) %>% addTiles() %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorFactor(c("red", "green"), domain = c("Anti", "Pro"))
    
    #leafletProxy("map", data =  myData[myData$Turnout >= input$range[1] & myData$Turnout <= input$range[2],]) %>%
    leafletProxy("map", data =  myData) %>%
      
     clearShapes() %>%
      
      
      # addCircleMarkers(
      #   radius = ~log(Turnout)*50,
      #   color = ~pal(Type),
      #   stroke = FALSE, fillOpacity = 1, popup = ~paste(Turnout)
      # )
      #addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
      addCircles(radius = ~log(Turnout)*50, weight = 1, color = "#777777",
                 #fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
                fillColor = ~pal(Type), fillOpacity = 0.7, popup = popup1

      )
  })
  
  # Use a separate observer to recreate the legend as needed.
 # observe({
    #proxy <- leafletProxy("map", data = quakes)
   # proxy <- leafletProxy("map", data = filteredData)
    
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    # proxy %>% clearControls()
    # if (input$legend) {
    #   pal <- colorpal()
    #   proxy %>% addLegend(position = "bottomright",
    #                      # pal = pal, values = ~mag
    #                      pal = pal, values = ~Turnout
    #   )
    # }
 # })
  
  # observe({
  #   proxy <- leafletProxy("map", data = read.csv('/Users/abhishekdedhe/Documents/CMU/General/Archives/Map/PuneCAAProtests.csv'))
  #   
  # })
  
}

shinyApp(ui, server)