# server.R

library(shiny)
library(shinyjs)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$map1 <- renderLeaflet({
    leaflet(chicago_mergedSP) %>%
      addProviderTiles('CartoDB.DarkMatter') %>%
      setView(-87.6298, 41.8381, zoom = 10) %>%
      addPolygons(layerId = ~chicago_mergedSP@data$COMMUNITY.AREA.NAME, fillColor = 'transparent', 
                color = 'black', weight = 0.5, smoothFactor = 0.1)
  })
  
  observe({
    pal <- colorBin(palette = "RdBu", reverse = TRUE, 
                    domain = chicago_mergedSP@data[[input$Indicator]],
                    bins = c(min(chicago_mergedSP@data[[input$Indicator]]),-1.96,-1.645,1.645,1.96,max(chicago_mergedSP@data[[input$Indicator]]))
    )
    
    leafletProxy("map1", data = chicago_mergedSP) %>%
      clearShapes() %>% 
      addPolygons(stroke = T, color = "black", opacity = 1, weight = 0.4,
                  fillOpacity = 0.7, 
                  smoothFactor = 0.5,
                  fillColor = ~pal(chicago_mergedSP@data[[input$Indicator]]),
                  popup = paste("Community area name: ", chicago_mergedSP@data$COMMUNITY.AREA.NAME, "<br>",
                                "Hardship Index: ", chicago_mergedSP@data$HARDSHIP.INDEX, "<br>",
                                "Latitude: ", chicago_mergedSP@data$lat, "<br>",
                                "Longitude: ",  chicago_mergedSP@data$long)
      ) %>% 
      clearControls() %>%
      addLegend("bottomright", 
                pal= pal, 
                values = ~chicago_mergedSP@data[[input$Indicator]], 
                title = input$Indicator, 
                labFormat = labelFormat(prefix = ""),
                opacity = 1
      )
  })
  observe({
    pal2 <- colorBin(palette = "RdBu", reverse = TRUE,
                     domain = chicago_mergedSP@data[[input$Indicator2]],
                     bins = c(-20,-1.96,1.96,20)
    )
    
    leafletProxy("map1", data = chicago_mergedSP) %>%
      clearShapes() %>% 
      addPolygons(stroke = T, color = "black", opacity = 1, weight = 0.4,
                  fillOpacity = 0.7, 
                  smoothFactor = 0.5,
                  fillColor = ~pal2(chicago_mergedSP@data[[input$Indicator2]]),
                  popup = paste("Community area name: ", chicago_mergedSP@data$COMMUNITY.AREA.NAME, "<br>",
                                "Hardship Index: ", chicago_mergedSP@data$HARDSHIP.INDEX, "<br>",
                                "Latitude: ", chicago_mergedSP@data$lat, "<br>",
                                "Longitude: ",  chicago_mergedSP@data$long)
      ) %>% 
      clearControls() %>%
      addLegend("bottomright", 
                pal= pal2, 
                values = ~chicago_mergedSP@data[[input$Indicator2]], 
                title = input$Indicator2, 
                labFormat = labelFormat(prefix = ""),
                opacity = 1
      )
  })
  observe({
    pal3 <- colorBin(palette = "PRGn",
                     domain = chicago_mergedSP@data[[input$Indicator3]],
                     bins = c(min(chicago_mergedSP@data[[input$Indicator3]]),-1.96,-1.645,1.645,1.96, max(chicago_mergedSP@data[[input$Indicator3]]))
    )
    
    leafletProxy("map1", data = chicago_mergedSP) %>%
      clearShapes() %>% 
      addPolygons(stroke = T, color = "black", opacity = 1, weight = 0.4,
                  fillOpacity = 0.7, 
                  smoothFactor = 0.5,
                  fillColor = ~pal3(chicago_mergedSP@data[[input$Indicator3]]),
                  popup = paste("Community area name: ", chicago_mergedSP@data$COMMUNITY.AREA.NAME, "<br>",
                                "Hardship Index: ", chicago_mergedSP@data$HARDSHIP.INDEX, "<br>",
                                "Latitude: ", chicago_mergedSP@data$lat, "<br>",
                                "Longitude: ",  chicago_mergedSP@data$long)
      ) %>% 
      clearControls() %>%
      addLegend("bottomright", 
                pal= pal3, 
                values = ~chicago_mergedSP@data[[input$Indicator3]], 
                title = input$Indicator3, 
                labFormat = labelFormat(prefix = ""),
                opacity = 1
      )
  })
  
  #### Data table ###
  
  output$ca_table <- renderDataTable(
    DT::datatable(chicago_dt@data[1:15], rownames=FALSE,extensions = c("FixedColumns","FixedHeader"),
                  filter = "top",
                  options = list(sDom  = '<"top">lrt<"bottom">ip',
                                 paging = FALSE,
                                 fixedHeader=TRUE),
                  escape = FALSE
    )
  )
})