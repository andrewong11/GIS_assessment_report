# ui.R

##### Libraries #####
library(tmap)
library(geojsonio)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(OpenStreetMap)
library(sf)
library(sp)
library(leaflet)
library(geojson)
library(RCurl)
library(RJSONIO)
library(plyr)
library(spatstat)
library(stats)
library(MASS)
library(maptools)
library(GISTools)
library(rgeos)
library(tmaptools)
library(raster)
library(fpc)
library(mapview)
library(reshape2)
library(reshape)
library(spdep)
library(spgwr)
library(corrplot)
library(GWmodel)
library(broom)
library(shiny)
library(shinyjs)

source("mini_proj.R")

shinyUI(fluidPage(
  # Application title
  titlePanel(HTML("<P ALIGN=CENTER><font face=Verdana size=5 color=darkviolet> <b>
                  Exploratory spatial analysis of the relationship between the distribution of 
                  affordable housing units/capita and socio-economic indicators in Chicago's 
                  community areas </b> </font></P>")),
  
  navbarPage(HTML("<b>Contents</b>"), id="nav",
             
       tabPanel("Interactive map of Spatial analytics results", div(class="outer",
          
          leafletOutput("map1", width = "100%", height = 550),
          
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                top = 175, left = "auto", right = 40, bottom = "auto",
                width = 350, height = "auto",
          
                # Select variable to study spatial autocorrelation
                tags$h5(tags$b(HTML("<font face=Verdana size=3 color=navy>Spatial autocorrelation of indicator</font>"))),
                selectInput("Indicator", HTML("<font face=Verdana size=2>Local Getis-Ord z-score for:</font>"),
                            names(chicago_mergedSP@data[19:28])),
                
                selectInput("Indicator2", HTML("<font face=Verdana size=2>Local Moran's I z-score for:</font>"),
                            names(chicago_mergedSP@data[30:39])),
                
                tags$h5(tags$b(HTML("<font size=3 color=navy>Geographically-Weighted Regression (GWR) of 
                                    affordable units/capita against statistically significant socioeconomic 
                                    indicators (determined by stepwise regression)</font>"))),
                selectInput("Indicator3", HTML("<font face=Verdana size=2>GWR Z-scores of Pct_crowded and Pct_poverty:</font>"),
                            names(chicago_mergedSP@data[45:46]))
                ),
          tags$div(id="cite", HTML("<em>Data obtained from </em>"), tags$a(href = "https://data.cityofchicago.org/", HTML("<b>City of Chicago data</b>")),
          tags$p(HTML("<em>Shiny R code learnt and inspired through </em>"), tags$a(href = "https://shiny.rstudio.com/gallery/superzip-example.html", HTML("<b>Shiny Tutorial example</b>")), ", ", 
                  tags$a(href = "http://rpubs.com/adam_dennett/443357", HTML("<b>Dr. Adam Dennett's Tutorial</b>")), HTML("<em>, and Stackoverflow forums</em>")))
          )
       ),
       
       tabPanel("Data Frame",
          hr(),
          dataTableOutput("ca_table")
          )
  )
))

  