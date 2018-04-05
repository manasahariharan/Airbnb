library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggvis)
library(ggplot2)
library(shiny)
library(data.table)
library(shinythemes)
if(FALSE){
  library(RSQLite)
  library(dbplyr)
}
listings = read.csv("proc2_listings.csv")
# Choices for drop-downs
varscolor <- c(
  "Zipcode" = "zipcode",
  "Room Type" = "room_type",
  "Property Type" = "property_type",
  "Neighbourhood" = "neighbourhood_cleansed",
  "Instant Bookable?" = "instant_bookable",
  "Cancellation Policy" = "cancellation_policy",
  "Is superhost?" = "host_is_superhost"
  
) 

varsize = c(
  "Price" = "price",
  "Property accommodates" = "accommodates",
  "Description analysis meeasure" = "descr_measure",
  "Review analysis measure" = "dif_review",
  "Neighbourhood analysis measure" = "neigh_measure"
  
)

axis_varsx = c("review_scores_location", "property_type", "dif_review","review_scores_rating","host_period", "number_of_reviews",
               "host_is_superhost","descr_measure",  "room_type", "cancellation_policy")

axis_varsy = c("price", "neigh_measure","review_scores_rating",  "availability")
ui = dashboardPage(
  dashboardHeader(title = "Airbnb Boston"),
  dashboardSidebar(sidebarMenu(
    menuItem("Interactive map", tabName = "Im", icon = icon("map"),
             menuSubItem("Map", tabName = "Im1",icon = NULL),
             menuSubItem(selectInput("color", "Color", varscolor, selected = "neighbourhood_cleansed"),icon = NULL),
             menuSubItem(selectInput("size", "Size", varsize, selected = "price"),icon = NULL))),
    sidebarMenu(menuItem("Interactive Graph", tabName = "Ig", icon = icon("bar-chart-o"),
             menuSubItem("Graph", tabName = "Ig1",icon = NULL),
             menuSubItem(selectInput("xvar", "X-axis variable", axis_varsx, selected = "review_scores_rating"),icon = NULL),
             menuSubItem(selectInput("yvar", "Y-axis variable", axis_varsy, selected = "price"),icon = NULL))), 
    sidebarMenu(menuItem("Graphs", tabName = "gphs", icon = icon("line-chart"),
                         menuSubItem("descr. vs price", tabName = "gphs1", icon = NULL),
                         menuSubItem("host period vs. price", tabName = "gphs2", icon = NULL),
                         menuSubItem("neigh. rating vs price", tabName = "gphs3", icon = NULL),
                         menuSubItem("no. of reviews vs price", tabName = "gphs4", icon = NULL),
                         menuSubItem("correlation", tabName = "gphs5", icon = NULL),
                         menuSubItem("correlation 2", tabName = "gphs6", icon = NULL),
                         menuSubItem("revmeasure vs. rating", tabName = "gphs7", icon = NULL),
                         menuSubItem("room type vs price", tabName = "gphs8", icon = NULL))),
    selectInput("neighbourhood", "neighbourhood",
                c("all", "Allston","Back Bay","Bay Village", "Beacon Hill","Brighton","Charlestown",            
                  "Chinatown", "Dorchester","Downtown","East Boston","Fenway","Hyde Park",              
                  "Jamaica Plain","Leather District", "Longwood Medical Area",
                  "Mattapan", "Mission Hill","North End", "Roslindale","Roxbury","South Boston",           
                  "South Boston Waterfront", "South End", "West End","West Roxbury" )), 
    sliderInput("accommodates", "Accomodates",
                1, 16, 2, step = 1),
    sliderInput("price", "price range", 10, 1500, value = c(10,1500)),
    sliderInput("beds", "number of beds", 1, 16, 2, step = 1),
    sliderInput("rating","minimum rating",20, 100, 50, step = 5)),

  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "Im1", (leafletOutput("map", width  = 1320, height = 720))), 
    
    # Second tab content
    tabItem(tabName = "Ig1", 
      box(plotOutput("plot1"),height = 650, width = 1300),
               wellPanel(span("Number of houses selected:",
                         textOutput("n_houses"))
               )),
    tabItem(tabName = "gphs1", imageOutput("plot11")),
    tabItem(tabName = "gphs2", imageOutput("plot12")),
    tabItem(tabName = "gphs3", imageOutput("plot13")),
    tabItem(tabName = "gphs4", imageOutput("plot14")),
    tabItem(tabName = "gphs5", imageOutput("plot15")),
    tabItem(tabName = "gphs6", imageOutput("plot16")),
    tabItem(tabName = "gphs7", imageOutput("plot17")),
    tabItem(tabName = "gphs8", imageOutput("plot18"))
  ))
) 

server = {
  function(input, output, session) {
    
 
    
    # Create the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -71.06, lat = 42.36, zoom = 13)
    })
    
    housesInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(listings[FALSE,])
      acco = input$accommodates
      minprice = input$price[1]
      maxprice = input$price[2]
      minbeds = input$beds
      minrating = input$rating
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      h1 = listings %>%
        filter(
          accommodates >= acco,
          price >= minprice,
          price <= maxprice,
          beds >= minbeds,
          review_scores_rating >= minrating,
          latitude >= latRng[1] & latitude <= latRng[2],
          longitude >= lngRng[1] & longitude <= lngRng[2]
        )
      if (input$neighbourhood != "all") {
        neighbourhood = input$neighbourhood
        h1 <- h1 %>% filter(neighbourhood_cleansed %like% neighbourhood)
      }
      h1 <- as.data.frame(h1)
      h1
    })    
 
    
    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    observe({
      colorBy <- input$color
      sizeBy <- input$size
      
      
      if (colorBy == "zipcode") {
        # Color and palette are treated specially in the "superzip" case, because
        # the values are categorical instead of continuous.
        colorData <- as.numeric(housesInBounds()[[colorBy]])
        pal <- colorBin("magma", colorData, 7, pretty = TRUE)
      } else { 
        colorData <- housesInBounds()[[colorBy]]
        pal <- colorFactor("magma", colorData)
      } 
      
      if(sizeBy == "price"){
        radius <- housesInBounds()[[sizeBy]] / 50
      } 
      else if(sizeBy == "accommodates"){
        radius <- (housesInBounds()[[sizeBy]]/3)*4
      } 
      else if(sizeBy == "neigh_measure"){
        radius <- (housesInBounds()[[sizeBy]]*2.3)
      } 
      else if(sizeBy == "dif_review"){
        radius <- (housesInBounds()[[sizeBy]])*1.7
      }
      else{
        radius <- housesInBounds()[[sizeBy]]
      } 
      
      leafletProxy("map", data = housesInBounds()) %>%
        clearMarkers() %>%
        addCircleMarkers(~longitude, ~latitude, radius=radius, layerId=~id,
                   stroke=FALSE, fillOpacity=0.7, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }) 
    
    # Show a popup at the given location
    showHousePopup <- function(id, lat, lng) {
      housesInBounds1 = isolate(housesInBounds())
      selectedHouse <- housesInBounds1[housesInBounds1$id == id,]
      content <- as.character(tagList(
        tags$h4(selectedHouse$name),
        tags$strong(HTML(sprintf("%s, %s",
                                 selectedHouse$neighbourhood_cleansed, selectedHouse$zipcode
        ))), tags$br(), 
        sprintf("Price: %s", dollar(selectedHouse$price)), tags$br(),
        sprintf("Accommodates: %s", selectedHouse$accommodates), tags$br(),
        sprintf("Type of accomodation: %s", selectedHouse$property_type), tags$br(),
        sprintf("Overall Rating: %s", selectedHouse$review_scores_rating), tags$br(),
        sprintf("Review Measure: %s", selectedHouse$dif_review), tags$br(),
        sprintf("Neighborhood Measure: %s", selectedHouse$neigh_measure), tags$br(),
        sprintf("Description Measure: %s", selectedHouse$desc_measure), tags$br()
      )) 
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
    } 
    
    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_marker_click
      if (is.null(event))
        return()
      
      isolate({
        showHousePopup(event$id, event$lat, event$lng)
      }) 
    })
    
    houses = reactive({
      acco = input$accommodates
      minprice = input$price[1]
      maxprice = input$price[2]
      minbeds = input$beds
      minrating = input$rating
      
      
      # Apply filters
      h = listings %>%
        filter(
          accommodates >= acco,
          price >= minprice,
          price <= maxprice,
          beds >= minbeds,
          review_scores_rating >= minrating
        ) 
      
      if (input$neighbourhood != "all") {
        neighbourhood = input$neighbourhood
        h <- h %>% filter(neighbourhood_cleansed %like% neighbourhood)
      } 
      
      
      h <- as.data.frame(h)
      h
      
    })
    


    
    output$plot1 = renderPlot({ggplot(houses(), aes(eval(parse(text = input$xvar)), eval(parse(text = input$yvar)), size = 1 )) +
        facet_grid(.~host_is_superhost, labeller = label_both) + geom_jitter(aes(color = factor(round(dif_review)))) + 
        labs(x = input$xvar, y = input$yvar) + guides(color=guide_legend(title="Review Measure"))
}, 
        height = 600, width = 1250)
    output$n_houses = renderText({ nrow(houses()) })
    
    output$plot11 = renderImage({filename = normalizePath("./figures/descrmeasure_price.png")
    list(src = filename,  height = 450, width = 500)}, deleteFile = FALSE)
    
    output$plot12 = renderImage({filename = normalizePath("./figures/hostperiod_price.png")
    list(src = filename,  height = 450, width = 500)}, deleteFile = FALSE)
    
    output$plot13 = renderImage({filename = normalizePath("./figures/neighborhoodrating_measure.png")
    list(src = filename,  height = 450, width = 500)}, deleteFile = FALSE)
    
    output$plot14 = renderImage({filename = normalizePath("./figures/numberreviews_price.png")
    list(src = filename,  height = 450, width = 500)}, deleteFile = FALSE)
    
    output$plot15 = renderImage({filename = normalizePath("./figures/patterns.png")
    list(src = filename)}, deleteFile = FALSE)
    
    output$plot16 = renderImage({filename = normalizePath("./figures/patterns2.png")
    list(src = filename)}, deleteFile = FALSE)
    
    output$plot17 = renderImage({filename = normalizePath("./figures/reviewsmeasure_rating.png")
    list(src = filename,  height = 450, width = 500)}, deleteFile = FALSE)
    
    output$plot18 = renderImage({filename = normalizePath("./figures/roomtype_price.png")
    list(src = filename)}, deleteFile = FALSE)
    
    

    
  }
  
}


shinyApp(ui = ui, server = server)
