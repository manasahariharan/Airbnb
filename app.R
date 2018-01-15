library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggvis)
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
  "Rating overall" = "review_scores_rating",
  "Description analysis meeasure" = "descr_measure",
  "Review analysis measure" = "dif_review",
  "Neighbourhood analysis measure" = "neigh_measure"
  
)

axis_varsx = c("review_scores_location","neighbourhood_cleansed", "property_type", "host_period", 
               "host_is_superhost","descr_measure", "dif_review", "number_of_reviews", "room_type", "cancellation_policy")

axis_varsy = c("price", "neigh_measure","review_scores_rating")
ui =  navbarPage("Airbnb", id = "nav", theme = shinytheme("united"),
                 tabPanel("Airbnb Boston",
                          fluidPage(titlePanel("Introduction to the dataset"), theme = shinytheme("united"),
                                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                  h3 ("Airbnb is an online lodging rental marketplace, 
where people can rent out their homes, rooms and beds to guests. Airbnb does not own any 
of the rooms on its website, it is merely a broker that facilitates the interaction between 
the host and the guest and receives a commission fee from both of them. This data contains information 
about the various listings on Airbnb in Boston, MA, and the various reviews that the listings have 
received. We want to look at the various factors and trends present in the Boston listings on Airbnb. 
This app consists of an interactive map and an interactive graph that shows the relationship between variables. 
To showcase the the influence of various factors on the price of houses and the ratings given, we have created 
a few measure variables from the vast dataset we have. These include three measures for reviews, descriptions and 
neighbourhood descriptions. The review measure is basically the difference between the counts of commonly used 
positive and negative words used in the reviews for a listing. The description and neighbourhood measures are 
counts of commonly used words for describing houses and neighbourhoods by the hosts in an appealing way so as 
to attract customers. Apart from these we have variables for details about the houses, their prices, location 
and the hosts themselves to see how they all contribute towards providing a good customer experience at 
Airbnb.    
")))
                 ),
                 
                 tabPanel("Interactive map", 
                          div(class="outer",
                              
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css")
                              ),
                              
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width  = 1600, height = 700),
                              
                              # Shiny versions prior to 0.11 should use class = "modal" instead.
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 350, height = "auto",
                                            
                                            h2("Airbnb explorer"),
                                            
                                            selectInput("color", "Color", varscolor, selected = "neighbourhood_cleansed"),
                                            selectInput("size", "Size", varsize, selected = "price")
                                            
                              )
                          )
                 ), 
                 tabPanel("Interactive Graph",
                          {actionLink <- function(inputId, ...) {
                            tags$a(href='javascript:void',
                                   id=inputId,
                                   class='action-button',
                                   ...)
                          }
                          
                          fluidPage(
                            titlePanel("Airbnb Boston"),
                            fluidRow(
                              column(3,
                                     wellPanel(
                                       h4("Filter"),
                                       sliderInput("accommodates", "Accomodates",
                                                   1, 16, 2, step = 1),
                                       sliderInput("price", "price range", 10, 4000, value = c(10,4000)),
                                       sliderInput("beds", "number of beds", 1, 16, 2, step = 1),
                                       sliderInput("rating","minimum rating",20, 100, 50, step = 5),
                                       selectInput("neighbourhood", "neighbourhood",
                                                   c("all", "Allston","Back Bay","Bay Village", "Beacon Hill","Brighton","Charlestown",            
                                                     "Chinatown", "Dorchester","Downtown","East Boston","Fenway","Hyde Park",              
                                                     "Jamaica Plain","Leather District", "Longwood Medical Area",
                                                     "Mattapan", "Mission Hill","North End", "Roslindale","Roxbury","South Boston",           
                                                     "South Boston Waterfront", "South End", "West End","West Roxbury" ))
                                     ),
                                     wellPanel(
                                       selectInput("xvar", "X-axis variable", axis_varsx, selected = "host_is_superhost"),
                                       selectInput("yvar", "Y-axis variable", axis_varsy, selected = "price")
                                     )
                              ),
                              column(9,
                                     ggvisOutput("plot1"),
                                     wellPanel(
                                       span("Number of houses selected:",
                                            textOutput("n_houses")
                                       )
                                     )
                              )
                            )
                          )
                          }),
                 tabPanel("Categorical Plot", plotOutput("catplot"))
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
    
    # A reactive expression that returns the set of zips that are
    # in bounds right now
    housesInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(listings[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(listings,
             latitude >= latRng[1] & latitude <= latRng[2] &
               longitude >= lngRng[1] & longitude <= lngRng[2])
    }) 
    
    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    observe({
      colorBy <- input$color
      sizeBy <- input$size
      
      
      if (colorBy == "zipcode") {
        # Color and palette are treated specially in the "superzip" case, because
        # the values are categorical instead of continuous.
        colorData <- as.numeric(listings[[colorBy]])
        pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      } else { 
        colorData <- listings[[colorBy]]
        pal <- colorFactor("viridis", colorData)
      } 
      
      if(sizeBy == "price"){
        radius <- listings[[sizeBy]] / max(listings[[sizeBy]]) * 400
      } 
      else if(sizeBy == "accommodates"){
        radius <- listings[[sizeBy]] / max(listings[[sizeBy]]) * 200
      } 
      else if(sizeBy == "review_scores_rating"){
        radius <- exp(listings[[sizeBy]]/5) * 3^-14
      } 
      else{
        radius <- listings[[sizeBy]] / max(listings[[sizeBy]]) * 200
      } 
      
      leafletProxy("map", data = listings) %>%
        clearShapes() %>%
        addCircles(~longitude, ~latitude, radius=radius, layerId=~id,
                   stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    }) 
    
    # Show a popup at the given location
    showHousePopup <- function(id, lat, lng) {
      selectedHouse <- listings[listings$id == id,]
      content <- as.character(tagList(
        tags$h4(selectedHouse$name),
        tags$strong(HTML(sprintf("%s, %s",
                                 selectedHouse$neighbourhood_cleansed, selectedHouse$zipcode
        ))), tags$br(), 
        sprintf("Price: %s", dollar(selectedHouse$price)), tags$br(),
        sprintf("Accommodates: %s", selectedHouse$accommodates), tags$br(),
        sprintf("Type of accomodation: %s", selectedHouse$property_type), tags$br(),
        sprintf("Overall Rating: %s", selectedHouse$review_scores_rating), tags$br()
      )) 
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
    } 
    
    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
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
    
    # Function for generating tooltip text
    house_tooltip <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.null(x$id)) return(NULL)
      
      listings = isolate(houses())
      house <- listings[listings$id == x$id, ]
      
      paste0("<b>", house$name, "</b><br>",
             house$price, "<br>",
             house$review_scores_rating
      )
    }
    
    # A reactive expression with the ggvis plot
    vis = reactive({
      # Lables for axes
      xvar_name <- names(axis_varsx)[axis_varsx == input$xvar]
      yvar_name <- names(axis_varsy)[axis_varsy == input$yvar]
      
      xvar <- prop("x", as.symbol(input$xvar))
      yvar <- prop("y", as.symbol(input$yvar))
      
      houses %>%
        ggvis(x = xvar, y = yvar) %>%
        layer_points(size := 50, size.hover := 200,
                     fillOpacity := 0.2, fillOpacity.hover := 0.5,
                     stroke = ~instant_bookable, shape = ~room_type, key := ~id) %>%
        add_tooltip(house_tooltip, "hover") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name) %>%
        add_legend("stroke", title = "Instant bookable", values = c("Yes", "No"),
                   properties = legend_props(legend = list(x= 1000,
                                                           y = 400))) %>%
        add_legend("shape", title = 'room type', 
                   values = c('Entire home/apt', 'Private room', 'shared room'), 
                   properties = legend_props(legend = list(x= 1000,
                                                           y = 300))) %>%
        set_options(duration = 0) %>%
        scale_nominal("stroke", domain = c("Yes", "No"),
                      range = c("orange", "#aaa")) %>%
        scale_nominal('shape', domain =  c('Entire home/apt', 'Private room', 'shared room'),
                      range = c("circle","square","diamond") )%>%
        set_options(width = 1400, height = 600)
    }) 
    
    vis %>% bind_shiny("plot1")
    
    output$n_houses = renderText({ nrow(houses()) })
    
    output$catplot = renderPlot(plot(listings$room_type, listings$price, col = c('powderblue', 'mistyrose', 'turquoise1')))
    
  }
  
}


shinyApp(ui = ui, server = server)