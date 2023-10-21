#install library
list_packages <- c('ggplot2', 'shiny', 'lubridate', 'dplyr', 'leaflet', 'maps', 
                   'plotly', 'scales', 'shinydashboard', 'shinyWidgets', 
                   'tidyverse')
new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages) 

# Load the necessary libraries
library(plotly)
library(shiny)
library(leaflet)
library(shinyWidgets)
library(tidyverse)
library(readr)
library(httr)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(dplyr)
library(maps)
library(scales)
library(shinydashboard)
library(sf)
library(rgdal)
library(shinyjs)

# Hotel Data Preprocess
# Read the 'listings.csv' file and filter the data for listings in Melbourne
listings <- read.csv("data/listings.csv")
listings %>% 
  filter(neighbourhood == "Melbourne") -> listings

# Select only the required columns from the 'listings' data
listings %>% 
  select(id, name, host_id, host_name, neighbourhood, 
         latitude, longitude, room_type, price, number_of_reviews) -> listings

# Read the 'listings_full.csv' file
listings_full <- read.csv("data/listings_full.csv")

# Select only the required columns from the 'listings_full' data
listings_full %>% select(id, listing_url, host_id, picture_url, review_scores_rating) -> listings_full

# Join the 'listings' and 'listings_full' data based on id and host_id columns
listings %>% left_join(listings_full, by = c("id", "host_id")) -> hotel_data

# Remove rows with missing values (NA) from the 'hotel_data' data
hotel_data %>% na.omit() -> hotel_data

# Convert the 'longitude' column to numeric data type and remove rows with missing values (NA)
hotel_data %>% 
  mutate(longitude = as.numeric(longitude)) %>% 
  na.omit() -> hotel_data

# Convert the 'number_of_reviews' column to numeric data type and remove rows with missing values (NA)
hotel_data %>% 
  mutate(number_of_reviews = as.numeric(number_of_reviews)) %>% 
  na.omit() -> hotel_data

# Add a new column 'info' to 'hotel_data' with formatted information about the hotel
hotel_data %>% 
  mutate(info = paste0("Host Name: ", host_name, "<br>", 
                       "Room Type: ", room_type, "<br>", 
                       "Price: ", price, "<br>", 
                       "Number of Reviews: ", number_of_reviews, "<br>", 
                       "Review Scores Rating: ", review_scores_rating, "<br>", 
                       "<img src = ", picture_url, 
                       ' alt="Image" width="300" height="200" style="max-width: 300px; max-height: 200px;">')) -> hotel_data

# Add a new column 'level' to 'hotel_data' based on the number of reviews
hotel_data %>% mutate(level = case_when(number_of_reviews < 200 ~ "level3", 
                                        number_of_reviews < 400 ~ "level2", 
                                        T ~ "level1")) -> hotel_data

# POI Data Preprocess
data <- reactive({
  file_path <- "data/melbourne_city_landmarks.csv"
  data <- read_csv(file_path)
  extracted_latitude <- vector("character", length(data$Location))
  extracted_longitude <- vector("character", length(data$Location))
  pattern_latitude <- "-37\\.[0-9]+"
  pattern_longitude <- "144\\.[0-9]+"
  for (i in 1:length(data$Location)) {
    matches_latitude <- regmatches(data$Location[i], gregexpr(pattern_latitude, data$Location[i]))
    matches_longitude <- regmatches(data$Location[i], gregexpr(pattern_longitude, data$Location[i]))
    if (length(matches_latitude[[1]]) > 0) {
      extracted_latitude[i] <- as.numeric(matches_latitude[[1]])
    } else {
      extracted_latitude[i] <- NA
    }
    if (length(matches_longitude[[1]]) > 0) {
      extracted_longitude[i] <- as.numeric(matches_longitude[[1]])
    } else {
      extracted_longitude[i] <- NA
    }
  }
  data$Latitude <- as.double(extracted_latitude)
  data$Longitude <- as.double(extracted_longitude)
  return(data)
})

api_url <- "https://api.openweathermap.org/data/3.0/onecall?lat=-37.815227&units=metric&lon=144.963611&appid=3696ab6dee1baaf964285dc399f69417"
response <- GET(api_url)
weather_data <- fromJSON(api_url)
current_data <- weather_data$current
temperature <- current_data$temp
feelslike <- current_data$feels_like
pressure <- current_data$pressure
humidity <- current_data$humidity
uvi <- current_data$uvi
clouds <- current_data$clouds
weather <- current_data$weather
weather_main <- weather$main
weather_description <- weather$description
weather_icon_url <- paste0("https://openweathermap.org/img/wn/", weather$icon, ".png")

# restaurants Data Preprocess
restaurants <- read.csv("data/cafes-and-restaurants-with-seating-capacity.csv", header = T)
ct_restaurants <- restaurants[restaurants$CLUE.small.area == "Melbourne (CBD)", ]
ct_restaurants <- ct_restaurants[!is.na(ct_restaurants$Longitude), ]
ct_restaurants <- ct_restaurants[!is.na(ct_restaurants$Latitude), ]
ct_restaurants <- ct_restaurants[ct_restaurants$Industry..ANZSIC4..description 
                                 == "Cafes and Restaurants" | 
                                   ct_restaurants$Industry..ANZSIC4..description
                                 == "Pubs, Taverns and Bars" | 
                                   ct_restaurants$Industry..ANZSIC4..description
                                 == "Takeaway Food Services", ]
#filter the lastest 10 years data
ct_restaurants <- ct_restaurants %>% 
  filter(Census.year >= 2013)

ct_restaurants <- ct_restaurants %>%
  arrange(Location, desc(Census.year)) %>%  
  distinct(Location, .keep_all = TRUE)  # keep the lastest year data

# use sub() function to extract the content before "- "
ct_restaurants$Seating.type <- sub(".*- ", "", ct_restaurants$Seating.type)

restaurant_group <- ct_restaurants %>%
  group_by(Industry..ANZSIC4..description) %>% 
  summarise(Num = n())

customIcon <- makeIcon(
  iconUrl = "data/restaurant.png",
  iconWidth = 30,  # 图标宽度
  iconHeight = 30  # 图标高度
)

# Transport Data Preprocess

# Read transportation data
bus_data <- st_read("data/BusMetroRoutes.geojson")
bicycle_data <- st_read("data/Melbourne_Bicycle_Routes_MGA.geojson")
tram_data <- st_read("data/PTV_METRO_TRAM_ROUTE.geojson")
train_data <- st_read("data/trainStations.geojson")

# Calculate the max route length for BusMetroRoutes
max_route_length_bus <- ceiling(max(bus_data$ROUTE_KM, na.rm = TRUE))

# Calculate the max route length for TramRoutes
max_route_length_tram <- ceiling(max(tram_data$ROUTE_KM, na.rm = TRUE))

# XXXX Data Preprocess

# TODO: Title
ui <- navbarPage("TODO: Title",
                 # Hotel Page
                 tabPanel("Hotel", fluidPage(
                   fluidRow(
                     sidebarLayout(
                       sidebarPanel(
                         # Select Room Type input element
                         selectInput("room_type", 
                                     label = "Select Room Type", 
                                     choices = unique(hotel_data$room_type), 
                                     selected = unique(hotel_data$room_type)[1:3], 
                                     multiple = T), 
                         # Select Range of Price slider input element
                         sliderInput("price", 
                                     label = "Select Range of Price", 
                                     min = hotel_data$price %>% min(), 
                                     max = hotel_data$price %>% quantile(.99), 
                                     value = c(hotel_data$price %>% quantile(.20), 
                                               hotel_data$price %>% quantile(.95))), 
                         # Select Range of Number of Review slider input element
                         sliderInput("number_of_reviews", 
                                     label = "Select Range of Number of Review", 
                                     min = hotel_data$number_of_reviews %>% min(), 
                                     max = hotel_data$number_of_reviews %>% max(), 
                                     value = c(200, hotel_data$number_of_reviews %>% max())),
                         # Select Range of Review Score slider input element
                         sliderInput("review_scores_rating", 
                                     label = "Select Range of Review Score", 
                                     min = hotel_data$review_scores_rating %>% min(), 
                                     max = hotel_data$review_scores_rating %>% max(), 
                                     value = c(3, hotel_data$review_scores_rating %>% max())), 
                         width = 3
                       ), 
                       mainPanel(
                         # Leaflet map output element
                         leafletOutput("map", height = "500px"), 
                         width = 9
                       )
                     )
                   ), 
                   fluidRow(
                     # Column 1 with plotly output element
                     column(width = 3, plotlyOutput("plot1", height = "300px")), 
                     # Column 2 with plotly output element
                     column(width = 4, plotlyOutput("plot2", height = "300px")), 
                     # Column 3 with plotly output element
                     column(width = 5, plotlyOutput("plot3", height = "300px"))
                   ))),
                   
                   # POI Page
                   tabPanel("Places of Interest", fluidPage(
                     sidebarLayout(
                       sidebarPanel(
                         h2("Current Weather"),
                         textOutput("weather_main"),
                         textOutput("temperature"),
                         textOutput("feelslike"),
                         textOutput("humidity"),
                         textOutput("pressure"),
                         textOutput("uvi"),
                         textOutput("clouds"),
                         textOutput("weather_description"),
                         imageOutput("weather_icon"),
                         style = "max-width: 400px; margin: 0 auto;",
                         class = "container"
                       ), 
                       mainPanel(
                         leafletOutput("poi_map")
                       )
                      )
                     )
                    ),
                 # restaurant Page
                 tabPanel(
                   "Resaurants in Melbourne CBD",
                   span(h6(
                     "You can check restaurants' information by clicking on each circle."),
                     style = "color:black"),
                   fluidRow(
                     valueBoxOutput("cafe"),
                     valueBoxOutput("bar"),
                     valueBoxOutput("takeaway"),
                   ),
                   sidebarLayout(
                     sidebarPanel(
                       span(h6("You can choose different seat type.")
                            ,style="color:black"),
                       radioButtons("seat", "Seat Type: ", c("Indoor","Outdoor"), 
                                    selected = "Indoor"),
                       span(h6("You can check different type of restaurants' information.")
                            ,style="color:black"),
                       selectInput(
                         "type", 
                         label = "Choose a type of restaurant to display",
                         choices = restaurant_group$Industry..ANZSIC4..description,
                         selected = "Cafes and Restaurants"
                       ),
                       sliderInput("number", "Choose number of seat range: ", 
                                   min = min(ct_restaurants$Number.of.seats), 
                                   max = max(ct_restaurants$Number.of.seats),
                                   value = c(5, 15)),
                     ),
                     mainPanel(
                       leafletOutput("map1")
                     )
                   )
                 ),
                 
                 # Transport Page
                 tabPanel("Transportation",
                          fluidPage(
                            
                            # Custom JavaScript code
                            useShinyjs(),
                            extendShinyjs(text = "
                            Shiny.addCustomMessageHandler('initialize', function(message) {
                              $(document).on('click', '.custom-radio-button', function() {
                                var value = $(this).attr('data-value');
                                $('#transportType').val(value).trigger('change');
                                $('.custom-radio-button').removeClass('active');
                                $(this).addClass('active');
                                Shiny.setInputValue('transportType', value, {priority: 'event'});
                              });
                            });
                          ", functions = c()),
                            
                            # CSS style
                            tags$head(
                              tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css"),
                              tags$style(HTML(
                                "#map-controls {
                                  position: absolute;
                                  top: 50px;  
                                  right: 20px;   
                                  z-index: 999;
                                  background-color: rgba(255, 255, 255, 0.8);  
                                  padding: 15px;
                                  border-radius: 10px;  
                                  box-shadow: 0 4px 6px rgba(0,0,0,0.1); 
                                  display: flex;  
                                  flex-direction: column; 
                                  align-items: center;  
                                }
                                #map-controls label,
                                #customRadioGroup,
                                #mapButtonGroupBusTram,
                                #mapButtonGroupTrain {
                                  text-align: center;
                                  margin-top: 15px;
                                  font-size: 1em;
                                }
                                #map-controls select,
                                #map-controls input {
                                  width: 200px; 
                                }
                                .custom-radio-button {
                                  display: inline-block;
                                  padding: 8px 12px;
                                  margin: 2px;
                                  border: 1px solid #ccc;
                                  cursor: pointer;
                                  border-radius: 15px;
                                  transition: background-color 0.3s, border 0.3s;
                                }
                                .custom-radio-button.active {
                                  background-color: #007bff;
                                  color: white;
                                  border: 1px solid #007bff;
                                }
                                #transportType {
                                  display: none;
                                }
                                #mapButtonGroupBusTram button,
                                #mapButtonGroupTrain button {
                                  background-color: #0074D9;
                                  color: white;
                                  border: none;
                                  padding: 10px 20px;
                                  border-radius: 8px;
                                  cursor: pointer;
                                  transition: background-color 0.3s;
                                }
                                #mapButtonGroupBusTram button:hover,
                                #mapButtonGroupTrain button:hover {
                                  background-color: #0056b3;
                                }
                              "))
                            ),
                            
                            # Main title panel
                            titlePanel("Melbourne Guide"),
                            
                            # Main panel
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Transportation", 
                                         div(
                                           leafletOutput("Transportation", width = "100%", height = "85vh"),
                                           tags$div(id = "map-controls",
                                                    radioButtons("transportType", "Select Transport Type:", 
                                                                 choices = c("Bus", "Tram", "Train", "Bicycle")),
                                                    tags$div(
                                                      id = 'customRadioGroup',
                                                      tags$div(class = "custom-radio-button active", tags$i(class="fas fa-bus"), " Bus", `data-value` = "Bus"),
                                                      tags$div(class = "custom-radio-button", tags$i(class="fas fa-tram"), " Tram", `data-value` = "Tram"),
                                                      tags$div(class = "custom-radio-button", tags$i(class="fas fa-train"), " Train", `data-value` = "Train"),
                                                      tags$div(class = "custom-radio-button", tags$i(class="fas fa-bicycle"), " Bicycle", `data-value` = "Bicycle")
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.transportType === 'Bus' || input.transportType === 'Tram'",
                                                      selectizeInput("routeName", "Search Route by Name:", choices = NULL, multiple = FALSE, options = list(placeholder = 'Type route name...'), selected = ""),
                                                      sliderInput("routeLength", "Filter by Route Length (KM):", 0, max = max_route_length_bus, value = c(0, max_route_length_bus), step = 1),
                                                      selectInput("startStation", "Choose Start Station:", "", selected = ""),
                                                      selectInput("endStation", "Choose End Station:", "", selected = ""),
                                                      tags$div(id = 'mapButtonGroupBusTram',
                                                               actionButton("goToGoogleMapBusTram", "Go to Google Map")
                                                      )
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.transportType === 'Train'",
                                                      selectizeInput("stationName", "Search Station by Name:", choices = NULL, multiple = FALSE, options = list(placeholder = 'Type station name...'), selected = ""),
                                                      tags$div(id = 'mapButtonGroupTrain',
                                                               actionButton("goToGoogleMapTrain", "Go to Google Map")
                                                      )
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.transportType === 'Bicycle'",
                                                      selectInput("bicycleType", "Select Bicycle Type:", "", selected = "")
                                                    )
                                           )
                                         )
                                )
                              )
                            )
                          )
                 )
)

# Server function definition
server <- function(input, output, session) {
  # Hotel Server
  # Render plotly output for plot2
  output$plot2 <- renderPlotly({
    # Filter hotel data based on input values
    hotel_data %>% 
      filter(room_type %in% input$room_type) %>% 
      filter(price <= input$price[2]) %>% 
      filter(price >= input$price[1]) %>% 
      filter(number_of_reviews <= input$number_of_reviews[2]) %>% 
      filter(number_of_reviews >= input$number_of_reviews[1]) %>% 
      filter(review_scores_rating <= input$review_scores_rating[2]) %>% 
      filter(review_scores_rating >= input$review_scores_rating[1]) %>% 
      select(room_type, price) %>% 
      # Filter out extreme values using quantile
      filter(price <= quantile(price, 0.99)) %>% 
      ggplot() + 
      geom_boxplot(aes(x = room_type, y = price, color = room_type)) + 
      theme_minimal() + 
      theme(legend.position = "none") + 
      xlab("Room Type") + 
      ylab("Price") + 
      ggtitle("Price Distribution of Different Room Type") -> p
    # Convert plot to plotly format
    ggplotly(p)
  })
  
  # Render plotly output for plot1
  output$plot1 <- renderPlotly({
    # Filter hotel data based on input values
    hotel_data %>% 
      filter(room_type %in% input$room_type) %>% 
      filter(price <= input$price[2]) %>% 
      filter(price >= input$price[1]) %>% 
      filter(number_of_reviews <= input$number_of_reviews[2]) %>% 
      filter(number_of_reviews >= input$number_of_reviews[1]) %>% 
      filter(review_scores_rating <= input$review_scores_rating[2]) %>% 
      filter(review_scores_rating >= input$review_scores_rating[1]) %>% 
      ggplot() + 
      geom_histogram(aes(x = review_scores_rating), 
                     bins = 60, color = "white", fill = "darkgreen") + 
      theme_minimal() + 
      xlab("Rating") + 
      ylab("Frequency") + 
      ggtitle("Distribution of Review Scores") -> p
    # Convert plot to plotly format
    ggplotly(p)
  })
  
  # Render plotly output for plot3
  output$plot3 <- renderPlotly({
    # Filter hotel data based on input values
    hotel_data %>% 
      filter(room_type %in% input$room_type) %>% 
      filter(price <= input$price[2]) %>% 
      filter(price >= input$price[1]) %>% 
      filter(number_of_reviews <= input$number_of_reviews[2]) %>% 
      filter(number_of_reviews >= input$number_of_reviews[1]) %>% 
      filter(review_scores_rating <= input$review_scores_rating[2]) %>% 
      filter(review_scores_rating >= input$review_scores_rating[1]) %>% 
      # Group by host_name
      group_by(host_name) %>% 
      # Calculate the total number of reviews for each host
      summarise(number_of_reviews = sum(number_of_reviews)) %>% 
      # Select host_name and number_of_reviews
      select(host_name, number_of_reviews) %>% 
      # Arrange hosts in descending order of number of reviews
      arrange(desc(number_of_reviews)) %>% 
      # Select top 20 hosts with most reviews
      slice(1:20) %>% 
      ggplot() + 
      geom_col(aes(x = reorder(host_name, -number_of_reviews), y = number_of_reviews), 
               fill = "darkgreen", width = 0.5) + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ylab("Number of Reviews") + 
      xlab("Host Name") + 
      ggtitle("Top 20 Hosts with Most Number of Reviews") -> p
    # Convert plot to plotly format
    ggplotly(p)
  })
  
  # Render leaflet output for map
  output$map <- renderLeaflet({
    # Subset hotel data based on input values
    hotel_data_sub <- 
      hotel_data %>% 
      filter(room_type %in% input$room_type) %>% 
      filter(price <= input$price[2]) %>% 
      filter(price >= input$price[1]) %>% 
      filter(number_of_reviews <= input$number_of_reviews[2]) %>% 
      filter(number_of_reviews >= input$number_of_reviews[1]) %>% 
      filter(review_scores_rating <= input$review_scores_rating[2]) %>% 
      filter(review_scores_rating >= input$review_scores_rating[1])
    
    leaflet() %>%
      # Add default tile layer
      addTiles() %>% 
      # Set initial view based on mean lat/lng
      setView(lng = mean(hotel_data_sub$longitude), lat = mean(hotel_data_sub$latitude), zoom=17) %>% 
      # Add markers for level1
      addMarkers(lng = hotel_data_sub %>% filter(level == "level1") %>% pull(longitude), 
                 lat = hotel_data_sub %>% filter(level == "level1") %>% pull(latitude), 
                 popup = hotel_data_sub %>% filter(level == "level1") %>% pull(info), 
                 group = "level1") %>% 
      # Add markers for level2
      addMarkers(lng = hotel_data_sub %>% filter(level == "level2") %>% pull(longitude), 
                 lat = hotel_data_sub %>% filter(level == "level2") %>% pull(latitude), 
                 popup = hotel_data_sub %>% filter(level == "level2") %>% pull(info), 
                 group = "level2") %>% 
      # Add markers for level3
      addMarkers(lng = hotel_data_sub %>% filter(level == "level3") %>% pull(longitude), 
                 lat = hotel_data_sub %>% filter(level == "level3") %>% pull(latitude), 
                 popup = hotel_data_sub %>% filter(level == "level3") %>% pull(info), 
                 group = "level3") %>% 
      # Set zoom levels for group level3
      groupOptions("level3", zoomLevels = 18:20) %>% 
      # Set zoom levels for group level2
      groupOptions("level2", zoomLevels = 16:20)
  })
  
  # POI Server
  output$poi_map <- renderLeaflet({
    m <- leaflet(data()) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~paste(Title, ": ", Description))
    m
  })
  
  output$weather_main <- renderText(paste("Weather: ", weather_main))
  output$temperature <- renderText(paste("Temperature: ", temperature, "°C"))
  output$pressure <- renderText(paste("Pressure: ", pressure, "hPa"))
  output$humidity <- renderText(paste("Humidity: ", humidity, "%"))
  output$clouds <- renderText(paste("clouds: ", clouds, "%"))
  output$uvi <- renderText(paste("uvi: ", uvi))
  output$feelslike <- renderText(paste("Feels like: ", feelslike, "°C"))
  output$weather_description <- renderText(paste("Weather Description: ", weather_description))
  output$weather_icon <- renderImage({
    list(src = weather_icon_url, contentType = "image/png")
  }, deleteFile = FALSE)
  
  #Restaurant Server
  #number text
  output$cafe <- renderValueBox({
    valueBox(
      prettyNum(restaurant_group[restaurant_group$Industry..ANZSIC4..description 
                                 == "Cafes and Restaurants",]$Num,big.mark = ","), 
      "Cafes and Restaurants"
    )
  })
  output$bar <- renderValueBox({
    valueBox(
      prettyNum(restaurant_group[restaurant_group$Industry..ANZSIC4..description 
                                 == "Pubs, Taverns and Bars",]$Num,big.mark = ","), 
      "Pubs, Taverns and Bars"
    )
  })
  output$takeaway <- renderValueBox({
    valueBox(
      prettyNum(restaurant_group[restaurant_group$Industry..ANZSIC4..description 
                                 == "Takeaway Food Services",]$Num,big.mark = ","), 
      "Takeaway Food Services"
    )
  })
  #filter restaurants
  filtered_data <- reactive({
    type <- input$type
    seat <- input$seat
    range_selected <- input$number
    # filter data
    filtered_df <- ct_restaurants %>%
      filter(
        ct_restaurants$Seating.type == seat &
          ct_restaurants$Industry..ANZSIC4..description == type &
          ct_restaurants$Number.of.seats >= range_selected[1] &
          ct_restaurants$Number.of.seats <= range_selected[2]
      )
    
  })
  
  #map
  output$map1 <- renderLeaflet({
    leaflet(filtered_data()) %>% 
      addTiles() %>%
      addMarkers(
        lng = ~Longitude,  
        lat = ~Latitude,  
        popup = ~paste("<b>Restaurant Name: </b>", Trading.name, "<br>", 
                       "<b>Seating Type: </b>", Seating.type,"<br>",
                       "<b>Number of seats: </b>", Number.of.seats)
      )
  })
  # Tansportation Server
  session$sendCustomMessage(type = 'initialize', message = 'ready')
  
  start_stations <- NULL
  route_names <- NULL
  all_start_stations <- NULL
  end_stations <- NULL
  all_end_stations <- NULL
  
  # Initialize start/end stations and route length based on the initial data
  observe({
    selected_transport_type <- input$transportType
    if (selected_transport_type == "Bus") {
      start_stations <- unique(bus_data$FIRST_STOP_NAME)
      end_stations <- unique(bus_data$LAST_STOP_NAME)
      max_val <- max_route_length_bus
      updateSliderInput(session, "routeLength", max = max_route_length_bus, value = c(0, max))
    } else if (selected_transport_type == "Tram") {
      start_stations <- unique(tram_data$FIRST_STOP_NAME)  
      end_stations <- unique(tram_data$LAST_STOP_NAME)
      max_val <- max_route_length_tram
      updateSliderInput(session, "routeLength", max = max_route_length_tram, value = c(0, max))
    }
    updateSelectInput(session, "startStation", choices = c("", start_stations))
    updateSelectInput(session, "endStation", choices = c("", end_stations))
  })
  
  # Initialize bicycle types
  observe({
    selected_transport_type <- input$transportType
    if (selected_transport_type == "Bicycle") {
      bicycle_types <- unique(bicycle_data$type)
      updateSelectInput(session, "bicycleType", choices = c("", bicycle_types))
    }
  })
  
  # Populate route name choices
  observe({
    selected_transport_type <- input$transportType
    if (selected_transport_type == "Bus") {
      route_names <- unique(bus_data$ROUTE_LONG_NAME)
    } else if (selected_transport_type == "Tram") {
      route_names <- unique(tram_data$ROUTE_LONG_NAME)
    }
    updateSelectizeInput(session, "routeName", choices = route_names, selected = "")
  })
  
  # Update based on selected routeName, startStation, and endStation
  observe({
    selected_transport_type <- input$transportType
    if (selected_transport_type == "Bus") {
      data_source <- bus_data
    } else if (selected_transport_type == "Tram") {
      data_source <- tram_data
    }
    
    # Capture the current selections
    current_start_station <- input$startStation
    current_end_station <- input$endStation
    
    
    if (input$startStation != "" && input$endStation == "") {
      data_for_start_station <- data_source[data_source$FIRST_STOP_NAME == input$startStation,]
      available_end_stations <- unique(data_for_start_station$LAST_STOP_NAME)
      updateSelectInput(session, "endStation", choices = c("", available_end_stations))
    }
    
    if (input$endStation != "" && input$startStation == "") {
      data_for_end_station <- data_source[data_source$LAST_STOP_NAME == input$endStation,]
      available_start_stations <- unique(data_for_end_station$FIRST_STOP_NAME)
      updateSelectInput(session, "startStation", choices = c("", available_start_stations))
    }
    
    if (input$startStation == "" && input$endStation == "") {
      if (selected_transport_type == "Bus") {
        all_start_stations <- unique(bus_data$FIRST_STOP_NAME)
        all_end_stations <- unique(bus_data$LAST_STOP_NAME)
      } else if (selected_transport_type == "Tram") {
        all_start_stations <- unique(tram_data$FIRST_STOP_NAME)
        all_end_stations <- unique(tram_data$LAST_STOP_NAME)
      }
      updateSelectInput(session, "startStation", choices = c("", all_start_stations))
      updateSelectInput(session, "endStation", choices = c("", all_end_stations))
    }
  })
  
  # Update the choices for the selectize input based on the available train stations
  observe({
    if (input$transportType == "Train") {
      train_stations <- unique(train_data$STATIONNAM)
      updateSelectizeInput(session, "stationName", choices = train_stations, selected = "")
    }
  })
  
  # Filter train stations based on the input text
  filtered_train_stations <- reactive({
    if (input$transportType == "Train") {
      data <- train_data
      if (input$stationName != "") {
        data <- data[grep(input$stationName, data$STATIONNAM),]
      }
      return(data)
    } else {
      return(NULL)
    }
  })
  
  # Filter data based on route name and length
  filtered_data <- reactive({
    selected_transport_type <- input$transportType
    if (selected_transport_type == "Bus") {
      data <- bus_data
    } else if (selected_transport_type == "Tram") {
      data <- tram_data
    }
    
    if (input$startStation != "") { data <- data[data$FIRST_STOP_NAME == input$startStation,] }
    if (input$endStation != "") { data <- data[data$LAST_STOP_NAME == input$endStation,] }
    if (input$routeName != "") { data <- data[grep(input$routeName, data$ROUTE_LONG_NAME),] }
    if (input$routeLength[1] > 0 || input$routeLength[2] < 100) { data <- data[data$ROUTE_KM >= input$routeLength[1] & data$ROUTE_KM <= input$routeLength[2],] }
    
    return(data)
  })
  
  # Filter bicycle data based on the input type
  filtered_bicycle_data <- reactive({
    if (input$transportType == "Bicycle") {
      data <- bicycle_data
      if (input$bicycleType != "") {
        data <- data[data$type == input$bicycleType,]
      }
      return(data)
    } else {
      return(NULL)
    }
  })
  
  
  # Render the Leaflet map
  output$Transportation <- renderLeaflet({
    
    selected_transport_type <- input$transportType
    selected_data <- switch(selected_transport_type,
                            "Bus" = filtered_data(),
                            "Bicycle" = filtered_bicycle_data(),
                            "Train" = filtered_train_stations(),
                            "Tram" = filtered_data(),
                            NULL)
    
    
    leaflet_data <- leaflet(selected_data) %>% addProviderTiles(providers$OpenStreetMap)
    
    
    geom_type <- st_geometry_type(selected_data)[1]
    if (selected_transport_type == "Train") {
      leaflet_data <- leaflet_data %>% 
        addCircleMarkers(clusterOptions = markerClusterOptions(maxClusterRadius = 80, disableClusteringAtZoom = 12), 
                         popup = ~paste0("Station Name: ", STATIONNAM,
                                         "<br>Station Type: ", STATIONTYP,
                                         "<br>Stop Mode: ", STOPMODENA,
                                         "<br>Zones: ", ZONES),
                         color = ~ifelse(STOPMODENA == "Train", rgb(23, 107, 135, maxColorValue = 255), rgb(255, 128, 128, maxColorValue = 255)),
                         fillColor = ~ifelse(STOPMODENA == "Train", rgb(23, 107, 135, maxColorValue = 255), rgb(255, 128, 128, maxColorValue = 255)),
                         fillOpacity = 0.8,
                         weight = 3,
                         radius = 8,
                         layerId = ~METLINKSTO) %>%
        addLegend(
          position = "bottomleft",
          title = "Stop Mode",
          colors = c(rgb(23, 107, 135, maxColorValue = 255), rgb(255, 128, 128, maxColorValue = 255)), 
          labels = c("Train", "V/Line Train"), 
          layerId = "legend_station",
          opacity = 1)
    } else if (selected_transport_type == "Bus") {
      leaflet_data <- leaflet(filtered_data()) %>%  
        addProviderTiles(providers$OpenStreetMap) %>%
        addPolylines(popup = ~paste0("Route Name: ", ROUTE_LONG_NAME,
                                     "<br>Start Station: ", FIRST_STOP_NAME,
                                     "<br>End Station: ", LAST_STOP_NAME,
                                     "<br>Number of Stops: ", NUM_OF_STOPS,
                                     "<br>Route Length (KM): ", ROUTE_KM),
                     stroke = TRUE, 
                     color = rgb(23, 107, 135, maxColorValue = 255),
                     weight = 3,
                     opacity = 0.6,
                     dashArray = c(5,10),
                     group = "RoutesLines",
                     layerId = ~OBJECTID_1)
    } else if (selected_transport_type == "Tram") {
      leaflet_data <- leaflet(filtered_data()) %>%  
        addProviderTiles(providers$OpenStreetMap) %>%
        addPolylines(popup = ~paste0("Route Name: ", ROUTE_LONG_NAME, 
                                     "<br>Start Station: ", FIRST_STOP_NAME, 
                                     "<br>End Station: ", LAST_STOP_NAME, 
                                     "<br>Number of Stops: ", NUM_OF_STOPS, 
                                     "<br>Route Length (KM): ", ROUTE_KM),  
                     stroke = TRUE, 
                     color = rgb(23, 107, 135, maxColorValue = 255),
                     weight = 3,
                     opacity = 0.8,
                     dashArray = c(5,10),
                     group = "RoutesLines",
                     layerId = ~ROUTE_ID)  
    } else if (selected_transport_type == "Bicycle") {
      leaflet_data <- leaflet(selected_data) %>%  # 
        addProviderTiles(providers$OpenStreetMap) %>%
        addPolylines(popup = ~paste0("Route Name: ", name,  
                                     "<br>Type: ", type,  
                                     "<br>Direction: ", direction,  
                                     "<br>Status: ", status,
                                     "<br>Notes: ", notes), 
                     stroke = TRUE, 
                     color = ~ifelse(direction == "Both Directions", rgb(23, 107, 135, maxColorValue = 255), rgb(255, 128, 128, maxColorValue = 255)),
                     weight = 3,
                     opacity = 0.8,
                     dashArray = c(5,10),
                     group = "RoutesLines",
                     layerId = ~Shape_Length) %>%
        addLegend(
          position = "bottomleft",
          title = "Direction",
          colors = c(rgb(23, 107, 135, maxColorValue = 255), rgb(255, 128, 128, maxColorValue = 255)), 
          labels = c("Both Direction", "One Way"), 
          layerId = "legend_bicycle",
          opacity = 1
        )
    }
    
    return(leaflet_data)
  })
  
  # Go to Google Map
  observeEvent(input$goToGoogleMapBusTram, {
    selected_transport_type <- input$transportType
    google_map_url <- NULL  # Initialize to NULL
    if (selected_transport_type %in% c("Bus", "Tram")) {
      start_station <- input$startStation
      end_station <- input$endStation
      google_map_url <- paste0("https://www.google.com/maps/dir/", 
                               URLencode(start_station), "/", 
                               URLencode(end_station))
    } 
    if (!is.null(google_map_url)) {
      browseURL(google_map_url)
    }
  })
  observeEvent(input$goToGoogleMapTrain, {
    selected_transport_type <- input$transportType
    google_map_url <- NULL  # Initialize to NULL
    
    if (input$transportType == "Train") {
      station_name <- input$stationName
      google_map_url <- paste0("https://www.google.com/maps/dir/", 
                               URLencode(paste0(station_name, ", Australia")))
    }
    
    if (!is.null(google_map_url)) {
      browseURL(google_map_url)
    }
  })
  
  # Highlight clicked route
  clicked_line <- reactiveVal(NULL)
  clicked_station <- reactiveVal(NULL)
  observeEvent(input$Transportation_shape_click, {
    click_data <- input$Transportation_shape_click
    selected_transport_type <- input$transportType
    
    selected_data <- switch(selected_transport_type,
                            "Bus" = bus_data,
                            "Tram" = tram_data,
                            "Bicycle" = bicycle_data,
                            NULL)
    
    object_id_col <- switch(selected_transport_type,
                            "Bus" = "OBJECTID_1",
                            "Tram" = "ROUTE_ID",
                            "Bicycle" = "Shape_Length", 
                            NULL)
    
    popup_content <- switch(selected_transport_type,
                            "Bus" = ~paste0("Route Name: ", ROUTE_LONG_NAME,
                                            "<br>Start Station: ", FIRST_STOP_NAME,
                                            "<br>End Station: ", LAST_STOP_NAME,
                                            "<br>Number of Stops: ", NUM_OF_STOPS,
                                            "<br>Route Length (KM): ", ROUTE_KM),
                            "Tram" = ~paste0("Route Name: ", ROUTE_LONG_NAME,
                                             "<br>Start Station: ", FIRST_STOP_NAME,
                                             "<br>End Station: ", LAST_STOP_NAME,
                                             "<br>Number of Stops: ", NUM_OF_STOPS,
                                             "<br>Route Length (KM): ", ROUTE_KM),
                            "Bicycle" = ~paste0("Route Name: ", name,
                                                "<br>Type: ", type,
                                                "<br>Direction: ", direction,
                                                "<br>Status: ", status,
                                                "<br>Notes: ", notes),
                            NULL)
    
    if(click_data$group == "RoutesLines") {
      # Get the ID of the clicked route
      new_clicked_id <- click_data$id
      
      # Get the corresponding direction for the clicked bicycle line
      if (selected_transport_type == "Bicycle") {
        clicked_direction <- selected_data[selected_data[[object_id_col]] == new_clicked_id,]$direction[1]
      }
      
      
      # Check if this route has been clicked before
      if(is.null(clicked_line()) || new_clicked_id != clicked_line()) {
        # Update the color of the clicked route to red
        leafletProxy("Transportation") %>%
          addPolylines(data = selected_data[selected_data[[object_id_col]] == new_clicked_id,],
                       popup = popup_content,
                       color = rgb(249, 148, 23, maxColorValue = 255),
                       weight = 3,
                       opacity = 1,
                       dashArray = c(5,10),
                       group = "RoutesLines",
                       layerId = new_clicked_id)
        
        # Update the ID of the clicked route
        clicked_line(new_clicked_id)
        
      } else {
        # Reset the color of the previously clicked route to the original color
        original_color <- ifelse(selected_transport_type == "Bicycle", 
                                 ifelse(clicked_direction == "Both Directions", rgb(23, 107, 135, maxColorValue = 255), rgb(255, 128, 128, maxColorValue = 255)), 
                                 rgb(23, 107, 135, maxColorValue = 255))
        # Reset the color of the previously clicked route to blue
        leafletProxy("Transportation") %>%
          addPolylines(data = selected_data[selected_data[[object_id_col]] == new_clicked_id,],
                       popup = popup_content,
                       color = original_color,
                       group = "RoutesLines",
                       weight = 3,
                       opacity = 1,
                       dashArray = c(5,10),
                       layerId = new_clicked_id)
        
        # Reset the ID of the clicked route
        clicked_line(NULL)
      }
    }
    
  })
}

  

shinyApp(ui, server, options=list(launch.browser=TRUE))


