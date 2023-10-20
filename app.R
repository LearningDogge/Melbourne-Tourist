# Load the necessary libraries
library(plotly)
library(shiny)
library(leaflet)
library(shinyWidgets)
library(tidyverse)
library(readr)

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

# XXXX Data Preprocess

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
                         # TODO: sidebarPanel
                         ), 
                       mainPanel(
                         leafletOutput("poi_map")
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
}

shinyApp(ui, server, options=list(launch.browser=TRUE))


