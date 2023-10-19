library(plotly)
library(shiny)
library(leaflet)
library(shinyWidgets)
library(tidyverse)
library(readr)

# Hotel Data Preprocess
listings <- read.csv("data/listings.csv")
listings %>% 
  filter(neighbourhood == "Melbourne") -> listings
listings %>% 
  select(id, name, host_id, host_name, neighbourhood, 
         latitude, longitude, room_type, price, number_of_reviews) -> listings
listings_full <- read.csv("data/listings_full.csv")
listings_full %>% select(id, listing_url, host_id, picture_url, review_scores_rating) -> listings_full
listings %>% left_join(listings_full, by = c("id", "host_id")) -> hotel_data
hotel_data %>% na.omit() -> hotel_data
hotel_data %>% 
  mutate(longitude = as.numeric(longitude)) %>% 
  na.omit() -> hotel_data
hotel_data %>% 
  mutate(number_of_reviews = as.numeric(number_of_reviews)) %>% 
  na.omit() -> hotel_data

hotel_data %>% 
  mutate(info = paste0("Host Name: ", host_name, "<br>", 
                       "Room Type: ", room_type, "<br>", 
                       "Price: ", price, "<br>", 
                       "Number of Reviews: ", number_of_reviews, "<br>", 
                       "Review Scores Rating: ", review_scores_rating, "<br>", 
                       "<img src = ", picture_url, 
                       ' alt="Image" width="300" height="200" style="max-width: 300px; max-height: 200px;">')) -> hotel_data

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

# TODO: Title
ui <- navbarPage("TODO: Title",
                 # Hotel Page
                 tabPanel("Hotel", fluidPage(
                   fluidRow(
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("room_type", 
                                     label = "Select Room Type", 
                                     choices = unique(hotel_data$room_type), 
                                     selected = unique(hotel_data$room_type)[1:3], 
                                     multiple = T), 
                         sliderInput("price", 
                                     label = "Select Range of Price", 
                                     min = hotel_data$price %>% min(), 
                                     max = hotel_data$price %>% quantile(.99), 
                                     value = c(hotel_data$price %>% quantile(.20), 
                                               hotel_data$price %>% quantile(.95))), 
                         sliderInput("number_of_reviews", 
                                     label = "Select Range of Number of Review", 
                                     min = hotel_data$number_of_reviews %>% min(), 
                                     max = hotel_data$number_of_reviews %>% max(), 
                                     value = c(200, hotel_data$number_of_reviews %>% max())), 
                         sliderInput("review_scores_rating", 
                                     label = "Select Range of Review Score", 
                                     min = hotel_data$review_scores_rating %>% min(), 
                                     max = hotel_data$review_scores_rating %>% max(), 
                                     value = c(3, hotel_data$review_scores_rating %>% max())), 
                         width = 3
                       ), 
                       mainPanel(
                         leafletOutput("map", height = "500px"), 
                         width = 9
                       )
                     )
                   ), 
                   fluidRow(
                     column(width = 3, plotlyOutput("plot1", height = "300px")), 
                     column(width = 4, plotlyOutput("plot2", height = "300px")), 
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

server <- function(input, output, session) {
  # Hotel Server
  output$plot2 <- renderPlotly({
    hotel_data %>% 
      filter(room_type %in% input$room_type) %>% 
      filter(price <= input$price[2]) %>% 
      filter(price >= input$price[1]) %>% 
      filter(number_of_reviews <= input$number_of_reviews[2]) %>% 
      filter(number_of_reviews >= input$number_of_reviews[1]) %>% 
      filter(review_scores_rating <= input$review_scores_rating[2]) %>% 
      filter(review_scores_rating >= input$review_scores_rating[1]) %>% 
      select(room_type, price) %>% 
      filter(price <= quantile(price, 0.99)) %>% 
      ggplot() + 
      geom_boxplot(aes(x = room_type, y = price, color = room_type)) + 
      theme_minimal() + 
      theme(legend.position = "none") + 
      xlab("Room Type") + 
      ylab("Price") + 
      ggtitle("Price Distribution of Different Room Type") -> p
    ggplotly(p)
  })
  
  output$plot1 <- renderPlotly({
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
    ggplotly(p)
  })
  
  output$plot3 <- renderPlotly({
    hotel_data %>% 
      filter(room_type %in% input$room_type) %>% 
      filter(price <= input$price[2]) %>% 
      filter(price >= input$price[1]) %>% 
      filter(number_of_reviews <= input$number_of_reviews[2]) %>% 
      filter(number_of_reviews >= input$number_of_reviews[1]) %>% 
      filter(review_scores_rating <= input$review_scores_rating[2]) %>% 
      filter(review_scores_rating >= input$review_scores_rating[1]) %>% 
      group_by(host_name) %>% 
      summarise(number_of_reviews = sum(number_of_reviews)) %>% 
      select(host_name, number_of_reviews) %>% 
      arrange(desc(number_of_reviews)) %>% 
      slice(1:20) %>% 
      ggplot() + 
      geom_col(aes(x = reorder(host_name, -number_of_reviews), y = number_of_reviews), 
               fill = "darkgreen", width = 0.5) + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ylab("Number of Reviews") + 
      xlab("Host Name") + 
      ggtitle("Top 20 Hosts with Most Number of Reviews") -> p
    ggplotly(p)
  })
  
  output$map <- renderLeaflet({
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
      addTiles() %>% 
      setView(lng = mean(hotel_data_sub$longitude), lat = mean(hotel_data_sub$latitude), zoom=17) %>% 
      addMarkers(lng = hotel_data_sub %>% filter(level == "level1") %>% pull(longitude), 
                 lat = hotel_data_sub %>% filter(level == "level1") %>% pull(latitude), 
                 popup = hotel_data_sub %>% filter(level == "level1") %>% pull(info), 
                 group = "level1") %>% 
      addMarkers(lng = hotel_data_sub %>% filter(level == "level2") %>% pull(longitude), 
                 lat = hotel_data_sub %>% filter(level == "level2") %>% pull(latitude), 
                 popup = hotel_data_sub %>% filter(level == "level2") %>% pull(info), 
                 group = "level2") %>% 
      addMarkers(lng = hotel_data_sub %>% filter(level == "level3") %>% pull(longitude), 
                 lat = hotel_data_sub %>% filter(level == "level3") %>% pull(latitude), 
                 popup = hotel_data_sub %>% filter(level == "level3") %>% pull(info), 
                 group = "level3") %>% 
      groupOptions("level3", zoomLevels = 18:20) %>% 
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


