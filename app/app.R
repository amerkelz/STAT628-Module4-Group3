library(shiny)
library(dplyr)
library(httr)
library(stringr)
library(purrr)
library(tm)
library(spotifyr)
# Get Spotify token
id <- '6611dfd73c4349c5bee592bbf962f03e'
secret <- '612a1022c11147bdb731a2675460048b'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
episode_data <- read.csv('episode_data.csv')
#pca_scores <- read.csv('pca_scores.csv')
#top_200_words<-read.csv("top_200_words.csv")

top_200_words <- c("learn", "choices", "instagram", "trump", "youtube", 
                   "one", "can", "megaphone", "life", "use", "support", 
                   "book", "time", "today", "code", "first", "media", 
                   "sponsors", "read", "show", "times", "spotify", "next", 
                   "twitter", "apple", "privacy", "david", "candace", "people", 
                   "like", "harris", "social", "best", "daily", "please", 
                   "now", "follow", "just", "subscribe", "york", "video", 
                   "website", "also", "listen", "stories", "kamala", "discuss", 
                   "full", "minnect", "connect", "world", "story", "health", 
                   "kids", "news", "bet", "reading", "see", "check", "help", 
                   "want", "aloud", "day", "information", "tiktok", "find", 
                   "theo", "tickets", "business", "talk", "friends", "author", 
                   "music", "year", "brain", "channel", "make", "know", "bobby", 
                   "years", "politics", "links", "morning", "mel", "way", 
                   "policy", "andrew", "bad", "crime", "week", "sleep", "two", 
                   "books", "need", "debate", "thank", "part", "murder", 
                   "president", "latest", "back", "call", "hear", "donald", 
                   "brew", "american", "university", "purchase", "things", 
                   "death", "last", "case", "even", "much", "whether", "biden", 
                   "man", "questions", "live", "election", "patrick", "right", 
                   "month", "real", "war", "watch", "white", "ever", "including", 
                   "morgan", "director", "special", "research", "enjoy", "todays", 
                   "love", "body", "valuetainment", "text", "ceo", "merch", 
                   "exclusive", "advice", "former", "family", "future", "work", 
                   "culture", "available", "secret", "house", "every", "found", 
                   "listener", "state", "shawn", "resources", "pbd", "behind", 
                   "host", "club", "little", "america", "megyn", "tour", "made", 
                   "past", "many", "press", "ryan", "old", "early", "truth", 
                   "big", "israel", "california", "trial", "patreon", "huberman", 
                   "online", "click", "going", "founder", "police", "march", 
                   "moves", "friday", "dont", "kelly", "self", "november", "may", 
                   "campaign", "young", "along", "months", "sign", "school", 
                   "important", "facebook")



pca_result<-read_rds("pca_result.rds")
pca_scores <- as.data.frame(pca_result$x[, 1:2])
colnames(pca_scores) <- c("PC1", "PC2")         
pca_scores$Episode <- rownames(pca_scores)
access_token <- get_spotify_access_token()
token<-access_token
# search for podcasts by name
get_podcasts <- function(podcast_name, token) {
  response <- GET(
    url = 'https://api.spotify.com/v1/search',
    query = list(q = podcast_name, type = 'show', access_token = token)
  )
  
  if (http_status(response)$category != "Success") {
    warning("API request for podcasts failed")
    return(NULL)
  }
  
  res <- httr::content(response, as = "parsed", type = "application/json")
  if (is.null(res$shows$items)) {
    warning("No shows found")
    return(NULL)
  }
  
  podcasts <- map_df(seq_along(res$shows$items), function(x) {
    list(
      podcast_name = res$shows$items[[x]]$name,
      podcast_uri = str_replace(res$shows$items[[x]]$uri, 'spotify:show:', ''),
      podcast_img = ifelse(length(res$shows$items[[x]]$images) > 0, res$shows$items[[x]]$images[[1]]$url, NA)
    )
  })
  
  return(podcasts)
}
# get episodes of a podcast
get_episodes <- function(podcast_uri, token) {
  response <- GET(
    url = paste0('https://api.spotify.com/v1/shows/', podcast_uri, '/episodes'),
    query = list(access_token = token)
  )
  
  if (http_status(response)$category != "Success") {
    warning("API request for episodes failed")
    return(NULL)
  }
  
  res <- httr::content(response, as = "parsed", type = "application/json")
  if (is.null(res$items)) {
    warning("No episodes found")
    return(NULL)
  }
  

  episodes_df <- map_df(seq_along(res$items), function(x) {
    tmp <- res$items[[x]]
    data.frame(
      episode_name = tmp$name,
      episode_uri = str_replace(tmp$uri, 'spotify:episode:', ''),
      episode_description = tmp$description,
      episode_duration_ms = tmp$duration_ms,
      episode_release_date = tmp$release_date,
      episode_img = ifelse(length(tmp$images) > 0, tmp$images[[1]]$url, NA),
      stringsAsFactors = FALSE
    )
  })
  
  return(episodes_df)
}

# fetch descriptions
get_description <- function(episode_uri, token) {
  response <- GET(
    url = paste0('https://api.spotify.com/v1/episodes/', episode_uri),
    query = list(access_token = token)
  )
  
  if (http_status(response)$category != "Success") {
    warning("API request for episode description failed")
    return(NULL)
  }
  
  res <- httr::content(response, as = "parsed", type = "application/json")
  if (is.null(res)) {
    warning("Failed to fetch episode details")
    return(NULL)
  }
  
  return(list(
    description = res$description,
    duration_ms = res$duration_ms,
    release_date = res$release_date
  ))
}



client_id <- '6611dfd73c4349c5bee592bbf962f03e'
client_secret <- '612a1022c11147bdb731a2675460048b'
token <-access_token
# Function to search for podcasts by name
#get_podcasts <- function(query, limit = 20) {
  ##  q = query,
   # type = 'show',
  #  limit = limit
 # )
  
 # if (!is.null(results) && nrow(results) > 0) {
   # podcasts <- results %>%
  #    select(
   #     podcast_name = name,  
   #     podcast_uri = uri    
   #   ) %>%
   #   mutate(
   #     podcast_uri = str_replace(podcast_uri, 'spotify:show:', '')
  #    )
 #   return(podcasts)
#  }
 # return(NULL)
#}
#get_episodes <- function(podcast_uri, limit = 50) {
 # episodes <- spotifyr::get_show_episodes(
 #   show_id = podcast_uri, 
  #  limit = limit
 # )
 # if (!is.null(episodes) && nrow(episodes) > 0) {
  #  episodes_df <- episodes %>%
  #    select(
   #     episode_name = name,   # E
   #     episode_uri = uri      
    #  ) %>%
    #  mutate(
    ##    episode_uri = str_replace(episode_uri, 'spotify:episode:', '') # 
   #   )
    
   # return(episodes_df)
 # }
  # 
 # return(NULL)
#}
#get_description <- function(episode_uri, token) {
#  url <- paste0('https://api.spotify.com/v1/episodes/', episode_uri)
# response <- GET(
 #   url,
   # add_headers(Authorization = paste("Bearer", token))
 # )
 # if (http_status(response)$category == "Success") {
#    episode_details <- httr::content(response, as = "parsed", type = "application/json")
 #   return(episode_details$description) #
 # }
 # 
 # stop("Failed to fetch episode description.")
#}



# Define UI
ui <- fluidPage(
  titlePanel("Spotify Project: STAT 628 Module 4 Group3"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("podcast_search", "Search Podcast:", value = ""),
      actionButton("search_btn", "Search"),
      selectInput("podcast", "Select Podcast:", choices = NULL),
      selectInput("episode", "Select Episode:", choices = NULL),
      actionButton("add_btn", "Add to Playlist")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Clustering Image", 
                 plotOutput("clustering_plot")
        ),
        tabPanel("Recommendations", 
                 h4("Top Recommendations"),
                 tableOutput("recommendation_table"),
        ),
        tabPanel("Playlist",
                 h4("Your Playlist"),
                 tableOutput("playlist_table")
        )
      )
    )
  ),
  
  # About This App section with larger font and 4 lines of text
  h3(style = " font-size: 24px;", "About This App"),
  
  p(style = "font-size: 18px;", "1. Our database contains episode data from the top 50 U.S. podcasts, with up to 200 episodes each, in total 10,970 episodes. We applied Principal Component Analysis to generate new visual metrics based on the first two principal components."),
  
  p(style = " font-size: 18px;", "2. The search bar allows users to search any Spotify podcast, select episodes from a dropdown, and add them to your playlist, which shows your select history and their episode descriptions."),
  p(style = " font-size: 18px;", "3. We analyze new episode descriptions using our PCA model and provide a scatter plot based on the two PCA components."),
  
  p(style = " font-size: 18px;", "4. Recommendations are based on the two primary PCA components, selecting the 10 most 'nearest' episodes based on Euclidean distance and displaying them in the recommendation list.")
)


# Define server
server <- function(input, output, session) {
  podcasts <- reactiveVal(data.frame())
  episodes <- reactiveVal(data.frame())
  playlist <- reactiveVal(data.frame(Podcast = character(), Episode = character(), Description = character(), stringsAsFactors = FALSE))
  pca_scores <- reactiveVal(data.frame(pca_scores))
  new_pca <- reactiveVal(data.frame(PC1 = numeric(0), PC2 = numeric(0), Episode = character(), Type = character())) # New points
 # episode_data <- reactiveVal(data.frame(episode_data))
  # Search for podcasts
  observeEvent(input$search_btn, {
    # Check if the input is non-empty and has more than 2 characters
    if (nchar(input$podcast_search) > 2) {
      result <- get_podcasts(input$podcast_search, token)
      if (!is.null(result)) {
        podcasts(result)
        updateSelectInput(session, "podcast", choices = result$podcast_name)
      } else {
        showNotification("No podcasts found for the search term.", type = "warning")
      }
    } else {
      # Show an error or warning notification if the input is too short
      showNotification("Please enter at least 3 characters to search.", type = "warning")
    }
  })
  # Get episodes 
  observeEvent(input$podcast, {
    if (!is.null(input$podcast) && input$podcast %in% podcasts()$podcast_name) {
      selected_podcast <- podcasts() %>% filter(podcast_name == input$podcast)
      result <- get_episodes(selected_podcast$podcast_uri, token)
      if (!is.null(result)) {
        episodes(result)
       # episode_data(result)  
        updateSelectInput(session, "episode", choices = result$episode_name)
      }
    }
  })
  
  # Add to playlist and compute PCA for new episode
  observeEvent(input$episode, {
    if (!is.null(input$episode) && input$episode %in% episodes()$episode_name) {
      selected_episode <- episodes() %>% filter(episode_name == input$episode)
      description <- get_description(selected_episode$episode_uri, token)$description
      clean_text <- tolower(description)
      clean_text <- gsub("[[:punct:]]", " ", clean_text)      
      clean_text <- gsub("[[:digit:]]", " ", clean_text)    
      clean_text <- removeWords(clean_text, stopwords("en")) 
      clean_text <- stripWhitespace(clean_text)              
      word_list <- strsplit(clean_text, " ")[[1]]            
      word_freq <- table(word_list)                             
      new_episode_vector <- rep(0, length(top_200_words))
      names(new_episode_vector) <- top_200_words
      common_words <- intersect(names(word_freq), top_200_words)
      new_episode_vector[common_words] <- word_freq[common_words]

      new_episode_df <- as.data.frame(t(new_episode_vector))
      #colnames(new_episode_df) <- colnames(tdm_df) 
      new_episode_pca <- predict(pca_result, newdata = new_episode_df)
      new_episode_coords <- new_episode_pca[1, 1:2]
      new_point <- data.frame(PC1 = new_episode_coords[1], PC2 = new_episode_coords[2], 
                              Episode = input$episode, Type = "New")
  
      pca_scores(pca_scores())
      new_pca(rbind(new_pca(), new_point))
      new_entry <- data.frame(
        Podcast = input$podcast,
        Episode = input$episode,
        Description = description,
        stringsAsFactors = FALSE
      )
      playlist(rbind(playlist(), new_entry))
    }
  })
  output$playlist_table <- renderTable({
    playlist()
  }) 
  # Render PCA plot for clustering visualization
  output$clustering_plot <- renderPlot({
    pca_data <- pca_scores()
    new_points <- new_pca()
    ggplot() +
      geom_point(data = pca_data, aes(x = PC1, y = PC2), color = "blue", alpha = 0.6) +
      geom_point(data = new_points, aes(x = PC1, y = PC2), color = "red", size = 3) +
      labs(
        title = "PCA Visualization of Podcast Episodes",
        x = "Principal Component 1 (PC1)",
        y = "Principal Component 2 (PC2)"
      ) +
      theme_minimal()
  })
  #  Euclidean distance 
  calculate_distance <- function(point1, point2) {
    point1 <- as.numeric(point1)
    point2 <- as.numeric(point2)
    sqrt((point1[1] - point2[1])^2 + (point1[2] - point2[2])^2)
  }
  
  # Recommend 10 nearest episodes
  output$recommendation_table <- renderTable({
    new_point <- new_pca() 
    
    distances <- sapply(1:nrow(pca_scores()), function(i) {
      calculate_distance(c(pca_scores()$PC1[i], pca_scores()$PC2[i]), c(new_point$PC1, new_point$PC2))
    })
    
    pca_scores_with_dist <- cbind(pca_scores(), Distance = as.numeric(distances))
    pca_scores_with_dist <- pca_scores_with_dist %>%
      filter(!is.na(Episode), grepl("^[0-9]+$", Episode)) %>%
      mutate(Episode = as.numeric(Episode))
    
    recommendations <- pca_scores_with_dist %>%
      arrange(Distance) %>%
      mutate(
        Podcast = episode_data$podcast_name[as.numeric(Episode)],  
        Episode = episode_data$episode_name[as.numeric(Episode)]  
      ) %>%
      select(Podcast, Episode, Distance)
    recommendations <- recommendations %>%
      distinct(Episode, .keep_all = TRUE)
    recommendations <- head(recommendations, 10)
    
    recommendations
  })
  
  
}


shinyApp(ui = ui, server = server)




