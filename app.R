library(tidyverse)
library(shiny)
library(skimr)


film_dat <- read_csv("movies.csv")
world_coordinates <- map_data("world")

final_film <- drop_na(film_dat)

#bar graphs
bar_data <- final_film %>%
  select(c(score, genre, runtime, rating, year))
bar_data <- bar_data %>%
  rename_with(str_to_title)


country_bar_data <- final_film %>%
  select(c(score, genre, runtime, rating, year, country))
country_bar_data <- country_bar_data %>%
  rename_with(str_to_title)

#map graphs
map_data <- final_film %>%
  select(c(gross, runtime, budget, country))
map_data <- map_data %>%
  rename_with(str_to_title)
map_graph_names <- map_data %>%
  select(-Country) %>%
  rename_with(str_to_title)

world_film <- world_coordinates %>%
  filter(region %in% unique(final_film$country))


#data wrangling for time plots
#getting mean by each group w groups being cols

time_dat <- final_film %>%
  group_by(year) %>%
  summarize(mean_score = mean(score), 
            mean_votes = mean(votes),
            mean_budget = mean(budget),
            mean_gross = mean(gross),
            mean_runtime = mean(runtime),
            .groups = 'drop')

time_country <- final_film %>%
  group_by(country) %>%
  summarize(mean_score = mean(score), 
            mean_votes = mean(votes),
            mean_budget = mean(budget),
            mean_gross = mean(gross),
            mean_runtime = mean(runtime))



library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Movie Data Analysis (1980-2020)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Trends by Country", tabName = "distCountry", icron = icon("map")),
      menuItem("Background & Data", tabName = "background", icon = icon("file"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "Dashboard",
              p("The 'Dashboard' page illustrates general trends by the key variables in the dataset."),
              fluidRow(
                box(plotOutput("distPlot", height = 250)),
                
                box(
                  title = "General Trends",
                  selectInput("bar_var",
                              "Choose a variable to display:",
                              choices = colnames(bar_data),
                              selected = "Rating"))
      ),
      
      tabItem(tabName = "distYear",
              p("These graphs illustrates general trends by the average of key variables over time."),
              fluidRow(
                box(plotOutput("timePlot")),
                box(title = "Time Trends", 
                    selectInput("bar_var3",
                                "Choose a variable to display:",
                                choices = c("Average Scores", "Average Votes", 
                                            "Average Runtime", "Average Budget", 
                                            "Average Gross"),
                                selected = "Average Scores")
                ))),
      
      tabItem(tabName = "background",
              textOutput("backgroundInfo"))
    )
  ),
  
  tabItems(
    tabItem(tabName = "distCountry",
            p("Observe summary trends by country"),
            fluidRow(
              box(plotOutput("barCountry")),
              box(title = "Summary Bar Graphs",
                  selectInput("barC",
                              "Choose a variable to observe:",
                              choices = colnames(bar_data),
                              selected = "Rating"))
            ),
        tabItem(tabName ="distCountry",
                p("Observe time trends by country"),
                fluidRow(
                  box(plotOutput("timeCountry")),
                  box(title = "Summary Time Graphs",
                      selectInput("timeC",
                                  "Choose a variable to observe:",
                                  choices = c("Average Scores", "Average Votes", 
                                              "Average Runtime", "Average Budget", 
                                              "Average Gross"),
                                  selected = "Average Scores"))
                ))
            )
  )
)
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    
    bar_x_lab <- switch(input$bar_var,
                        "Rating" = "Distribution of Film Ratings",
                        "Genre" = "Genres",
                        "Runtime" = "Distribution of Runtime",
                        "Score" = "Distribution of Scores", 
                        "Year" = "Year")
    
    bar_var <- switch(input$bar_var,
                      "Score" = bar_data$Score,
                      "Genre" = bar_data$Genre,
                      "Runtime" = bar_data$Runtime,
                      "Rating" = bar_data$Rating,
                      "Year" = bar_data$Year)
    
    ggplot(data = bar_data, aes(x = bar_var)) + 
      geom_bar(fill = "purple",
               color = "orange") + 
      xlab(bar_x_lab) + 
      ylab("Count") + 
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 0.5, 
                                       hjust=1),
            title = element_text(size = 13,
                                 face = "bold"))
  })
  
  output$timePlot <- renderPlot (
    {
      y_label <- input$bar_var3
      var_t <- switch(input$bar_var3,
                      "Average Scores" = time_dat$mean_score,
                      "Average Runtime" = time_dat$mean_runtime,
                      "Average Budget" = time_dat$mean_budget,
                      "Average Gross" = time_dat$mean_gross,
                      "Average Votes" = time_dat$mean_votes
      )
      
      ggplot(data = time_dat,
             aes(x = year,
                 y = var_t)) +
        theme_classic() +
        geom_line(color = "purple") +
        geom_point(color = "orange") +
        ylab(y_label) +
        xlab("Year") +
        ggtitle("Trends over Time") +
        theme(title = element_text(face = "bold",
                                   size = 12))
    })
  
  output$backgroundInfo <- renderText({
    ("This dashboard aims to summarize overall trends in the film industry. 
     This data was found on Kaggle and can be accessed here: https://www.kaggle.com/datasets/danielgrijalvas/movies")})
  
  output$timeCountry <- renderPlot
  ({
    
    country_dat <- final_film %>%
      filter(country == input$timeC) %>%
      group_by(year) %>%
      summarize(mean_score = mean(score), 
                mean_votes = mean(votes),
                mean_budget = mean(budget),
                mean_gross = mean(gross),
                mean_runtime = mean(runtime))
      
    
    y_label <- input$timeC 
    var_c <- switch(input$timeC,
                    "Average Scores" = country_dat$mean_score,
                    "Average Runtime" = country_dat$mean_runtime,
                    "Average Budget" = country_dat$mean_budget,
                    "Average Gross" = country_dat$mean_gross,
                    "Average Votes" = country_dat$mean_votes)
    
    ggplot(data = country_dat,
           aes(x = year,
               y = var_c)) +
      theme_classic() +
      geom_line(color = "purple") +
      geom_point(color = "orange") +
      ylab(y_label) +
      xlab("Year") +
      ggtitle() +
      theme(title = element_text(face = "bold",
                                 size = 12))
    
    
    
    
  })
  
}

shinyApp(ui, server)
