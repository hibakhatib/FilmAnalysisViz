library(tidyverse)
library(shiny)
library(skimr)
library(shinydashboard)
library(maps)
library(ggrepel)


film_dat <- read_csv("movies.csv")

final_film <- drop_na(film_dat)


  #will probably use this for the bar graphs and delete data wrangling below
map_film <- final_film %>%
  select(c(rating, genre, year, votes, 
           score, country, gross, budget, 
           runtime)) %>%
  rename_with(str_to_title)

  #average for fills on world map

fill_map <- map_film %>%
  group_by(Country) %>%
  summarize(mean_gross = mean(Gross),
            mean_score = mean(Score),
            mean_budget = mean(Budget),
            mean_votes = mean(Votes),
            mean_runtime = mean(Runtime),
            .groups = 'drop') %>%
  pivot_longer(!Country, names_to = "cat", values_to = "value")



#bar graphs
bar_data <- final_film %>%
  select(c(score, genre, runtime, rating, year))
bar_data <- bar_data %>%
  rename_with(str_to_title)

#map graphs
country_bar_data <- final_film %>%
  select(c(score, genre, runtime, rating, year, country))
country_bar_data <- country_bar_data %>%
  rename_with(str_to_title)

map_data <- final_film %>%
  select(c(gross, runtime, budget, country))
map_data <- map_data %>%
  rename_with(str_to_title)
map_graph_names <- map_data %>%
  select(-Country) %>%
  rename_with(str_to_title)


##data wrangling forcompanies

company_avgs <- final_film %>%
  group_by(company) %>%
  summarize(mean_gross = mean(gross),
            mean_score = mean(score),
            mean_budget = mean(budget),
            mean_votes = mean(votes),
            mean_runtime = mean(runtime),
            .groups = 'drop')

top_3 <- company_avgs %>%
  filter(mean_gross >= 966554929)

worst_3 <- company_avgs %>%
  filter(mean_gross < 6000)

#data wrangling for time plots

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


#### APP CODE STARTS HERE 

ui <- dashboardPage(
  dashboardHeader(title = "Movie Data Analysis (1980-2020)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Trends by Company", tabName = "distCompany", icon = icon("pen")),
      menuItem("Histograms", tabName = "distCountry", icon = icon("list-alt")),
      menuItem("Trends by Country", tabName = "mapGraphs", icon = icon("map"))
      #menuItem("Trends by Directors", tabName = "distDirector", icon = icon("film"))
    )
  ),
  dashboardBody(

    tabItems(
      tabItem(tabName = "Dashboard",
              h4("The 'Dashboard' page illustrates general trends by the key variables in the dataset."),
              h5("Movie Data (1980-2020"),
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
                      h4("These graphs illustrates general trends by the average of key variables over time."),
                      fluidRow(
                        box(plotOutput("timePlot")),
                        
                        box(
                          title = "Time Trends", 
                          selectInput("bar_var3",
                                      
                                      "Choose a variable to display:",
                                      choices = c("Average Scores", "Average Votes", 
                                                  "Average Runtime", "Average Budget", 
                                                  "Average Gross"),
                                      selected = "Average Scores")
                        ))),
              
      ),
      
      ##COMPANY LAYOUT START 
      tabItem(tabName = "distCompany",
              h4("These graphs display summary trends by top 3 worst/best grossing companies"),
              h5("Movie Data (1980-2020"),
              fluidRow(
                box(plotOutput("barTop")),
                
                box(title = "Top 3 Grossing Companies",
                    selectInput("top3_var",
                                
                                "Choose a variable to observe:",
                                choices = c("Average Scores", "Average Votes", 
                                            "Average Runtime", "Average Budget", 
                                            "Average Gross"),
                                selected = "Average Scores")
                )
              ),
              tabItem(tabName ="distCompany",
                      fluidRow(
                        box(plotOutput("barBottom")),
                        box(title = "Worst 3 Grossing Companies",
                            selectInput("worst3_var",
                                        "Choose a variable to observe:",
                                        choices = c("Average Scores", "Average Votes", 
                                                    "Average Runtime", "Average Budget", 
                                                    "Average Gross"),
                                        selected = "Average Scores"))))
      ),
      
      ##COMPANY LAYOUT END
      
      ##HISTOGRAM LAYOUT START
      tabItem(tabName = "distCountry",
              h4("Explore data with histograms"),
              fluidRow(
                box(plotOutput("bCount")),
                box(title = "Interact with the slider, fill, and variable options:",
                    selectInput("fill", 
                                label = "Select fill/legend variable",
                                choices = c("Genre", "Rating"),
                                selected = "Genre"),
                    
                    sliderInput("bins",
                                "Number of bins:",
                                min = 5,
                                max = 50,
                                value = 30),
                    
                    radioButtons("var",
                                 label = "Variable of interest",
                                 choices = c("Gross", 
                                             "Budget", 
                                             "Score", 
                                             "Votes", 
                                             "Runtime"),
                                 selected = "Score"))
              )),
      
       ## HIST ENDS HERE 
      
       ## MAP STARTS HERE  & continues to break :)
      
      tabItem(tabName = "mapGraphs",
              h4("Explore overall world film data & data by countries"),
              fluidRow(
                box(plotOutput("main_world_map")),
                
                box(title = "Average Financial Trends",
                    selectInput("country_var",
                                "Choose a variable to observe:",
                                choices = unique(fill_map$Country),
                                selected = "Aruba")
                )
              )
              # ,
              # tabItem(tabName ="mapGraphs",
              #         fluidRow(
              #           box(plotOutput("main_scores")),
              #           box(title = "Averages over time",
              #               selectInput("variable_choice",
              #                           "Choose a variable to observe:",
              #                           choices = unique(fill_map$Country),
              #                           selected = "Aruba")),
              #           box(
              #             title = "Time Trends", 
              #             selectInput("overTime",
              #                         
              #                         "Choose a variable to display:",
              #                         choices = c("Average Scores", "Average Votes", 
              #                                     "Average Runtime", "Average Budget", 
              #                                     "Average Gross"),
              #                         selected = "Average Scores")
              #           )))
              
      )
      
      
      
      )))


server <- function(input, output) {
  
  ### MAIN DASHBOARD STARTS PAGE HERE 
  
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
        theme(title = element_text(face = "bold",
                                   size = 12))
    })
  
  ### MAIN DASHBOARD PAGE ENDS HERE
  
  # ------------------------------------------------------------------------
  
  ### COMPANY PAGE STARTS HERE
  
  output$barTop <- renderPlot({
    
    bar_top3 <- switch(input$top3_var, 
                       "Average Scores" = top_3$mean_score,
                       "Average Runtime" = top_3$mean_runtime,
                       "Average Budget" = top_3$mean_budget,
                       "Average Gross" = top_3$mean_gross,
                       "Average Votes" = top_3$mean_votes)
    
    
    ggplot(data = top_3, 
           aes(y = company,
               x = bar_top3)) + 
      geom_col(fill = "blue", 
               color = "orange", width = 0.85) +
      ggtitle("Trends for Top 3 Grossing Companies") +
      theme_classic() + 
      ylab(NULL) +
      xlab(input$top3_var) +
      theme(text = element_text(size = 12,
                                face = "bold"))
    
  })
  
  output$barBottom <- renderPlot(
    {
      bar_bad3 <- switch(input$worst3_var, 
                         "Average Scores" = worst_3$mean_score,
                         "Average Runtime" = worst_3$mean_runtime,
                         "Average Budget" = worst_3$mean_budget,
                         "Average Gross" = worst_3$mean_gross,
                         "Average Votes" = worst_3$mean_votes)
      
      ggplot(data = worst_3, 
             aes(y = company,
                 x = bar_bad3, width = 0.8)) + 
        geom_col(fill = "blue", 
                 color = "orange") +
        ggtitle("Trends for Worst 3 Grossing Companies") +
        theme_classic() +
        ylab(NULL) + 
        xlab(input$worst3_var) +
        theme(text = element_text(size = 12,
                                  face = "bold"))
      
    })
  
  ### COMPANY PAGE ENDS HERE
  
  # ------------------------------------------------------------------------
  
  
  ### HISTOGRAMS START HERE
  output$bCount <- renderPlot({
    
    fill_hist <- switch(input$fill,
                        "Genre" = final_film$genre,
                        "Rating" = final_film$rating)
    
    x_lab <- switch(input$var,
                    "Genre" = "Film Genre",
                    "Rating" = "Rating")
    
    legend_label = switch(input$fill,
                          "Score" = "Score",
                          "Votes" = "Votes",
                          "Runtime" = "Runtime",
                          "Gross" = "Gross",
                          "Budget" = "Budget")
    
    var_hist <- switch(input$var,
                       "Score" = final_film$score,
                       "Votes" = final_film$votes,
                       "Runtime" = final_film$runtime,
                       "Gross" = final_film$gross,
                       "Budget" = final_film$budget)
    
    breaks <- seq(min(var_hist), max(var_hist), length.out = input$bins + 1)
    
    ggplot(data = final_film) +
      geom_histogram(aes(x = var_hist,
                         fill = fill_hist),
                     color = "black",
                     breaks = breaks) +
      theme_classic() +
      scale_fill_discrete(name = legend_label) +
      labs(fill = input$fill, 
           x = x_lab, 
           y = "Count", 
           title = legend_label) +
      xlab("Variable of interest") +
      ylab("Count") +
      ggtitle(legend_label)
  })
  
  ### HISTOGRAMS END HERE

#------------------------------------------------------------------------
  ### COUNTRY PAGE STARTS HERE
  
  
  
  output$main_world_map <- renderPlot({
    
    country_dat <- fill_map %>%
      filter(Country == input$country_var) %>%
      filter(cat %in% c("mean_budget", "mean_gross"))
    
    g_title <- input$country_var 
    
    ggplot(country_dat,
           aes(x = cat,
               y = value)) +
      geom_col(fill = "darkorange3",
               color = "blue") +
      theme_classic() +
      ggrepel::geom_text_repel(aes(label = value), colour = "black")+
      theme(text = element_text(size = 12),
            axis.text.x = element_text(angle = 90, 
                                       vjust = 0.5, 
                                       hjust=1, 
                                       face = "bold")) +
      ylab("Value") +
      xlab(NULL) +
      ggtitle(g_title)
  
  })
  
  # output$main_scores <- renderPlot
  # ({
  #   
  #   country_dat <- fill_map %>%
  #     filter(Country == input$country_scores) %>%
  #     filter(cat %in% c("mean_scores", "mean_votes", "mean_runtime"))
  #   
  #   g_title <- input$country_scores 
  #   
  #   ggplot(country_dat,
  #          aes(x = cat,
  #              y = value)) +
  #     geom_col(fill = "darkorange3",
  #              color = "blue") +
  #     theme_classic() +
  #     ggrepel::geom_text_repel(aes(label = value), colour = "black")+
  #     theme(text = element_text(size = 12),
  #           axis.text.x = element_text(angle = 90, 
  #                                      vjust = 0.5, 
  #                                      hjust=1, 
  #                                      face = "bold")) +
  #     ylab("Value") +
  #     xlab(NULL) +
  #     ggtitle(g_title)
  #   
  # })
  
}

shinyApp(ui, server)