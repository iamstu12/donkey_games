
# LIBRARIES --------------------------------------------------------------------

library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinythemes)

# DATA -------------------------------------------------------------------------

games <- read_csv("clean_data/clean_sales_data.csv")

# USER INTERFACE ---------------------------------------------------------------

ui <- dashboardPage(
  
  # Theme ----
  skin = "blue",
  
  # Main title ----
  dashboardHeader(title = "Video Games", 
                  titleWidth = 250),
  
  # Sidebar ----
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Games History", tabName = "history", icon = icon("scroll")),
      menuItem("Game Genres", tabName = "genres", icon = icon("gamepad")),
      menuItem("Regional Sales", tabName = "regional", icon = icon("globe")),
      menuItem("Top Games", tabName = "top", icon = icon("trophy"))
    )
    
  ),
  
  # Main body and tabs ----
  dashboardBody(
    tabItems(
      
      # <----------------------------------------------------------------- tab 1
      tabItem(tabName = "history",
              h2("Games History"),
              fluidRow( # <----------------------------------------- fluid row 1
                
                box(title = "Controls", # <----------- drop down
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    height = NULL,
                    selectInput("decade", 
                                "Choose your decade:", 
                                choices = unique(games_decade_data$decade),
                    )
                ),
                
    
              ), # <----------------------------------------- closes fluid row 1
              
              fluidRow( # <----------------------------------------- fluid row 2
                
                box(title = "Plot", # <--------------- plot
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = NULL,
                    plotOutput("plot1")
            
                )
                
              ), # <------------------------------------------ close fluid row 2
              
              
      fluidRow( # <----------------------------------------- fluid row 3
        
        
        box(title = "Table", # <-------------- table
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = NULL,
            DT::dataTableOutput("table1")
        )
        
      ) # <-------------------------------------------------- closes fluid row 3
              
              
      ), # <------------------------------------------------------- closes tab 1
      
      # <----------------------------------------------------------------- tab 2
      tabItem(tabName = "genres",
              h2("Game Genres")
      ), # <------------------------------------------------------- closes tab 2
      
      # <----------------------------------------------------------------- tab 3
      tabItem(tabName = "regional",
              h2("Regional Sales")
      
      ), # <------------------------------------------------------- closes tab 3
      
      # <----------------------------------------------------------------- tab 4
      tabItem(tabName = "top",
              h2("Top Games")
      
      ) # <-------------------------------------------------------- closes tab 4
    
    ) # <------------------------------------------------------ closes tab items
  ) # <--------------------------------------------------- closes dashboard body
) # <----------------------------------------------------- closes dashboard page

# SERVER -----------------------------------------------------------------------
  
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    games_decade_data %>% 
      ggplot(aes(x = reorder(genre, global_sales_millions), 
                 y = global_sales_millions)) +
      geom_col(alpha = 0.8, colour = "white", fill = "#cc9900",
               data = games_decade_data[games_decade_data$decade == 
                                          input$decade,]) +
      coord_flip() +
      theme_light() +
      labs(title = "Top Performing Games Genres", 
           subtitle = "",
           x = "Genre", 
           y = "Global Unit Sales (millions)") 
    
  })
  
  output$table1 <- DT::renderDataTable({
 
    games %>% 
      mutate(year_of_release = as.numeric(year_of_release)) %>% 
      filter(year_of_release != "Unknown") %>% 
      mutate(decade = floor(year_of_release / 10) * 10) %>% 
      filter(decade != 2020) %>% 
      group_by(decade, genre, name) %>% 
      summarise(global_sales_millions = sum(global_sales)) %>% 
      arrange(desc(global_sales_millions)) %>% 
      filter(decade == input$decade)
    
  })
  
  
}

# APP FUNCTION -----------------------------------------------------------------

shinyApp(ui, server)




      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
