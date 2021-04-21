
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
      menuItem("Console Sales", tabName = "console", icon = icon("gamepad"))
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
                                choices = unique(decade$decade)
                                
                    )
                )

    
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
              h2("Game Genres"),
              fluidRow( # <----------------------------------------- fluid row 1
                
                box(title = "Controls", # <----------- drop down
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    height = NULL,
                    selectInput("genre", 
                                "Choose your genre:", 
                                choices = unique(genre$genre),
                    )
                ),
                
                
              ), # <----------------------------------------- closes fluid row 1
              
              fluidRow( # <----------------------------------------- fluid row 2
                
                box(title = "Plot", # <--------------- plot
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = NULL,
                    plotOutput("plot2")
                    
                )
                
              ), # <------------------------------------------ close fluid row 2
              
              fluidRow( # <----------------------------------------- fluid row 3
                
                
                box(title = "Table", # <-------------- table
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = NULL,
                    DT::dataTableOutput("table2")
                )
                
              ) # <------------------------------------------ closes fluid row 3
            
      ), # <------------------------------------------------------- closes tab 2
      
      # <----------------------------------------------------------------- tab 3
      tabItem(tabName = "regional",
              h2("Regional Sales"),
              fluidRow( # <----------------------------------------- fluid row 1
                
                box(title = "Controls", # <----------- drop down
                    status = "primary",
                    solidHeader = TRUE,
                    width = 6,
                    height = NULL,
                    selectInput("region", 
                                "Choose your region:", 
                                choices = unique(regional$region),
                    )
                ),
                
                
              ), # <----------------------------------------- closes fluid row 1
              
              
              fluidRow( # <----------------------------------------- fluid row 2
                
                box(title = "Plot", # <--------------- plot
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = NULL,
                    plotOutput("plot3")
                    
                )
                
              ), # <------------------------------------------ close fluid row 2
              
              fluidRow( # <----------------------------------------- fluid row 3
                
                
                box(title = "Table", # <-------------- table
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = NULL,
                    DT::dataTableOutput("table3")
                )
                
              ) # <------------------------------------------ closes fluid row 3
      
      ), # <------------------------------------------------------- closes tab 3
      
      # <----------------------------------------------------------------- tab 4
      tabItem(tabName = "console",
              h2("Top Games"),
              
              fluidRow( # <----------------------------------------- fluid row 1
                
                box(title = "Controls", # <----------- drop down
                    status = "primary",
                    solidHeader = TRUE,
                    width = 3,
                    height = NULL,
                    selectInput("console", 
                                "Choose your console:", 
                                choices = unique(console$console),
                    )
                )
              ), # <----------------------------------------- closes fluid row 1
                
                
              fluidRow( # <----------------------------------------- fluid row 2
                  
                box(title = "Plot", # <--------------- plot
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = NULL,
                    plotOutput("plot4")
                )
              ), # <----------------------------------------- closes fluid row 2
              
              fluidRow( # <----------------------------------------- fluid row 3
                
                box(title = "Table", # <-------------- table
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    height = NULL,
                    DT::dataTableOutput("table4")
                )
                
              ), # <------------------------------------------ close fluid row 3
      
      ) # <-------------------------------------------------------- closes tab 4
    
    
    ) # <------------------------------------------------------ closes tab items
  ) # <--------------------------------------------------- closes dashboard body
) # <----------------------------------------------------- closes dashboard page

# SERVER -----------------------------------------------------------------------
  
server <- function(input, output) {
  
  # Plot 1 ----
  
  output$plot1 <- renderPlot({
    
    decade %>% 
      ggplot(aes(x = reorder(genre, global_sales_millions), 
                 y = global_sales_millions)) +
      geom_col(alpha = 0.8, colour = "white", fill = "#cc9900",
               data = decade[decade$decade == 
                                          input$decade,]) +
      coord_flip() +
      theme_light() +
      labs(title = "Top Performing Games Genres", 
           subtitle = "",
           x = "Genre", 
           y = "Global Unit Sales (millions)") 
    
  })
  
  # Table 1 ----
  
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
  
  # Plot 2 ----
  
  output$plot2 <- renderPlot({
  
  genre %>% 
    
    ggplot(aes(x = decade, y = decade_sales)) +
    geom_area(alpha = 0.5, fill = "#666633",
              data = genre[genre$genre == 
                                         input$genre,]) +
    geom_line(colour = "#cc9900", size = 1.2,
              data = genre[genre$genre == 
                                   input$genre,]) +
    geom_point(colour = "#666633",
               data = genre[genre$genre == 
                                    input$genre,]) +
    labs(title = "",
         x = "Decade", 
         y = "Global Unit Sales (millions)") +
    theme_light()
    
  })
  
  # Table 2 ----
  
  output$table2 <- DT::renderDataTable({
  
  games %>% 
    group_by(name, genre) %>% 
    summarise(total_global_sales = sum(global_sales)) %>% 
    arrange(desc(total_global_sales)) %>% 
    filter(genre == input$genre)
    
  })
  
  # Plot 3 ----
  
  output$plot3 <- renderPlot({
    
    regional %>% 
      ggplot(aes(x = reorder(genre, sales_millions), 
                 y = sales_millions)) +
      geom_col(alpha = 0.8, colour = "white", fill = "#cc9900",
               data = regional[regional$region == input$region,]) +
      coord_flip() +
      theme_light() +
      labs(title = "", 
           subtitle = "",
           x = "Genre", 
           y = "Sales (millions)") 
      
  })
  
  # Table 3 ----
  
  output$table3 <- DT::renderDataTable({
    
    regional_2 %>% 
      group_by(name, region) %>% 
      summarise(total_sales = sum(sales)) %>% 
      arrange(desc(total_sales)) %>% 
      filter(region == input$region)
    
  })
  
  # Plot 4 ----
  
  output$plot4 <- renderPlot({
  
  console2 %>% 
    filter(console == input$console) %>% 
    ggplot(aes(x = reorder(genre, sales_millions), 
               y = sales_millions)) +
    geom_col(alpha = 0.8, colour = "white", fill = "#cc9900") +
    coord_flip() +
    theme_light() +
    labs(title = "", 
         subtitle = "",
         x = "Genre", 
         y = "Sales (millions)") 
    
  })
  
  # Table 4 ----
  
  output$table4 <- DT::renderDataTable({
    
   console %>% 
      group_by(name, console) %>% 
      summarise(total_sales = sum(sales)) %>% 
      arrange(desc(total_sales)) %>% 
      filter(console == input$console) 
      
    
  })
  
  
}

# APP FUNCTION -----------------------------------------------------------------

shinyApp(ui, server)




      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
