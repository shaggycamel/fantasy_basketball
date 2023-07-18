

source(here::here("shiny", "init_nba_shiny.R"))
  

# Header
header <- dashboardHeader(title = "NBA")
  
# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Player Overview", tabName = "player_overview", icon = icon("chart-bar")),
    menuItem("Player Trend", tabName = "player_trend", icon = icon("chart-line"))
  )
)
    
# Body
body <- 
  dashboardBody(
    tabItems(
      
      # Player Overview tab
      tabItem(tabName = "player_overview",
        fluidRow(
          column(
            width = 4, 
            selectInput("overview_select_stat", "Statistic", choices = stat_selection$formatted_name),
            sliderTextInput("overview_minute_filter", "Limit Minutes", choices = 0), # updated dynamically in server.R
            sliderInput("overview_slider_top_n", "Top N Players", min = 10, max = 20, value = 15, ticks = FALSE),
            checkboxInput("overview_scale_by_minutes", "Scale by Minutes"),
            checkboxInput("overview_free_agent_filter", "Only Show Non-Injured Free Agents")
          ),
        
          # Plot
          column(width = 8, plotlyOutput("player_overview_plot", height = 600)) # unsure how to make height dynamic, as in = "100%"
        )
      ),
      
      # Player Trend tab
      tabItem(tabName = "player_trend",
        fluidRow(
          column(
            width = 4, 
            selectInput("trend_select_stat", "Statistic", choices = stat_selection$formatted_name),
            selectInput("trend_select_player", "Player", multiple = TRUE, choices = character(0))
          ),
        
          # Plot
          column(width = 8, plotOutput("player_trend_plot", height = 600)) # unsure how to make height dynamic, as in = "100%"
        )
      )
    )
  )

# Instantiate page
ui <- dashboardPage(header, sidebar, body)