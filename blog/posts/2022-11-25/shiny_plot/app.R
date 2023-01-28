#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

df_clust <- arrow::read_parquet("df_clust.pq")
choices <- colnames(df_clust)[colnames(df_clust) != "kmeans"]

ui <- fluidPage(

  title = "Player Clusters",
  
  fluidRow(
    column(
      4,
      selectInput("x_select", "x-axis", choices = choices, selected = "min"),
      selectInput("y_select", "y-axis", choices = choices, selected = "pts"),
      selectInput("z_select", "z-axis", choices = choices, selected = "fga")
    ),
    column(8, plotlyOutput("kmeans_plot"))
  ),
)


server <- function(input, output) {
    
    output$kmeans_plot <- renderPlotly({
      df_clust |> 
        plot_ly(
          x = ~get(input$x_select),
          y = ~get(input$y_select),
          z = ~get(input$z_select),
          color = ~kmeans,
          text = df_clust$kmeans, # Replace with player name and season
          hoverinfo = "text"
        ) |> 
        add_markers() |> 
        layout(scene = list(
          xaxis = list(title = input$x_select),
          yaxis = list(title = input$y_select),
          zaxis = list(title = input$z_select)
        ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
