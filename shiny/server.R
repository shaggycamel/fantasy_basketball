

source(here::here("shiny", "init_nba_shiny.R"))


# Server
server <- function(input, output, session) {
  
# Overview Analysis -------------------------------------------------------

  # Initial query (original dataframe)
  df_overview <- dh_getQuery(postgre_nba_con, "shiny_overview_query.sql") |> 
    slice_max(order_by = id_player_nba, by = name_player) 
  # for some reason identical records appear with different player_id
  
  # Dynamically update filters: select_stat, minute_filter
  observe({
    updateSliderTextInput(session, "overview_minute_filter", choices = seq(from = max(df_overview$minutes_totals), to = min(df_overview$minutes_totals)), selected = round(quantile(df_overview$minutes_totals)[["75%"]]))
  })
  
  # Code to render plot
  output$player_overview_plot <- renderPlotly({
    
    # Free Agent filter (if selected)
    df_overview_plt <- if(input$overview_free_agent_filter) filter(df_overview, !is.na(fty_player_id))
      else df_overview
    
    # Minute filter
    df_overview_plt <- filter(df_overview_plt, minutes_totals >= as.numeric(input$overview_minute_filter))
    
    # Scale by minutes (if selected)
    if(input$overview_scale_by_minutes) df_overview_plt <- mutate(df_overview_plt, across(all_of(stat_selection$overview_name), ~ .x / minutes_totals))
    
    # Create df for plot
    df_overview_plt <- map(stat_selection$overview_name, ~ {
      
      col = sym(.x)
      
      if(col == sym("tov_totals")){
        slice_max(df_overview_plt, order_by = minutes_totals, prop = 0.35) |> 
          select(name_player, {{ col }}) |>
          arrange({{ col }}) |> 
          slice_head(n = input$overview_slider_top_n) |> 
          set_names(c("name_player", "value"))
      } else {
        select(df_overview_plt, name_player, {{ col }}) |>
          arrange(desc({{ col }})) |> 
          slice_head(n = input$overview_slider_top_n) |> 
          set_names(c("name_player", "value"))
      }
      
    }) |> 
      set_names(stat_selection$formatted_name) |> 
      bind_rows(.id = "stat") |> 
      mutate(top_cat_count = n(), .by = name_player) |> 
      mutate(top_cats = paste(stat, collapse = ", "), .by = name_player)
    
    # Stat selection and render plot
    plt <- filter(df_overview_plt, stat == input$overview_select_stat) |> 
      ggplot(aes(x = value, y = if(input$overview_select_stat == "Turnovers") reorder(name_player, -value) else reorder(name_player, value), fill = ordered(top_cat_count), text = top_cats)) +
      geom_col() +
      guides(fill = guide_legend(title = "Other Category Count", reverse=TRUE)) +
      labs(title = input$overview_select_stat, x = NULL, y = NULL) +
      theme_bw()

    ggplotly(plt, tooltip = "text") |> 
      reverse_legend_labels()
    
  })
  

# Trend Analysis ----------------------------------------------------------
 
  df_trend <- dh_getQuery(postgre_nba_con, "shiny_trend_query.sql") |> 
    mutate(slug_season = ordered(slug_season)) |> 
    mutate(type_season = ordered(type_season, c("Pre Season", "Regular Season", "Playoffs"))) |> 
    mutate(year_season_type = forcats::fct_cross(type_season, stringr::str_sub(slug_season, start = 6), sep=" "))
  
  # Segment season types used in plot
  season_type_segments <- df_trend |> 
    summarise(
      season_type_start = min(date_game), 
      season_type_end = max(date_game), 
      season_type_mid = season_type_start + round((season_type_end - season_type_start) / 2),
      .by = year_season_type
    )
  
  # Update drop box values
  observe({
    updateSelectInput(session, "trend_select_player", choices = sort(unique(df_trend$name_player)))
  })
  
  # Code to render plot
  output$player_trend_plot <- renderPlot({
    
    trend_selected_stat <- filter(stat_selection, formatted_name == input$trend_select_stat)$trend_name |> 
      stringr::str_remove("_totals") |> 
      sym()
    
    filter(df_trend, name_player %in% input$trend_select_player) |>
      ggplot(aes(x = date_game, y = {{ trend_selected_stat }}, colour = name_player)) +
      geom_point(alpha = 0.5) +
      geom_line(alpha = 0.5) +
      stat_smooth(na.rm = TRUE, show.legend = FALSE, se = FALSE) +
      scale_x_date(name = NULL, breaks = season_type_segments$season_type_mid, labels = season_type_segments$year_season_type) +
      geom_vline(xintercept = season_type_segments$season_type_start, colour = "grey") +
      ylim(0, NA) +
      labs(title = input$trend_select_stat, x = NULL, y = NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

    
  })
}
