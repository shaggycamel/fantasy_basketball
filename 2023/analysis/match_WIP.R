
library(ggradar)
source(here::here("data", "data_prep.R"))

# Scoring
# Field Goal Percentage (FG%)
# Free Throw Percentage (FT%)
# Three Pointers Made (3PM)
# Rebounds (REB)
# Assists (AST)
# Steals (STL)
# Blocks (BLK)
# Points (PTS) 

# Raw data ----------------------------------------------------------------

df_playerlog_raw <- 
  read_df("PlayerGameLog.pq") |> 
  mutate(
    player_id = as_factor(player_id)
    , season_id = fct_inseq(str_remove(season_id, "^2")) 
    , game_date = as.Date(game_date, format = "%b %d, %Y")
    , week = fct_inseq(paste0(season_id, str_pad(lubridate::week(game_date), 2, pad = "0")))
    , wl = as_factor(wl)
    , .before = everything()
  ) |> 
  arrange(player_id, game_date) |> 
  left_join(select(df_players, player_id = person_id, player_slug)) |> 
  relocate(player_slug, .after = player_id)


# Group by Week -----------------------------------------------------------

df_playerlog_week <- df_playerlog_raw |> 
  group_by(player_slug, week) |> 
  summarise(across(all_of(c(anl_cols, tmp_cols)), ~ sum(.x)), .groups = "drop") |> 
  calc_pct()


# Group by Year -----------------------------------------------------------

df_playerlog_year <- df_playerlog_raw |> 
  group_by(player_slug, season_id) |> 
  summarise(across(all_of(c(anl_cols, tmp_cols)), ~ sum(.x)), .groups = "drop") |> 
  calc_pct() |> 
  filter(season_id == 2021)


# Top Performers 2021 Season ----------------------------------------------

l = list()
walk(str_subset(anl_non_perc, "tov", negate = TRUE), ~{ 
  l[[.x]] <<- 
    slice_max(df_playerlog_year, !!sym(.x), n = 5) |> 
    select(player_slug, !!sym(.x))
})

l_plt <- imap(l, ~ {
  ggplot(.x, aes(y = fct_reorder(player_slug, !!sym(.y) ), x = !!sym(.y), label = round(!!sym(.y)))) +
    geom_col() +
    geom_label() +
    labs(x = NULL, y = NULL, title = paste("Top 5:", str_to_upper(.y)), subtitle = "2022 Regular Season") +
    theme_bw()
})


# 
# 
# # Spider graph ------------------------------------------------------------
# 
# wk21 <- filter(df_playerlog_week, str_starts(df_playerlog_week$week, "2021")) |> 
#   pull(week) |> 
#   unique()
# 
# map(wk21, ~ {
#   filter(df_playerlog_week, week == .x) |> 
#     select(full_name, all_of(anl_cols)) |> 
#     mutate(across(all_of(anl_cols), scales::rescale)) |> 
#     rowwise() |> 
#     mutate(total = sum(c_across(anl_cols))) |> 
#     ungroup() |>
#     slice_max(order_by = total, n = 5) |> 
#     select(-total) |> 
#     ggradar(values.radar = "", plot.title = .x)
# })
# 
# 
# # Min graph ------------------------------------------------------------
# 
# df_playerlog <- filter(df_playerlog_raw, full_name == "Russell Westbrook")
# 
# df_playerlog |> 
#   ggplot(aes(x = game_date, y = min, colour = season_id)) +
#   geom_path() +
#   geom_point() +
#   scale_y_continuous(limits = c(0, 60)) +
#   labs(title = df_playerlog$full_name[[1]])
# 


# Analytical Question -----------------------------------------------------

# What combination of players, at what time throughout the season
# attribute to the greatest area of a spider chart polygon?
