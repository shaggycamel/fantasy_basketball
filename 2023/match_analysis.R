
library(ggradar)
source(here::here('env_setup.R'))

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

df_playerlog_raw <- read_parquet(here::here(".parquet", "PlayerGameLog.pq")) |> 
  janitor::clean_names() |> 
  mutate(
    player_id = as_factor(player_id)
    , season_id = fct_inseq(str_remove(season_id, "^2")) 
    , game_date = as.Date(game_date, format = "%b %d, %Y")
    , week = fct_inseq(paste0(season_id, str_pad(lubridate::week(game_date), 2, pad = "0")))
    , wl = as_factor(wl)
  ) |> 
  arrange(player_id, game_date) |> 
  left_join(select(df_players, player_id = id, full_name))


# Group by Week -----------------------------------------------------------

df_playerlog_week <- group_by(df_playerlog_raw, full_name, week) |> 
  summarise(across(all_of(c(anl_cols, tmp_cols)), ~ sum(.x)), .groups = "drop") |> 
  pct_calc()

# Spider graph ------------------------------------------------------------

wk21 <- filter(df_playerlog_week, str_starts(df_playerlog_week$week, "2021")) |> 
  pull(week) |> 
  unique()

map(wk21, ~ {
  filter(df_playerlog_week, week == .x) |> 
    select(full_name, all_of(anl_cols)) |> 
    mutate(across(all_of(anl_cols), scales::rescale)) |> 
    rowwise() |> 
    mutate(total = sum(c_across(anl_cols))) |> 
    ungroup() |>
    slice_max(order_by = total, n = 5) |> 
    select(-total) |> 
    ggradar(values.radar = "", plot.title = .x)
})


# Min graph ------------------------------------------------------------

df_playerlog <- filter(df_playerlog_raw, full_name == "Russell Westbrook")

df_playerlog |> 
  ggplot(aes(x = game_date, y = min, colour = season_id)) +
  geom_path() +
  geom_point() +
  scale_y_continuous(limits = c(0, 60)) +
  labs(title = df_playerlog$full_name[[1]])



# Analytical Question -----------------------------------------------------

# What combination of players, at what time throughout the season
# attribute to the greatest area of a spider chart polygon?
