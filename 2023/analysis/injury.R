

# Environ -----------------------------------------------------------------

library(tidyverse)
source(here::here("data", "data_prep.R"))

df_BoxScore <- read_df("BoxScoreTraditionalV2.pq")

# Data --------------------------------------------------------------------

source(here::here("data", "injury_data.R"))

# Feature Engineering -----------------------------------------------------

#> season_games_played:     Games played this season
#> season_play_game_ratio:   Ratio of a players season playing status
#> season_win_ratio:         The rolling ratio of a players win/loss ratio for a season
#> season_mins:             Minutes played this season
#> additional_mins:         The difference between current and previous mins played
#> miss_prev_game:          Was the previous game missed
# cum_season_mins:
# mins_prev_game:
#> previous_opponent
#> next_opponent:            Next team to play
# next_opponent_wl_ratio
# previous_game_total_score
# in_prev_start_lineup:     Was player in starting line-up for previous match
#> age:
#> head_coach:               Head Coach
#> trn_coach_count:          [Assistant Coach for Player Development, Assistant Trainer, Director of Athletic Development, Strength and Conditioning Coach, Trainer]
#> ass_coach_count:          [Assistant Coach, Associate Head Coach, Lead Assistant Coach]
#> position
#> days_since_last_game
# off_season_length


# GAME SCHEDULE ONLY GOES UP TO ARPIL 2022. WHY ???
x <- mutate(df_InjuryLog, game_id = as.character(game_id)) |> 
  group_by(season_id, player_id) |> 
  # Cleaing
  mutate(
    min = replace_na(min, 0)
    , wl = replace_na(wl, "DNP")
  ) |> 
  # Game and min calcs
  mutate(
    season_games_played = cumsum(play_status)
    , season_game_play_ratio = season_games_played / cumsum(as.numeric(game_id == game_id))
    , season_win_ratio = cumsum(as.numeric(wl == "W")) / cumsum(as.numeric(game_id == game_id))
    , season_mins = cumsum(min)
    , game_min_diff = replace_na(min - lag(min, 1), 0)
    , days_since_last_game = replace_na(as.numeric(game_date - lag(game_date, 1)), 0)
    , days_since_last_game_tmp = if_else(!play_status, days_since_last_game, 0)
    , days_since_last_game_tmp = ave(days_since_last_game_tmp, cumsum(days_since_last_game_tmp == 0), FUN = cumsum)
    , days_since_last_game_tmp = na_if(days_since_last_game_tmp, 0)
    , days_since_last_game = coalesce(days_since_last_game_tmp, days_since_last_game)
    , .after = min
  ) |> 
  select(-days_since_last_game_tmp) |> 
  # Lineup, bio and next/prev calcs
  left_join(
    select(df_Players, person_id, position, birthdate, height, weight, season_exp)
    , by = c("player_id"="person_id")
  ) |> 
  mutate(
    miss_prev_game = replace_na(!lag(play_status, 1), FALSE)
    # , in_prev_start_lineup
    , age_days = game_date - as.Date(birthdate)
    , .after = play_status
  ) |>
  left_join(select(df_GameSchedule, game_id, team_id, ends_with("_game"))) |>
  # Team mgmt
  mutate(season_id = str_sub(season_id, 2, -1)) |> 
  left_join(df_TeamMgmt, by = c("season_id"="season", "team_id")) |> 
  select(-any_of(c(tmp_cols, oth_cols, anl_cols)), -birthdate)

head(x, 5000) |> view(">")



  
