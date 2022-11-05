





# Team Game Log -----------------------------------------------------------

df_TeamGameLog <- read_df("TeamGameLog.pq") |> 
  select(team_id, game_id, matchup, game_date) |> 
  mutate(game_id = ordered(game_id)) |> 
  mutate(game_date = as.Date(game_date, format = "%B %d, %Y"))


# Player Game Log ---------------------------------------------------------

df_PlayerGameLog <- read_df("PlayerGameLog.pq") |> 
  select(-game_date, -season_id) |> 
  mutate(game_id = ordered(game_id)) |> 
  left_join(select(df_TeamGameLog, -game_date)) |> 
  relocate(team_id, .before = everything())


# Team Game Roster --------------------------------------------------------

df_TeamGameRoster <- df_PlayerGameLog |> 
  select(ends_with("_id"), matchup) |>
  arrange(player_id, game_id) |> 
  group_by(player_id) |>
  mutate(
    team_roll = (team_id != lag(team_id, n = 1))
    , team_roll = as.numeric(replace_na(team_roll, FALSE))
    , team_roll = cumsum(team_roll)
  ) |> 
  group_by(player_id, team_id, team_roll) |>
  summarise(min_game = min(game_id), max_game = max(game_id), .groups = "drop") |>
  (\(.df) fuzzyjoin::fuzzy_left_join(
    df_TeamGameLog
    , .df
    , by = c("team_id", "game_id"="min_game", "game_id"="max_game")
    , match_fun = list(`==`, `>=`, `<=`)
  ))() |>
  rename(team_id = team_id.x) |> 
  select(-team_id.y, -team_roll, -min_game, -max_game) |> 
  arrange(game_id, team_id, player_id)


# League Schedule ---------------------------------------------------------

df_LeagueSchedule <- read_df("LeagueSchedule.pq") |> mutate(game_id = as.character(game_id))
df_GameSchedule <- 
  bind_rows(
    pivot_wider(df_LeagueSchedule, names_from = home_team_id, names_prefix = "team_", values_from = away_team_id) |> 
      mutate(home_away = "home") |> 
      select(game_date, game_id, home_away, starts_with("team_"))
    , pivot_wider(df_LeagueSchedule, names_from = away_team_id, names_prefix = "team_", values_from = home_team_id) |> 
        mutate(home_away = "away") |> 
        select(game_date, game_id, home_away, starts_with("team_"))
  ) |> 
  pivot_longer(starts_with("team_"), names_to = "team_id", values_to = "opponent_id") |> 
  mutate(team_id = str_remove(team_id, "team_")) |> 
  na.omit() |> 
  arrange(team_id, game_date) |> 
  group_by(team_id) |> 
  mutate(
    prev_game = lag(opponent_id)
    , next_game = lead(opponent_id)
    , next2_game = lead(opponent_id, 2)
  )


# Injury Log --------------------------------------------------------------

# Now relevant cols from PlayerGameLong
df_InjuryLog <- df_TeamGameRoster |> 
  left_join(
    select(df_PlayerGameLog, -matchup)
    , by = c("team_id", "game_id", "player_id")
  ) |> 
  arrange(player_id, game_id) |>
  mutate(play_status = if_else(!is.na(wl), TRUE, FALSE), .after = player_id) |>
  (\(.df) fuzzyjoin::fuzzy_left_join(
    .df
    , df_Season
    , by = c("game_date"="season_start", "game_date"="season_end")
    , match_fun = list(`>=`, `<=`)
  ))() |>
  relocate(season_id, .before = everything()) |>
  select(-season_start, -season_end)


# Team Mgmt ---------------------------------------------------------------

trn_coach <- c("Assistant Coach for Player Development", "Assistant Trainer", "Director of Athletic Development", "Strength and Conditioning Coach", "Trainer")
ass_coach <- c("Assistant Coach", "Associate Head Coach", "Lead Assistant Coach")
df_TeamMgmt <- read_df('CoachTeamRoster.pq') |> 
  mutate(
    head_coach = if_else(coach_type == "Head Coach", coach_id, NA_character_)
    , head_coach = ordered(head_coach)
  ) |>
  mutate(mgmt_type = case_when(
    coach_type %in% trn_coach ~ "trn_coach"
    , coach_type %in%  ass_coach ~ "ass_coach"
    , TRUE ~ NA_character_
  )) |> 
  group_by(team_id, season) |> 
  summarise(
    head_coach = max(head_coach, na.rm = TRUE)
    , head_coach = replace_na(as.character(head_coach), "0")
    , trn_coach_count = sum(mgmt_type == "trn_coach", na.rm = TRUE)
    , ass_coach_count = sum(mgmt_type == "ass_coach", na.rm = TRUE)
    , .groups = "drop"
  )
  