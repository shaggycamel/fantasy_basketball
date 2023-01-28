

# Get Player Career Stats -----------------------------------------------------

get_PlayerCareerStats <- function(){
  read_df('PlayerCareerStats.pq') |>
    filter(team_abbreviation != "TOT") |> 
    mutate(player_id = as.factor(player_id))
}


# Get Player Info ------------------------------------------------------------

get_PlayerInfo <- function(){
  read_df("PlayerInfo.pq") |> 
    dplyr::mutate(
      person_id = as.factor(person_id)
      , position = strsplit(position, "-")
      , position = purrr::map(position, ~ sort(unlist(.x)))
      , position = as.character(position)
      , position = dplyr::na_if(position, "character(0)")
      , position = stringr::str_remove(position, "^c")
      , position = stringr::str_remove_all(position, "[:punct:]")
      , position = stringr::str_replace(position, " ", ", ")
    ) 
} 


# Get Season Game Log ---------------------------------------------------------

get_Season <- function(){
  read_df("PlayerGameLog.pq") |> 
    mutate(game_date = as.Date(game_date, format = "%B %d, %Y")) |> 
    group_by(season_id) |> 
    summarise(season_start = min(game_date), season_end = max(game_date)) |> 
    mutate(season_id = str_remove(season_id, "^2"))
}


# Get Team Mgmt -----------------------------------------------------------

get_TeamMgmt <- function(){
  
  trn_coach <- c("Assistant Coach for Player Development", "Assistant Trainer", "Director of Athletic Development", "Strength and Conditioning Coach", "Trainer")
  ass_coach <- c("Assistant Coach", "Associate Head Coach", "Lead Assistant Coach")

  read_df('CoachTeamRoster.pq') |> 
    mutate(
      head_coach = ifelse(coach_type == "Head Coach", coach_id, NA_character_)
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
    ) |> 
    rename(season_id = season) |> 
    suppressWarnings()
}
