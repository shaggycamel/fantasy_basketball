

# Player Career Stats -----------------------------------------------------

df_PlayerCareerStats <- 
  read_df('PlayerCareerStats.pq') |>
  filter(team_abbreviation != "TOT") |> 
  mutate(player_id = as.factor(player_id))


# Player Info ------------------------------------------------------------

df_PlayerInfo <- 
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

