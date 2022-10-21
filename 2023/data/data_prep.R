
library(tidyverse)

id_cols <- c("player_id", "min")
tmp_cols <- c("fgm", "fga", "ftm", "fta")
anl_cols <- c("tov", "fg3m", "reb", "ast", "stl", "blk", "pts", "fg_pct", "ft_pct")
anl_non_perc <- str_subset(anl_cols, "_pct", negate=TRUE)

# Custom read parque data -------------------------------------------------

read_df <- function(file){
  arrow::read_parquet(here::here("data", ".parquet", file)) |> 
  janitor::clean_names() 
}

# df_players
df_players <- 
  read_df("PlayerInfo.pq") |> 
  mutate(
    person_id = as.factor(person_id)
    , position = strsplit(position, "-")
    , position = map(position, ~ sort(unlist(.x)))
    , position = as.character(position)
    , position = na_if(position, "character(0)")
    , position = str_remove(position, "^c")
    , position = str_remove_all(position, "[:punct:]")
    , position = str_replace(position, " ", ", ")
  ) 


# Recalc _pct -------------------------------------------------------------

calc_pct <- function(df){
  
  # stops code on error
  select(df, all_of(tmp_cols)) 
  
  # drop existing _pct cols
  df <- select(df, -ends_with("_pct"))
  
  # New calcs
  df |> 
    mutate(
      fg_pct = fgm / fga
      , ft_pct = ftm / fta
    ) |> 
    mutate(across(ends_with("_pct"), ~ ifelse(is.na(.x), 0, .x))) |> 
    select(-all_of(tmp_cols))
}

