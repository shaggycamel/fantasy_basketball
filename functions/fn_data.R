

# Custom read parquet data -------------------------------------------------

read_df <- function(file){
  arrow::read_parquet(here::here("data", ".parquet", file)) |> 
  janitor::clean_names() |> 
  mutate(across(ends_with("id"), as.factor))
}



# Recalc _pct -------------------------------------------------------------

calc_pct <- function(df){
  
  # stops code on error
  dplyr::select(df, tidyselect::any_of(tmp_cols)) 
  
  # drop existing _pct cols
  df <- dplyr::select(df, -tidyselect::ends_with("_pct"))
  
  # New calcs
  df |> 
    dplyr::mutate(
      fg_pct = fgm / fga
      , ft_pct = ftm / fta
    ) |> 
    dplyr::mutate(dplyr::across(tidyselect::ends_with("_pct"), ~ ifelse(is.na(.x), 0, .x))) |> 
    dplyr::select(-tidyselect::all_of(tmp_cols))
}

