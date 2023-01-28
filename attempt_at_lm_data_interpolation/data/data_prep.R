

# Environ -----------------------------------------------------------------

library(tidyverse)
library(janitor)

stat_cols <- c("g", "gs", "mp", "fg", "fga", "x3p", "x3pa", "x2p", "x2pa", "ft", "fta", "orb", "drb", "trb", "ast", "stl", "blk", "tov", "pf", "pts")
perc_cols <- rlang::quos(
    "fg_percent" = fg / fga
    , "x3p_percent" = x3p / x3pa
    , "x2p_percent" = x2p / x2pa
    , "ft_percent" = ft / fta
    , "e_fg_percent" = (x2p + 1.5 * x3p) / fga
)


# Read Data Function ------------------------------------------------------

read_df <- function(file){
    read_delim(here::here("data", file), ";", locale = locale(encoding = "LATIN1"), show_col_types = FALSE) |> 
    mutate(across(everything(), ~ str_remove(.x, "\\*"))) |> 
    clean_names()
}

# Teams -------------------------------------------------------------------

df_teams <- read_df("teams.csv") |> 
    filter(!str_detect(team, "Division")) |> 
    mutate(across(everything(), ~ parse_guess(.x, guess_integer = TRUE, na = c("", "NA", "\u0097"))))

# MVPs --------------------------------------------------------------------

df_mvps <- read_df("mvps.csv") |> 
    mutate(across(everything(), ~ parse_guess(.x, guess_integer = TRUE))) |> 
    mutate(
        rank = str_remove(rank, "T")
        , rank = str_pad(rank, 3, side = "left", pad = "0")
        , rank = ordered(rank)
        , rank = as.numeric(rank)
        , mvp_flag = 1
    )

# Players -----------------------------------------------------------------

df_players <- read_df("players.csv") |> 
    filter(!str_detect(rk, "Rk")) |> 
    select(-rk) |> 
    mutate(across(everything(), ~ parse_guess(.x, guess_integer = TRUE))) |> 
    group_by(year, player) |> 
    mutate(
        across(all_of(stat_cols), ~ sum(.x, na.rm = TRUE))
        , !!!perc_cols
        , across(all_of(names(perc_cols)), ~ replace_na(.x, 0))
        , pos = str_remove(pos, "-\\w+")
    ) |> 
    slice_head(n = 1) |> 
    left_join(select(df_mvps, year, player, rank, mvp_flag), by = c("player", "year")) |>
    ungroup() |> 
    mutate(
        mvp_flag = factor(replace_na(mvp_flag, 0))
        , rank = replace_na(as.numeric(rank), 999)
        # , age = ordered(age)
    )