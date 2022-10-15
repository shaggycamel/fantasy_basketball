# NBA Draft Analysis
# Oli
# 2022-10-10

# Statistics of interest
# Rebounds (REB)
# Assists (AST)
# Steals (STL)
# Blocks (BLK)
# Points (PTS) 
# Field Goal Percentage (FG%) #scratch#
# Free Throw Percentage (FT%) #scratch#
# Three Pointers Made (3PM)   #scratch#

# Environ -----------------------------------------------------------------

library(tidyverse)

source(here::here('env_setup.R'))

# Raw data ----------------------------------------------------------------

df_draft_raw <- 
  read_df('PlayerCareerStats.pq') |>
  filter(team_abbreviation != "TOT") |> 
  mutate(player_id = as.factor(player_id))


# _pct recalc -------------------------------------------------------------

df_draft <- df_draft_raw |> 
  select(any_of(c(id_cols, tmp_cols, anl_cols))) |> 
  group_by(player_id) |> 
  summarise(across(everything(), ~ sum(.x))) |> 
  calc_pct() |> 
  # Player season count
  left_join({
    group_by(df_draft_raw, player_id) |> 
    summarise(season_cnt = n_distinct(season_id))
  }) |> 
  # Remove players who avg < 1000mins per season
  mutate(avg_season_mins = min / season_cnt) |>
  filter(avg_season_mins > 1000) |> 
  # Calc rate of statistic (by total mins played)
  mutate(across(all_of(anl_cols), ~ .x / min)) |> 
  # Position
  left_join({
    read_df('PlayerInfo.pq') |> 
    mutate(person_id = as.factor(person_id)) |> 
    select(player_id = person_id, player_slug, position)
  })


# Plot by position / meausure ---------------------------------------------

df_anl <- df_draft |> 
  select(player_slug, position, all_of(anl_cols)) |> 
  pivot_longer(cols = all_of(anl_cols), names_to = "stat") |> 
  nest_by(position, stat, .keep = TRUE) |> 
  mutate(data = list(slice_max(data, value, n = 10))) |> 
  mutate(plt = list(
    ggplot(data, aes(y = fct_reorder(player_slug, value), x = value)) +
    geom_col() +
    labs(title = paste0(position, ": ", stat), x = "rate", y = NULL)
  ))
  
df_anl$plt
# df_anl$data[[1]]


# Position / stat intersection -----------------------------------------

l = list()
walk(unique(df_anl$position), ~ {
  l[[.x]] <<- filter(df_anl, position == .x) |> 
    pull(data) |> 
    bind_rows() |> 
    group_by(player_slug) |> 
    summarise(count = n(), stats = paste(stat, collapse = ", "))
})

# New list
bind_rows(l, .id = "position") |> 
  filter(count > 1) |> 
  arrange(position, desc(count)) |> 
  view(">")


# Desired order
# Joel Embiid		              tov, reb, blk, pts *
# Russell Westbrook		        tov, ast, pts *
# James Harden		            tov, ast, pts --
# Trae Young		              tov, ast, pts --
# Nikola Jokic		            reb, ast, pts --
# Luka Doncic		              tov, ast, pts --
# Ja Morant		                tov, ast, pts --
# LaMelo Ball		              tov, ast, stl *
# LeBron James		            tov, pts --
# Karl-Anthony Towns		      reb, pts --
# Ben Simmons		              tov, ast --
# Devin Booker		            tov, pts --
# Dwight Howard		            reb, blk *
# John Wall		                tov, ast --
# Rudy Gobert		              reb, blk --
# Jarrett Allen		            reb, blk --
# Rajon Rondo		              ast, stl *
# Clint Capela		            reb, blk *
# T.J. McConnell		          ast, stl
# Hassan Whiteside		        reb, blk
# Nerlens Noel		            stl, blk
# Michael Carter-Williams		  tov, stl
# Tyus Jones		              ast, stl
# Mo Bamba		                reb, blk

      
# tov     3
# ast     2       
# reb     2  
# blk     2  
# pts     2     
# stl     1   
# fg_pct  0
# ft_pct  0
# fg3m    0



# Scratch
# Anthony Davis
# Chet Holmgren
# Chris Paul
# Corey Kispert
# Danilo Gallinari
# Danny Green
# DeAndre Jordan
# E.J. Liddell
# Eric Bledsoe
# Jaren Jackson Jr.
# Joe Ingles
# Lonzo Ball
# Marcus Zegarowski
# Markelle Fultz
# Matt Thomas
# Ricky Rubio
# Robert Williams III
# Scottie Lewis
# T.J. Warren
# Ty Jerome
# Zion Williamson
