
library(nba.dataRub)
library(tidyverse)
library(here)
library(patchwork)


# data --------------------------------------------------------------------

db_con <- nba.dataRub::dh_createCon("postgres")
df_raw <- dh_getQuery(db_con, "seasonal.sql")

df <- df_raw |> 
  filter(season_id >= "2015-16") |> 
  pivot_wider(
    id_cols = season_id, 
    names_from = player_name, 
    values_from = 3:ncol(df_raw)
  ) |> 
  (\(t_df){
    
    for(col in colnames(df_raw)[3:20]){
      cs <- syms(sort(str_subset(colnames(t_df), col)))
      t_df <- mutate(t_df, !!col := !!cs[[1]] / !!cs[[2]])
    }
    select(t_df, colnames(df_raw)[colnames(df_raw) != "player_name"]) |> 
      mutate(player_name = "Nikola Jokic")
  })() |> 
  pivot_longer(!c(season_id, player_name), names_to = "stat")


# Plot --------------------------------------------------------------------

interest_stats <- c("ast", "blk", "fg3_m", "fgm", "ftm", "pts", "reb", "stl", "tob")

(filter(df, stat %in% interest_stats) |> 
  ggplot(aes(x = season_id, y = value, colour = stat, group = stat)) +
  geom_path() +
  geom_point() +
  theme_bw()) |> 
  plotly::ggplotly()

