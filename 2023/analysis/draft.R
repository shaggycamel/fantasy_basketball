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

library(plotly)
source(here::here("data", "data_prep.R"))

# Raw data ----------------------------------------------------------------

df_draft_raw <- 
  read_df('PlayerCareerStats.pq') |>
  filter(team_abbreviation != "TOT") |> 
  mutate(player_id = as.factor(player_id))


# Avg mins per season -----------------------------------------------------

df_avg_mins <- df_draft_raw |> 
  group_by(player_id) |> 
  summarise(season_avg_mins = sum(min) / n_distinct(season_id)) |> 
  left_join(select(df_players, player_id = person_id, player_slug))

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
  filter(avg_season_mins > 1666) |> 
  
  # Calc rate of statistic (by total mins played)
  mutate(across(all_of(anl_cols), ~ .x / min)) |> 
  
  # Position and player name
  left_join(select(df_players, player_id = person_id, player_slug, position))



# Plot by position / meausure ---------------------------------------------

top_x <- 15
l <- list()
walk(c("Forward", "Center", "Guard"), ~ {
   t <- df_draft |> 
    filter(str_detect(position, .x)) |> 
    pivot_longer(cols = all_of(anl_cols), names_to = "stat") |> 
    group_by(stat)
   
   l[[.x]] <<- map_dfr(anl_cols, ~{
     p <- filter(t, str_detect(stat, .x))
       if(.x == "tov") slice_min(p, order_by = value, n = top_x)
     else slice_max(p, order_by = value, n = top_x)
   })
})

l <-ungroup(bind_rows(l, .id = "cat_position")) |> 
  mutate(pos_cnt = factor(if_else(str_detect(position, ","), 2, 1)))

l_plt <- map(set_names(anl_cols), ~ {
  filter(l, stat == .x) |> 
    ggplot(aes(
      x = value
      , y = if(.x=="tov") reorder(player_slug, -value) else reorder(player_slug, value)
      # , fill = pos_cnt
    )) +
    geom_col(colour = "black", fill = "dodgerblue") +
    labs(title = paste("Stat:", str_to_upper(.x)), subtitle = paste("Top", top_x, "Players by Stat & Position"), y = NULL, x = "rate") +
    facet_grid(rows = vars(cat_position), scales = "free") +
    theme_bw()
})


# Position / stat intersection -----------------------------------------

df_intersect <- l |> 
  group_by(player_slug, stat) |> 
  slice(1) |> 
  group_by(player_slug) |> 
  summarise(
    stat_cat_cnt = n()
    , stats = paste(stat, collapse = ", ")
    , position = position
  ) |> 
  distinct() |> 
  filter(stat_cat_cnt >= 5)

df_intersect |> 
  ggplot(aes(
    y = fct_reorder(player_slug, stat_cat_cnt)
    , x = stat_cat_cnt
    , fill = position
    , text = stats
  )) +
  geom_col(colour = "black") +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  labs(title = "Player Stat Category Count", x = "Number of Stat Categories", y = NULL) +
  theme_bw()

intersect_plt <- ggplotly(tooltip = "stats") |> 
  layout(title = list(text = paste0(
    "Player Stat Category Count" # title
    , "<br>"
    , "<sup>"
    , "Hover on Bars for Stat Categories" # subtitle
    ,"</sup>"
  ))) |>
  config(displayModeBar = FALSE)






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
