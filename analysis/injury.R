

# Data --------------------------------------------------------------------

source(here::here("data", "injury_data.R"))

cols$injury_cols <- c(
  "start_flag"
  , "team"
  , "game_seq"
  , "age"
  , "height"
  , "weight"
  , "position"
  # , "head_coach" group by count
  , "trn_coach_count"
  , "ass_coach_count"
  , "opponent"
  , "prev_opponent"
  , "next_opponent"
  , "season_games_played"
  , "season_game_play_ratio"
  , "season_win_ratio"
  , "season_mins"
  , "lag_min"
  , "lag_reb"
  , "lag_ast"
  , "lag_stl"
  , "lag_blk"
  , "lag_tov"
  , "lag_pts"
  , "lag_reb_game_ratio"
  , "lag_ast_game_ratio"
  , "lag_stl_game_ratio"
  , "lag_blk_game_ratio"
  , "lag_tov_game_ratio"
  , "lag_pts_game_ratio")

# EDA ---------------------------------------------------------------------

count(df_anl, season_id, game_seq, start_flag) |> view()

df_anl |> 
  group_by(season_id, game_seq) |> 
  summarise(st_prop = sum(start_flag) / n()) |> 
  ggplot(aes(x = game_seq, y = st_prop)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  facet_grid(rows = vars(season_id))

# team
ggplot(df_anl, aes(x = team, fill = injured_flag)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# age dist
ggplot(df_anl, aes(x = as.numeric(age), colour = injured_flag)) +
  geom_density() 

# height
ggplot(df_anl, aes(x = height, fill = injured_flag)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# weight
ggplot(df_anl, aes(x = weight, colour = injured_flag)) +
  geom_density()

# position     
ggplot(df_anl, aes(x = position, fill = injured_flag)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# head_coach
ggplot(df_anl, aes(x = head_coach, fill = injured_flag)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# trn_coach_count 
ggplot(df_anl, aes(x = ordered(trn_coach_count), fill = injured_flag)) + 
  geom_bar(position = "fill") 

# ass_coach_count 
ggplot(df_anl, aes(x = ordered(ass_coach_count), fill = injured_flag)) + 
  geom_bar(position = "fill") 

# opponent   
ggplot(df_anl, aes(x = opponent, fill = injured_flag)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# prev_opponent  
ggplot(df_anl, aes(x = prev_opponent, fill = injured_flag)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# next_opponent    
ggplot(df_anl, aes(x = next_opponent, fill = injured_flag)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# season_games_played
ggplot(df_anl, aes(x = season_games_played, colour = injured_flag)) +
  geom_density()

# season_game_play_ratio
ggplot(df_anl, aes(x = season_game_play_ratio, colour = injured_flag)) +
  geom_density()

# season_win_ratio
ggplot(df_anl, aes(x = season_win_ratio, colour = injured_flag)) +
  geom_density()

# season_mins 
ggplot(df_anl, aes(x = season_mins, colour = injured_flag)) +
  geom_density()

# lag_wl  
ggplot(df_anl, aes(x = lag_wl, fill = injured_flag)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# lag_min  
ggplot(df_anl, aes(x = lag_min, colour = injured_flag)) +
  geom_density()

# lag_reb 
ggplot(df_anl, aes(x = lag_reb, colour = injured_flag)) +
  geom_density()

# lag_ast 
ggplot(df_anl, aes(x = lag_ast, colour = injured_flag)) +
  geom_density()

# lag_stl 
ggplot(df_anl, aes(x = lag_stl, colour = injured_flag)) +
  geom_density()

# lag_blk 
ggplot(df_anl, aes(x = lag_blk, colour = injured_flag)) +
  geom_density()

# lag_tov  
ggplot(df_anl, aes(x = lag_tov, colour = injured_flag)) +
  geom_density()

# lag_pts 
ggplot(df_anl, aes(x = lag_pts, colour = injured_flag)) +
  geom_density()

# lag_min_game_ratio 
ggplot(df_anl, aes(x = lag_min_game_ratio, colour = injured_flag)) +
  geom_density()

# lag_reb_game_ratio 
ggplot(df_anl, aes(x = lag_reb_game_ratio, colour = injured_flag)) +
  geom_density()

# lag_ast_game_ratio 
ggplot(df_anl, aes(x = lag_ast_game_ratio, colour = injured_flag)) +
  geom_density()

# lag_stl_game_ratio 
ggplot(df_anl, aes(x = lag_stl_game_ratio, colour = injured_flag)) +
  geom_density()

# lag_blk_game_ratio 
ggplot(df_anl, aes(x = lag_blk_game_ratio, colour = injured_flag)) +
  geom_density()

# lag_tov_game_ratio 
ggplot(df_anl, aes(x = lag_tov_game_ratio, colour = injured_flag)) +
  geom_density()

# lag_pts_game_ratio
ggplot(df_anl, aes(x = lag_pts_game_ratio, colour = injured_flag)) +
  geom_density()


# Fuck --------------------------------------------------------------------


glm(
  start_flag ~ .
  , data = select(df_anl, all_of(cols$injury_cols))
  , family = binomial()
) |> summary()


# potential predictive factors:
# Effort/retaliation/defense factor
# Fast breaks/attack factor
# Efficiency ratio (look it up)
 

# Time series -------------------------------------------------------------

df_anl |> 
  ggplot(aes(x = game_seq, y = injured_flag, group = 1, colour = season_id)) +
  geom_line() +
  geom_point() +
  trelliscopejs::facet_trelliscope( ~ player_id)

