

# Environ -----------------------------------------------------------------

if(!exists("df_players")) suppressMessages(source(here::here("eda", "eda.R")))


# Lm Model ----------------------------------------------------------------


df_lm <- map_df(stat_cols, ~{
    
    a <- list("attribute"=.x)
    l <- lm(as.formula(paste(.x, "~ year")), df_players) |> 
        summary() |> 
        broom::tidy() |> 
        filter(term == "year") |> 
        select(estimate)
    
    bind_cols(a, l)
}) |> pivot_wider(
    names_from = attribute
    , values_from = estimate
)

write.csv(df_lm, here::here("data", "step_lm_recipe", "df_lm.csv"))

# Lm Transform ------------------------------------------------------------

df_lm <- map_dfr(seq_len(nrow(df_players)), ~ df_lm)
df_players_lm <- select(df_players, all_of(stat_cols)) |>
    (\(.df) .df * df_lm)() |> 
    bind_cols(select(df_players, !all_of(stat_cols))) |> 
    select(all_of(colnames(df_players)))

walk(stat_cols, ~{
    df_players_lm <<- df_players_lm |> 
        mutate(!!.x := !!sym(.x) * (year - min(year) + 1))
})

