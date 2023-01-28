

# Environ -----------------------------------------------------------------

library(magrittr)
library(ggbiplot)
library(dplyr)

if(!exists("df_players")) source(here::here("data", "data_prep.R"))
skimr::skim(df_players)

# Correlation Plot --------------------------------------------------------

DataExplorer::plot_correlation(select(df_players, all_of(stat_cols), all_of(names(perc_cols))))

# Categorical Analysis ----------------------------------------------------
cat_cols <- c("age", "pos", "tm", "mvp_flag")

df_cat_plt <- select(df_players, all_of(cat_cols)) |> 
    filter(mvp_flag == 1) |> 
    pivot_longer(cols = -mvp_flag, names_to = "metric", values_to = "value") |> 
    nest_by(metric, .keep = TRUE) |> 
    mutate(plt = list(
        ggplot(data, aes(x = value)) +
            geom_bar() +
            labs(title = metric, x = NULL) 
    ))

# Alter text orientation of tm plot
df_cat_plt[df_cat_plt["metric"] == "tm",]$plt[[1]] %<>% +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

df_cat_plt$plt

# Stat trend analysis -----------------------------------------------------

df_trend_plt <- group_by(df_players, year, mvp_flag) |> 
    summarise(across(all_of(stat_cols), ~ mean(.x, na.rm = TRUE))) |> 
    pivot_longer(-c(year, mvp_flag), names_to = "metric", values_to = "average") |> 
    ungroup() |> 
    nest_by(metric, .keep = TRUE) |> 
    mutate(plt = list(
        ggplot(data, aes(x = year, y = average, colour = mvp_flag)) +
            geom_point() +
            geom_path(aes(group = mvp_flag)) +
            geom_smooth() + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
            labs(title = metric)
    ))

df_trend_plt$plt


# Percentage Scatter Trend ------------------------------------------------

df_scatter_plt <- select(df_players, mvp_flag, year, all_of(names(perc_cols))) |> 
    pivot_longer(cols = -c(mvp_flag, year), names_to = "metric", values_to = "percent") |> 
    nest_by(metric, .keep = TRUE) |> 
    mutate(plt = list(
        ggplot(data, aes(x = year, y = percent, colour = mvp_flag)) +
            geom_point(alpha = 0.7) + 
            geom_smooth(se = FALSE) +
            labs(title = metric)
    ))

df_scatter_plt$plt


# Distribution Analysis ---------------------------------------------------

df_dist_plt <- df_players |> 
    select(mvp_flag, all_of(stat_cols), all_of(names(perc_cols))) |> 
    pivot_longer(cols = -mvp_flag, names_to = "metric", values_to = "value") |> 
    nest_by(metric, .keep = TRUE) |> 
    mutate(plt = list(
        ggplot(data, aes(x = value, fill = mvp_flag)) +
            geom_density(alpha = 0.5) + 
            labs(title = metric)
    ))

df_dist_plt$plt


# Attribute Linear Modelling ----------------------------------------------

df_lm <- map_df(stat_cols, ~{
    
    a <- list("attribute"=.x)
    l <- lm(as.formula(paste(.x, "~ year")), df_players) |> 
        summary() |> 
        broom::tidy() |> 
        filter(term == "year") |> 
        select(estimate)
    
    bind_cols(a, l)
}) |> 
    pivot_wider(names_from = attribute, values_from = estimate)

# Use these values to recalculate stats
# Percentages will need to be recalculated from new values


# PCA ---------------------------------------------------------------------

df_pca <- prcomp(
    as.matrix(select(df_players, where(is.numeric), -rank))
    , scale. = TRUE
    , center = TRUE
)

summary(df_pca)

dim <- as.data.frame(t(combn(1:8,2)))
map2(dim$V1, dim$V2, ~ {
    ggbiplot(df_pca, choices = c(.x, .y), groups = df_players$mvp_flag) +
        labs(title = paste0("PC", .x, " & PC", .y))
})









