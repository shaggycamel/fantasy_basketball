
# Environ -----------------------------------------------------------------

if(!exists("df_players")) source(here::here("data", "data_lm_transform.R"))

library(yardstick)
library(tidyverse)

df_train <- filter(df_players, year != 2021)
df_train_lm <- filter(df_players_lm, year != 2021)
train <- map(str_subset(objects(), "train"), get)


df_test  <- filter(df_players, year == 2021)
df_test_lm <- filter(df_players_lm, year == 2021)
test <- map(str_subset(objects(), "test"), get)

remove_cols <- c("player", "rank", "year")

# Logistic Regression -----------------------------------------------------

log_reg <- map(df_train, ~ {
    glm(
        data = dplyr::select(.x, !all_of(remove_cols))
        , formula = mvp_flag ~ .
        , binomial(link = "logit")
    ) 
})
    
map(log_reg, summary)

log_reg_pred <- map2(log_reg, test, ~ {
    as.data.frame(predict(.x, .y)) |> 
    setNames("prob") |> 
    bind_cols(select(.y, player, mvp_flag)) |> 
    mutate(pred = as.factor(if_else(prob > 0.5, 1, 0)))
})

map(log_reg_pred, ~ conf_mat(.x, mvp_flag, pred))
walk(log_reg_pred, view)


# lreg --------------------------------------------------------------------

x <- glm(
    data = dplyr::select(df_train, !all_of(remove_cols))
    , formula = mvp_flag ~ .
    , binomial(link = "logit")
) 

xp <- as.data.frame(predict(x, df_test)) |> 
    setNames("prob") |> 
    bind_cols(select(df_test, player, mvp_flag)) |> 
    mutate(pred = as.factor(if_else(prob > 0.5, 1, 0)))

conf_mat(xp, mvp_flag, pred)
