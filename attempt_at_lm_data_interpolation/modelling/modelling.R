

# Environ --------------------------------------------------------------------------------------------------------------

if(!exists("df_players")) source(here::here("data", "data_lm_transform.R"))
source(here::here("data", "step_lm_recipe", "step_lm.R"))

library(tidymodels)
library(themis)
library(workflowsets)

# Set seed
seed <- 123


# Data Split -----------------------------------------------------------------------------------------------------------

split <- initial_time_split(df_players, prop = 1-(sum(df_players$year == 2021) / nrow(df_players)))
df_train <- training(split)
df_test <- testing(split)


# Recipe ---------------------------------------------------------------------------------------------------------------

# Original recipe with no re-sampling
x_rec <- recipe(mvp_flag ~., data = df_train) |> 
    update_role(player, new_role = "id") |> 
    # step_lm(stat_cols)
    step_rm(rank, tm, year) |> 
    step_scale(all_numeric_predictors()) |>
    # step_pca(all_numeric_predictors(), num_comp = 4) |>
    step_dummy(pos)


# Re-sampling recipes to address class imbalance in mvp_flag
rec_downsample <- step_downsample(x_rec, mvp_flag, seed = seed)
rec_upsample <- step_upsample(x_rec, mvp_flag, seed = seed)
rec_smote <- step_smote(x_rec, mvp_flag, seed = seed) # PREFER
rec_rose <- step_rose(x_rec, mvp_flag, seed = seed)

view(juice(prep(x_rec))) # observe a recipe

# Recipes
recipes <- mget(str_subset(objects(), "^rec_"))


# Model Construction ---------------------------------------------------------------------------------------------------

# XGBoost
# boost_tree_xgboost_spec <- boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) |>
#     set_engine("xgboost") |>
#     set_mode("classification")

# GLMnet
logistic_reg_glmnet_spec <- logistic_reg(penalty = tune(), mixture = tune()) |>
    set_engine("glmnet")

# GLM
logistic_reg_spec <- logistic_reg() |>
    set_engine("glm")


# Random Forrest
# rand_forest_ranger_spec <- rand_forest(trees = tune(), min_n = tune()) |>
#     set_engine("ranger") |>
#     set_mode("classification")

# SVM
# svm_linear_kernlab_spec <- svm_linear(cost = tune()) |>
#     set_engine("kernlab") |>
#     set_mode("classification")

# Models
models <- mget(str_subset(objects(), "_spec$"))


# Model Workflow -------------------------------------------------------------------------------------------------------

# Combine the recipes and models into a workflow set
wflows <- workflow_set(recipes, models)


# Model Train ----------------------------------------------------------------------------------------------------------

# CV folds
resamples <- vfold_cv(df_train, strata = mvp_flag, v = 10)

# Training metrics
metrics <- metric_set(yardstick::spec, yardstick::sens, yardstick::bal_accuracy, yardstick::ppv, yardstick::f_meas, yardstick::npv)

# Parallel processing
cl <- parallel::makeCluster(parallel::detectCores())
doParallel::registerDoParallel(cl)
    #--- Tuning of models
    tuned_mods <- workflow_map(
      wflows
      , fn = "tune_grid"
      , verbose = TRUE
      , seed = seed
      , resamples = resamples
      , grid = 10
      , metrics = metrics
    )
    #---    
parallel::stopCluster(cl)
foreach::registerDoSEQ()
#---


# Model Comparison -----------------------------------------------------------------------------------------------------

# Desired model metric
chosen_metric <- "bal_accuracy"

# Relabel cols for autoplot
for(.x in seq_len(nrow(tuned_mods))) tuned_mods$info[[.x]]$preproc <- str_extract(tuned_mods$wflow_id[[.x]], "rec_[[:lower:]]+")
autoplot(tuned_mods, rank_metric = chosen_metric)

# Model results and tuning configurations
# tuned_mods |>
#     mutate(metrics = map(result, collect_metrics)) |> # collect_metrics mapped
#     select(wflow_id, metrics) |>
#     unnest(cols = metrics) |> 
#     filter(.metric == chosen_metric) |> 
#     view("model configurations")


# Best Model Selection ----------------------------------------------------

# Extract id of best model
best_mod_id <- rank_results(tuned_mods, rank_metric = chosen_metric, select_best = TRUE) |>
    slice_min(order_by = rank, n = 1, with_ties = FALSE) |> 
    pull(wflow_id)

# Extract best model workflow and tuning configuration
best_mod_wf <- extract_workflow(tuned_mods, id = best_mod_id)
best_mod_cfg <- filter(tuned_mods, wflow_id == best_mod_id) |> 
    pull(result) |> 
    pluck(1) |> 
    select_best(chosen_metric)


# Best Model Fit ----------------------------------------------------------

# Fit best model on test data
best_mod_fit <- finalize_workflow(best_mod_wf, best_mod_cfg) |> 
    last_fit(split)

# Metrics (where are spec and sens?)
collect_metrics(best_mod_fit)

# Test predictions & Confusion Matrix
(preds <- collect_predictions(best_mod_fit))
conf_mat(preds, truth = mvp_flag, estimate = .pred_class)


# Model Metrics --------------------------------------------------------------------------------------------------------

# Holistic view of model metrics
mod_metrics <- function(p, est){
  bind_rows(
    specificity(p, truth = mvp_flag, estimate = {{ est }})
    , sensitivity(p, truth = mvp_flag, estimate = {{ est }})
    , accuracy(p, truth = mvp_flag, estimate = {{ est }})
    , npv(p, truth = mvp_flag, estimate = {{ est }})
    , j_index(p, truth = mvp_flag, estimate = {{ est }})
  )
}

mod_metrics(preds, .pred_class)
autoplot(roc_curve(preds, truth = mvp_flag, estimate = .pred_1, event_level = "second"))


# Variable Importance -----------------------------------------------------

extract_workflow(best_mod_fit) |> 
    extract_fit_parsnip() |> 
    vip::vip(geom = "col")
