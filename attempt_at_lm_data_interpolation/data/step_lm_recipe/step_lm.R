step_lm <- function(
  recipe, 
  ..., 
  role = NA, 
  trained = FALSE, 
  ref_dist = NULL,
  options = list(df_rec = recipe$template, names = TRUE),
  skip = FALSE,
  id = rand_id("lm")
  ) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures 
  ##  the values and also checks to make sure that they are not empty.  
  terms <- ellipse_check(...) 

  add_step(
    recipe, 
    step_lm_new(
      terms = terms, 
      trained = trained,
      role = role, 
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id
    )
  )
}


step_lm_new <- 
  function(terms, role, trained, ref_dist, options, skip, id) {
    step(
      subclass = "lm", 
      terms = terms,
      role = role,
      trained = trained,
      ref_dist = ref_dist,
      options = options,
      skip = skip,
      id = id
    )
  }


prep.step_lm <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info) 
  
    ## We'll use the names later so make sure they are available
  if (x$options$names == FALSE) {
    rlang::abort("`names` should be set to TRUE")
  }
  
  # Compute percentile grid
  ref_dist <- purrr::map(training[, col_names],  get_lm_trsfm, args = x$options)

  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  step_lm_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    ref_dist = ref_dist,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}


get_lm_trsfm <- function(x, args = NULL) {
    
    df_lm <- map_dfr(1:length(x), ~ read.csv(here::here("data", "step_lm_reipe", "df_lm.csv")))
    exec <- 
        "df_players_lm <- select(df_rec, all_of(stat_cols)) |>
            (\\(.df) .df * df_lm)() |> 
            bind_cols(select(df_rec, !all_of(stat_cols))) |> 
            select(all_of(colnames(df_rec)))
        
        walk(stat_cols, ~{
            df_players_lm <<- df_players_lm |> 
                mutate(!!.x := !!sym(.x) * (year - min(year) + 1))
        })"
    
    res <- rlang::exec(exec, x = x, !!!args)
}