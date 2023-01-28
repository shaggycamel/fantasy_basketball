
library(tidyverse)
library(tidymodels)

source(here::here("functions", "fn_data.R"))
source(here::here("data", "common_data_objects.R"))
source(here::here("data", "useful_objects.R"))


clust_rec <- 
  recipe(read_df("PlayerGameLog.pq")) |> 
  update_role_requirements(role = "NA", bake = FALSE) |> 
  step_select(any_of(c(cols$anl_cols, cols$anl_non_perc, cols$oth_cols, cols$tmp_cols))) |> 
  step_select(!contains(c("_pct", "video", "plus_minus"))) |> 
  step_zv(everything()) |> 
  step_normalize(everything()) 

df_clust <- bake(prep(clust_rec), new_data = NULL)


# Visual ------------------------------------------------------------------

corrplot::corrplot(cor(df_clust), tl.col = "black")

# Stat distributions
df_clust |> 
  pivot_longer(everything(), names_to = "stat") |> 
  nest_by(stat, .keep=TRUE) |> 
  mutate(plt = list(
    ggplot(data, aes(x = value)) +
    geom_density(fill = "dodgerblue") +
    theme_bw() +
    labs(title = stat, x = NULL)
  )) |> 
  pull(plt)


# Clusters ----------------------------------------------------------------

library(factoextra)
library(cluster)

# fviz_nbclust(
#   df_clust, 
#   pam, 
#   k.max = 6,
#   method = "wss",
#   # diss = get_dist(df_clust, "kendall")
# )

clust_num <- 4
df_clust <- 
  df_clust |> 
  mutate(
    kmeans = factor(kmeans(df_clust, centers = clust_num)$cluster)
    # , dbscan = fpc::dbscan(df_clust, eps = 0.45, MinPts = 1000, method = "hybrid")
  )


# Avg stat per cluster
df_plt <- df_clust |> 
  group_by(kmeans) |> 
  summarise(across(everything(), mean)) |> 
  pivot_longer(cols = -kmeans, names_to = "stat") 

ggplot(df_plt, aes(x = value, y = stat, fill = kmeans)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(kmeans))

nest_by(df_plt, kmeans, .keep = TRUE) |> 
  mutate(plot = list(
    ggplot(data, aes(x = value, y = stat, fill = kmeans)) +
    geom_col(position = "dodge") +
    xlim(min(df_plt$value), max(df_plt$value)) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(4, "Pastel2"), drop = FALSE)
  )) |> 
  pull(plot)




# Build 3D visual to plot clusters ----------------------------------------

unscale_metrics <- tidy(prep(clust_rec), 4) |> 
  pivot_wider(statistic, names_from = terms, values_from = value) |> 
  column_to_rownames("statistic")

for(col in colnames(unscale_metrics)) df_clust[col] <- (df_clust[col] * unscale_metrics["sd", col]) + unscale_metrics["mean", col]

arrow::write_parquet(df_clust, here::here("blog", "posts", "2022-11-25", "shiny_plot", "df_clust.pq"))

