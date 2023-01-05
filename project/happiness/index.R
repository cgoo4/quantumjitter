library(tidyverse)
library(tidymodels)
library(tidytext)
library(tidyclust)
library(glue)
library(readxl)
library(janitor)
library(ggrepel)
library(sf)
library(scales)
library(usedthese)

theme_set(theme_bw())

cols <- c("#0AC449", "#CF4E0A", "#0057B7", "#FFD700", "#870AC4") |>
  fct_inorder()

tibble(x = 1:5, y = 1) |>
  ggplot(aes(x, y, fill = cols)) +
  geom_col(colour = "white") +
  geom_label(aes(label = cols), size = 4, vjust = 2, fill = "white") +
  annotate(
    "label",
    x = 3, y = 0.5,
    label = "Custom Pallette",
    fill = "white",
    alpha = 0.8,
    size = 6
  ) +
  scale_fill_manual(values = as.character(cols)) +
  theme_void() +
  theme(legend.position = "none")

raw_df <-
  read_xlsx("london-borough-profiles.xlsx", sheet = 2) |>
  clean_names() |>
  filter(str_starts(code, "E")) |>
  mutate(across(where(is.character), ~ na_if(., ".")),
    inner_outer_london = str_remove(inner_outer_london, " London")
  )

raw_df |> 
  rowwise() |> 
  mutate(na_count = sum(is.na(cur_data()))) |> 
  select(area_name, na_count) |>
  filter(na_count != 0) |>
  arrange(desc(na_count))

pca_fit <- raw_df |>
  select(where(is.numeric)) |>
  prcomp(scale = TRUE)

pca_augmented <-
  pca_fit |>
  augment(raw_df)

pca_augmented |>
  ggplot(aes(.fittedPC1, .fittedPC2, fill = inner_outer_london)) +
  geom_label(aes(label = area_name), size = 2, hjust = "inward") +
  scale_fill_manual(values = as.character(cols)) +
  labs(
    title = "33 London Areas", fill = "London",
    x = "Principal Component 1", y = "Principal Component 2",
    caption = "Source: data.london.gov.uk"
  )

pca_tidied <- pca_fit |>
  tidy(matrix = "eigenvalues")

pct_explained <-
  pca_tidied |>
  pluck("cumulative", 2)

pca_tidied |>
  ggplot(aes(PC, percent)) +
  geom_col(aes(fill = if_else(PC <= 2, TRUE, FALSE)),
    alpha = 0.8, show.legend = FALSE
  ) +
  scale_y_continuous(labels = label_percent(1)) +
  scale_fill_manual(values = as.character(cols)) +
  coord_flip() +
  labs(
    title = glue(
      "{percent(pct_explained, 0.1)} of the ",
      "Variance Explained by Principal Components 1 & 2"
    ),
    x = "Principal Component", y = NULL
  )

pattern <-
  str_c("_\\d{4}|_st.+|_score|_rates|^percent(_of)?_|",
        "^proportion_of_|^population(_of)?_|^number(_of)?_|", 
        "_\\d{2}_out_of_\\d{2}|_estimate|_percent")

pca_fit |>
  tidy(matrix = "rotation") |>
  pivot_wider(names_from = "PC", names_prefix = "PC", 
              values_from = "value") |>
  mutate(column = str_remove_all(column, pattern)) |>
  ggplot(aes(PC1, PC2)) +
  geom_segment(
    xend = 0, yend = 0, colour = "grey70",
    arrow = arrow(ends = "first", length = unit(8, "pt"))
  ) +
  geom_text_repel(aes(label = column), size = 3) +
  theme_minimal() +
  labs(
    x = "PC 1", y = "PC 2",
    title = "Characteristics Influencing Area Positioning",
    caption = "Source: data.london.gov.uk"
  ) +
  theme(axis.text = element_blank())

pca_long <- 
  pca_augmented |>
  select(area_name, matches("happ|anx|average_age")) |>
  rename_with(~ str_remove(., "_.*")) |>
  rename("avg_age" = "average") |>
  pivot_longer(-area, values_to = "score") |>
  mutate(area = reorder_within(area, score, name)) 

pca_long |>
  ggplot(aes(area, score, colour = name)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~name, scales = "free") +
  scale_x_reordered() +
  scale_colour_manual(values = as.character(cols)) +
  coord_flip() +
  labs(x = NULL, caption = "Source: data.london.gov.uk")

set.seed(2022)

kclusts <-
  tibble(k = 1:6) |>
  mutate(
    kclust = map(k, \(k) kmeans(
      pca_augmented |> select(.fittedPC1, .fittedPC2), k)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, pca_augmented)
  )

assignments <-
  kclusts |>
  unnest(cols = c(augmented))

clusters <-
  kclusts |>
  unnest(cols = c(tidied))

assignments |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point(aes(color = .cluster)) +
  facet_wrap(~k, nrow = 2) +
  scale_colour_manual(values = as.character(cols[c(1:6)])) +
  geom_point(data = clusters, size = 4, shape = 13) +
  labs(
    title = "How Many Clusters Best Captures the Groupings?",
    subtitle = "X Marks the Cluster Centre",
    caption = "Source: data.london.gov.uk"
  )

kclusts |>
  unnest(cols = c(glanced)) |>
  ggplot(aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  geom_label(aes(label = if_else(k == 3, "Elbow", NA_character_)),
    nudge_y = -25, fill = cols[1]
  ) +
  labs(
    title = "Elbow Method",
    x = "Clusters", y = "Within-Cluster Variance"
  )

assignments |>
  filter(k == 3) |>
  ggplot(aes(.fittedPC1, .fittedPC2, fill = .cluster)) +
  geom_label(aes(label = area_name), 
             size = 2, hjust = "inward", overlap = FALSE) +
  scale_fill_manual(values = as.character(cols[c(1, 2, 4)])) +
  labs(
    title = "Closely-Related London Areas", fill = "Cluster",
    x = "Principal Component 1", y = "Principal Component 2",
    caption = "Source: data.london.gov.uk"
  )

kmeans_spec <- k_means(num_clusters = tune()) |> 
  set_engine("stats", algorithm = "Hartigan-Wong")

kmeans_rec <- raw_df |> 
  select(where(is.numeric)) |> 
  recipe(~ .) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_predictors()) |> 
  step_pca(all_predictors(), threshold = 0.9)

kmeans_wflow <- workflow() |>
  add_model(kmeans_spec) |>
  add_recipe(kmeans_rec)

kmeans_cv <- vfold_cv(pca_augmented, v = 5)

kmeans_res <- tune_cluster(
  kmeans_wflow,
  resamples = kmeans_cv,
  grid = crossing(
    num_clusters = seq(1, 6, 1)
  ),
  control = control_grid(save_pred = TRUE),
  metrics = cluster_metric_set(sse_total, sse_ratio)
)

kmeans_metrics <- kmeans_res |> collect_metrics()

kmeans_metrics |>
  filter(.metric == "sse_ratio") |>
  ggplot(aes(num_clusters, mean)) +
  geom_point() +
  geom_line() +
  geom_label(aes(label = if_else(num_clusters == 3, "Elbow", NA_character_)),
             nudge_y = -0.1, fill = cols[1]) +
  labs(title = "Elbow Method", x = "Clusters", y = "WSS") +
  scale_x_continuous(breaks = 1:6)

kmeans_spec <- k_means(num_clusters = 3) |> 
  set_engine("stats", algorithm = "Hartigan-Wong")

kmeans_wflow <- kmeans_wflow |> 
  update_model(kmeans_spec)

kmeans_fit <- kmeans_wflow |> 
  fit(pca_augmented) 

kmeans_clust <- kmeans_fit |> 
  extract_centroids() |> 
  rename_with(~ str_c(".fitted", .), starts_with("PC"))

kmeans_aug <- kmeans_fit |> 
  augment(pca_augmented)

kmeans_aug |>
  mutate(.pred_cluster = str_remove(.pred_cluster, "Cluster_")) |> 
  ggplot(aes(.fittedPC1, .fittedPC2)) +
  geom_label(aes(label = area_name, colour = .pred_cluster),
             size = 2, hjust = "inward") +
  scale_colour_manual(values = as.character(cols[c(1:3, 5)])) +
  geom_point(data = kmeans_clust, size = 4, shape = 13) +
  labs(
    title = "Closely-Related London Areas", fill = "Cluster",
    subtitle = "X Marks the Cluster Centre",
    x = "Principal Component 1", y = "Principal Component 2",
    colour = "Cluster",
    caption = "Source: data.london.gov.uk"
  )

shape_df <-
  st_read("statistical-gis-boundaries-london/ESRI",
    "London_Borough_Excluding_MHW",
    as_tibble = TRUE, quiet = TRUE
  ) |>
  left_join(assignments |> 
              filter(k == 3), by = join_by(GSS_CODE == code)) |>
  select(.cluster, inner_outer_london, NAME, geometry) |>
  pivot_longer(c(.cluster, inner_outer_london)) |>
  mutate(value = recode(value, "1" = "Cluster 1", 
                        "2" = "Cluster 2", "3" = "Cluster 3"))

shape_df |>
  mutate(name = recode(name,
    ".cluster" = "By Cluster",
    "inner_outer_london" = "By Inner/Outer"
  )) |>
  ggplot() +
  geom_sf(aes(fill = value), colour = "white") +
  geom_sf_label(aes(label = NAME), size = 2, alpha = 0.7) +
  scale_fill_manual(values = as.character(cols[c(3, 4, 1, 2, 5)])) +
  facet_wrap(~name) +
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = NULL)

used_here()
