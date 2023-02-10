library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(clock)
library(wesanderson)
library(hansard)
library(dendextend)
library(corrplot)
library(broom)
library(factoextra)
library(glue)
library(ggrepel)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

(cols <- wes_palette(name = "Moonrise2"))

url_prefix <- "http://data.parliament.uk/members/"

mps <- commons_members() |>
  filter(party_value == "Labour" | about == str_c(url_prefix, "478")) |>
  mutate(ID = str_replace(about, url_prefix, ""))

saveRDS(mps, file = "mps.rds")

mps <- readRDS("mps.rds")

start_date <- "2017-06-08"
end_date <- "2018-01-28"

pull_votes <- \(x) {
  mp_vote_record(x,
    start_date = start_date,
    end_date = end_date,
    verbose = FALSE
  ) |>
    mutate(mp = x)
}

votes <-
  map(mps$ID, possibly(pull_votes, NULL)) |>
  compact() |>
  map(simplify, "tibbles") |>
  list_rbind() |> 
  rename("lobby" = "vote")

saveRDS(votes, file = "votes.rds")

votes <- readRDS("votes.rds")

votes_df <- votes |>
  left_join(mps, by = join_by(mp == ID)) |>
  select(about = about.x, title, date_value, 
         lobby, mp, name = full_name_value) |>
  transmute(
    vote = if_else(lobby == "aye", 1, -1),
    mp = str_c(name, " (", mp, ")"),
    about = str_replace(about, "http://data.parliament.uk/resources/", ""),
    title = str_c(title, " (", about, ")")
  ) |> 
  select(-about) |> 
  pivot_wider(names_from = title, values_from = vote, values_fill = 0)

scaled_df <-
  votes_df |>
  mutate(across(-mp, scale))

scaled_df |>
  summarise(across(-mp, list(mean = mean, sd = sd))) |> 
  summarise(
    sd_min = min(pick(ends_with("_sd"))),
    sd_max = max(pick(ends_with("_sd"))),
    mean_min = min(pick(ends_with("_mean"))) |> round(1),
    mean_max = max(pick(ends_with("_mean"))) |> round(1)
  )

scaled_df |>
  select(-mp) |>
  get_clust_tendency(nrow(votes_df) - 1) |>
  pluck("hopkins_stat")

scaled_df |>
  select(-mp) |>
  dist() |>
  fviz_dist(
    show_labels = FALSE,
    gradient = list(
      low = cols[1],
      mid = cols[3],
      high = cols[4]
    )
  )

orig_dist <- scaled_df |>
  select(-mp) |>
  dist()

dend_meths <-
  c(
    "complete",
    "average",
    "single",
    "ward.D",
    "ward.D2",
    "mcquitty",
    "median",
    "centroid"
  )

dend_list <-
  map(dend_meths, \(x) {
    orig_dist |>
      hclust(x) |>
      as.dendrogram()
  })

dend_list |>
  reduce(dendlist) |>
  set_names(dend_meths) |>
  cor.dendlist() |>
  corrplot(
    "pie",
    "lower",
    col = cols[1],
    mar = c(1, 0.5, 4, 0.5),
    order = "AOE",
    tl.cex = 0.8,
    tl.col = "black",
    cl.cex = 0.7
  )

methods <- list(
  "complete",
  "average",
  "single",
  "ward.D",
  "ward.D2",
  "mcquitty",
  "median",
  "centroid"
)

best_method <- map(methods, \(x) {
  co_comp <-
    orig_dist |>
    hclust(x) |>
    cophenetic()
  tibble(
    correlation = cor(orig_dist, co_comp),
    method = x
  )
}) |> 
  list_rbind()

best_method |>
  ggplot(aes(reorder(method, correlation), correlation)) +
  geom_col(fill = cols[1], width = 0.8) +
  geom_text(aes(label = str_c(method, "  ", round(correlation, 2))),
    hjust = 1.3, colour = "white"
  ) +
  coord_flip() +
  labs(
    x = "Method", y = "Correlation",
    title = "Cluster Method Correlation Coefficients",
    caption = "Source: Hansard"
  )

dend_avg <- orig_dist |>
  hclust("average") |>
  as.dendrogram()

labels(dend_avg) <- scaled_df$mp[order.dendrogram(dend_avg)]

dend <- dend_avg |>
  color_branches(k = 2, col = cols[4]) |>
  set("labels_cex", 0.4)

start_formatted <- date_parse(start_date, format = "%Y-%m-%d") |> 
  date_format(format = "%b %d, %Y")

end_formatted <- date_parse(end_date, format = "%Y-%m-%d") |> 
  date_format(format = "%b %d, %Y")

ggplot(rev(dend), horiz = TRUE, offset_labels = -0.2) +
  labs(
    y = "\nDistance", title = "Hierarchical Clustering of Labour MPs",
    subtitle = "Based on House of Commons Divisions Since the 2017 Election",
    caption = glue(
      "Source: Hansard ({start_formatted} to {end_formatted})")
  ) +
  theme(panel.border = element_blank())

dend_cuts <- dend |>
  assign_values_to_leaves_nodePar(19, "pch") |>
  assign_values_to_leaves_nodePar(5, "cex") |>
  assign_values_to_leaves_nodePar(cols[1], "col") |>
  set("labels_cex", 0.4) |>
  set("branches_lwd", 2.5) |>
  color_branches(k = 2, col = cols[1]) |>
  cut(h = 50)

ggplot(rev(dend_cuts$lower[[1]]),
  horiz = TRUE,
  nodePar = nodePar,
  offset_labels = -0.5
) +
  labs(
    title = "Cluster of Six",
    subtitle = "MPs who Branch off First in the Dendrogram"
  ) +
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

fewest_votes <- votes |>
  left_join(mps, by = join_by(mp == ID)) |>
  summarise(n_lobby = n(), .by = c(full_name_value, lobby)) |>
  rename(mp = full_name_value) |> 
  pivot_wider(names_from = "lobby", values_from = "n_lobby") |>
  mutate(total = aye + no,
         mp = fct_reorder(mp, total)) |>
  slice_min(n = 10, order_by = total) |>
  pivot_longer(cols = -mp) |>
  filter(name != "total")

fewest_votes |>
  ggplot(aes(mp, value, fill = name)) +
  geom_col() +
  geom_label(aes(label = value), position = position_stack()) +
  scale_fill_manual(values = cols[c(1, 3)]) +
  coord_flip() +
  labs(title = "Labour MPs Voting Fewest Times",
       y = "Votes", x = NULL, fill = NULL)

tidy_df <- votes_df |>
  pivot_longer(cols = -mp, names_to = "title", values_to = "vote")

mod <- lm(vote ~ ., data = tidy_df)

mod_df <- mod |>
  augment() |>
  as_tibble()

ggplot(mod_df, aes(title, .cooksd, colour = mp)) +
  geom_jitter() +
  geom_label_repel(aes(label = if_else(.cooksd > 0.002, mp, NA)), size = 4) +
  scale_colour_manual(values = wes_palette(220, name = "Moonrise2", type = "continuous")) +
  labs(title = "Cook's Distance") +
  coord_flip() +
  theme(
    panel.border = element_blank(),
    axis.text = element_text(size = 6),
    legend.position = "none"
  )

mod_df |>
  filter(str_detect(title, "759161|824379|809989")) |>
  mutate(title = str_wrap(title, 30)) |> 
  ggplot(aes(title, .cooksd, colour = mp)) +
  geom_point(size = 4) +
  geom_label_repel(aes(label = if_else(.cooksd > 0.0015, mp, NA)), size = 4) +
  ggtitle("Cook's Distance") +
  theme(
    axis.line.x = element_line(color = "grey60"),
    axis.text = element_text(size = 8),
    legend.position = "none",
    axis.title = element_blank()
  ) +
  scale_colour_manual(values = wes_palette(
    210, name = "Moonrise2", type = "continuous")) +
  coord_flip()

used_here()
