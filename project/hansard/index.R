library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(clock)
library(hansard)
library(dendextend)
library(ggdendro)
library(corrplot)
library(broom)
library(factoextra)
library(glue)
library(ggrepel)
library(geomtextpath)
library(ggfoundry)
library(paletteer)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

pal_name <- "wesanderson::Moonrise2"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

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
      low = pal[1],
      mid = pal[3],
      high = pal[4]
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
    col = pal[1],
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
  ggplot(aes(correlation, reorder(method, correlation))) +
  geom_col(fill = pal[1], width = 0.8) +
  geom_text(aes(label = str_c(method, "  ", round(correlation, 2))),
    hjust = 1.3, colour = "white"
  ) +
  labs(
    x = "Method", y = "Correlation",
    title = "Cluster Method Correlation Coefficients",
    caption = "Source: Hansard"
  )

dend_avg <- orig_dist |>
  hclust("average") |>
  as.dendrogram()

labels(dend_avg) <- scaled_df$mp[order.dendrogram(dend_avg)]

dd <- dendro_data(dend_avg, type = "rectangle")

dd$labels <- dd$labels |> mutate(fill = if_else(x <= 6, "darkorange", "darkgreen"))

angle <- seq(75, by = -1.58, length.out = 210)

ggplot(segment(dd)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(
    aes(x = x, y = y, label = label, colour = fill), angle = angle,
    size = 2, hjust = -0.1, data = dd$labels
    ) +
  geom_casting(
    aes(x = x, y = y, fill = fill), shape = "hibiscus",
    size = 0.025, data = dd$labels
    ) +
  scale_y_reverse(expand = c(0.2, 0)) + 
  scale_fill_identity() +
  scale_colour_identity() +
  coord_radial() +
  theme_void()

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
  ggplot(aes(value, mp, fill = name)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = value), position = position_stack()) +
  scale_fill_manual(values = pal[c(1, 3)]) +
  labs(title = "Labour MPs Voting Fewest Times",
       y = "Votes", x = NULL, fill = NULL)

tidy_df <- votes_df |>
  pivot_longer(cols = -mp, names_to = "title", values_to = "vote")

mod <- lm(vote ~ ., data = tidy_df)

mod_df <- mod |>
  augment() |>
  as_tibble()

mod_df |>
  mutate(label = str_c(mp, "\n", str_wrap(title, 25))) |>
  ggplot(aes(.cooksd, title, colour = if_else(.cooksd > 0.002, mp, NA))) +
  geom_jitter(size = 0.4) +
  geom_label_repel(aes(label = if_else(.cooksd > 0.002, label, NA)), size = 1.5) +
  geom_texthline(
    yintercept = 120, label = " Cook's Distance ",
    arrow = arrow(
      angle = 30, length = unit(0.1, "inches"),
      ends = "last", type = "closed"
    ),
    hjust = 0, vjust = 0.5, color = pal[1]
  ) +
  scale_colour_manual(values = pal[c(1:4, 1)]) +
  labs(x = NULL, y = NULL) +
  theme(
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  coord_polar()

mod_df |>
  filter(str_detect(title, "759161|824379|809989")) |>
  mutate(title = str_wrap(title, 30)) |> 
  ggplot(aes(.cooksd, title, colour = if_else(.cooksd > 0.0015, mp, NA))) +
  geom_point(size = 4) +
  geom_label_repel(aes(label = if_else(.cooksd > 0.0015, mp, NA)), size = 4) +
  ggtitle("Cook's Distance") +
  theme(
    axis.line.x = element_line(colour = "grey60"),
    axis.text = element_text(size = 8),
    legend.position = "none",
    axis.title = element_blank()
  ) +
  scale_colour_manual(values = pal[c(1:4, 1:2)])

used_here()
