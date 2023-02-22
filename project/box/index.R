library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidytext)
library(rvest)
library(paletteer)
library(janitor)
library(glue)
library(ggwordcloud)
library(patchwork)
library(clock)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

n <- 4
palette <- "harrypotter::ronweasley2"

cols <- paletteer_c(palette, n = n)

tibble(x = 1:n, y = 1) |>
  ggplot(aes(x, y, fill = cols)) +
  geom_col(colour = "white") +
  geom_label(aes(label = cols |> str_remove("FF$")), 
             size = 4, vjust = 2, fill = "white") +
  annotate(
    "label",
    x = (n + 1) / 2, y = 0.5,
    label = palette,
    fill = "white",
    alpha = 0.8,
    size = 6
  ) +
  scale_fill_manual(values = as.character(cols)) +
  theme_void() +
  theme(legend.position = "none")

tidy <-
  c(
    tidyverse::tidyverse_packages(),
    fpp3::fpp3_packages(),
    tidymodels::tidymodels_packages()
  ) |>
  unique()

base_packages <- c(
  "stats",
  "graphics",
  "grDevices",
  "utils",
  "datasets",
  "methods",
  "base"
)

used_df <-
  used_there("https://www.quantumjitter.com/project/", 21) |>
  mutate(multiverse = case_match(
    Package,
    tidy ~ "tidy",
    base_packages ~ "base",
    .default = "special"
  ))

n_url <- used_df |> summarise(n_distinct(url)) |> pull()

pack_df <- used_df |>
  count(Package, multiverse, wt = n) |>
  mutate(name = "package")

fun_df <- used_df |>
  count(Function, multiverse, wt = n) |>
  mutate(name = "function")

packfun_df <- pack_df |>
  bind_rows(fun_df) |>
  arrange(desc(n)) |>
  mutate(
    packfun = coalesce(Package, Function),
    name = fct_rev(name),
    .by = name
  )

p1 <- packfun_df |>
  filter(name == "package") |> 
  ggplot(aes(fct_reorder(packfun, n), n, fill = multiverse)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_label(aes(label = n), hjust = "inward", size = 2, fill = "white") +
  scale_fill_manual(values = cols[c(2, 3, 4)]) +
  labs(x = NULL, y = NULL, 
       subtitle = glue("Package Usage"))

min_n <- 4

p2 <- packfun_df |>
  filter(name == "function", n >= min_n) |> 
  ggplot(aes(fct_reorder(packfun, n), n, fill = multiverse)) +
  geom_col() +
  coord_flip() +
  geom_label(aes(label = n), hjust = "inward", size = 2, fill = "white") +
  scale_fill_manual(values = cols[c(2, 3, 4)]) +
  labs(x = NULL, y = NULL, 
       subtitle = glue("Function Usage >= {min_n}"))

p1 + p2 +
  plot_annotation(
    title = glue("Favourite Things"),
    subtitle = glue(
      "Used Across {n_url} Projects as at ",
      "{date_format(date_today('Europe/London'), format = '%B %d, %Y')}"
    )
  )

set.seed = 123

packfun_df |>
  mutate(angle = 45 * sample(-2:2, n(), 
                             replace = TRUE, 
                             prob = c(1, 1, 4, 1, 1))) |>
  ggplot(aes(
    label = packfun,
    size = n,
    colour = multiverse,
    angle = angle
  )) +
  geom_text_wordcloud(
    eccentricity = 1,
    grid_margin = 0.95,
    seed = 789
  ) +
  scale_size_area(max_size = 20) +
  scale_colour_manual(values = cols[c(2, 3, 4)]) +
  theme_void() +
  theme(plot.background = element_rect(fill = cols[1]))

used_here()
