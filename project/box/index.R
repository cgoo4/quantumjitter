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
library(geomtextpath)
library(ggfoundry)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

n <- 6
pal_name <- "LaCroixColoR::CranRaspberry"

pal <- paletteer_d(pal_name, n = n)

display_palette(fill = pal, n = n, pal_name = pal_name)

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
  used_there("https://www.quantumjitter.com/project/") |>
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

prep_data <- \(x, y){
  used_df |>
    count({{ x }}, multiverse, wt = n) |>
    filter(n > y) |>
    arrange(multiverse, n) |>
    mutate(row = row_number())
}

radial_df <- prep_data(Package, 10)
radial_df2 <- prep_data(Function, 20)

prep_lines <- \(data){
  data |>
    summarise(
      start = min(row),
      end = max(row),
      .by = multiverse
    )
}

lines_df <- prep_lines(radial_df)
lines_df2 <- prep_lines(radial_df2)

prep_plot <- \(data, data2, x){
  data |>
    mutate({{ x }} := fct_reorder({{ x }}, row)) |>
    ggplot(aes({{ x }}, n, fill = multiverse, colour = multiverse)) +
    geom_col(colour = pal[6]) +
    geom_textpath(aes(label = n), colour = "white", vjust = -0.2, size = 3) +
    geom_textpath(aes(label = {{ x }}),
      size = 3, colour = "black",
      text_only = TRUE, offset = unit(-10, "pt"), angle = -70, hjust = 1
    ) +
    geom_textsegment(
      aes(start, 0.6, xend = end, yend = 0.6, label = multiverse),
      data = data2,
      linewidth = 1,
      size = 2, gap = FALSE,
      offset = unit(-7, "pt")
    ) +
    coord_radial(inner.radius = 0.25) +
    scale_y_log10() +
    scale_fill_manual(values = pal[c(1, 4, 5)]) +
    scale_colour_manual(values = pal[c(1, 4, 5)]) +
    theme_void() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "none"
    )
}

p1 <- prep_plot(radial_df, lines_df, Package)
p2 <- prep_plot(radial_df2, lines_df2, Function)

p1 + p2 + plot_annotation(
  title = "Top Package & Function Usage",
  subtitle = glue("Across {n_url} Projects"),
  theme = theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
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
  scale_colour_manual(values = pal[c(1, 6, 5)]) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"))

used_here()
