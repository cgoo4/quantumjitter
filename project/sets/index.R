library(conflicted)
library(tidyverse)
library(rvest)
library(mirai)
library(tictoc)
library(ggupset)
library(ggVennDiagram)
library(glue)
library(paletteer)
library(ggfoundry)
library(usedthese)

conflict_prefer_all("dplyr", quiet = TRUE)
conflict_scout()

daemons(10)

theme_set(theme_bw())

pal_name <- "wesanderson::Royal2"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

path <- "https://www.applytosupply.digitalmarketplace.service.gov.uk/g-cloud/search"

lot <- "iaas-and-paas"

lot_label <- "Lot 1a"

lot_page <- str_c(path, "?lot=", lot) |>
  read_html()

is_leaf <- \(values) {
  map_lgl(values, \(v) !any(str_detect(values, str_c("^", fixed(v), "-"))))
}

cat_urls <-
  lot_page |>
  html_elements('input[name="serviceCategories"]') |>
  map(\(input) {
    tibble(
      value = html_attr(input, "value"),
      label = input |>
        html_element(xpath = "following-sibling::label") |>
        html_text2()
    )
  }) |>
  list_rbind() |>
  filter_out(is.na(value), is.na(label)) |>
  filter(is_leaf(value)) |>
  mutate(
    category = str_remove(label, "\\s*\\([0-9,]+\\)\\s*$"),
    n_services = as.numeric(str_extract(label, "\\d+")),
    pages = ceiling(n_services / 30),
    url = str_c("&serviceCategories=", value)
  ) |>
  select(url, pages, category)

version <- lot_page |>
  html_elements(".app-search-result:first-child") |>
  html_text() |>
  str_extract("G-Cloud \\d\\d")

tic()

scrape_ids <- possibly(
  \(url, page, category, path, lot, lot_label) {
    refs <- stringr::str_c(
      path,
      "?page=",
      page,
      url,
      "&lot=",
      lot
    ) |>
      rvest::read_html() |>
      rvest::html_elements("#js-dm-live-search-results .govuk-link") |>
      rvest::html_attr("href")

    tibble::tibble(
      lot = lot_label,
      service_id = stringr::str_extract(refs, "[[:digit:]]{15}"), # <1>
      category = category
    )
  },
  otherwise = NULL
)

scraped <- mirai_map(
  uncount(cat_urls, pages, .id = "page"),
  scrape_ids,
  .args = list(path = path, lot = lot, lot_label = lot_label)
)[.progress]

data_df <-
  scraped |>
  list_rbind() |>
  mutate(
    abbr = str_remove(category, "and") |> abbreviate(3) |> str_to_upper()
  ) |>
  filter_out(is.na(service_id)) |>
  distinct()

toc()

all_cats <- data_df |>
  summarise(ids = list(service_id), .by = abbr) |>
  tibble::deframe()

four_cats <- all_cats[c("GNP", "CMO", "OOB", "BRMT")]

four_cats |>
  ggVennDiagram(label = "count", label_alpha = 0) +
  scale_fill_gradient(low = pal[5], high = pal[3]) +
  scale_colour_manual(values = rep(pal[4], 4)) +
  labs(
    x = "Category Combinations",
    y = NULL,
    fill = "# Services",
    title = "The Most Frequent Category Combinations",
    subtitle = glue("Focusing on Four {version} Service Categories"),
    caption = "Source: digitalmarketplace.service.gov.uk\n"
  )

four_cats |> reduce(intersect)

four_cats[c("OOB", "BRMT")] |> reduce(intersect)

make_set_df <- \(df) {
  df |>
    mutate(category = list(sort(unique(category))), .by = service_id) |>
    distinct(service_id, category) |>
    mutate(n = n(), .by = category)
}

top_combos <- \(df, k) {
  keep <- df |>
    distinct(category, n) |>
    slice_max(n, n = k, with_ties = FALSE)

  semi_join(df, keep, by = "category")
}

set_df <- data_df |>
  filter(abbr %in% c("GNP", "CMO", "OOB", "BRMT")) |>
  make_set_df()

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = pal[1]) +
  geom_label(
    aes(y = n, label = n),
    data = \(d) distinct(d, category, n),
    vjust = -0.1,
    size = 3,
    fill = pal[5]
  ) +
  scale_x_upset() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations",
    y = NULL,
    title = "The Most Frequent Category Combinations",
    subtitle = glue("Focusing on Four {version} Service Categories"),
    caption = "Source: digitalmarketplace.service.gov.uk"
  )

set_df <- data_df |>
  filter(n() == 1, .by = service_id) |>
  make_set_df() |>
  top_combos(10)

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = pal[2]) +
  geom_label(
    aes(y = n, label = n),
    data = \(d) distinct(d, category, n),
    vjust = -0.1,
    size = 3,
    fill = pal[3]
  ) +
  scale_x_upset() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations",
    y = NULL,
    title = "10 Most Frequent Single-Category Services",
    subtitle = "Service Categories in the IaaS and PaaS Lots",
    caption = "Source: digitalmarketplace.service.gov.uk"
  )

flatten_combos <- \(df, col) {
  df |>
    summarise(
      combo = str_flatten(sort(unique(.data[[col]])), " | "),
      .by = service_id
    )
}

cat_mix <- data_df |>
  flatten_combos("category") |>
  count(combo, name = "n") |>
  arrange(desc(n)) |>
  slice(1:21) |>
  rename(
    "Intersecting Categories" = combo,
    "Services Count" = n
  )

cat_mix

set_df <- data_df |>
  make_set_df() |>
  top_combos(21)

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = pal[5]) +
  geom_label(
    aes(y = n, label = n),
    data = \(d) distinct(d, category, n),
    vjust = -0.1,
    size = 3,
    fill = pal[4]
  ) +
  scale_x_upset() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations",
    y = NULL,
    title = "Top Intersections Across all Sets",
    subtitle = "Service Categories in the IaaS and PaaS Lots",
    caption = "Source: digitalmarketplace.service.gov.uk"
  )

combos_df <- data_df |>
  flatten_combos("abbr")

top5_int <- combos_df |>
  semi_join(
    count(combos_df, combo, name = "n") |> slice_max(n, n = 5),
    by = "combo"
  )

top5_int |>
  summarise(service_ids = n_distinct(service_id))

used_here()
