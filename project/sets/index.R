library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr")
library(rvest)
library(furrr)
library(wesanderson)
library(tictoc)
library(ggupset)
library(ggVennDiagram)
library(glue)
library(usedthese)

conflict_scout()

plan(multisession)

theme_set(theme_bw())

(cols <- wes_palette("Royal2"))

path <- 
  str_c("https://www.applytosupply.digitalmarketplace", 
        ".service.gov.uk/g-cloud/search?lot=cloud-")

lot_urls <-
  c(
    str_c(path, "hosting"),
    str_c(path, "software"),
    str_c(path, "support")
  )

cat_urls <- future_map(lot_urls, \(x) {
  nodes <- x |>
    read_html() |>
    html_elements(".app-lot-filter__last-list li a")

  tibble(
    url = nodes |>
      html_attr("href"),

    pages = nodes |>
      html_text()
  )
}) |>
  list_rbind() |> 
  mutate(
    pages = parse_number(as.character(pages)),
    pages = if_else(pages %% 30 > 0, pages %/% 30 + 1, pages %/% 30),
    lot = str_extract(url, "(?<=cloud-)[\\w]+"),
    url = str_remove(url, ".*(?=&)")
  )

version <- lot_urls[[1]] |> 
  read_html() |> 
  html_elements(".app-search-result:first-child") |> 
  html_text() |> 
  str_extract("G-Cloud \\d\\d")

tic()

data_df <-
  future_pmap(
    list(
      cat_urls$url,
      cat_urls$pages,
      cat_urls$lot
    ),
    \(x, y, z) {
      future_map_dfr(1:y, \(y) {
        refs <- str_c(
          "https://www.applytosupply.digitalmarketplace", 
          ".service.gov.uk/g-cloud/search?page=",
          y,
          x,
          "&lot=cloud-",
          z
        ) |>
          read_html() |>
          html_elements("#js-dm-live-search-results .govuk-link") |>
          html_attr("href") 
        
       tibble(
            lot = str_c("Cloud ", str_to_title(z)),
            service_id = str_extract(refs, "[[:digit:]]{15}"), # <1>
            cat = str_remove(x, "&serviceCategories=") |>
              str_replace_all("\\Q+\\E", " ") |> # <2>
              str_remove("%28[[:print:]]+%29")
          )
      })
    }
  ) |>
  list_rbind() |>
  select(lot:cat) |>
  mutate(
    cat = str_trim(cat) |> str_to_title(),
    abbr = str_remove(cat, "and") |> abbreviate(3) |> str_to_upper()
  )

toc()

host_df <- data_df |>
  filter(lot == "Cloud Hosting") |>
  group_by(abbr)

keys <- host_df |> 
  group_keys() |> 
  pull(abbr)

all_cats <- host_df |> 
  group_split() |>
  map("service_id") |> 
  set_names(keys)

four_cats <- all_cats[c("CAAH", "PAAS", "OBS", "IND")]

four_cats |> 
  ggVennDiagram(label = "count", label_alpha = 0) +
  scale_fill_gradient(low = cols[5], high = cols[3]) +
  scale_colour_manual(values = cols[c(rep(4, 4))]) +
  labs(
    x = "Category Combinations", y = NULL, fill = "# Services",
    title = "The Most Frequent Category Combinations",
    subtitle = glue("Focusing on Four {version} Service Categories"),
    caption = "Source: digitalmarketplace.service.gov.uk\n"
  )

four_cats |> reduce(intersect)

list(
  four_cats$OBS,
  four_cats$IND
) |>
  reduce(intersect)

set_df <- data_df |>
  filter(abbr %in% c("CAAH", "PAAS", "OBS", "IND")) |>
  mutate(category = list(cat), .by = service_id) |>
  distinct(service_id, category) |>
  mutate(n = n(), .by = category)

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = cols[1]) +
  geom_label(aes(y = n, label = n), vjust = -0.1, size = 3, fill = cols[5]) +
  scale_x_upset() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations", y = NULL,
    title = "The Most Frequent Category Combinations",
    subtitle = glue("Focusing on Four {version} Service Categories"),
    caption = "Source: digitalmarketplace.service.gov.uk"
  )

set_df <- data_df |>
  filter(n() == 1, lot == "Cloud Hosting", .by = service_id) |>
  mutate(category = list(cat), .by = service_id) |>
  distinct(service_id, category) |>
  mutate(n = n(), .by = category)

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = cols[2]) +
  geom_label(aes(y = n, label = n), vjust = -0.1, size = 3, fill = cols[3]) +
  scale_x_upset(n_sets = 10) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations", y = NULL,
    title = "10 Most Frequent Single-Category Services",
    subtitle = "Focused on Service Categories in the Cloud Hosting Lot",
    caption = "Source: digitalmarketplace.service.gov.uk"
  )

cat_mix <- data_df |>
  filter(lot == "Cloud Hosting") |>
  mutate(x = cat) |>
  pivot_wider(service_id, names_from = cat, values_from = x, values_fill = "^") |>
  unite(col = intersect, -service_id, sep = "/") |>
  count(intersect) |>
  mutate(
    intersect = str_replace_all(intersect, "(?:\\Q/^\\E|\\Q^/\\E)", ""),
    intersect = str_replace_all(intersect, "/", " | ")
  ) |>
  arrange(desc(n)) |>
  slice(1:21)

cat_mix |>
  rename(
    "Intersecting Categories" = intersect,
    "Services Count" = n
  )

set_df <- data_df |>
  filter(lot == "Cloud Hosting") |>
  mutate(category = list(cat), .by = service_id) |>
  distinct(service_id, category) |>
  mutate(n = n(), .by = category)

set_df |>
  ggplot(aes(category)) +
  geom_bar(fill = cols[5]) +
  geom_label(aes(y = n, label = n), vjust = -0.1, size = 3, fill = cols[4]) +
  scale_x_upset(n_sets = 22, n_intersections = 21) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.border = element_blank()) +
  labs(
    x = "Category Combinations", y = NULL,
    title = "Top Intersections Across all Sets",
    subtitle = "Focused on Service Categories in the Cloud Hosting Lot",
    caption = "Source: digitalmarketplace.service.gov.uk"
  )

top5_int <- data_df |>
  filter(lot == "Cloud Hosting") |>
  select(service_id, abbr) |>
  mutate(x = abbr) |>
  pivot_wider(names_from = abbr, values_from = x, values_fill = "^") |>
  unite(col = intersect, -service_id, sep = "/") |>
  mutate(
    intersect = str_replace_all(intersect, "(?:\\Q/^\\E|\\Q^/\\E)", ""),
    intersect = str_replace(intersect, "/", " | ")
  ) |>
  mutate(count = n_distinct(service_id), .by = intersect) |>
  arrange(desc(count), intersect, service_id) |>
  add_count(intersect, wt = count, name = "temp") |>
  mutate(temp = dense_rank(desc(temp))) |>
  filter(temp %in% 1:5) |>
  distinct(service_id)

top5_int |>
  summarise(service_ids = n_distinct(service_id))

used_here()
