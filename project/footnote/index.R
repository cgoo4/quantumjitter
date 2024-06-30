library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(readxl)
library(gt)
library(ggfoundry)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

pal <- c(
  "#FFF1E5", "#F2DFCE",
  "#333333", "#800D33",
  "#C00000", "#00994D"
)

pal_name <- "Financial Times"

display_palette(pal, pal_name)

read_data <- \(x) {
  read_xlsx(
    x,
    skip = 12,
    col_names = c(
      "occupation",
      "persons"
    ),
    col_types = c(
      "text",
      "numeric",
      "skip",
      "skip",
      "skip",
      "skip",
      "skip"
    )
  )
} |> 
  mutate(year = x |> str_remove(".xlsx") |> as.integer())

pop_df <- list("2004.xlsx", "2021.xlsx") |> 
  map(read_data) |> 
  list_rbind()

change_df <- pop_df |> 
  filter(str_starts(occupation, "\\d{4} ")) |> 
  pivot_wider(names_from = year, values_from = persons) |> 
  separate_wider_regex(occupation, 
                       c(soc = "\\d{4}", " ", occupation = ".*")) |> 
  mutate(change = `2021` / `2004` - 1) |> 
  arrange(desc(change)) |> 
  mutate(group = if_else(row_number() <= 10, "Risers", "Fallers")) |> 
  slice(c(1:10, (n()-10):n())) |> 
  relocate(group)

gt_tbl <- change_df |>
  gt(rowname_col = c("occupation"), groupname_col = "group") |>
  tab_header(title = "UK Employment by Occupation") |> 
  tab_options(table.width = pct(100)) |> 
  fmt_number(
    columns = starts_with("2"),
    decimals = 0
  ) |>
  fmt_percent(
    columns = starts_with("c"),
    decimals = 0,
    force_sign = TRUE
  ) |>
  sub_missing() |>
  tab_spanner(
    label = "Year",
    columns = starts_with("2")
  ) |> 
  tab_style(
    style = cell_text(transform = "capitalize"),
    locations = cells_column_labels(!starts_with("s"))
  ) |> 
  tab_style(
    style = cell_text(transform = "uppercase"),
    locations = cells_column_labels("soc")
  ) |> 
  tab_footnote(
    footnote = "Not elsewhere classified",
    locations = cells_stub(rows = contains("n.e.c."))
  ) |> 
  tab_footnote(
    footnote = "Count of all persons",
    locations = cells_column_spanners()
  ) |>
  tab_footnote(
    footnote = "Standard Occupational Classification 2020",
    locations = cells_column_labels(columns = "soc")
  ) |>
  tab_footnote(
    footnote = "Top & bottom 10 occupations ordered by percent change",
    locations = cells_row_groups(groups = c("Risers", "Fallers"))
  ) |>
  tab_footnote(
    footnote = "Figures suppressed as statistically unreliable",
    locations = cells_body(
      columns = c(change, `2021`),
      rows = is.na(change)
    )
  ) |>
  tab_source_note(source_note = "Source: Office for National Statistics (ONS)")

gt_tbl |>
  tab_style_body(
    style = cell_fill(color = "lightblue"),
    pattern = "n.e.c.",
    extents = "stub"
  ) |> 
  opt_stylize(style = 6, color = "gray", add_row_striping = TRUE) |> 
  as_raw_html()

gt_ft <- gt_tbl |> 
  tab_options(
    table.border.top.color = pal[1],
    table.border.bottom.color = pal[1],
    table.background.color = pal[1],
    table.font.size = px(10),
    table.font.color = pal[3],
    heading.align = "left",
    heading.title.font.size = px(20),
    heading.title.font.weight = "bold",
    heading.background.color = pal[1],
    row.striping.include_table_body = TRUE,
    row.striping.include_stub = TRUE,
    row.striping.background_color = pal[2],
    row_group.background.color = pal[1]
  ) |> 
  tab_header(title = html("UK Employment by Occupation  ", 
                          local_image("logo.png", height = 20))) |> 
  tab_style(
    style = list(
      cell_text(font = "Financier Display"),
      cell_borders(sides = "bottom", weight = px(3), color = "#262A33")
      ),
    locations = cells_title()
  ) |>
  tab_style(
    style = cell_text(size = px(14)),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = cell_text(color = pal[4], weight = "bold"),
    locations = cells_stub()
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_column_labels(), 
                     cells_column_spanners(), 
                     cells_row_groups())
  ) |> 
  tab_style(
    style = cell_borders(style = "hidden"),
    locations = list(cells_body(),
                     cells_row_groups(),
                     cells_stub())
  ) |>
  tab_style(
    style = cell_text(color = pal[6], weight = "bold"),
    locations = cells_body(
      columns = change,
      rows = change >= 0
    )
  ) |> 
  tab_style(
    style = cell_text(color = pal[5], weight = "bold"),
    locations = cells_body(
      columns = change,
      rows = change < 0
    )
  ) |> 
  tab_style(
    style = cell_text(color = "grey40", size = px(9)),
    locations = list(cells_footnotes(), cells_source_notes())
  )

gt_ft |> as_raw_html()

used_here()
