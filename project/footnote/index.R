library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr")
library(readxl)
library(gt)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

cols <- c(
  "#FFF1E5", "#F2DFCE",
  "#333333", "#800D33",
  "#C00000", "#00994D"
) |>
  fct_inorder()

tibble(x = 1:6, y = 1) |>
  ggplot(aes(x, y, fill = cols)) +
  geom_col(colour = "white") +
  geom_label(aes(label = cols),
    nudge_y = -0.1, fill = "white"
  ) +
  annotate(
    "label",
    x = 3.5, y = 0.5,
    label = "Financial Times",
    fill = "white",
    alpha = 0.8,
    size = 6
  ) +
  scale_fill_manual(values = as.character(cols)) +
  theme_void() +
  theme(legend.position = "none")

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
  separate(occupation, into = c("soc", "occupation"), sep = 5) |> 
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
    table.border.top.color = "#FFF1E5",
    table.border.bottom.color = "#FFF1E5",
    table.background.color = "#FFF1E5",
    table.font.size = px(10),
    table.font.color = "#262A33",
    heading.align = "left",
    heading.title.font.size = px(20),
    heading.title.font.weight = "bold",
    heading.background.color = "#FFF1E5",
    row.striping.include_table_body = TRUE,
    row.striping.include_stub = TRUE,
    row.striping.background_color = "#F2DFCE",
    row_group.background.color = "#FFF1E5"
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
    style = cell_text(color = "#800D33", weight = "bold"),
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
    style = cell_text(color = "#00994D", weight = "bold"),
    locations = cells_body(
      columns = change,
      rows = change >= 0
    )
  ) |> 
  tab_style(
    style = cell_text(color = "#C00000", weight = "bold"),
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
