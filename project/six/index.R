library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(clock)
library(scales)
library(ggfoundry)
library(paletteer)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

n <- 9
pal_name <- "wesanderson::Chevalier1"

pal <- paletteer_d(pal_name)
pal <- colorRampPalette(pal)(n)

display_palette(
  fill = pal, n = n, pal_name = pal_name, 
  shape = "tube", shape_size = 0.9, label_size = 2
  )

url <- 
  "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/"

gcloud_csv <- str_c(url, "678283/G-Cloud-spend-Dec2017.csv")

dos_csv <- str_c(url, "678286/DOS-spend-Dec2017.csv")

names <- c(gcloud_csv, dos_csv)

map(names, guess_encoding)

colnam <-
  c("sector",
    "lot",
    "month",
    "spend",
    "status",
    "supplier",
    "customer",
    "framework")

read_dm <- \(x){
  read_csv(
    x,
    col_names = colnam,
    skip = 1,
    locale = locale(encoding = "ISO-8859-1"),
    col_types = NULL,
    show_col_types = FALSE)
}

combined_df <- map(names, read_dm) |> 
  set_names(c("gcloud", "dos")) |> 
  bind_rows() |> 
  mutate(framework = if_else(is.na(framework), "DOS", framework))

clean_df <- combined_df |>
  mutate(
    month_end = date_parse(str_c(month, "01", sep = "-"), 
                           format = "%b-%y-%d") |> 
      add_months(1) |> add_days(-1),
    version = str_remove(framework, fixed("-Cloud ")),
    version = str_replace(version, fixed("G-Cloud"), "G1"),
    version = str_replace(version, fixed("GIII"), "G3"),
    version = str_replace(version, fixed("GServices II"), "G2"),
    framework = str_extract(framework, ".{3,7}"),
    spend = str_remove(spend, fixed("£")),
    spend = str_replace(spend, "^\\(", "-"),
    spend = parse_number(spend),
    SME_spend = if_else(status == "SME", spend, 0)
  )

version_df <- clean_df |>
  filter(version != "DOS") |> 
  summarise(start = min(month_end), .by = version) |> 
  mutate(next_start = lead(start),
         interval = lubridate::interval(start, next_start) %/% months(1))

vers_summary <- clean_df |>
  filter(version != "DOS") |> 
  summarise(sales = sum(spend) / 1000000,
            .by = c(version, month_end))

vers_summary |> 
  ggplot(aes(month_end, sales, colour = version)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  scale_colour_manual(values = pal) +
  labs(x = NULL, y = NULL, title = "The Lifecycle of G-Cloud Versions", 
       subtitle = "Monthly Sales by Version") + 
  labs(caption = "\nSource: GOV.UK's Digital Marketplace")

fw_summary <- clean_df |>
  summarise(pct = sum(SME_spend, na.rm = TRUE) / sum(spend, na.rm = TRUE),
            .by = c(framework, month_end))

fw_summary |> 
  ggplot(aes(month_end, pct, colour = framework)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1), labels = label_percent()) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_colour_manual(values = pal[c(1, 9)]) +
  labs(x = NULL, y = NULL, 
       title = "The Waning SME Share of Sales", 
       subtitle = "% Monthly Sales Value via SME (vs Large Enterprise) Suppliers") + 
  labs(caption = "\nSource: GOV.UK's Digital Marketplace")

sect_summary <-
  clean_df |>
  filter(!sector %in% c(
    "Unregistered or Unknown",
    "Utility (Historic)",
    "Wider Public Sector"
  )) |>
  summarise(
    sales = sum(spend) / 1000000,
    pct = sum(SME_spend, na.rm = TRUE) / sum(spend, na.rm = TRUE),
    .by = c(sector, month_end)
  )

sect_summary |> 
  ggplot(aes(month_end, sales, colour = sector)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~ sector, scales = "free_y") +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  scale_colour_manual(values = pal) +
  labs(x = NULL, y = NULL, 
       title = "All Sectors Increase Digital Marketplace Spend", 
       subtitle = "G-Cloud & DOS Spend by Sector") + 
  labs(caption = "\nSource: GOV.UK's Digital Marketplace")

sect_summary |> 
  ggplot(aes(month_end, pct, colour = sector)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~ sector, scales = "free_y") +
  theme(legend.position = "none") +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent()) +
  scale_colour_manual(values = pal) +
  labs(x = NULL, y = NULL, 
       title = "Most Sectors Spend Proportionately Less on SMEs", 
       subtitle = "Pct SME G-Cloud & DOS Spend by Sector") + 
  labs(caption = "\nSource: GOV.UK's Digital Marketplace")

used_here()
