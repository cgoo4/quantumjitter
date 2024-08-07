library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
conflicts_prefer(lubridate::as_date)
library(clock)
conflicts_prefer(clock::date_format)
library(janitor)
library(scales)
library(glue)
library(ggfoundry)
library(paletteer)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

pal_name <- "wesanderson::Royal1"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

url <- str_c(
  "https://www.gov.uk/government/",
  "uploads/system/uploads/attachment_data/",
  "file/639799/g-cloud-sales-figures-july2017.csv"
)

gcloud_df <-
  read_csv(url) |> 
  clean_names() |> 
  mutate(
    evidenced_spend = str_remove_all(evidenced_spend, "[^0-9-]") |>
      parse_number(),
    spend_date = as.Date(as.numeric(return_month), origin = "1899-12-30"),
    spend_date = if_else(
      is.na(spend_date),
      dmy(return_month),
      spend_date
    ),
    sme_status = if_else(sme_status == "SME", "SME", "Non-SME"),
    sme_spend = if_else(sme_status == "SME", evidenced_spend, 0)
  )

share_df <- gcloud_df |> 
  summarise(
    evidenced_spend = sum(evidenced_spend, na.rm = TRUE),
    sme_spend = sum(sme_spend, na.rm = TRUE),
    pct = sme_spend / evidenced_spend, 
    .by = spend_date
  ) |> 
  arrange(spend_date)

last_date <- gcloud_df |> 
  summarise(max(spend_date)) |> 
  pull() |> 
  date_format(format = "%B %d, %Y")

share_df |> 
  ggplot(aes(spend_date, pct)) +
  geom_point(colour = pal[4]) +
  geom_smooth(colour = pal[2], fill = pal[3]) +
  scale_y_continuous(labels = label_percent()) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  labs(
    x = NULL, y = NULL,
    title = glue("SME Share of G-Cloud to {last_date}"), 
    subtitle = "Dots = % Monthly Sales via SMEs",
    caption = "Source: GOV.UK G-Cloud Sales"
  )

sector_df <- gcloud_df |>
  mutate(sector = if_else(
    sector %in% c("Central Government", "Local Government", 
                  "Police", "Health"), sector, "Other Sector")
    ) |>
  summarise(
    evidenced_spend = sum(evidenced_spend, na.rm = TRUE),
    sme_spend = sum(sme_spend, na.rm = TRUE),
    pct = sme_spend / evidenced_spend,
    .by = c(customer_name, sector)
  ) |>
  filter(evidenced_spend >= 100000) |>
  mutate(median_pct = median(pct), .by = sector) |>
  mutate(sector = fct_reorder(sector, median_pct))

n_df <- sector_df |> summarise(n = n(), .by = sector)

sector_df |> 
  ggplot(aes(sector, pct)) +
  geom_boxplot(outlier.shape = FALSE, fill = pal[3]) +
  geom_jitter(width = 0.2, alpha = 0.5, colour = pal[2]) +
  geom_label(aes(y = .75, label = glue("n = {n}")),
    data = n_df,
    fill = pal[1], colour = "white"
  ) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = NULL, y = NULL,
    title = glue("SME Share of G-Cloud to {last_date}"),
    subtitle = "% Sales via SMEs for Buyers with Cumulative Sales >= £100k",
    caption = "Source: gov.uk G-Cloud Sales"
  )

used_here()
