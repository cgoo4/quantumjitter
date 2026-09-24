library(conflicted)
library(tidyverse)
library(fpp3)
library(scales)
library(clock)
library(ggfoundry)
library(paletteer)
library(usedthese)

conflict_prefer_all("dplyr", quiet = TRUE)

conflict_scout()

theme_set(theme_bw())

pal_name <- "wesanderson::IsleofDogs2"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

forecast_plot <- function(mod_ts, hist_ts, facet_var, title) {
  fcast_ts <- mod_ts |>
    forecast(h = "2 years") |>
    mutate(`95%` = hilo(spend, 95), `80%` = hilo(spend, 80)) |>
    unpack_hilo(c("95%", "80%")) |>
    rename(fc_spend = spend) |>
    bind_rows(hist_ts)

  ggplot(fcast_ts, aes(date, fill = {{ facet_var }})) +
    geom_line(aes(y = spend), colour = pal[5], na.rm = TRUE) +
    geom_ribbon(
      aes(ymin = `95%_lower`, ymax = `95%_upper`),
      fill = pal[1],
      colour = NA,
      na.rm = TRUE
    ) +
    geom_ribbon(
      aes(ymin = `80%_lower`, ymax = `80%_upper`),
      fill = pal[2],
      colour = NA,
      na.rm = TRUE
    ) +
    geom_line(aes(y = .mean), colour = "white", na.rm = TRUE) +
    scale_y_continuous(labels = label_currency(prefix = "£", suffix = "m")) +
    facet_wrap(vars({{ facet_var }})) +
    labs(
      title = title,
      x = NULL,
      y = "Spend",
      subtitle = "80 & 95% Prediction Intervals"
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

url <-
  "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/"

gcloud_csv <- str_c(url, "703943/G-Cloud_spend_data_to_end_March_2018.csv")

dos_csv <- str_c(url, "703952/DOS_spend_data_to_end_March_2018.csv")

csv_paths <- c(gcloud_csv, dos_csv)

# walk() avoids printing the list element numbers
walk(csv_paths, \(x) print(guess_encoding(x)))

colnam <-
  c(
    "sector",
    "lot",
    "date",
    "spend",
    "status",
    "supplier",
    "customer",
    "framework"
  )

read_dm <- \(x) {
  read_csv(
    x,
    col_names = colnam,
    skip = 1,
    locale = locale(encoding = "ISO-8859-1"),
    show_col_types = FALSE
  )
}

raw <- map(csv_paths, read_dm) |>
  set_names(c("gcloud", "dos")) |>
  bind_rows() |>
  mutate(framework = if_else(is.na(framework), "DOS", framework))

both <- raw |>
  mutate(
    month_end = date_parse(str_c(date, "01", sep = "-"), format = "%b-%y-%d") |>
      add_months(1) |>
      add_days(-1),
    date = yearmonth(month_end),
    framework = str_extract(framework, "G-Cloud|DOS"),
    spend = str_remove(spend, coll("£")),
    spend = str_replace(spend, "^\\(", "-"),
    spend = parse_number(spend) / 1000000,
    lot = replace_values(
      lot,
      "Software as a Service (SaaS)" ~ "Cloud Software",
      c(
        "Infrastructure as a Service (IaaS)",
        "Platform as a Service (PaaS)"
      ) ~ "Cloud Hosting",
      "Specialist Cloud Services" ~ "Cloud Support"
    )
  )

both_ts <- both |>
  summarise(spend = sum(spend), .by = c(date, framework)) |>
  as_tsibble(key = framework, index = date)

ggplot(both_ts, aes(date, spend, colour = framework)) +
  geom_line(key_glyph = "timeseries") +
  scale_y_continuous(labels = label_currency(prefix = "£", suffix = "m")) +
  scale_colour_manual(values = pal[c(3, 4)]) +
  labs(x = NULL, y = NULL, title = "Monthly Digital Marketplace Sales")

both_ts |>
  model(stl = STL(spend ~ trend(window = 7) + season(window = "periodic"))) |>
  components() |>
  autoplot() +
  scale_colour_manual(values = pal[c(3, 4)]) +
  labs(x = NULL, title = "Time Series Decomposition")

mod_ts <- both_ts |>
  filter(framework == "G-Cloud") |>
  model(ARIMA = ARIMA(spend, stepwise = TRUE, approximation = FALSE)) |>
  bind_rows(
    both_ts |>
      filter(framework == "DOS") |>
      model(ARIMA = ARIMA(spend ~ 1 + pdq(0, 1, 1) + PDQ(0, 0, 0)))
  )

mod_ts |>
  glance() |>
  select(-ar_roots, -ma_roots)

tidy(mod_ts)

forecast_plot(
  mod_ts,
  both_ts,
  framework,
  "Digital Marketplace Sales Forecast by Framework"
)

gcloud_ts <- both |>
  filter(framework == "G-Cloud") |>
  summarise(spend = sum(spend), .by = c(date, lot)) |>
  as_tsibble(key = lot, index = date)

gcloud_mod <- gcloud_ts |>
  model(ARIMA = ARIMA(spend, stepwise = TRUE, approximation = FALSE))

gcloud_mod |>
  glance() |>
  select(-ar_roots, -ma_roots)

tidy(gcloud_mod)

forecast_plot(gcloud_mod, gcloud_ts, lot, "G-Cloud Sales Forecast by Lot")

used_here()
