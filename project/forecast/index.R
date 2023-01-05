library(tidyverse)
library(wesanderson)
library(fpp3)
library(scales)
library(clock)
library(usedthese)

theme_set(theme_bw())

(cols <- wes_palette(name = "IsleofDogs2"))

url <-
  "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/"

gcloud_csv <- str_c(url, "703943/G-Cloud_spend_data_to_end_March_2018.csv")

dos_csv <- str_c(url, "703952/DOS_spend_data_to_end_March_2018.csv")

names <- c(gcloud_csv, dos_csv)

# Use walk to suppress the printing of list element numbers

walk(names, \(x) {
  p <- guess_encoding(x)
  print(p)
})

colnam <- 
  c("sector",
    "lot",
    "date",
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
    show_col_types = FALSE)
}

raw <- map(names, read_dm) |> 
  set_names(c("gcloud", "dos")) |> 
  bind_rows() |> 
  mutate(framework = if_else(is.na(framework), "DOS", framework))

both <- raw |>
  mutate(
    month_end = date_parse(str_c(date, "01", sep = "-"), 
                           format = "%b-%y-%d") |> 
      add_months(1) |> add_days(-1),
    date = yearmonth(month_end),
    framework = str_extract(framework, ".{3,7}"),
    spend = str_remove(spend, coll("£")),
    spend = str_replace(spend, "^\\(", "-"),
    spend = parse_number(spend) / 1000000,
    lot = recode(
      lot,
      "Software as a Service (SaaS)" = "Cloud Software",
      "Infrastructure as a Service (IaaS)" = "Cloud Hosting",
      "Platform as a Service (PaaS)" = "Cloud Hosting",
      "Specialist Cloud Services" = "Cloud Support"
      )
)

both_ts <- both |>
  summarise(spend = sum(spend), .by = c(date, framework)) |> 
  as_tsibble(key = framework, index = date)

both_ts |> 
  ggplot(aes(date, spend, colour = framework)) +
  geom_line(key_glyph = "timeseries") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  scale_colour_manual(values = cols[c(3, 4)]) +
  labs(x = NULL, y = NULL, title = "Monthly Digital Marketplace Sales")

both_ts |>
  model(stl = STL(spend ~ trend(window = 7) + season(window = "periodic"))) |>
  components() |>
  autoplot() +
  scale_colour_manual(values = cols[c(3, 4)]) +
  labs(x = NULL, title = "Time Series Decomposition")

mod_ts <- both_ts |>
  model(ARIMA = ARIMA(spend, stepwise = TRUE, approximation = FALSE))

mod_ts |> 
  glance() |>
  select(-ar_roots, -ma_roots)

mod_ts |> 
  tidy()

fcast_ts <- mod_ts |>
  forecast(h = "2 years") |> 
  mutate(`95%` = hilo(spend, 95), `80%` = hilo(spend, 80)) |> 
  unpack_hilo(c("95%", "80%")) |>
  rename(fc_spend = spend) |> 
  bind_rows(both_ts)

fcast_ts |>
  ggplot(aes(date, fill = framework)) +
  geom_line(aes(y = spend), colour = cols[5]) +
  geom_ribbon(aes(ymin = `95%_lower`, ymax = `95%_upper`),
    fill = cols[1], colour = NA
  ) +
  geom_ribbon(aes(ymin = `80%_lower`, ymax = `80%_upper`),
    fill = cols[2], colour = NA
  ) +
  geom_line(aes(y = .mean), colour = "white") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  facet_wrap(~framework) +
  labs(
    title = "Digital Marketplace Sales Forecast by Framework",
    x = NULL, y = "Spend",
    subtitle = "80 & 95% Prediction Intervals"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

gcloud_ts <- both |>
  filter(framework == "G-Cloud") |> 
  summarise(spend = sum(spend), .by = c(date, lot)) |> 
  as_tsibble(key = lot, index = date)

gc_ts <- gcloud_ts |>
  model(ARIMA = ARIMA(spend, stepwise = TRUE, approximation = FALSE))

gc_ts |> 
  glance() |>
  select(-ar_roots, -ma_roots)

gc_ts |> tidy()

fcgc_ts <- gc_ts |>
  forecast(h = "2 years") |> 
  mutate(`95%` = hilo(spend, 95), `80%` = hilo(spend, 80)) |> 
  unpack_hilo(c("95%", "80%")) |> 
  rename(fc_spend = spend) |> 
  bind_rows(gcloud_ts)

fcgc_ts |>
  ggplot(aes(date, fill = lot)) +
  geom_line(aes(y = spend), colour = cols[5]) +
  geom_ribbon(aes(ymin = `95%_lower`, ymax = `95%_upper`),
    fill = cols[1], colour = NA
  ) +
  geom_ribbon(aes(ymin = `80%_lower`, ymax = `80%_upper`),
    fill = cols[2], colour = NA
  ) +
  geom_line(aes(y = .mean), colour = "white") +
  scale_y_continuous(labels = label_dollar(prefix = "£", suffix = "m")) +
  facet_wrap(~lot) +
  labs(
    title = "G-Cloud Sales Forecast by Lot",
    x = NULL, y = "Spend",
    subtitle = "80 & 95% Prediction Intervals"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

used_here()
