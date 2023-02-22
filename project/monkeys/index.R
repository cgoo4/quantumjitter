library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
conflicts_prefer(purrr::map)
library(scales)
library(glue)
library(SPARQL)
library(lubridate)
library(wesanderson)
library(vctrs)
library(tsibble)
library(rvest)
library(Quandl)
library(corrr)
library(quantreg)
library(Qtools)
library(ggfx)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

(cols <- wes_palette(name = "Royal1"))

endpoint <- "https://landregistry.data.gov.uk/landregistry/query"

query <- 'PREFIX  text: <http://jena.apache.org/text#>
PREFIX  ppd:  <http://landregistry.data.gov.uk/def/ppi/>
PREFIX  lrcommon: <http://landregistry.data.gov.uk/def/common/>
  
SELECT  ?item ?ppd_propertyAddress ?ppd_hasTransaction ?ppd_pricePaid ?ppd_transactionCategory ?ppd_transactionDate ?ppd_transactionId ?ppd_estateType ?ppd_newBuild ?ppd_propertyAddressCounty ?ppd_propertyAddressDistrict ?ppd_propertyAddressLocality ?ppd_propertyAddressPaon ?ppd_propertyAddressPostcode ?ppd_propertyAddressSaon ?ppd_propertyAddressStreet ?ppd_propertyAddressTown ?ppd_propertyType ?ppd_recordStatus

WHERE
{ ?ppd_propertyAddress text:query _:b0 .
  _:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> lrcommon:postcode .
  _:b0 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:b1 .
  _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> "( SW10 )" .
  _:b1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:b2 .
  _:b2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> 3000000 .
  _:b2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
  ?item ppd:propertyAddress ?ppd_propertyAddress .
  ?item ppd:hasTransaction ?ppd_hasTransaction .
  ?item ppd:pricePaid ?ppd_pricePaid .
  ?item ppd:transactionCategory ?ppd_transactionCategory .
  ?item ppd:transactionDate ?ppd_transactionDate .
  ?item ppd:transactionId ?ppd_transactionId
  
  OPTIONAL { ?item ppd:estateType ?ppd_estateType }
  OPTIONAL { ?item ppd:newBuild ?ppd_newBuild }
  OPTIONAL { ?ppd_propertyAddress lrcommon:county ?ppd_propertyAddressCounty }
  OPTIONAL { ?ppd_propertyAddress lrcommon:district ?ppd_propertyAddressDistrict }
  OPTIONAL { ?ppd_propertyAddress lrcommon:locality ?ppd_propertyAddressLocality }
  OPTIONAL { ?ppd_propertyAddress lrcommon:paon ?ppd_propertyAddressPaon }
  OPTIONAL { ?ppd_propertyAddress lrcommon:postcode ?ppd_propertyAddressPostcode }
  OPTIONAL { ?ppd_propertyAddress lrcommon:saon ?ppd_propertyAddressSaon }
  OPTIONAL { ?ppd_propertyAddress lrcommon:street ?ppd_propertyAddressStreet }
  OPTIONAL { ?ppd_propertyAddress lrcommon:town ?ppd_propertyAddressTown }
  OPTIONAL { ?item ppd:propertyType ?ppd_propertyType }
  OPTIONAL { ?item ppd:recordStatus ?ppd_recordStatus }
}'

data_lst <- SPARQL(endpoint, query)

data_df <- data_lst |>
  pluck("results") |>
  as_tibble() |>
  mutate(
    date = new_datetime(ppd_transactionDate) |> as_date(),
    amount = ppd_pricePaid,
    cat = str_remove(ppd_transactionCategory, 
                     "<http://landregistry.data.gov.uk/def/ppi/"),
  ) |>
  filter(str_detect(cat, "standard")) |>
  arrange(date) |>
  mutate(yr_mon = yearmonth(date)) |>
  count(yr_mon)

next_month <- data_df |>
  summarise(last(yr_mon) + 1) |>
  pull()

# Add two months for predictions
data_df2 <- data_df |>
  rows_insert(tibble(yr_mon = next_month), by = "yr_mon") |>
  rows_insert(tibble(yr_mon = next_month + 1), by = "yr_mon")

# Price of a selection of goods & services for a typical consumer
cpi_df <- Quandl("RATEINF/CPI_GBR") |>
  select(date = Date, cpi_macro = Value) |>
  arrange(date) |>
  mutate(yr_mon = yearmonth(date)) |>
  select(-date)

# YOY rate of change in CPI
inflation_df <- Quandl("RATEINF/INFLATION_GBR") |>
  select(date = Date, inflation_macro = Value) |>
  arrange(date) |>
  mutate(yr_mon = yearmonth(date)) |>
  select(-date)

# BOE base rate
interest_df <-
  read_html("https://www.bankofengland.co.uk/boeapps/database/Bank-Rate.asp") |>
  html_elements("#stats-table") |>
  html_table() |>
  pluck(1) |>
  mutate(date = dmy(`Date Changed`)) |>
  select(date, interest_macro = Rate) |>
  arrange(date) |>
  mutate(yr_mon = yearmonth(date)) |>
  select(-date) |>
  slice_tail(n = 1, by = yr_mon)

# Effective exchange rate of sterling versus multiple other currencies
sterling_df <- Quandl("BOE/XUDLBK67") |>
  select(date = Date, sterling_macro = Value) |>
  arrange(date) |>
  mutate(yr_mon = yearmonth(date)) |>
  slice_tail(n = 1, by = yr_mon) |>
  select(-date)

macro_list <-
  list(
    inflation_df,
    cpi_df,
    interest_df,
    sterling_df
  )

macro_df <- reduce(macro_list, left_join, join_by(yr_mon)) |>
  arrange(yr_mon) |>
  fill(ends_with("macro"), .direction = "down") |>
  drop_na()

macro_df |>
  pivot_longer(-yr_mon) |>
  mutate(name = str_remove(name, "_macro")) |>
  ggplot(aes(yr_mon, value)) +
  geom_line(colour = "grey70") +
  geom_smooth() +
  facet_wrap(~name, scales = "free_y", nrow = 1) +
  labs(title = "Macroeconomic Factors", x = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

stamp_df <- read_html("https://www.investmentguide.co.uk/historical-stamp-duty/") |>
  html_elements("strong , .column-1, .column-2") |>
  html_text() |>
  as_tibble() |>
  filter(!value %in% c("Rate", "Charge band")) |>
  mutate(type = case_when(
    str_detect(value, "%") ~ "rate",
    str_detect(value, "£") ~ "band",
    .default = "date"
  )) |>
  mutate(yr_mon = if_else(type == "date", yearmonth(dmy(value)), NA)) |>
  fill(yr_mon) |>
  filter(type != "date") |>
  mutate(row = row_number(), .by = c(yr_mon, type)) |>
  pivot_wider(names_from = type, values_from = value) |>
  separate_wider_delim(band, " and under ",
    names = c("from", "to"), too_few = "align_start"
  ) |>
  mutate(
    to = if_else(str_starts(from, "Up to"), parse_number(from), parse_number(to)),
    to = replace_na(to, Inf)
  ) |>
  select(-from)

stamp_df2 <- stamp_df |>
  filter(to == Inf) |>
  select(yr_mon, stamp_macro = rate) |>
  mutate(stamp_macro = parse_number(stamp_macro))

# saveRDS(stamp_df, "stamp_df")
  

join_df <- data_df2 |>
  left_join(macro_df, join_by(yr_mon == yr_mon)) |>
  left_join(stamp_df2, join_by(closest(yr_mon >= yr_mon))) |>
  select(-starts_with("date"), -yr_mon.y) |>
  mutate(across(ends_with("_macro"), lag, 2)) |>
  drop_na(-n) |>
  rename(yr_mon = yr_mon.x) |>
  mutate(month = month(yr_mon))

join_df |> 
  mutate(month = month(yr_mon, label = TRUE)) |> 
  ggplot(aes(month, n, group = month)) +
  geom_boxplot() +
  labs(title = "Sales by Month")

join_df |>
  select(-month) |>
  correlate() |>
  focus(n) |>
  arrange(n)

join_df |>
  pivot_longer(cols = ends_with("_macro"), names_pattern = "(.*)_macro") |>
  ggplot(aes(value, n)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name, scales = "free_x", nrow = 1)

set.seed(123)

rq_fit <- rq(
  n ~ cpi_macro + sterling_macro + interest_macro +
    stamp_macro + inflation_macro + month + yr_mon,
  data = join_df,
  tau = c(0.05, 0.5, 0.95)
)

broom::tidy(rq_fit)

GOFTest(rq_fit)

lm_fit <- rq(
  n ~ cpi_macro + sterling_macro + interest_macro +
    stamp_macro + inflation_macro + month + yr_mon,
  data = join_df
)

rq_preds <- rq_fit |>
  predict(join_df,
    type = "quantiles",
    quantiles = c(0.05, 0.5, 0.95)
  ) |>
  as_tibble() |>
  rename(
    lower = `tau= 0.05`,
    median = `tau= 0.50`,
    upper = `tau= 0.95`
  ) |>
  bind_cols(join_df) |>
  mutate(coverage = if_else(between(n, lower, upper), TRUE, FALSE))

lm_preds <- lm_fit |>
  predict(join_df) |>
  as_tibble() |>
  bind_cols(join_df) |>
  select(yr_mon, lm = value)

coverage <- rq_preds |> 
  summarise(coverage = percent(mean(coverage, na.rm = TRUE), 0.1)) |> 
  pull()

rq_preds |>
  left_join(lm_preds, join_by(yr_mon == yr_mon)) |>
  ggplot(aes(yr_mon, median)) +
  as_reference(geom_ribbon(aes(ymin = lower, ymax = upper), 
                           fill = cols[1]), id = "ribbon") +
  with_blend(
    annotate(
      "rect",
      xmin = as_date("2022-12-31"), xmax = as_date("2023-02-28"),
      ymin = -Inf, ymax = Inf, fill = cols[2], linetype = "dashed"
    ),
    bg_layer = "ribbon", blend_type = "atop"
  ) +
  geom_line(aes(y = n), colour = "black") +
  geom_line(colour = "white", linewidth = 1) +
  geom_line(aes(y = lm), colour = cols[4], linetype = "dashed") +
  geom_vline(xintercept = as_date("2008-09-06"), 
             linetype = "dashed", colour = "grey30") +
  annotate("label",
    x = yearmonth("2008 Sep"), y = 100,
    label = "Lehman\nBrothers\nCollapses", size = 3
  ) +
  geom_vline(xintercept = as_date("2014-12-03"), 
             linetype = "dashed", colour = "grey30") +
  annotate("label",
    x = yearmonth("2014 Dec"), y = 100,
    label = "Jump in\nTop-rate\nStamp\nDuty", size = 3
  ) +
  geom_vline(xintercept = as_date("2016-06-23"), 
             linetype = "dashed", colour = "grey30") +
  annotate("label",
    x = yearmonth("2016 Jun"), y = 65,
    label = "Brexit\nVote", size = 3
  ) +
  annotate("label",
    x = yearmonth("2020 Jun"), y = 125,
    label = glue(
      "Actual (Black)\nLinear (Dashed Orange)\n",
      "Quantiles (Grey / White)\nPredicted (Red / White)"
    ),
    size = 3
  ) +
  annotate("label",
    x = yearmonth("1999 Jan"), y = 125,
    label = glue("{coverage} Coverage"), 
    size = 3, fill = cols[1], colour = "white"
  ) +
  scale_x_yearmonth(date_breaks = "2 years") +
  labs(
    title = "Monthly House Sales in London Postcode Area SW10",
    subtitle = "Quantile (0.05, 0.5, 0.95) & Linear Regression",
    x = NULL, y = "Number of Sales", fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

used_here()
