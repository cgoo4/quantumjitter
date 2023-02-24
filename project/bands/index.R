library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr")
conflict_prefer("as_date", "lubridate")
library(rvest)
library(scales)
library(SPARQL)
library(clock)
conflict_prefer("date_format", "clock")
library(RColorBrewer)
library(glue)
library(janitor)
library(infer)
library(tsibble)
library(ggfx)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

col <- "RdYlBu"

scale_fill_continuous <- \(...) scale_fill_distiller(palette = col)

cols <- brewer.pal(7, col)

tibble(x = 1, y = 1, fill = 7:1) |> 
  ggplot(aes(x, y, fill = fill)) +
  as_reference(geom_col(show.legend = FALSE), id = "cols") +
  with_blend(
    geom_text(
      x = 1,
      y = 3.5,
      label = col,
      size = 40,
      fontface = "bold"
    ),
    bg_layer = "cols",
    blend_type = "atop",
    flip_order = TRUE,
    id = "text"
  ) +
  with_outer_glow("text", colour = "white") +
  scale_fill_continuous() +
  coord_flip() +
  theme_void()

remove_pattern <-
  str_c(
    ", London, SW10 .+$", # <1>
    "FLAT ",
    "APARTMENT ",
    "CHELSEA HARBOUR",
    "(?<=COURT|SANDHILLS| HOUSE|WALK|ESTATE|ROW).*", # <2>
    "[,'\\.]",
    "(?<= )AT ",
    "(?<=VINT)N",
    "(?<=FARRIER)S",
    "(1ST|2ND|3RD|4TH|5TH|6TH) FLR ", # <3>
    "FLR (1ST|2ND|3RD|4TH|5TH|6TH) ", # <3>
    " ?- ?[0-9]{1,3}",
    sep = "|"
  )

swizzle_from <- "^([0-9]{1,3})([A-Z])(?= .*)" # <4>
swizzle_to <- "\\2 \\1" # <4>

url1 <-
  str_c(
    "https://www.tax.service.gov.uk/",
    "check-council-tax-band/",
    "search-council-tax-advanced?",
    "postcode=Fkvms5WVQum-uX3L00_pcA&",
    "filters.councilTaxBands="
  )

url2 <- "&filters.propertyUse=N&postcode=Fkvms5WVQum-uX3L00_pcA&page="

url3 <- "&filters.bandStatus=Current"

index <- crossing(band = LETTERS[1:8], page = seq(0, 120, 1))

band_df <- map2(index$band, index$page, possibly(\(i, j) {
  str_c(url1, i, url2, j, url3) |>
    read_html() |>
    html_element("#search-results-table") |>
    html_table(convert = FALSE)
}, otherwise = NA_character_)) |> 
  list_rbind()

# saveRDS(band_df, "band_df")
band_df <- readRDS("band_df")

band_df2 <- 
  band_df |> 
  clean_names() |> 
  mutate(postcode = str_extract(address, "SW10 .+$"),
         raw_band_address = str_remove(address, ", London, SW10 .+$"),
         address = str_remove_all(address, remove_pattern),
         address = str_replace(address, swizzle_from, swizzle_to),
         address = str_squish(address)
  )

endpoint <- "https://landregistry.data.gov.uk/landregistry/query"

query <- '
PREFIX  xsd:  <http://www.w3.org/2001/XMLSchema#>
PREFIX  text: <http://jena.apache.org/text#>
PREFIX  ppd:  <http://landregistry.data.gov.uk/def/ppi/>
PREFIX  lrcommon: <http://landregistry.data.gov.uk/def/common/>

SELECT  ?ppd_propertyAddress ?ppd_transactionCategory ?ppd_transactionDate ?ppd_pricePaid ?ppd_estateType ?ppd_propertyAddressCounty ?ppd_propertyAddressDistrict ?ppd_propertyAddressLocality ?ppd_propertyAddressPaon ?ppd_propertyAddressPostcode ?ppd_propertyAddressSaon ?ppd_propertyAddressStreet ?ppd_propertyAddressTown ?ppd_propertyType ?ppd_recordStatus

WHERE
  { { ?ppd_propertyAddress
                text:query               ( lrcommon:postcode "( SW10 )" 3000000 ) .
      ?item     ppd:propertyAddress      ?ppd_propertyAddress ;
                ppd:transactionCategory  ppd:standardPricePaidTransaction ;
                ppd:transactionDate      ?ppd_transactionDate ;
                ppd:pricePaid            ?ppd_pricePaid ;
      FILTER ( ?ppd_transactionDate >= "2020-01-01"^^xsd:date )
    }
    OPTIONAL{ ?item  ppd:estateType  ?ppd_estateType }
    OPTIONAL{ ?ppd_propertyAddress lrcommon:county  ?ppd_propertyAddressCounty}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:district  ?ppd_propertyAddressDistrict}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:locality  ?ppd_propertyAddressLocality}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:paon  ?ppd_propertyAddressPaon}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:postcode  ?ppd_propertyAddressPostcode}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:saon  ?ppd_propertyAddressSaon}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:street  ?ppd_propertyAddressStreet}
    OPTIONAL{ ?ppd_propertyAddress lrcommon:town  ?ppd_propertyAddressTown}
    OPTIONAL{ ?item  ppd:propertyType  ?ppd_propertyType }
    OPTIONAL{ ?item  ppd:recordStatus  ?ppd_recordStatus }
    BIND(ppd:standardPricePaidTransaction AS ?ppd_transactionCategory)
  }'

prices_list <- SPARQL(endpoint, query)

prices_df2 <-
  prices_list$results |>
  as_tibble() |> 
  clean_names() |>
  rename_with(~ str_remove_all(., "ppd_|property_address_")) |>
  mutate(
    transaction_date = as_datetime(transaction_date) |> as_date(),
    price_paid = price_paid / 1000000
  ) |>
  filter(transaction_date < "2022-01-01") |>
  mutate(
    raw_price_address = str_c(str_replace_na(saon, ""), 
                              paon, street, sep = " ") |> str_squish(),
    address = str_remove_all(raw_price_address, remove_pattern),
    address = str_replace(address, swizzle_from, swizzle_to)
  ) |>
  select(
    address,
    raw_price_address,
    postcode,
    price_paid,
    transaction_date,
    estate_type,
    property_type,
    transaction_category
  )

joined_df <-
  prices_df2 |>
  inner_join(band_df2, by = join_by(address, postcode)) |>
  relocate(raw_band_address, .after = raw_price_address) |>
  arrange(postcode, address) |>
  mutate(council_tax_band = factor(council_tax_band))

set.seed(2022)

joined_df |>
  select(`common address` = address,
         `band address` = raw_band_address,
         `price address` = raw_price_address,
         postcode) |>
  slice_sample(n = 6)

joined_df |>
  select(transaction_date, price_paid, council_tax_band) |>
  mutate(yearquarter = yearquarter(transaction_date)) |>
  count(yearquarter, council_tax_band) |>
  ggplot(aes(yearquarter, n, fill = council_tax_band)) +
  geom_col(position = position_fill()) +
  scale_x_yearquarter() +
  scale_y_continuous(labels = label_percent(1)) +
  scale_fill_manual(values = cols[c(1:7)]) +
  labs(
    title = "Distribution of Sales Transactions by Band & Quarter",
    x = "Quarter", y = "Proportion", fill = "Band"
  )

labels <- joined_df |>
  summarise(n = n(), mean_price = mean(price_paid),
            .by = council_tax_band)

transactions <-
  joined_df |>
  count() |>
  pull()

from <- joined_df |>
  summarise(min(transaction_date) |> yearquarter()) |>
  pull()

to <- joined_df |>
  summarise(max(transaction_date) |> yearquarter()) |>
  pull()

joined_df |>
  ggplot(aes(council_tax_band, price_paid)) +
  geom_violin(fill = cols[1]) +
  geom_label(aes(label = glue(
    "n = {n} \nAvg Price ",
    "{dollar(mean_price, prefix = '£', suffix = 'm', accuracy = 0.01)}"
  ), y = 16),
  data = labels, size = 2.3, alpha = 0.7, fill = "white"
  ) +
  scale_y_log10(labels = label_dollar(
    prefix = "£",
    suffix = "m", accuracy = 0.1
  )) +
  labs(
    title = "Droopy Bandings",
    subtitle = glue(
      "Sample of {transactions} Property ",
      "Transactions in SW10 ({from} to {to})"
    ),
    x = "Council Tax Band", y = "Sale Price (log10 scale)",
    caption = "Sources: tax.service.gov.uk & landregistry.data.gov.uk"
  )

joined_df2 <- joined_df |>
  mutate(`SW10 0JR` = if_else(postcode == "SW10 0JR", "Yes", "No"))

joined_df2 |>
  ggplot(aes(council_tax_band, price_paid, fill = `SW10 0JR`)) +
  geom_violin() +
  geom_label(aes(label = glue(
    "n = {n} \nAvg Price\n",
    "{dollar(mean_price, prefix = '£', suffix = 'm', accuracy = 0.01)}"
  ), y = 16),
  data = labels, size = 2.3, alpha = 0.7, fill = "white"
  ) +
  geom_hline(yintercept = 0.3, linetype = "dashed") +
  scale_y_log10(labels = label_dollar(
    prefix = "£",
    suffix = "m", accuracy = 0.1
  )) +
  scale_fill_manual(values = cols[c(1, 5)]) +
  labs(
    title = "Unusual Bandings",
    subtitle = glue(
      "Sample of {transactions} Property ",
      "Transactions in SW10 ({from} to {to})"
    ),
    x = "Council Tax Band", y = "Sale Price (log10 scale)",
    caption = "Sources: tax.service.gov.uk & landregistry.data.gov.uk"
  )

joined_df2 |>
  count(postcode, sort = TRUE) |>
  slice_head(n = 10)

joined_df3 <- joined_df |> 
  filter(postcode != "SW10 0JR")

labels <- joined_df3 |>
  summarise(n = n(), mean_price = mean(price_paid),
            .by = council_tax_band)

transactions <-
  joined_df3 |>
  count() |>
  pull()

joined_df3 |>
  ggplot(aes(council_tax_band, price_paid)) +
  geom_violin(fill = cols[1]) +
  geom_label(aes(label = glue(
    "n = {n} \nAvg Price ",
    "{dollar(mean_price, prefix = '£', suffix = 'm', accuracy = 0.01)}"
  ), y = 16),
  data = labels, size = 2.3, alpha = 0.7, fill = "white"
  ) +
  scale_y_log10(labels = label_dollar(prefix = "£", 
                                       suffix = "m", accuracy = 0.1)) +
  labs(
    title = "Drippy Bandings",
    subtitle = glue(
      "Sample of {transactions} Property ",
      "Transactions in SW10 ({from} to {to})"
    ),
    x = "Council Tax Band", y = "Sale Price (log10 scale)",
    caption = "Sources: tax.service.gov.uk & landregistry.data.gov.uk"
  )

joined_df3 |>
  filter(council_tax_band == "E", price_paid >= 0.6) |>
  select(
    address = raw_price_address,
    postcode,
    transaction_date,
    price_paid,
    tax_band = council_tax_band,
    estate_type,
    property_type
  ) |>
  mutate(across(ends_with("type"), \(x) str_remove_all(x, "^.*mon/|>$"))) |> # <1>
  select(-address)

bands_ef <- 
  joined_df3 |>
  filter(council_tax_band %in% c("E", "D"))

obs_stat <- 
  bands_ef |> 
  specify(price_paid ~ council_tax_band) |>
  calculate(stat = "diff in means", order = c("E", "D")) |>
  pull()

set.seed(2)

boot_dist <-
  bands_ef |> 
  specify(price_paid ~ council_tax_band) |>
  generate(reps = 2000, type = "bootstrap") |>
  calculate(stat = "diff in means", order = c("E", "D"))

perc_ci <- get_ci(boot_dist)

lower <- perc_ci |>
  pull(lower_ci) |>
  dollar(prefix = "£", suffix = "m", accuracy = 0.01)
upper <- perc_ci |>
  pull(upper_ci) |>
  dollar(prefix = "£", suffix = "m", accuracy = 0.01)

boot_dist |>
  visualise() +
  shade_confidence_interval(
    endpoints = perc_ci,
    color = cols[6], fill = cols[3]
  ) +
  geom_vline(xintercept = obs_stat, linetype = "dashed", colour = "white") +
  annotate("label",
    x = -0.12, y = 350, size = 3,
    label = glue(
      "Observed Difference\nBetween Bands D & E is ",
      "{dollar(obs_stat, prefix = '£', suffix = 'm', accuracy = 0.01)}"
    )
  ) +
  scale_x_continuous(labels = label_dollar(
    prefix = "£",
    suffix = "m", accuracy = 0.1
  )) +
  labs(
    subtitle = glue(
      "95% Confident the Difference ",
      "in Mean Prices Between Bands D & E is {lower} to {upper}"
    ),
    x = "Difference in Means", y = "Count",
    caption = "Sources: tax.service.gov.uk & landregistry.data.gov.uk"
  )

used_here()
