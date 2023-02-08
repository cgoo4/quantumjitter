library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(scales)
library(SPARQL)
library(clock)
conflict_prefer("date_format", "clock")
library(wesanderson)
library(glue)
library(vctrs)
library(tsibble)
library(patchwork)
library(ggmosaic)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

(cols <- wes_palette(name = "Darjeeling1"))

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

data_df <- data_lst$results |>
  as_tibble() |>
  mutate(
    trans_date = new_datetime(ppd_transactionDate) |> as_date(),
    amount = ppd_pricePaid,
    prop_type = str_extract(ppd_propertyType, "(?<=common/)[\\w]+"),
    est_type = str_extract(ppd_estateType, "(?<=common/)[\\w]+"),
    cat = str_remove(ppd_transactionCategory, "<http://landregistry.data.gov.uk/def/ppi/"),
    prop_type = recode(prop_type, otherPropertyType = "Other")
  ) |>
  filter(str_detect(cat, "standard"))

events <- tribble(
  ~event_date, ~change,
  "96-07-31", "Stamp Duty £250k (1.5%) £500k (2%)",
  "98-03-31", "£250k (2%) £500k (3%)",
  "99-03-31", "£250k (2.5%) £500k (3.5%)",
  "00-03-31", "£250k (3%) £500k (4%)",
  "11-04-30", "£250k (3%) £500k (4%) £1m (5%)",
  "12-03-31", "£250k (3%) £500k (4%) £1m (5%) £2m (7%)",
  "14-12-31", "£250k (5%) £925k (10%) 1.5m (12%)",
  "07-08-09", "Financial Crisis",
  "16-06-23", "Brexit Vote",
  "20-03-23", "Covid-19 Lockdown"
) |>
  mutate(event_date = date_parse(event_date, format = "%y-%m-%d"))

events |> 
  rename("Date" = event_date, "Event" = change)

to_date <- data_df |> 
  summarise(max(trans_date)) |> 
  pull() |> 
  date_format(format = "%b %d, %Y")

data_df |>
  ggplot(aes(trans_date, amount, colour = est_type)) +
  geom_point(alpha = 0.2, size = 0.7, show.legend = FALSE) +
  geom_smooth(se = FALSE, aes(linetype = est_type), size = 1.2) +
  labs(
    title = "SW10 Standard House Prices",
    subtitle = glue("Prices Paid to {to_date} (Prices > £5m Not Shown)"
    ),
    x = NULL,
    y = NULL,
    colour = "Type", linetype = "Type",
    caption = "Source: HM Land Registry"
  ) +
  geom_vline(xintercept = events$event_date, 
             size = 0.5, lty = 2, alpha = 0.4) +
  annotate("text", events$event_date, 5000000,
    angle = 90,
    label = events$change, vjust = 1.4, hjust = 1, size = 3, fontface = 2
  ) +
  coord_cartesian(ylim = c(0, 5000000)) +
  scale_colour_manual(values = cols[c(2, 3)]) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_dollar(accuracy = 0.1, prefix = "£", 
                                           scale_cut = cut_short_scale()))

qtr_start <- date_today("Europe/London") |> 
  as_year_quarter_day() |> 
  calendar_start("quarter") |> 
  as.Date()

qtile_df <- 
  data_df |> 
  filter(trans_date < qtr_start) |> 
  mutate(yr_qtr = yearquarter(trans_date)) |> 
  reframe(price = quantile(amount, c(0.25, 0.5, 0.75)), 
            quantile = c("lower", "median", "upper") |> factor(),
            n = n(),
            .by = yr_qtr) |> 
  pivot_wider(names_from = quantile, values_from = price)

last <- qtile_df |> summarise(max(yr_qtr)) |> pull()
first <- qtile_df |> summarise(min(yr_qtr)) |> pull()

p1 <- qtile_df |> 
  ggplot(aes(yr_qtr, median)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = cols[5]) +
  geom_line(colour = "white") +
  geom_hline(yintercept = 1000000, linetype = "dashed") +
  annotate("text", x = yearquarter("2020 Q2"), y = 300000, 
           label = "Covid-19\nLockdown", size = 3) +
  scale_x_yearquarter(date_breaks = "2 years") +
  scale_y_log10(labels = label_dollar(prefix = "£", 
                                      scale_cut = cut_short_scale())) +
  labs(
    title = glue("Median Quarterly SW10 Property Prices ({first} to {last})"), 
    subtitle = "With Upper / Lower Price Quartiles & Sales Volume",
    x = NULL, y = "Price (Log10)"
    ) +
  theme(axis.text.x = element_blank())

p2 <- qtile_df |> 
  ggplot(aes(yr_qtr, n)) +
  geom_line() +
  annotate("text", x = yearquarter("2007 Q3"), y = 180, 
           label = "Financial\nCrisis", size = 3) +
  annotate("text", x = yearquarter("2016 Q3"), y = 130, 
           label = "Brexit\nVote", size = 3) +
  scale_x_yearquarter(date_breaks = "2 years") +
  labs(x = NULL, y = "Transactions",
       caption = "Source: HM Land Registry") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1 / p2 + plot_layout(heights = c(2, 1))

num_trans <- data_df |> nrow()

data_df |> 
  ggplot() +
  geom_mosaic(aes(product(prop_type, est_type), fill = prop_type), 
              offset = 0.02, divider = mosaic("h")) +
  scale_fill_manual(values = cols[c(2:5)]) + 
  labs(
    title = "SW10 Transactions by Estate & Property Types",
    subtitle = glue("{comma(num_trans)} Transactions to {to_date}"),
    x = "", y = "", fill = "Property Type", 
    caption = "Source: HM Land Registry"
  ) +
  theme_minimal()

used_here()
