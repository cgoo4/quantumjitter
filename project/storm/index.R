library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(scales)
library(glue)
library(tidyquant)
library(clock)
conflicts_prefer(clock::date_format)
library(paletteer)
library(ggfoundry)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

pal_name <- "wesanderson::Moonrise2"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

symbols <-
  c(
    "SPY",
    "XLV",
    "XLK",
    "XLE",
    "XLF",
    "XLC",
    "XLI",
    "XLY",
    "XLP",
    "XLRE",
    "XLU",
    "XLB"
  )

from <- "2020-02-19"

from_formatted <- date_parse(from, format = "%Y-%m-%d") |> 
  date_format(format = "%b %d, %Y")

eod_sectors <-
  tq_get(symbols, from = from) |>
  mutate(
    norm_close = adjusted / first(adjusted) - 1,
    type = if_else(symbol == "SPY", "Market", "Sector"),
    sector = case_match(
      symbol,
      "SPY"  ~ "S&P 500",
      "XLB"  ~ "Materials",
      "XLE"  ~ "Energy",
      "XLU"  ~ "Utilities",
      "XLI"  ~ "Industrical",
      "XLRE" ~ "Real Estate",
      "XLV"  ~ "Health",
      "XLK"  ~ "Technology",
      "XLF"  ~ "Financial",
      "XLC"  ~ "Communication",
      "XLY"  ~ "Consumer Discretionary",
      "XLP"  ~ "Consumer Staples",
      .default = "Other"
    ), .by = symbol
  ) |>
  drop_na()

eod_sectors |>
  mutate(
    sector = str_wrap(sector, 12),
    sector = fct_reorder(sector, norm_close, last, .desc = TRUE)
  ) |>
  ggplot(aes(date, norm_close, colour = sign(norm_close))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey80") +
  geom_line() +
  facet_wrap(~sector) +
  scale_colour_gradient(low = pal[2], high = pal[1]) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "S&P 500 Sector Impact of Covid-19 & Ukraine",
    subtitle = glue("Relative to {from_formatted}"),
    x = NULL, y = NULL, colour = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

used_here()
