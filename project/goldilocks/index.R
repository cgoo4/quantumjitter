library(conflicted)
library(tidyverse)
library(scales)
library(paletteer)
library(glue)
library(ggfoundry)
library(usedthese)

conflict_prefer_all("dplyr", quiet = TRUE)
conflict_scout()

theme_set(theme_bw())

pal_name <- "wesanderson::Darjeeling2"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

stock_data <- tibble(
  stock = sprintf("Stock %02d", 1:50),
  rank = 1:50,
  expected_return = seq(0.22, 0.08, length.out = 50)
)

stock_data |>
  ggplot(aes(rank, expected_return)) +
  geom_line(colour = pal[2], linewidth = 0.8) +
  geom_point(colour = pal[2], size = 1.5) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title = "50 Ranked Stock Ideas",
    subtitle = "Expected return declines as the portfolio reaches further down the list",
    x = "Rank",
    y = "Expected Return"
  )

n_sims <- 5000
market_sd <- 0.08
stock_sd <- 0.25

portfolio_sizes <- c(2, 5, 10, 20, 50)

simulate_portfolio <- \(n, sims = n_sims) {
  tibble(
    portfolio_size = n,
    portfolio_return = mean(stock_data$expected_return[seq_len(n)]) +
      rnorm(sims, 0, market_sd) +
      rnorm(sims, 0, stock_sd / sqrt(n))
  )
}

set.seed(456)

portfolios <-
  map(portfolio_sizes, simulate_portfolio) |>
  list_rbind() |>
  mutate(
    portfolio_size = factor(
      portfolio_size,
      levels = portfolio_sizes
    )
  )

portfolio_summary <- portfolios |>
  summarise(
    mean_return = mean(portfolio_return),
    p05 = quantile(portfolio_return, 0.05),
    .by = portfolio_size
  )

bowls <- portfolios |>
  summarise(return_sd = sd(portfolio_return), .by = portfolio_size) |>
  left_join(portfolio_summary, by = join_by(portfolio_size)) |>
  mutate(
    # Illustrative steam bands; fill remains continuous.
    bowl = case_when(
      return_sd > 0.13 ~ "bowl2",
      return_sd > 0.095 ~ "bowl1",
      TRUE ~ "bowl0"
    )
  )

portfolios |>
  ggplot(aes(portfolio_size, portfolio_return, group = portfolio_size)) +
  geom_violin(fill = NA, colour = "grey55", linewidth = 0.5) +
  geom_casting(
    aes(y = mean_return, shape = bowl, fill = return_sd),
    data = bowls,
    colour = "grey20",
    size = 0.18,
    vjust = 0.36
  ) +
  geom_point(
    aes(y = p05),
    data = portfolio_summary,
    fill = pal[1],
    colour = "grey20",
    shape = 21,
    size = 3
  ) +
  geom_label(
    aes(y = mean_return, label = percent(mean_return, accuracy = 0.1)),
    data = portfolio_summary,
    nudge_y = 0.22,
    fill = "white",
    size = 3
  ) +
  geom_label(
    aes(y = p05, label = percent(p05, accuracy = 0.1)),
    data = portfolio_summary,
    nudge_y = -0.1,
    fill = "white",
    size = 3
  ) +
  scale_shape_identity() +
  scale_fill_viridis_c(
    option = "plasma",
    begin = 0.08,
    end = 0.92,
    name = "Return SD",
    labels = label_percent(accuracy = 0.1)
  ) +
  scale_y_continuous(
    labels = label_percent(),
    breaks = breaks_extended(8)
  ) +
  labs(
    title = "The Goldilocks Trade-off",
    subtitle = glue(
      "Mean and 5th-percentile return across {label_comma()(n_sims)} simulated outcomes"
    ),
    x = "Number of Stocks",
    y = "Portfolio Return",
    caption = "Bowls: mean return · Dots: 5th percentile\nHotter colours indicate greater variability; steam counts use illustrative bands."
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

used_here()
