library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(scales)
library(truncnorm)
library(paletteer)
library(ggfoundry)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

pal_name <- "wesanderson::Darjeeling2"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

set.seed(123)

stock_data <- tibble(
  stock = chartr("0123456789", "abcdefghij", sample(50)),
  return = rtruncnorm(50, a = -0.2, mean = 0.4, sd = 0.5)
)

mean(stock_data$return)

stock_data |> 
  ggplot(aes(return)) +
  geom_histogram(fill = pal[2]) +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "50 Randomly-generated Stock Returns", 
       x = "Annual Return", y = "Count")

portfolio <- \(x) {
  stock_data |>
    slice_sample(n = x, replace = TRUE) |>
    summarise(
      portfolio_return = mean(return),
      portfolio_size = x
    ) |>
    bind_rows()
}

set.seed(456)

portfolios <-
  map(c(
    rep(2, 1000),
    rep(5, 1000),
    rep(10, 1000),
    rep(20, 1000),
    rep(50, 1000)
  ), portfolio) |>
  list_rbind() |> 
  mutate(portfolio_size = factor(portfolio_size))

mean_returns <- portfolios |>
  summarise(
    mean_return = mean(portfolio_return),
    min_return = min(portfolio_return),
    .by = portfolio_size
  )

portfolios |>
  ggplot(aes(portfolio_size, portfolio_return, group = portfolio_size)) +
  geom_violin(aes(fill = portfolio_size), show.legend = FALSE) +
  geom_label(aes(portfolio_size, 1.5,
    label = percent(mean_return, accuracy = 1)
  ),
  data = mean_returns, fill = pal[4],
  ) +
  geom_label(aes(portfolio_size, -0.2,
    label = percent(min_return, accuracy = 1)
  ),
  data = mean_returns, fill = pal[1],
  ) +
  scale_y_continuous(labels = label_percent(), breaks = breaks_extended(9)) +
  scale_fill_manual(values = pal[c(1:5)]) +
  labs(
    x = "Portfolio Size", y = "Return",
    title = "How Portfolio Size Changes Downside & Upside Risk",
    subtitle = "BLUE Labels = Mean Return; BROWN Labels = Worst Return"
  )

used_here()
