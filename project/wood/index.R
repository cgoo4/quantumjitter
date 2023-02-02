library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(trelliscopejs)
library(rbokeh)
library(janitor)
library(vangogh)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

(cols <- vangogh_palette("StarryNight"))

cols9 <- colorRampPalette(cols)(9)

url <- str_c(
  "https://data.london.gov.uk/",
  "download/recorded_crime_rates/",
  "c051c7ec-c3ad-4534-bbfe-6bdfee2ef6bb/",
  "crime%20rates.csv"
)

crime_df <-
  read_csv(url, col_types = "cfcfdn") |>
  clean_names() |>
  mutate(
    year = str_extract(year, "(?:1999|200[0-9]|201[0-7])"),
    year = as.numeric(year)
  ) |>
  summarise(number_of_offences = sum(number_of_offences),
            .by = c(year, borough, offences)) |>
  filter(
    offences != "All recorded offences",
    !borough %in% c(
      "England and Wales",
      "Met Police Area",
      "Inner London",
      "Outer London"
    )
  )

crime_df |>
  mutate(borough = str_wrap(borough, 11)) |>
  ggplot(aes(year, number_of_offences, colour = offences, group = offences)) +
  geom_line() +
  facet_wrap(~borough, scales = "free_y", ncol = 4) +
  labs(
    x = NULL, y = NULL, title = "London Crime by Borough",
    colour = "Offence", caption = "Source: data.gov.uk"
  ) +
  scale_colour_manual(values = cols9) +
  guides(colour = guide_legend(nrow = 3)) +
  theme(
    strip.background = element_rect(fill = cols[4]),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

slope <- \(x, y) coef(lm(y ~ x))[2]

plot_data <- crime_df |>
  nest(.by = c(borough, offences)) |>
  mutate(
    additional_cogs = map_cog(
      data,
      ~ tibble(
        slope = cog(slope(.x$year, .x$number_of_offences),
          desc = "Steepness of the trend"
        ) |>
          round(2),
        mean_count = cog(mean(.x$number_of_offences),
          desc = "Average count"
        ),
        iqr_count = cog(IQR(.x$number_of_offences),
          desc = "Interquartile range"
        )
      )
    ),
    panel = map_plot(
      data,
      ~ figure(xlab = "Date", ylab = "Count") |>
        ly_lines(year, number_of_offences, color = cols[5], 
                 width = 2, data = .x) |>
        ly_points(year, number_of_offences,
          size = 10,
          fill_color = cols[9],
          hover = number_of_offences, data = .x
        ) |>
        theme_plot(
          background_fill_color = cols[2],
          background_fill_alpha = 0.5
        )
    )
  )

plot_data |>
  trelliscope(
    name = "London Crime",
    desc = "Source: data.gov.uk",
    nrow = 2,
    ncol = 3,
    state = list(
      sort = list(sort_spec("slope", dir = "desc")),
      labels = c("borough", "offences", "slope")
    ),
    path = "appfiles"
  )

used_here()
