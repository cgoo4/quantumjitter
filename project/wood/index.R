library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(trelliscope)
library(janitor)
library(vangogh)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

(cols <- vangogh_palette("StarryNight"))

cols12 <- colorRampPalette(cols)(12)

crime_df <- str_c(
  "https://data.london.gov.uk/download/recorded_crime_summary/",
  "934f2ddb-5804-4c6a-a17c-bdd79b33430e/", 
  "MPS%20Borough%20Level%20Crime%20%28Historical%29.csv"
  ) |> 
  read_csv(show_col_types = FALSE) |> 
  clean_names() |> 
  rename_with(\(x) str_remove_all(x, "_text|look_up_|_name")) |> 
  pivot_longer(where(is.numeric), names_to = "month", values_to = "num_offences") |> 
  mutate(month = parse_number(month) |> str_c("01") |> ymd())

crime_df |>
  summarise(num_offences = sum(num_offences), .by = c(major, borough, month)) |>
  ggplot(aes(month, num_offences, colour = major, group = major)) +
  geom_line() +
  facet_wrap(~borough, scales = "free_y", ncol = 4) +
  labs(
    x = NULL, y = NULL, title = "London Crime by Borough",
    colour = "Offence", caption = "Source: data.gov.uk"
  ) +
  scale_colour_manual(values = cols12) +
  guides(colour = guide_legend(nrow = 3)) +
  theme(
    strip.background = element_rect(fill = cols[4]),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

panels_df <- crime_df |>
  mutate(major = str_wrap(major, 16)) |> 
  ggplot(aes(month, num_offences)) +
  geom_line(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, colour = cols[5]) +
  facet_panels(vars(borough, major, minor), scales = "free") + 
  labs(colour = NULL, x = NULL, y = "Offence Count")

slope <- \(x, y) coef(lm(y ~ x))[2]

summary_df <- crime_df |> 
  summarise(
    mean_count = mean(num_offences), 
    slope = slope(month, num_offences),
    .by = c(borough, major, minor))
  
panels_df |> 
  as_panels_df(as_plotly = TRUE) |> 
  as_trelliscope_df(
    name = "Crime in 'The Smoke'",
    description = str_c(
      "Timeseries of offences by category ", 
      "across London's 33 boroughs sourced from data.gov.uk."
      )
    ) |> 
  left_join(summary_df, join_by(borough, major, minor)) |> 
  set_var_labels(
    major = "Major Category of Offence",
    minor = "Minor Category of Offence",
    mean_count = "Average Offences by Borough & Offence Category",
    slope = "Steepness of a Linear Trendline"
  ) |> 
  set_default_sort(c("slope"), dirs = "desc") |>
  set_tags(
    stats = c("mean_count", "slope"),
    info = c("borough", "major", "minor")
  ) |> 
  view_trelliscope()

used_here()
