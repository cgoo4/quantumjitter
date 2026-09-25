library(conflicted)
library(tidyverse)
library(tidymodels)
library(janitor)
library(scales)
library(vip)
library(poissonreg)
library(ggfoundry)
library(usedthese)

conflict_prefer_all("dplyr", quiet = TRUE)

conflict_scout()

theme_set(theme_bw())

pal_name <- "Custom Palette"

pal <- c("#798E87", "#C27D38", "#CCC591", "#29211F")

pal12 <- colorRampPalette(pal)(12)

# One well-separated pal12 colour per model, shared by the vip and metric plots
model_fills <- c(
  "rpart" = pal12[1],
  "rf with lags" = pal12[4],
  "ranger" = pal12[7],
  "random forest" = pal12[10],
  "glm" = pal12[12]
)

display_palette(pal12, pal_name)

# MPS Borough Level Crime (Historical).csv
url <- str_c(
  "https://data.london.gov.uk/download/recorded_crime_summary/",
  "a1b36c68-cd08-4a8a-99c2-c2313165b744/",
  "MPS%20Borough%20Level%20Crime%20%28Historical%29.csv"
)

crime_df <-
  read_csv(url, show_col_types = FALSE) |>
  clean_names() |>
  pivot_longer(
    starts_with("x"),
    names_to = "year",
    values_to = "number_of_offences"
  ) |>
  mutate(
    year = str_sub(year, 2, 5) |> as.numeric(),
    major_text = str_to_sentence(major_text)
  ) |>
  filter_out(year == 2022) |> # partial year
  rename(offences = major_text, borough = borough_name) |>
  summarise(
    number_of_offences = sum(number_of_offences),
    .by = c(year, borough, offences)
  )

crime_df |>
  mutate(borough = str_wrap(borough, 11)) |>
  ggplot(aes(year, number_of_offences, colour = offences, group = offences)) +
  geom_line() +
  facet_wrap(~borough, scales = "free_y", ncol = 4) +
  labs(
    x = NULL,
    y = NULL,
    title = "London Crime by Borough",
    colour = "Offence",
    caption = "Source: data.gov.uk"
  ) +
  scale_colour_manual(values = pal12) +
  guides(colour = guide_legend(nrow = 6)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

crime_df |>
  summarise(
    number_of_offences = sum(number_of_offences),
    .by = c(offences, borough)
  ) |>
  mutate(
    median_offences = median(number_of_offences),
    offences = str_wrap(offences, 20),
    .by = offences
  ) |>
  ggplot(aes(fct_reorder(offences, median_offences), number_of_offences)) +
  geom_boxplot(fill = pal[1]) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = NULL,
    title = "Number of Offences by Type",
    caption = "Source: data.gov.uk"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

crime_df |>
  summarise(
    number_of_offences = sum(number_of_offences),
    .by = c(offences, borough)
  ) |>
  mutate(
    median_offences = median(number_of_offences),
    offences = str_wrap(offences, 10),
    .by = borough
  ) |>
  ggplot(aes(number_of_offences, fct_reorder(borough, median_offences))) +
  geom_boxplot(fill = pal[1]) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = NULL,
    title = "Number of Offences by Borough",
    caption = "Source: data.gov.uk"
  )

crime_df |>
  summarise(number_of_offences = sum(number_of_offences), .by = year) |>
  ggplot(aes(year, number_of_offences)) +
  geom_line(colour = pal[4], linetype = "dashed") +
  geom_smooth(colour = pal[2]) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = NULL,
    title = "Number of Offences by Year",
    caption = "Source: data.gov.uk"
  )

set.seed(123)

# Each offence is under 10% of the data, so rsample pools the strata and
# warns; the split is then effectively a simple random sample. The call is
# retained to preserve the original split, with the expected warning muted.
data_split <-
  withCallingHandlers(
    crime_df |> initial_split(strata = offences),
    warning = function(w) {
      if (grepl("Too little data to stratify", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

crime_train <- data_split |>
  training()

crime_test <- data_split |>
  testing()

crime_recipe <-
  crime_train |>
  recipe() |>
  update_role(number_of_offences, new_role = "outcome") |>
  update_role(-has_role("outcome"), new_role = "predictor")

summary(crime_recipe)

rp_model <-
  decision_tree() |>
  set_engine("rpart") |>
  set_mode("regression")

rp_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(rp_model)

rp_fit <- rp_wflow |>
  fit(crime_train)

rp_fit |>
  extract_fit_parsnip() |>
  vip(aesthetics = list(fill = model_fills[["rpart"]])) +
  labs(title = "Feature Importance -- rpart")

rp_results <- rp_fit |>
  augment(crime_test) |>
  mutate(model = "rpart")

ranger_model <-
  rand_forest() |>
  set_engine("ranger", importance = "impurity", seed = 123) |>
  set_mode("regression")

ranger_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(ranger_model)

ranger_fit <- ranger_wflow |>
  fit(crime_train)

ranger_fit |>
  extract_fit_parsnip() |>
  vip(aesthetics = list(fill = model_fills[["ranger"]])) +
  labs(title = "Feature Importance -- Ranger")

ranger_results <- ranger_fit |>
  augment(crime_test) |>
  mutate(model = "ranger")

rf_model <-
  rand_forest() |>
  set_engine("randomForest") |>
  set_mode("regression")

rf_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(rf_model)

# randomForest has no seed argument of its own; it draws from the global RNG
set.seed(123)

rf_fit <- rf_wflow |>
  fit(crime_train)

rf_fit |>
  extract_fit_parsnip() |>
  vip(aesthetics = list(fill = model_fills[["random forest"]])) +
  labs(title = "Feature Importance -- Random Forest")

rf_results <- rf_fit |>
  augment(crime_test) |>
  mutate(model = "random forest")

poisson_model <-
  poisson_reg() |>
  set_engine("glm") |>
  set_mode("regression")

poisson_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(poisson_model)

poisson_fit <- poisson_wflow |>
  fit(crime_train)

poisson_fit |>
  extract_fit_parsnip() |>
  vip(aesthetics = list(fill = model_fills[["glm"]])) +
  labs(title = "Feature Importance -- glm")

poisson_results <- poisson_fit |>
  augment(crime_test) |>
  mutate(model = "glm")

plot_metrics <- function(results) {
  results |>
    ggplot(aes(model, .estimate, fill = model)) +
    geom_col() +
    geom_label(aes(label = round(.estimate, 2)), size = 3, fill = "white") +
    facet_wrap(~.metric, scales = "free_y") +
    scale_fill_manual(values = model_fills) +
    labs(x = NULL, y = NULL, title = "Comparison of Model Metrics") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

model_results <-
  list(rp_results, ranger_results, rf_results, poisson_results) |>
  list_rbind() |>
  group_by(model) |>
  metrics(truth = number_of_offences, estimate = .pred)

model_results |>
  plot_metrics()

lagged_df <-
  crime_df |>
  mutate(
    num_lag1 = lag(number_of_offences),
    num_lag2 = lag(number_of_offences, 2),
    num_lag3 = lag(number_of_offences, 3)
  ) |>
  drop_na()

set.seed(123)

data_split <-
  withCallingHandlers(
    lagged_df |> initial_split(strata = offences),
    warning = function(w) {
      if (grepl("Too little data to stratify", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

lag_train <- data_split |>
  training()

lag_test <- data_split |>
  testing()

lag_recipe <-
  lag_train |>
  recipe() |>
  update_role(number_of_offences, new_role = "outcome") |>
  update_role(-has_role("outcome"), new_role = "predictor")

summary(lag_recipe)

lag_model <-
  rand_forest() |>
  set_engine("randomForest") |>
  set_mode("regression")

lag_wflow <- workflow() |>
  add_recipe(lag_recipe) |>
  add_model(lag_model)

set.seed(123) # see note at the random forest fit above

lag_fit <- lag_wflow |>
  fit(lag_train)

lag_fit |>
  extract_fit_parsnip() |>
  vip(aesthetics = list(fill = model_fills[["rf with lags"]])) +
  labs(title = "Feature Importance -- Random Forest with Lags")

lag_results <- lag_fit |>
  augment(lag_test) |>
  metrics(truth = number_of_offences, estimate = .pred) |>
  mutate(model = "rf with lags")

updated_results <-
  model_results |>
  bind_rows(lag_results)

updated_results |>
  plot_metrics()

used_here()
