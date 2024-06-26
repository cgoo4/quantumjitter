library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidymodels)
library(janitor)
library(scales)
library(vip)
library(poissonreg)
library(ggfoundry)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

pal_name <- "Custom Palette"

pal <- c("#798E87", "#C27D38", "#CCC591", "#29211F")

pal12 <- colorRampPalette(pal)(12)

display_palette(fill = pal12, n = 12, pal_name = pal_name, shape_size = 0.6)

# MPS Borough Level Crime (Historical).csv
url <- str_c(
  "https://data.london.gov.uk/download/recorded_crime_summary/",
  "a1b36c68-cd08-4a8a-99c2-c2313165b744/", 
  "MPS%20Borough%20Level%20Crime%20%28Historical%29.csv"
  )

crime_df <-
  read_csv(url, show_col_types = FALSE) |>
  clean_names() |>
  pivot_longer(starts_with("x"), names_to = "year", values_to = "number_of_offences") |> 
  mutate(
    year = str_sub(year, 2, 5) |> as.numeric(),
    major_text = str_to_sentence(major_text)) |>
  filter(year != 2022) |> # partial year
  rename(offences = major_text, borough = borough_name) |> 
  summarise(number_of_offences = sum(number_of_offences),
            .by = c(year, borough, offences))

crime_df |>
  mutate(borough = str_wrap(borough, 11)) |>
  ggplot(aes(year, number_of_offences, 
             colour = offences, group = offences)) +
  geom_line() +
  facet_wrap(~borough, scales = "free_y", ncol = 4) +
  labs(
    x = NULL, y = NULL, title = "London Crime by Borough",
    colour = "Offence", caption = "Source: data.gov.uk"
  ) +
  scale_colour_manual(values = pal12) +
  guides(colour = guide_legend(nrow = 6, size = 4)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

crime_df |>
  summarise(number_of_offences = sum(number_of_offences),
            .by = c(offences, borough)) |>
  mutate(
    median_offences = median(number_of_offences),
    offences = str_wrap(offences, 20),
    .by = offences
  ) |>
  ggplot(aes(fct_reorder(offences, median_offences), number_of_offences)) +
  geom_boxplot(fill = pal[1]) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    x = NULL, y = NULL,
    title = "Number of Offences by Type",
    caption = "Source: data.gov.uk"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

crime_df |>
  summarise(number_of_offences = sum(number_of_offences),
            .by = c(offences, borough)) |>
  mutate(
    median_offences = median(number_of_offences),
    offences = str_wrap(offences, 10),
    .by = borough
  ) |>
  ggplot(aes(fct_reorder(borough, median_offences), number_of_offences)) +
  geom_boxplot(fill = pal[1]) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
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
    x = NULL, y = NULL,
    title = "Number of Offences by Year",
    caption = "Source: data.gov.uk"
  )

set.seed(123)

data_split <- 
  crime_df |>
  initial_split(strata = offences)

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
  vip(aesthetics = list(fill = pal[1])) +
  labs(title = "Feature Importance -- rpart")

rp_results <- rp_fit |> 
  augment(crime_test) |> 
  mutate(model = "rpart")

ranger_model <- 
  rand_forest() |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

ranger_wflow <- workflow() |>
  add_recipe(crime_recipe) |>
  add_model(ranger_model)

ranger_fit <- ranger_wflow |> 
  fit(crime_train)

ranger_fit |>
  extract_fit_parsnip() |> 
  vip(aesthetics = list(fill = pal[3])) +
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

rf_fit <- rf_wflow |> 
  fit(crime_train)

rf_fit |>
  extract_fit_parsnip() |> 
  vip(aesthetics = list(fill = "grey60")) +
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
  vip(aesthetics = list(fill = pal[4])) +
  labs(title = "Feature Importance -- glm")

poisson_results <- poisson_fit |> 
  augment(crime_test) |> 
  mutate(model = "glm")

model_results <- 
  rp_results |> 
  bind_rows(ranger_results) |> 
  bind_rows(rf_results) |> 
  bind_rows(poisson_results) |> 
  group_by(model) |> 
  metrics(truth = number_of_offences, estimate = .pred)

model_results |> 
  ggplot(aes(model, .estimate, fill = model)) +
  geom_col() +
  geom_label(aes(label = round(.estimate, 2)), size = 3, fill = "white") +
  facet_wrap(~ .metric, scales = "free_y") +
  scale_fill_manual(values = as.character(pal[c(4, 5, 3, 1)])) +
  labs(x = NULL, y = NULL, title = "Comparison of Model Metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

temp_df <- 
  crime_df |> 
  mutate(num_lag1 = lag(number_of_offences),
         num_lag2 = lag(number_of_offences, 2),
         num_lag3 = lag(number_of_offences, 3)) |> 
  drop_na()

set.seed(123)

data_split <- 
  temp_df |>
  initial_split(strata = offences)

temp_train <- data_split |>
  training()

temp_test <- data_split |>
  testing()

temp_recipe <-
  temp_train |>
  recipe() |>
  update_role(number_of_offences, new_role = "outcome") |>
  update_role(-has_role("outcome"), new_role = "predictor")

summary(temp_recipe)

temp_model <- 
  rand_forest() |>
  set_engine("randomForest") |>
  set_mode("regression")

temp_wflow <- workflow() |>
  add_recipe(temp_recipe) |>
  add_model(temp_model)

temp_fit <- temp_wflow |> 
  fit(temp_train)

temp_fit |>
  extract_fit_parsnip() |> 
  vip(aesthetics = list(fill = pal[2])) +
  labs(title = "Feature Importance -- Random Forest with Lags")

temp_results <- temp_fit |> 
  augment(temp_test) |> 
  metrics(truth = number_of_offences, estimate = .pred) |> 
  mutate(model = "rf with lags")

updated_results <- 
  model_results |> 
  bind_rows(temp_results)

updated_results |> 
  ggplot(aes(model, .estimate, fill = model)) +
  geom_col() +
  geom_label(aes(label = round(.estimate, 2)), size = 3, fill = "white") +
  facet_wrap(~ .metric, scales = "free_y") +
  scale_fill_manual(values = as.character(pal[c(4, 5, 3, 2, 1)])) +
  labs(x = NULL, y = NULL, title = "Comparison of Model Metrics") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

used_here()
