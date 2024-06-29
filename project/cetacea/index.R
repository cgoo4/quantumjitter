library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidymodels)
library(probably)
library(finetune)
library(textrecipes)
library(stopwords)
library(clock)
library(glue)
library(janitor)
library(vip)
conflicts_prefer(vip::vi)
library(tictoc)
library(patchwork)
library(future)
library(paletteer)
library(ggfoundry)
library(usedthese)

conflict_scout()

plan(multisession, workers = 10)

theme_set(theme_bw())


pal_name <- "wesanderson::Darjeeling2"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

strandings_df <- read_csv("strandings.csv", show_col_types = FALSE) |>
  clean_names() |> 
  mutate(
    date_rep = date_parse(date, format = "%d/%m/%Y"),
    length = parse_number(length_et),
    species_lumped = fct_lump_n(species, 20),
    across(ends_with("_val"), as.integer),
    rep_comment = comment
  )

# glimpse(strandings_df)

strandings_df2 <- 
  strandings_df |> 
  mutate(species_uncertainty =
      if_else(str_detect(species, "[?/]"), "Uncertain", "Known"))

strandings_df2 |> 
  filter(species_uncertainty == "Uncertain") |> 
  count(species, sort = TRUE, name = "Count") |> 
  slice_head(n = 6)

strandings_df2 |> 
  select(date_rep, year_val, month_val, day_val) |> 
  summary()

strandings_df3 <- strandings_df2 |>
  mutate(
    month_val = if_else(month_val == 0, mean(month_val) |> 
                          as.integer(), month_val),
    day_val = if_else(day_val == 0, mean(day_val) |> as.integer(), day_val),
    day_val = if_else(day_val == 0, 1L, day_val),
    date2 = date_build(year_val, month_val, day_val, invalid = "NA"),
    .by = species
  ) |> 
  mutate(date3 = coalesce(date_rep, date2),
         date_rep = if_else(is.na(date_rep), lag(date3), date3)
         ) |> 
  select(-date2, -date3, -ends_with("_val"))

example_species <-
  c("musculus", "melas", "crassidens", "borealis", "coeruleoalba")

known_species <- strandings_df3 |> 
  filter(species_uncertainty == "Known")

plot_date_feature <- \(var) {
  known_species |>
    mutate(
      year = get_year(date_rep),
      month = get_month(date_rep),
      week = as_iso_year_week_day(date_rep) |> get_week()
    ) |>
    filter(species %in% example_species) |>
    count(species, {{ var }}) |>
    ggplot(aes({{ var }}, species)) +
    geom_violin(
      alpha = 0.7,
      fill = pal[3],
      show.legend = FALSE
    ) +
    labs(
      title = glue("Variation in {str_to_title(as.character(var))}",
                   " of Stranding for Known Species"),
      x = NULL, y = glue("{str_to_title(as.character(var))}")
    )
}

c("year", "month", "week") |> 
  map(sym) |> 
  map(plot_date_feature) |> 
  wrap_plots(ncol = 1)

uki <- map_data("world", region = c("uk", "ireland"))

labels <- c("Mass", "Single")

uki |> 
  ggplot() +
  geom_map(aes(long, lat, map_id = region), map = uki, 
           colour = "black", fill = "grey90", size = 0.1) +
  geom_jitter(aes(longitude, latitude, colour = mass_single, 
                  size = mass_single), 
              alpha = 0.5, data = known_species) +
  facet_wrap(~ species_lumped, nrow = 3) +
  coord_map("mollweide") +
  scale_size_manual(values = c(1, 0.5), labels = labels) +
  scale_colour_manual(values = pal[c(3, 2)], labels = labels) +
  theme_void() +
  theme(legend.position = "top", 
        strip.text = element_text(colour = "grey50")) +
  labs(title = "Strandings by Species", 
       colour = NULL, size = NULL)

# Add history of mass stranding
strandings_df4 <- strandings_df3 |> 
  mutate(mass_possible = min(mass_single, na.rm = TRUE),
         .by = species)

known_species |>
  mutate(sex = case_match(
    sex,
    "M" ~ "Male",
    "F" ~ "Female",
    .default = "Unknown"
  )) |> 
  filter(species_lumped != "Other") |> 
  count(species_lumped, length, sex) |> 
  mutate(species_lumped = fct_reorder(species_lumped, 
                                      desc(length), min, na.rm = TRUE)) |> 
  ggplot(aes(length, species_lumped)) + 
  geom_violin(aes(fill = if_else(str_detect(species_lumped, "^de|^co"), 
                                 TRUE, FALSE)), show.legend = FALSE) +
  facet_wrap(~ sex) +
  scale_fill_manual(values = pal[c(1, 5)]) +
  labs(title = "Variation in Species Length by Sex", 
       x = NULL, y = "Length (metres)")

strandings_df4 |> 
  count(county) |> 
  filter(str_detect(county, "Shet|Northumberland")) |> 
  rename(County = county, Count = n)

regex_pattern <-
  c("[,/].*$",
    "(?<!Che|Hamp|Lanca|North York)-?shire",
    " Isl.*$",
    " &.*$",
    "[-.]$")

strandings_df5 <- strandings_df4 |>
  mutate(
    county = str_remove_all(county, str_c(regex_pattern, collapse = "|")),
    county = recode(
      county,
      "Carnarvon" = "Caernarvon",
      "E.Lothian" = "East Lothian",
      "Shetlands" = "Shetland",
      "W.Glamorgan" = "West Glamorgan",
      "S.Glamorgan" = "South Glamorgan"
    )
  ) 

strandings_df4 |>
  summarise(counties_before = n_distinct(county))

strandings_df5 |>
  summarise(counties_after = n_distinct(county))

strandings_df5 |>
  ggplot(aes(count, species, colour = species_uncertainty)) +
  geom_jitter(alpha = 0.5, size = 2) +
  scale_x_log10() +
  scale_colour_manual(values = pal[c(1, 5)]) +
  labs(title = "How 'Count' Relates to Species", 
       x = NULL, y = "Count (log10)", colour = "Species") +
  theme(legend.position = "top")

known_species <- strandings_df5 |>
  filter(species_uncertainty == "Known") |>
  mutate(across(
    c(
      "species",
      "mass_single",
      "mass_possible",
      "county",
      "location",
      "sex",
      "fam_genus"
    ),
    factor
  ))

set.seed(123)

data_split <-
  known_species |>
  mutate(species = fct_drop(species)) |> 
  initial_split(strata = species)

train <- data_split |> training()

test <- data_split |> testing()

predictors <-
  c(
    "latitude",
    "longitude",
    "length",
    "mass_single",
    "mass_possible",
    "county",
    "location",
    "rep_comment",
    "sex",
    "fam_genus"
  )

recipe <-
  train |>
  recipe() |>
  update_role(species, new_role = "outcome") |>
  update_role(all_of(predictors), new_role = "predictor") |>
  update_role(!has_role("outcome") & !has_role("predictor"), 
              new_role = "id") |>
  step_date(date_rep, features = c("decimal", "week", "month", "year"), 
            label = FALSE) |>
  step_tokenize(location, rep_comment) |>
  step_stopwords(location, rep_comment) |>
  step_tokenfilter(location, rep_comment, max_tokens = tune()) |> #100
  step_tf(location, rep_comment) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors())

xgb_model <-
  boost_tree(trees = tune(), # 440
             mtry = tune(), # 0.6
             learn_rate = 0.02) |> 
  set_mode("classification") |>
  set_engine("xgboost", 
             count = FALSE,
             verbosity = 0,
             tree_method = "hist")

xgb_wflow <- workflow() |>
  add_recipe(recipe) |>
  add_model(xgb_model)

set.seed(9)

folds <- vfold_cv(train, strata = species)

set.seed(10)

tic()

tune_result <- xgb_wflow |>
  tune_race_anova(
    resamples = folds,
    grid = crossing(
      trees = seq(200, 520, 40),
      mtry = seq(0.5, 0.7, 0.1),
      max_tokens = seq(80, 120, 20)
      ),
    control = control_race(),
    metrics = metric_set(accuracy)
  )

toc()

tune_result |> 
  plot_race() + 
  labs(title = "Early Elimination of Parameter Combinations")

set.seed(123)

xgb_fit <- xgb_wflow |>
  finalize_workflow(tune_result |> 
                      select_best(metric = "accuracy")) |> 
  fit(train)

xgb_results <- xgb_fit |> 
  augment(new_data = test)

xgb_results |>
  accuracy(species, .pred_class)
  
xgb_results |>
  conf_mat(species, .pred_class) |>
  autoplot(type = "heatmap") +
  scale_fill_gradient2(
    mid = "white",
    high = pal[1],
    midpoint = 0
  ) +
  labs(title = "Confusion Matrix") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vi(xgb_fit |> extract_fit_parsnip()) |> 
  arrange(desc(Importance)) |> 
  mutate(ranking = row_number()) |> 
  slice_head(n = 40)

xgb_preds <- xgb_fit |> 
  augment(new_data = strandings_df5 |> 
            filter(species_uncertainty == "Uncertain"))

species_levels <- xgb_preds |> 
  select(starts_with(".pred_"), -.pred_class) |> 
  names() |> 
  as.factor()

subset_df <- xgb_preds |>
  mutate(
    .class_pred = make_class_pred(
      .pred_acutorostrata,
      .pred_acutus,
      .pred_albirostris,
      .pred_ampullatus,
      .pred_bidens,
      .pred_borealis,
      .pred_breviceps,
      .pred_cavirostris,
      .pred_coeruleoalba,
      .pred_crassidens,
      .pred_delphis,
      .pred_electra,
      .pred_europaeus,
      .pred_griseus,
      .pred_leucas,
      .pred_macrocephalus,
      .pred_melaena,
      .pred_melas,
      .pred_mirus,
      .pred_monoceros,
      .pred_musculus,
      .pred_novaeangliae,
      .pred_orca,
      .pred_phocoena,
      .pred_physalus,
      .pred_sp.indet.,
      .pred_truncatus,
      levels = levels(species_levels),
      min_prob = .9
    )
  )

subset_df |>
  summarise(n = n(), .by = c(species, .class_pred)) |> 
  arrange(species, desc(n)) |> 
  rename("Actual" = species, "Predicted" = .class_pred, "Count" = n)

used_here()
