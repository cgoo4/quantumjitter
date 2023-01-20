library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr")
library(wesanderson)
library(guardianapi)
library(quanteda)
library(scales)
library(tictoc)
library(clock)
library(patchwork)
library(text2vec)
library(topicmodels)
library(slider)
library(glue)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

(cols <- wes_palette(name = "Chevalier1"))

dates_df <- tibble(start_date = date_build(2020, 1:11, 25)) |> 
  mutate(end_date = add_months(start_date, 1) |> add_days(-1))

dates_df

tic()

read_slowly <- slowly(gu_content)

article_df <-
  pmap(dates_df, \(start_date, end_date) {
    read_slowly(
      "brexit",
      from_date = start_date,
      to_date = end_date
    )
  }) |> 
  list_rbind()

toc()

trade_df <-
  article_df |>
  filter(!str_detect(id, "/live/"), 
         section_id %in% c("world", "politics", "business")) |>
  mutate(
    body = str_remove_all(body, "<.*?>") |> str_to_lower(),
    body = str_remove_all(body, "[^a-z0-9 .-]"),
    body = str_remove_all(body, "nbsp")
  )

trade_corp <- trade_df |> 
  corpus(docid_field = "short_url", 
         text_field = "body", unique_docnames = FALSE)

window <- 5

trade_fcm <-
  trade_corp |>
  fcm(context = "window", window = window, 
      count = "weighted", weights = window:1)

glove <- GlobalVectors$new(rank = 60, x_max = 10)

set.seed(42)

wv_main <- glove$fit_transform(trade_fcm, n_iter = 10)
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context)

search_coord <- 
  word_vectors["barnier", , drop = FALSE]

word_vectors |> 
  sim2(search_coord, method = "cosine") |> 
  as_tibble(rownames = NA) |> 
  rownames_to_column("term") |> 
  rename(similarity = 2) |> 
  slice_max(similarity, n = 10)

context_df <- 
  trade_df |> 
  filter(str_detect(body, "barnier|frost|uk-eu")) 

context_corp <- 
  context_df |> 
  corpus(docid_field = "short_url", text_field = "body")

set.seed(123)

context_corp |>
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) |>
  kwic(pattern = phrase(c("trade negotiation", "trade deal", "trade talks")), 
       valuetype = "regex", window = 7) |>
  as_tibble() |>
  left_join(article_df, by = join_by(docname == short_url)) |> 
  slice_sample(n = 10) |> 
  select(docname, pre, keyword, post, headline)

tic()

sent_df <- 
  context_corp |> 
  tokens() |> 
  dfm(dictionary = data_dictionary_LSD2015) |> 
  as_tibble() |>
  left_join(context_df, by = join_by(doc_id == short_url)) |> 
  mutate(
    pos = positive + neg_negative,
    neg = negative + neg_positive,
    web_date = date_ceiling(as.Date(web_publication_date), "week"),
    pct_pos = pos / (pos + neg)
  )

sent_df |> 
  select(Article = doc_id, "Pos Score" = pos, "Neg Score" = neg) |> 
  slice(1:10)

summary_df <- sent_df |> 
  summarise(pct_pos = mean(pct_pos), 
            n = n(),
            .by = web_date)

toc()

width <- 7

sent_df2 <- sent_df |>
  mutate(web_date = as.Date(web_publication_date)) |> 
  group_by(web_date) |>
  summarise(pct_pos = sum(pos) / sum(neg + pos)) |> 
  mutate(
    roll_mean = slide_dbl(pct_pos, mean, .before = 6),
    roll_lq = slide_dbl(pct_pos, ~ quantile(.x, probs = 0.25), .before = 6),
    roll_uq = slide_dbl(pct_pos, ~ quantile(.x, probs = 0.75), .before = 6)
  )

p1 <- sent_df2 |>
  ggplot(aes(web_date)) +
  geom_line(aes(y = roll_mean), colour = cols[1]) +
  geom_ribbon(aes(ymin = roll_lq, ymax = roll_uq), 
              alpha = 0.33, fill = cols[1]) +
  geom_hline(yintercept = 0.5, linetype = "dashed", 
             colour = cols[4], linewidth = 1) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  labs(
    title = "Changing Sentiment Towards a UK-EU Trade Deal",
    subtitle = glue("Rolling {width} days Since the Withdrawal Agreement"),
    x = NULL, y = "Positive Sentiment"
  )

p2 <- summary_df |> 
  ggplot(aes(web_date, n)) +
  geom_line(colour = cols[1]) +
  labs(x = "Weeks", y = "Article Count",
       caption = "Source: Guardian Newspaper")

p1 / p2 + 
  plot_layout(heights = c(2, 1))

used_here()
