library(conflicted)
library(tidyverse)
conflicts_prefer(ggplot2::annotate, readtext::texts, quanteda::tokens, dplyr::filter)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(readtext)
library(glue)
library(scales)
library(gutenbergr)
library(usedthese)

conflict_scout()

set_theme(theme_bw(paper = "#ddd9d7", ink = "#181d1e"))

# One-time Convert PDF → text
# system2("pdftotext", args = c("-nopgbrk", "manuscript.pdf", "manuscript.txt"))

book_text <- readtext("manuscript.txt")

# Define a regular expression for your section headers
section_pattern <- 
  regex("\\b(Chapter\\s+\\d+|Prologue|Disclaimer|Acknowledgements)\\b")

# Split based on this pattern
sections <- str_split(book_text, pattern = section_pattern, simplify = FALSE)[[1]]

# Extract headers separately to keep them
headers <- str_extract_all(book_text, section_pattern, simplify = TRUE)

head_foot <- 
  regex("carl goodwin|\\n\\n\\n\\s*[0-9]+\\n\\n|vii|such an odd word to use")

# Combine headers and content into a tibble
book_split <- tibble(
  heading = str_to_title(headers),
  text = str_trim(sections[-1])  # -1 drop preamble before first heading
) |> 
  mutate(text = str_remove_all(text, head_foot) |> str_squish())

book_corp <- book_split |> 
  corpus(docid_field = "heading") 

book_toks <- book_corp |> 
  ntoken(split_hyphens = TRUE, remove_punct = TRUE) |> 
  as_tibble(rownames = "heading") |> 
  filter(str_starts(heading, "Cha|Pro")) |> 
  mutate(
    heading = fct_inorder(heading),  # Preserve factor order
    cum_toks = cumsum(value)
  )

word_count <- comma(sum(book_toks$value))

avg_words <- mean(book_toks$value)

book_toks |> 
  ggplot(aes(heading, value)) +
  geom_col(fill = "#82b777", colour = "#181d1e") +
  geom_hline(yintercept = avg_words, linetype = "dashed") +
  annotate("label", x = 15.5, y = avg_words, label = "Average") +
  labs(
    title = glue("Such an Odd Word to Use -- {word_count} Words"),
    x = NULL, y = "Words"
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# # x <- gutenberg_works()
# 
# books <- gutenberg_download(
#   c(11, 42, 2097, 19337, 64317),
#   meta_fields = c("title", "author")
#   ) |>
#   distinct() |>
#   mutate(front_matter = case_when(
#     author == "Carroll, Lewis" & row_number() <= 17 ~ 1,
#     author == "Doyle, Arthur Conan" & row_number() <= 17 ~ 1,
#     author == "Dickens, Charles" & row_number() <= 83 ~ 1,
#     author == "Stevenson, Robert Louis" & row_number() <= 22 ~ 1,
#     author == "Fitzgerald, F. Scott (Francis Scott)" & row_number() <= 22 ~ 1,
#     .default = 0
#   ), .by = title)
# 
# saveRDS(books, "books")

methods <- c("Flesch.Kincaid")

extract_title_phrase <- function(text) {
  words <- str_split(text, "\\s+")[[1]]
  
  # Clean punctuation but preserve letters, accents, apostrophes, dashes
  clean_words <- str_remove_all(words, "[^\\p{L}\\p{Pd}’'-]")

  allowed_lc <- c("the", "and")

  # Title-case detection with Unicode support via stringi
  is_title <- stringi::stri_detect_regex(clean_words,
                                pattern = "^[\\p{Lu}][\\p{Ll}’']*(?:-[\\p{Lu}][\\p{Ll}’']*)?$") |
              tolower(clean_words) %in% allowed_lc

  stop_index <- which(!is_title)[1]
  if (is.na(stop_index) || stop_index < 2) return(NA_character_)

  str_c(words[1:(stop_index - 1)], collapse = " ")
}

such <- book_split |> 
  rename(chapter = heading) |> 
  filter(str_starts(chapter, "Chapter|Prol")) |> 
  mutate(
    title = "Such an Odd Word to Use", 
    author = "Goodwin, Carl",
    chapter = if_else(chapter == "Prologue", 0, parse_number(chapter)),
    chapter_name = map_chr(text, extract_title_phrase),
    chapter_name = str_remove(chapter_name, "\\s+\\S+$"),
    chapter_name = str_remove(chapter_name, "As$|Amid$|In$|Over$"),
    chapter_name = if_else(chapter == 0, "Prologue", chapter_name)
    )

pattern <- "chapter|scene|the prologue|stave"

books_corp <- readRDS("books") |> 
  filter(front_matter != 1) |> 
  mutate(
    text = if_else(
      str_detect(text, "^[[:upper:]]{2}") & author == "Stevenson, Robert Louis",
      str_c("Chapter ", text), text
      ),
    text = if_else(
      str_count(str_squish(text), ".") <= 4 & title == "The Great Gatsby",
      str_c("Chapter ", str_squish(text)), text
      ),
    chapter = cumsum(str_starts(text, regex(pattern, ignore_case = TRUE))),
    .by = title
    ) |> 
  bind_rows(such) |> 
  summarise(
    text = str_c(text, collapse = " "), 
    .by = c("title", "author", "chapter")
    ) |> 
  mutate(doc_id = str_c(title, author, chapter, sep = "|")) |> 
  corpus()

books_toks <- books_corp |> 
  ntoken(split_hyphens = TRUE, remove_punct = TRUE) |> 
  as_tibble(rownames = "doc") |> 
  separate_wider_delim(doc, delim = "|", names = c("title", "author", "chapter")) |> 
  summarise(words = sum(value), .by = c("title", "author")) |> 
  mutate(
    title = str_c(title, " | ", author),
    words = label_number(
      accuracy = 0.1, 
      scale_cut = append(cut_short_scale(), 1, 1))(words)
    )

readability_df <- books_corp |> 
  textstat_readability(measure = methods) |>
  pivot_longer(-document) |>
  separate_wider_delim(
    document, delim = "|", names = c("title", "author", "chapter")
    )

readability_df |> 
  mutate(
    title = str_c(title, " | ", author), 
    fill = if_else(str_detect(title, "Such"), "#82b777", NA)
    ) |> 
  ggplot(aes(value, title)) +
  geom_boxplot(aes(fill = fill)) +
  geom_text(
    aes(x = -Inf, y = title, label = words), 
    size = 3, hjust = -0.3, data = books_toks
    ) +
  scale_fill_identity() +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  labs(
    x = "Score", y = NULL,
    title = "Readability Boxplot (Flesch-Kincaid)",
    subtitle = "Dot = Outlier Chapter"
    ) +
  theme(axis.text.y = element_text(size = 9))

read_df <- such |> 
  mutate(doc_id = str_c(chapter, " | ", chapter_name)) |> 
  corpus() |> 
  textstat_readability(measure = methods) |>
  pivot_longer(-document) |>
  mutate(document = fct_inorder(document)) |>
  rename(chapter = document) 

avgs <- read_df |> 
  summarise(avg_score = mean(value), .by = name)

read_df |> 
  ggplot(aes(chapter, value, group = name)) +
  geom_hline(
    aes(yintercept = avg_score), linetype = "dashed",
    colour = "grey80", data = avgs
    ) +
  geom_label(
    aes(
      "15 | Pancakes and Paranoia", avg_score, 
      label = glue("Average\n{round(avgs$avg_score, 1)}")
      ), 
    nudge_y = -1.5, size = 3, data = avgs
    ) +
  geom_line(colour = "#82b777") +
  geom_point() +
  coord_cartesian(ylim = c(0, 14), expand = TRUE) +
  labs(
    x = NULL, y = "Score", 
    title = "Such an Odd Word to Use",
    subtitle = "Chapter Readability (Flesch-Kincaid)"
    ) +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1),
    legend.position = "none"
  )

such |> 
  mutate(docid = str_c(chapter, " | ", chapter_name)) |> 
  corpus(docid_field = "docid") |> 
  tokens(split_hyphens = TRUE) |> 
  kwic(
    pattern = c("Alistair", "Lana", "Imogen"), 
    valuetype = "fixed"
    ) |>
  textplot_xray() +
  scale_x_continuous(
    breaks = c(0, 0.5, 1), 
    labels = c("Start", "Middle", "End")) +
  labs(
    x = "Relative Position in Chapter", y = "Chapter", 
    title = "Lexical Dispersion - Character Appearance"
    ) +
  theme(
    panel.spacing.x = unit(0.5, "lines"),
    legend.position = "none",
    strip.background = element_rect(fill = "#82b777", colour = "#181d1e"),
    panel.background = element_rect(fill = "#ddd9d7", colour = NA),
    plot.background = element_rect(fill = "#ddd9d7", colour = NA)
  )

used_here()
