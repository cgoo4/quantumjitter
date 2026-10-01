## ---- libraries ----
library(conflicted)
library(tidyverse)
library(shiny)
library(bslib)
library(rvest)
library(scales)
library(httr2)
library(wesanderson)

conflict_prefer_all("dplyr", quiet = TRUE)

## ---- scrape ----
charts <-
  tibble(
    chart = read_html(str_c(
      "https://en.wikipedia.org/wiki/",
      "Category:Statistical_charts_and_diagrams"
    )) |>
      html_elements(".mw-category-group a") |>
      html_text()
  )

## ---- pageview ----
pv <- function(article, end_date = today()) {
  request("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article") |>
    req_url_path_append(
      "en.wikipedia",
      "all-access",
      "user",
      URLencode(article, reserved = TRUE),
      "daily",
      "2015070100",
      str_c(format(end_date, "%Y%m%d"), "00")
    ) |>
    req_perform() |>
    resp_body_json(simplify = TRUE) |>
    pluck("items") |>
    as_tibble() |>
    mutate(date = ymd(str_sub(timestamp, 1, 8))) |>
    select(article, date, views)
}

## ---- app-theme ----
theme_set(theme_bw())

pal <- wes_palette(8, name = "IsleofDogs1", type = "continuous")

## ---- ui ----
logo <- "logo.png"

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "simplex", primary = "#9986A5"),
  title = tags$span(
    tags$img(src = logo, height = "40px", alt = "Plot Plotter logo"),
    "Plot Plotter"
  ),
  tags$style(
    ".navbar {
      background-color: var(--bs-body-bg, #fff) !important;
      color: var(--bs-body-color) !important;
    }
    .navbar .container-fluid {
      justify-content: center !important;
    }"
  ),
  sidebar = sidebar(
    open = list(desktop = "open", mobile = "always-above"),
    card(
      card_header("Options"),
      card_body(
        dateRangeInput("dates",
          label = "Date range",
          start = "2015-07-01",
          end = NULL
        ),
        selectizeInput(
          inputId = "article",
          label = "Chart type",
          choices = charts$chart,
          selected = c(
            "Violin plot",
            "Dendrogram",
            "Histogram",
            "Pie chart",
            "Q–Q plot",
            "Error bar"
          ),
          options = list(maxItems = 8),
          multiple = TRUE
        ),
        selectInput(
          inputId = "scales",
          label = "Fixed or free y-axis",
          choices = c("Fixed" = "fixed", "Free" = "free"),
          selected = "fixed"
        ),
        selectInput(
          inputId = "log10",
          label = "Log 10 or normal y-axis",
          choices = c("Log 10" = "log10", "Normal" = "norm"),
          selected = "log10"
        )
      )
    )
  ),
  card(
    full_screen = TRUE,
    plotOutput("line")
  )
)

## ---- server ----
server <- function(input, output, session) {
  pv_cached <- memoise::memoise(
    pv,
    cache = cachem::cache_mem(
      max_size = 32 * 1024^2,
      max_age = 3600
    )
  )

  pv_history <- reactive({
    req(input$article)
    end_date <- today()
    input$article |>
      map(pv_cached, end_date = end_date) |>
      list_rbind() |>
      mutate(article = str_replace_all(article, "_", " "))
  })

  pv_df <- reactive({
    pv_history() |>
      filter(date >= input$dates[1], date <= input$dates[2])
  })

  output$line <- renderPlot({
    p <- ggplot(
      pv_df(),
      aes(date, views, colour = article)
    ) +
      geom_line() +
      geom_smooth(colour = pal[7]) +
      scale_colour_manual(values = pal) +
      facet_wrap(~article, nrow = 1, scales = input$scales) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(1, 1, 1, 1, "cm")
      ) +
      labs(
        x = NULL, y = NULL,
        caption = "\nSource: Daily Wikipedia Article Page Views"
      )

    switch(input$log10,
      norm = p,
      log10 = p + scale_y_log10(
        labels = label_number(scale_cut = cut_short_scale())
      )
    )
  })
}

shinyApp(ui, server)
