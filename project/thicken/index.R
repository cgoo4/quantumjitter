library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(shiny)
library(gridlayout)
library(rvest)
library(scales)
library(pageviews)
library(bslib)
library(paletteer)
library(ggfoundry)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

pal_name <- "wesanderson::IsleofDogs1"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

charts <-
  tibble(
    chart = read_html(str_c(
      "https://en.wikipedia.org/wiki/",
      "Category:Statistical_charts_and_diagrams"
    )) |>
      html_elements(".mw-category-group a") |>
      html_text()
  )

pv <- \(article) {
  article_pageviews(
    project = "en.wikipedia",
    article,
    user_type = "user",
    start = "2015070100",
    end = today()
  )
}

ui <- grid_page(
  theme = bs_theme(version = 5, bootswatch = "simplex"),
  layout = c(
    "header  header",
    "sidebar line "
  ),
  row_sizes = c(
    "100px",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    item_alignment = "top",
    title = "Options",
    item_gap = "13px",
    dateRangeInput("dates",
      label = "Date range",
      start = "2015-07-01",
      end = NULL
    ),
    selectizeInput(
      inputId = "article",
      label = "Chart type",
      choices = charts,
      selected = c(
        "Violin plot",
        "Dendrogram",
        "Histogram",
        "Pie chart"
      ),
      options = list(maxItems = 6),
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
  ),
  grid_card_text(
    area = "header",
    content = "   Plot Plotter   ",
    alignment = "center",
    is_title = FALSE,
    icon = "logo1.png",
    img_height = 30
  ),
  grid_card_plot(area = "line")
)

server <- \(input, output, session) {
  subsetr <- reactive({
    req(input$article)
    pageviews <- map(input$article, pv) |>
      mutate(
        date = ymd(date),
        article = str_replace_all(article, "_", " ")
      ) |>
      filter(date >= input$dates[1], date <= input$dates[2])
  }) |> 
    list_rbind()

  output$line <- renderPlot({
    p <- ggplot(
      subsetr(),
      aes(date,
        views,
        colour = article
      )
    ) +
      geom_line() +
      scale_colour_manual(values = pal) +
      geom_smooth(colour = "lightgrey") +
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

used_here()
