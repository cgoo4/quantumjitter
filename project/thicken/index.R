library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr", quiet = TRUE)
library(shiny)
library(bslib)
library(rvest)
library(scales)
library(httr2)
library(wesanderson)
library(paletteer)
library(ggfoundry)
library(usedthese)


theme_set(theme_bw())

pal_name <- "wesanderson::IsleofDogs1"

pal <- paletteer_d(pal_name)

display_palette(pal, pal_name)

# charts <-
#   tibble(
#     chart = read_html(str_c(
#       "https://en.wikipedia.org/wiki/",
#       "Category:Statistical_charts_and_diagrams"
#     )) |>
#       html_elements(".mw-category-group a") |>
#       html_text()
#   )

# pv <- \(article) {
#   request("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article") |>
#     req_url_path_append(
#       "en.wikipedia",
#       "all-access",
#       "user",
#       article,
#       "daily",
#       "2015070100",
#       str_c(format(today(), "%Y%m%d"), "00")
#     ) |>
#     req_perform() |>
#     resp_body_json(simplify = TRUE) |>
#     pluck("items") |>
#     as_tibble() |>
#     mutate(date = ymd(str_sub(timestamp, 1, 8))) |>
#     select(article, date, views)
# }

# ui <- page_sidebar(
#   theme = bs_theme(bootswatch = "simplex", primary = "#9986A5"),
#   title = "Plot Plotter",
#   sidebar(
#     open = "desktop",
#     card(
#       card_header("Options"),
#       card_body(
#         dateRangeInput("dates",
#           label = "Date range",
#           start = "2015-07-01",
#           end = NULL
#         ),
#         selectizeInput(
#           inputId = "article",
#           label = "Chart type",
#           choices = charts$chart,
#           selected = c(
#             "Violin plot",
#             "Dendrogram",
#             "Histogram",
#             "Pie chart",
#             "Q–Q plot",
#             "Error bar"
#           ),
#           options = list(maxItems = 8),
#           multiple = TRUE
#         ),
#         selectInput(
#           inputId = "scales",
#           label = "Fixed or free y-axis",
#           choices = c("Fixed" = "fixed", "Free" = "free"),
#           selected = "fixed"
#         ),
#         selectInput(
#           inputId = "log10",
#           label = "Log 10 or normal y-axis",
#           choices = c("Log 10" = "log10", "Normal" = "norm"),
#           selected = "log10"
#         )
#       )
#     )
#   ),
#   card(
#     full_screen = TRUE,
#     plotOutput("line")
#   )
# )

# server <- \(input, output, session) {
#   pv_df <- reactive({
#     req(input$article)
#     input$article |>
#       map(pv) |>
#       list_rbind() |>
#       mutate(article = str_replace_all(article, "_", " ")) |>
#       filter(date >= input$dates[1], date <= input$dates[2])
#   })
# 
#   output$line <- renderPlot({
#     p <- ggplot(
#       pv_df(),
#       aes(date, views, colour = article)
#     ) +
#       geom_line() +
#       geom_smooth(colour = pal[7]) +
#       scale_colour_manual(values = pal) +
#       facet_wrap(~article, nrow = 1, scales = input$scales) +
#       theme(
#         legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.margin = margin(1, 1, 1, 1, "cm")
#       ) +
#       labs(
#         x = NULL, y = NULL,
#         caption = "\nSource: Daily Wikipedia Article Page Views"
#       )
# 
#     switch(input$log10,
#       norm = p,
#       log10 = p + scale_y_log10(
#         labels = label_number(scale_cut = cut_short_scale())
#       )
#     )
#   })
# }
# 
# shinyApp(ui, server)

used_here()
