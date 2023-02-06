library(conflicted)
library(tidyverse)
conflict_prefer_all("dplyr")
library(rgeolocate)
library(R.utils)
library(leaflet)
library(rgdal)
library(wesanderson)
library(htmlwidgets)
library(usedthese)

conflict_scout()

theme_set(theme_bw())

(cols <- wes_palette("Darjeeling2"))

zip_file <- "world_shape_file.zip"
shape_file <- "TM_WORLD_BORDERS_SIMPL-0.3"

str_c("http://thematicmapping.org/downloads/", shape_file, ".zip") |>
  download.file(zip_file)

unzip(zip_file)

world_spdf <- readOGR(getwd(), shape_file, verbose = FALSE)

url <- "http://geolite.maxmind.com/download/geoip/database/GeoLite2-City.mmdb.gz"

file_name <- basename(url)

download.file(url, file_name)

gunzip(file_name, overwrite = TRUE)

stats <- read_csv("stats.csv")

ip_df <- map2(stats$IP, stats$Pages, \(x, y) {
  maxmind(
    x,
    "GeoLite2-City.mmdb",
    c(
      "country_name",
      "city_name",
      "longitude",
      "latitude",
      "region_name"
    )
  ) |>
    mutate(IP = x) |>
    rename(
      country = country_name,
      region = region_name,
      city = city_name
    ) |>
    mutate(
      Pages = y,
      Views = case_when(
        Pages < 500 ~ 1,
        Pages < 1000 ~ 2,
        Pages < 2000 ~ 3,
        .default = 4
      )
    )
}) |>
  list_rbind()

ip_df <- ip_df |>
  filter(!is.na(longitude) | !is.na(latitude)) |>
  arrange(Pages)

ip_df <- ip_df |> 
  mutate(Views = if_else(IP == "104.192.74.35", 2000, Views),
         Pages = if_else(IP == "104.192.74.35", 2000, Pages))

pal <-
  colorFactor(cols[c(2:5)],
    domain = c(1, 2, 3, 4)
  )

map1 <- leaflet(world_spdf) |> # World view
  addProviderTiles(providers$CartoDB.Positron,
    options = providerTileOptions(maxZoom = 21)
  ) |>
  setView(-30, 35, zoom = 2) |> # World view
  addPolygons(
    fillColor = cols[1],
    stroke = TRUE,
    fillOpacity = 1,
    color = cols[5],
    weight = 0.3,
    highlight = highlightOptions(
      weight = 3,
      color = cols[3],
      fillOpacity = 0.3,
      bringToFront = FALSE
    ),
    label = world_spdf@data$NAME,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal"),
      textsize = "12px"
    )
  ) |>
  addCircleMarkers(
    lng = ip_df$longitude,
    lat = ip_df$latitude,
    radius = ~ case_match(
      ip_df$Views,
      1 ~ 5,
      2 ~ 10,
      3 ~ 15,
      .default = 20
    ),
    fillColor = ~ pal(ip_df$Views),
    color = cols[5],
    weight = 1,
    fillOpacity = 0.7,
    popup = str_c(
      "<b>",
      ip_df$city,
      "</b>",
      "<br/>",
      ip_df$region,
      "<br/>",
      as.character(ip_df$Pages),
      " ",
      "page views"
    )
  ) |>
  addLegend(
    colors = cols[c(2:5)],
    labels = c("<500", "500+", "1,000+", "2,000+"),
    opacity = 1,
    title = "Page Views<br/>Oct-23 to Dec-31 2017",
    position = "bottomleft"
  )

saveWidget(map1, "world.html")

used_here()
