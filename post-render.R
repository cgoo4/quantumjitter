# Post-render tidy-up: align generated URLs with canonical addresses
# - sitemap: normalise URLs to canonical addresses, drop the 404 page,
#   and deduplicate complete <url> records accumulated by incremental
#   renders. Deduplication is record-wise, never line-wise: repeated
#   structural tags and dates across entries are legitimate XML.
# - HTML: internal hrefs to trailing-slash directories, encode spaces

library(xml2)

sitemap <- "_site/sitemap.xml"
site_url <- "https://www.quantumjitter.com/"

# Canonical URL conventions for this site
normalize_loc <- function(loc) {
  loc <- sub("/index\\.html$", "/", loc)
  loc <- sub(
    "^https://www\\.quantumjitter\\.com/LICENSE\\.html$",
    paste0(site_url, "license/"),
    loc
  )
  gsub(" ", "%20", loc, fixed = TRUE)
}

escape_xml <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

parse_sitemap <- function(path) {
  doc <- read_xml(path)
  url_nodes <- xml_find_all(doc, "./*[local-name() = 'url']")
  locs <- xml_text(xml_find_first(url_nodes, "./*[local-name() = 'loc']"))
  lastmods <- xml_text(xml_find_first(
    url_nodes,
    "./*[local-name() = 'lastmod']"
  ))
  tibble::tibble(
    loc = normalize_loc(locs),
    lastmod = dplyr::if_else(lastmods %in% c("", NA), NA_character_, lastmods)
  )
}

# Parse W3C dates/times to UTC instants. Accepts date-only values and
# timestamps with a Z suffix or an explicit UTC offset; anything else
# (including NA) is invalid and yields NA
parse_lastmod <- function(lastmod) {
  lastmod[is.na(lastmod)] <- ""
  out <- as.POSIXct(rep(NA_character_, length(lastmod)), tz = "UTC")

  is_date_only <- grepl("^\\d{4}-\\d{2}-\\d{2}$", lastmod)
  out[is_date_only] <- as.POSIXct(
    lastmod[is_date_only],
    format = "%Y-%m-%d",
    tz = "UTC"
  )

  # Normalise offsets to +HHMM for %z, and treat a Z suffix as UTC
  with_offset <- !is_date_only & grepl("[+-]\\d{2}(:?\\d{2})?$", lastmod)
  zulu <- !is_date_only & !with_offset & grepl("Z$", lastmod)
  out[zulu] <- as.POSIXct(
    sub("Z$", "", lastmod[zulu]),
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
  offset_forms <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", lastmod[with_offset])
  out[with_offset] <- as.POSIXct(
    offset_forms,
    format = "%Y-%m-%dT%H:%M:%OS%z",
    tz = "UTC"
  )
  out
}

# One row per canonical URL. Where duplicates exist, retain the latest
# valid lastmod by chronological comparison rather than input order.
# Invalid lastmod values are cleared, so URLs with no valid date are
# written undated rather than invented
dedupe_sitemap <- function(sitemap_df) {
  sitemap_df |>
    dplyr::mutate(
      lastmod_time = parse_lastmod(lastmod),
      lastmod = dplyr::if_else(is.na(lastmod_time), NA_character_, lastmod)
    ) |>
    dplyr::filter(loc != paste0(site_url, "404.html")) |>
    dplyr::slice_max(lastmod_time, n = 1, by = loc, with_ties = FALSE) |>
    dplyr::arrange(loc) |>
    dplyr::select(loc, lastmod)
}

write_sitemap <- function(sitemap_df, path) {
  entry <- function(loc, lastmod) {
    lastmod_line <- if (!is.na(lastmod)) {
      paste0("    <lastmod>", escape_xml(lastmod), "</lastmod>\n")
    } else {
      ""
    }
    paste0(
      "  <url>\n    <loc>",
      escape_xml(loc),
      "</loc>\n",
      lastmod_line,
      "  </url>"
    )
  }
  body <- paste(
    mapply(entry, sitemap_df$loc, sitemap_df$lastmod),
    collapse = "\n"
  )
  writeLines(
    c(
      '<?xml version="1.0" encoding="UTF-8"?>',
      '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
      body,
      "</urlset>"
    ),
    path,
    useBytes = TRUE
  )
}

write_sitemap(dedupe_sitemap(parse_sitemap(sitemap)), sitemap)

# Rewrite internal hrefs pointing at index.html files and encode spaces
html_files <- list.files(
  "_site",
  pattern = "\\.html$",
  recursive = TRUE,
  full.names = TRUE
)
for (f in html_files) {
  html <- readLines(f, encoding = "UTF-8", warn = FALSE)
  html <- gsub('href="([^"]*)/index\\.html"', 'href="\\1/"', html)
  html <- gsub('href="([^" ]*) ([^"]*)"', 'href="\\1%20\\2"', html)
  writeLines(html, f, useBytes = TRUE)
}
