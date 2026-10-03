# Post-render tidy-up: align generated URLs with canonical addresses
# - sitemap: emit the complete canonical URL inventory from the rendered
#   pages' canonical link tags, without lastmod values. Quarto's
#   incremental renderer rebuilds sitemap.xml from a lossy in-memory
#   state (readSitemap() drops undated entries and lets later entries
#   inherit earlier dates), so the existing sitemap is neither a
#   reliable source of the inventory nor of its dates.
# - HTML: internal hrefs to trailing-slash directories, encode spaces

library(xml2)

sitemap_path <- "_site/sitemap.xml"
site_dir <- "_site"
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

# Canonical href from a rendered page's <link rel="canonical"> tag;
# NA when the page has none
canonical_loc <- function(path) {
  doc <- xml2::read_html(path)
  href <- xml2::xml_text(
    xml2::xml_find_first(doc, "//link[@rel='canonical']/@href")
  )
  if (is.na(href)) NA_character_ else href
}

# Complete canonical URL inventory from the rendered pages, sorted and
# deduplicated. Canonicals pointing outside the site are excluded
build_inventory <- function(site_dir, site_url) {
  html_files <- list.files(
    site_dir,
    pattern = "\\.html$",
    recursive = TRUE,
    full.names = TRUE
  )
  tibble::tibble(file = html_files) |>
    dplyr::mutate(loc = purrr::map_chr(file, canonical_loc)) |>
    dplyr::mutate(loc = normalize_loc(loc)) |>
    dplyr::filter(!is.na(loc), startsWith(loc, site_url)) |>
    dplyr::distinct(loc) |>
    dplyr::arrange(loc)
}

# Rewrite sitemap.xml with the canonical inventory. The existing file
# is deliberately ignored: whatever URLs or dates it holds cannot be
# trusted after an incremental render
repair_sitemap <- function(sitemap_path, site_dir, site_url) {
  inventory <- build_inventory(site_dir, site_url) |>
    dplyr::filter(loc != paste0(site_url, "404.html"))
  write_sitemap(inventory$loc, sitemap_path)
  inventory
}

write_sitemap <- function(locs, path) {
  body <- paste(
    paste0(
      "  <url>\n    <loc>",
      escape_xml(locs),
      "</loc>\n  </url>"
    ),
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

if (sys.nframe() == 0L) {
  repair_sitemap(sitemap_path, site_dir, site_url)

  # Rewrite internal hrefs pointing at index.html files and encode spaces
  html_files <- list.files(
    site_dir,
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
}
