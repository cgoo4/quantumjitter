# Post-render tidy-up: align generated URLs with canonical addresses
# - sitemap: index.html variants to trailing-slash directories, dedupe,
#   encode spaces, LICENSE.html to license/
# - HTML: internal hrefs to trailing-slash directories, encode spaces

sitemap <- "_site/sitemap.xml"
xml <- readLines(sitemap, encoding = "UTF-8", warn = FALSE)

xml <- gsub("<loc>(.*)/index\\.html</loc>", "<loc>\\1/</loc>", xml)
xml <- gsub(
  "<loc>https://www\\.quantumjitter\\.com/LICENSE\\.html</loc>",
  "<loc>https://www.quantumjitter.com/license/</loc>",
  xml
)
xml <- gsub("(?<=<loc>[^<]{0,200}) ", "%20", xml, perl = TRUE)

# Deduplicate URLs accumulated by incremental renders
xml <- xml[!duplicated(xml)]

writeLines(xml, sitemap, useBytes = TRUE)

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
