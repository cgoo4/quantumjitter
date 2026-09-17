# Post-render: align sitemap URLs with canonical tags by replacing
# index.html variants with their trailing-slash directory addresses

sitemap <- "_site/sitemap.xml"
xml <- readLines(sitemap, encoding = "UTF-8", warn = FALSE)

xml <- gsub("<loc>(.*)/index\\.html</loc>", "<loc>\\1/</loc>", xml)

writeLines(xml, sitemap, useBytes = TRUE)
