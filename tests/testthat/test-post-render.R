# Tests for post-render.R's sitemap repair. post-render.R guards its
# side effects with sys.nframe() == 0, so source()ing it here is safe.

# Locate the repository root (this is a Quarto site, not an R package,
# so testthat's package-root assumption does not apply)
repo_root <- getwd()
while (!file.exists(file.path(repo_root, "_quarto.yml"))) {
  repo_root <- dirname(repo_root)
}
source(file.path(repo_root, "post-render.R"), local = TRUE)

site_url <- "https://www.quantumjitter.com/"

# Fixture: a rendered site with four pages plus a 404 page that does
# carry a canonical tag (and must still be excluded)
make_fixture <- function() {
  # Defer cleanup to the calling test, not this function's return
  root <- withr::local_tempdir(.local_envir = parent.frame())
  site_dir <- file.path(root, "_site")
  for (page in c("alpha", "beta", "gamma", "delta")) {
    dir.create(file.path(site_dir, "blog", page), recursive = TRUE)
    writeLines(
      paste0(
        "<!DOCTYPE html><html><head>",
        '<link rel="canonical" href="',
        site_url,
        "blog/",
        page,
        '/">',
        "</head><body></body></html>"
      ),
      file.path(site_dir, "blog", page, "index.html")
    )
  }
  writeLines(
    paste0(
      "<!DOCTYPE html><html><head>",
      '<link rel="canonical" href="',
      site_url,
      '404.html">',
      "</head><body>404</body></html>"
    ),
    file.path(site_dir, "404.html")
  )
  list(
    root = root,
    site_dir = site_dir,
    sitemap_path = file.path(site_dir, "sitemap.xml")
  )
}

expected_locs <- sort(paste0(
  site_url,
  "blog/",
  c("alpha", "beta", "delta", "gamma"),
  "/"
))

# Sitemap inputs that must have no effect on the result: a truncated
# inventory, entries with malformed dates, entries with inherited
# (plausible-looking but wrong) dates, and no file at all
sitemap_variants <- function() {
  url <- function(path) {
    paste0(site_url, "blog/", path, "/")
  }
  header <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">'
  )
  list(
    # Only a subset of the pages, undated — as Quarto's lossy reader
    # would leave it
    truncated = c(
      header,
      paste0("  <url>\n    <loc>", url("alpha"), "</loc>\n  </url>"),
      "</urlset>"
    ),
    # Same subset with a malformed date
    bad_dates = c(
      header,
      paste0(
        "  <url>\n    <loc>",
        url("alpha"),
        "</loc>\n",
        "    <lastmod>not-a-date</lastmod>\n  </url>"
      ),
      "</urlset>"
    ),
    # Same subset with plausible-looking dates inherited from a
    # neighbouring entry
    inherited_dates = c(
      header,
      paste0(
        "  <url>\n    <loc>",
        url("alpha"),
        "</loc>\n",
        "    <lastmod>2024-06-28T17:34:41.938Z</lastmod>\n  </url>"
      ),
      "</urlset>"
    ),
    absent = NULL
  )
}

test_that("inventory is complete, sorted, and dateless", {
  fx <- make_fixture()
  out <- repair_sitemap(fx$sitemap_path, fx$site_dir, site_url)
  expect_setequal(out$loc, expected_locs)
  expect_equal(out$loc, expected_locs) # sorted
  expect_false("lastmod" %in% names(out))
  contents <- paste(readLines(fx$sitemap_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("<lastmod>", contents, fixed = TRUE))
})

test_that("truncated, malformed-date and inherited-date inputs cannot affect the inventory", {
  fx <- make_fixture()
  variants <- sitemap_variants()
  results <- list()
  for (name in names(variants)) {
    if (is.null(variants[[name]])) {
      unlink(fx$sitemap_path)
    } else {
      writeLines(variants[[name]], fx$sitemap_path)
    }
    results[[name]] <- repair_sitemap(fx$sitemap_path, fx$site_dir, site_url)
  }
  expect_identical(results$truncated, results$bad_dates)
  expect_identical(results$truncated, results$inherited_dates)
  expect_identical(results$truncated, results$absent)

  # Each variant yields the same file bytes as the reference repair
  reference <- file.path(fx$root, "reference.xml")
  unlink(fx$sitemap_path)
  write_sitemap(
    repair_sitemap(fx$sitemap_path, fx$site_dir, site_url)$loc,
    reference
  )
  expect_identical(
    readBin(fx$sitemap_path, "raw", n = 1e6),
    readBin(reference, "raw", n = 1e6)
  )
})

test_that("repeated execution is byte-identical", {
  fx <- make_fixture()
  repair_sitemap(fx$sitemap_path, fx$site_dir, site_url)
  first <- readBin(fx$sitemap_path, "raw", n = 1e6)
  repair_sitemap(fx$sitemap_path, fx$site_dir, site_url)
  second <- readBin(fx$sitemap_path, "raw", n = 1e6)
  expect_identical(first, second)
})

test_that("404 page with a canonical tag stays excluded", {
  fx <- make_fixture()
  out <- repair_sitemap(fx$sitemap_path, fx$site_dir, site_url)
  expect_false(paste0(site_url, "404.html") %in% out$loc)
})
