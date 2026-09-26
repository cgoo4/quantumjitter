# quantumjitter

Workspace of Quarto-based data projects under `project/`. User prefers tidyverse style (https://style.tidyverse.org) and MHRA prose style; R packages wrapped in braces, e.g. {rvest}.

## project/sets — "Where Clouds Cross"

Set-overlap analysis (Venn + UpSet plots) of UK G-Cloud services. Single document: `project/sets/index.qmd` (plus `index.R` extraction, `references.bib`, `feature.gif`). Renders cleanly as of 2026-09-26 via `quarto render` to `_site/project/sets/index.html`.

### Scraping target

- Base URL: `https://www.applytosupply.digitalmarketplace.service.gov.uk/g-cloud/search`
- Framework as of 2026-09-26: **G-Cloud 15** (version auto-extracted from `.app-search-result:first-child` via regex `G-Cloud \d\d`).
- Lots were restructured in G-Cloud 14; old `lot=cloud-hosting`/`cloud-software` URLs no longer resolve. Current lots: `iaas-and-paas` (1a), `isaas` (2a), `saas` (2b), `cloud-support` (3).
- Analysis deliberately focuses on **Lot 1a only** (`lot=iaas-and-paas`, label "Lot 1a") — user's choice; do not re-add other lots without asking.
- Categories are now a **nested taxonomy tree** of checkboxes on the lot page: `input[name="serviceCategories"]` with coded values (e.g. `a-201-301-401`); each input's sibling `label` holds the name and service count, e.g. "General purpose (376)".
- Only **leaf categories** are kept (an `is_leaf()` helper: a value is a leaf when no other value extends it with `-`), so parent roll-ups don't double-count.
- Search URL per category: `?page=<N>&serviceCategories=<value>&lot=iaas-and-paas` — note the category query must start with `&` (a leading `?` after `?page=` silently fails inside `possibly()`).
- Results: 30 per page under `#js-dm-live-search-results .govuk-link`; service IDs are 15-digit numbers in `/g-cloud/services/<id>` hrefs.
- Scraped via {rvest} + {mirai} (`daemons(10)`; `mirai_map()` over pages expanded with `tidyr::uncount(..., .id = "page")`, collected with `[.progress]` — note `x[.progress]` cannot sit on the RHS of the native pipe), results cached with `#| cache: true`. `scrape_ids` namespaces {rvest}/{stringr}/{tibble} calls so daemons don't need packages attached.

### Gotchas (hit during 2026-09 refactor)

- `readr::parse_number()` treats a hyphen as a number token: "Arm-based instances (170)" → NA. Use `str_extract(label, "\\d+")` instead.
- Column named `cat` collides with base function in `mutate()`; the column is named `category` — keep it that way.
- `select(lot:cat)`-style ranges can resolve tidyselect symbols to global variables (e.g. `lot <- "iaas-and-paas"`) rather than columns; avoid range selects around these names.
- The lot search page also contains many unrelated checkbox filter groups (socialValue, support features, etc.) — filter to `serviceCategories` only; some inputs have NA values/labels.

### Current data shape

- `data_df`: one row per service-category membership (lot, service_id, category, abbr); 4,053 rows, 35 leaf categories.
- Venn/UpSet selection: `GNP` (General purpose, 376), `CMO` (Compute optimised, 242), `OOB` (Object or Bucket, 85), `BRMT` (Bare metal, 206); the OOB ∩ BRMT intersection is used in the worked example.
- Abbreviations come from `abbreviate(cat, 3)` after removing "and" — can produce odd ones (e.g. `A-I`, `LGDMAA`).
- Packages: {tidyverse} (dplyr 1.2.x: uses `filter_out()`), {rvest}, {mirai}, {tictoc}, {glue}, {paletteer} (`wesanderson::Royal2`), {ggVennDiagram}, {ggupset}, {ggfoundry}, {usedthese}, {conflicted}.
- Refactor note (2026-09-26): intersection tables built with `summarise(str_flatten(sort(unique(.data[[col]]))))` instead of the old pivot_wider/unite/regex gymnastics; `dplyr::when_any()` is element-wise (like `|`), NOT a scalar `any()` replacement — don't use it in `is_leaf()`. `mori::share()` considered and skipped: nothing large crosses the process boundary.
- ggupset gotcha (2026-09-26): `scale_x_upset()` orders intersections and picks displayed sets by row frequency, so the plot data must stay one row per service (with `category = list(sort(unique(category)), .by = service_id)` — sorted/unique, else duplicate-order variants stack bars and scatter labels). Pre-slice top-N combos with a `top_combos()` helper (distinct/slice_max/semi_join on the list key) and call `scale_x_upset()` bare — its own `n_sets`/`n_intersections` truncation drops rows with ties broken alphabetically (it silently hid the 236-service "Integration software" bar).
