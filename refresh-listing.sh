#!/usr/bin/env bash
# Refresh the project listing "Updated" dates and publish them.
# Usage: ./refresh-listing.sh [commit message]
set -euo pipefail
cd "$(dirname "$0")"

quarto render project/index.qmd

files=(
  _site/project/index.html
  _site/project/index-r.xml
  _site/project/index.xml
  _site/search.json
  _site/sitemap.xml
)

if git diff --quiet -- "${files[@]}"; then
  echo "Listing unchanged - nothing to publish."
  exit 0
fi

git add -- "${files[@]}"
git commit -m "${1:-Refresh project listing updated dates}"
git push
