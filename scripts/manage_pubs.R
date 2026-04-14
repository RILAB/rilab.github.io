args <- commandArgs(trailingOnly = TRUE)

if (!length(args)) {
  stop("Usage: Rscript scripts/manage_pubs.R <render|add> [doi] [preprint_doi]")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) NULL)
script_dir <- dirname(script_path %||% normalizePath("scripts/manage_pubs.R", winslash = "/", mustWork = TRUE))
repo_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)

generator_env <- new.env(parent = globalenv())
sys.source(file.path(repo_root, "scripts", "generate_pubs_from_doi.R"), envir = generator_env)

parse_pubs_yaml <- get("parse_pubs_yaml", envir = generator_env)
fetch_pub_record <- get("fetch_pub_record", envir = generator_env)
read_json_object <- get("read_json_object", envir = generator_env)
write_json_object <- get("write_json_object", envir = generator_env)
build_pub_row <- get("build_pub_row", envir = generator_env)
pubs_to_markdown <- get("pubs_to_markdown", envir = generator_env)

paths <- list(
  pubs_yaml = file.path(repo_root, "data", "pubs.yml"),
  pubs_rmd  = file.path(repo_root, "pubs.Rmd"),
  pubs_html = file.path(repo_root, "pubs.html"),
  cache     = file.path(repo_root, "data", "pubs_cache.json")
)

begin_marker <- "<!-- PUBS:BEGIN -->"
end_marker <- "<!-- PUBS:END -->"

ensure_support_files <- function() {
  if (!file.exists(paths$cache)) {
    write_json_object(paths$cache, list())
  }
}

read_lines_utf8 <- function(path) {
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

write_lines_utf8 <- function(path, lines) {
  writeLines(lines, path, useBytes = TRUE)
}

split_rmd <- function(path) {
  lines <- read_lines_utf8(path)
  begin_idx <- grep(begin_marker, lines, fixed = TRUE)
  end_idx <- grep(end_marker, lines, fixed = TRUE)

  if (length(begin_idx) != 1L || length(end_idx) != 1L || begin_idx >= end_idx) {
    stop("Could not find generated publications block markers in pubs.Rmd")
  }

  list(
    before = lines[seq_len(begin_idx)],
    after = lines[end_idx:length(lines)]
  )
}

write_generated_block <- function(body_text) {
  parts <- split_rmd(paths$pubs_rmd)
  body_lines <- if (nzchar(body_text)) strsplit(body_text, "\n", fixed = TRUE)[[1]] else character(0)
  write_lines_utf8(paths$pubs_rmd, c(parts$before, body_lines, parts$after))
}

load_cache <- function() {
  read_json_object(paths$cache)
}

save_cache <- function(cache) {
  write_json_object(paths$cache, cache)
}

ensure_cache_record <- function(cache, doi, refresh = FALSE, timeout_sec = 20) {
  if (!refresh && length(cache[[doi]])) {
    return(cache)
  }
  cache[[doi]] <- fetch_pub_record(doi, timeout_sec = timeout_sec)
  cache
}

render_html <- function() {
  rmarkdown::render(paths$pubs_rmd, output_file = paths$pubs_html, quiet = TRUE)
}

build_rows_from_state <- function(entries, cache, overrides) {
  rows <- lapply(entries, function(entry) {
    record <- cache[[entry$primary]]
    if (!length(record)) {
      stop("Missing cache record for DOI: ", entry$primary)
    }
    override <- overrides[[entry$primary]] %||% list()
    build_pub_row(entry, record, override)
  })

  rows[order(
    vapply(rows, `[[`, integer(1), "section_rank"),
    vapply(rows, `[[`, integer(1), "item_rank")
  )]
}

render_all <- function(timeout_sec = 20) {
  ensure_support_files()
  parsed <- parse_pubs_yaml(paths$pubs_yaml)
  entries <- parsed$entries
  overrides <- parsed$overrides
  cache <- load_cache()

  for (entry in entries) {
    cache <- ensure_cache_record(
      cache, entry$primary,
      refresh = TRUE, timeout_sec = timeout_sec
    )
  }

  save_cache(cache)
  rows <- build_rows_from_state(entries, cache, overrides)
  write_generated_block(pubs_to_markdown(rows))
  render_html()
}

# Add a new publication entry to pubs.yml, then re-render.
# The new entry is appended with only doi (and optionally preprint_doi).
# Additional overrides (lab_authors, first_authors, etc.) can be added
# manually to pubs.yml afterwards.
add_one <- function(primary_doi, preprint_doi = NA_character_,
                    timeout_sec = 20) {
  ensure_support_files()

  primary_doi <- trimws(primary_doi)
  preprint_doi <- trimws(preprint_doi %||% "")
  if (!nzchar(primary_doi)) stop("DOI must not be empty")

  parsed <- parse_pubs_yaml(paths$pubs_yaml)
  existing <- vapply(parsed$entries, `[[`, character(1), "primary")
  if (primary_doi %in% existing) {
    stop("DOI already present in pubs.yml: ", primary_doi)
  }

  # Append new entry to pubs.yml
  yml_lines <- readLines(paths$pubs_yaml, warn = FALSE, encoding = "UTF-8")
  new_entry <- if (nzchar(preprint_doi)) {
    c(sprintf("  - doi: %s", primary_doi),
      sprintf("    preprint_doi: %s", preprint_doi))
  } else {
    sprintf("  - doi: %s", primary_doi)
  }
  writeLines(c(yml_lines, new_entry), paths$pubs_yaml, useBytes = TRUE)

  cache <- load_cache()
  cache <- ensure_cache_record(
    cache, primary_doi,
    refresh = TRUE, timeout_sec = timeout_sec
  )
  save_cache(cache)

  parsed2 <- parse_pubs_yaml(paths$pubs_yaml)
  rows <- build_rows_from_state(parsed2$entries, cache, parsed2$overrides)
  write_generated_block(pubs_to_markdown(rows))
  render_html()
}

command <- args[[1]]
if (identical(command, "render")) {
  render_all()
} else if (identical(command, "add")) {
  if (!(length(args) %in% c(2L, 3L))) {
    stop("Usage: Rscript scripts/manage_pubs.R add <doi> [preprint_doi]")
  }
  add_one(args[[2]], if (length(args) == 3L) args[[3]] else NA_character_)
} else {
  stop("Unknown command: ", command)
}
