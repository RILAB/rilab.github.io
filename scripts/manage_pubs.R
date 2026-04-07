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

parse_doi_file <- get("parse_doi_file", envir = generator_env)
fetch_pub_record <- get("fetch_pub_record", envir = generator_env)
read_json_object <- get("read_json_object", envir = generator_env)
write_json_object <- get("write_json_object", envir = generator_env)
build_pub_row <- get("build_pub_row", envir = generator_env)
pubs_to_markdown <- get("pubs_to_markdown", envir = generator_env)

paths <- list(
  doi = file.path(repo_root, "doi.txt"),
  pubs_rmd = file.path(repo_root, "pubs.Rmd"),
  pubs_html = file.path(repo_root, "pubs.html"),
  cache = file.path(repo_root, "data", "pubs_cache.json"),
  overrides = file.path(repo_root, "data", "pubs_overrides.yml")
)

begin_marker <- "<!-- PUBS:BEGIN -->"
end_marker <- "<!-- PUBS:END -->"

ensure_support_files <- function() {
  if (!file.exists(paths$overrides)) {
    writeLines(character(0), paths$overrides, useBytes = TRUE)
  }
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

load_overrides <- function() {
  if (!file.exists(paths$overrides) || !length(readLines(paths$overrides, warn = FALSE, encoding = "UTF-8"))) {
    return(list())
  }
  yaml::read_yaml(paths$overrides)
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

write_doi_entries <- function(entries) {
  lines <- vapply(entries, function(entry) {
    if (!is.na(entry$preprint) && nzchar(entry$preprint)) {
      paste(entry$primary, entry$preprint)
    } else {
      entry$primary
    }
  }, character(1))
  write_lines_utf8(paths$doi, unique(lines))
}

render_all <- function(timeout_sec = 20) {
  ensure_support_files()
  entries <- parse_doi_file(paths$doi)
  cache <- load_cache()
  overrides <- load_overrides()

  for (entry in entries) {
    cache <- ensure_cache_record(cache, entry$primary, refresh = TRUE, timeout_sec = timeout_sec)
  }

  save_cache(cache)
  rows <- build_rows_from_state(entries, cache, overrides)
  write_generated_block(pubs_to_markdown(rows))
  render_html()
}

add_one <- function(primary_doi, preprint_doi = NA_character_, timeout_sec = 20) {
  ensure_support_files()

  primary_doi <- trimws(primary_doi)
  preprint_doi <- trimws(preprint_doi %||% "")
  if (!nzchar(primary_doi)) {
    stop("DOI must not be empty")
  }

  entries <- parse_doi_file(paths$doi)
  existing_primary <- vapply(entries, `[[`, character(1), "primary")
  if (primary_doi %in% existing_primary) {
    stop("DOI already present in doi.txt: ", primary_doi)
  }

  entries[[length(entries) + 1L]] <- list(
    line = if (nzchar(preprint_doi)) paste(primary_doi, preprint_doi) else primary_doi,
    primary = primary_doi,
    preprint = if (nzchar(preprint_doi)) preprint_doi else NA_character_
  )

  cache <- load_cache()
  overrides <- load_overrides()
  cache <- ensure_cache_record(cache, primary_doi, refresh = TRUE, timeout_sec = timeout_sec)

  save_cache(cache)
  write_doi_entries(entries)

  rows <- build_rows_from_state(entries, cache, overrides)
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
