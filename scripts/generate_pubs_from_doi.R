parse_doi_file <- function(doi_file) {
  if (!file.exists(doi_file)) {
    stop(sprintf("DOI file not found: %s", doi_file))
  }

  raw_lines <- readLines(doi_file, warn = FALSE, encoding = "UTF-8")
  doi_lines <- trimws(raw_lines)
  doi_lines <- doi_lines[nzchar(doi_lines) & !grepl("^#", doi_lines)]

  seen <- new.env(parent = emptyenv())
  entries <- list()

  for (line in doi_lines) {
    if (exists(line, envir = seen, inherits = FALSE)) next
    assign(line, TRUE, envir = seen)

    parts <- strsplit(line, "[[:space:]]+")[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    if (!length(parts)) next

    entries[[length(entries) + 1L]] <- list(
      line = line,
      primary = parts[[1]],
      preprint = if (length(parts) >= 2L) parts[[2]] else NA_character_
    )
  }

  entries
}

crossref_work <- function(doi) {
  safe_doi <- URLencode(doi, reserved = TRUE)
  url <- sprintf("https://api.crossref.org/works/%s", safe_doi)

  con <- url(url, open = "rb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  txt <- readLines(con, warn = FALSE)
  obj <- jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = TRUE)
  obj$message
}

datacite_work <- function(doi) {
  safe_doi <- URLencode(doi, reserved = TRUE)
  url <- sprintf("https://api.datacite.org/dois/%s", safe_doi)

  con <- url(url, open = "rb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  txt <- readLines(con, warn = FALSE)
  obj <- jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = TRUE)
  attrs <- obj$data$attributes

  title_vals <- attrs$titles$title
  if (is.null(title_vals)) title_vals <- character(0)

  descs <- attrs$descriptions
  abstract_val <- character(0)
  if (!is.null(descs) && NROW(descs)) {
    desc_type <- tolower(as.character(descs$descriptionType))
    idx <- which(desc_type %in% c("abstract", "other"))
    if (!length(idx)) idx <- seq_len(NROW(descs))
    abstract_val <- as.character(descs$description[idx[1]])
  }

  creators <- attrs$creators
  author_df <- NULL
  if (!is.null(creators) && NROW(creators)) {
    author_df <- data.frame(
      given = if ("givenName" %in% names(creators)) creators$givenName else NA_character_,
      family = if ("familyName" %in% names(creators)) creators$familyName else NA_character_,
      name = if ("name" %in% names(creators)) creators$name else NA_character_,
      stringsAsFactors = FALSE
    )
  }

  pub_year <- suppressWarnings(as.integer(attrs$publicationYear))
  if (is.na(pub_year) && !is.null(attrs$published) && nzchar(attrs$published)) {
    pub_year <- suppressWarnings(as.integer(substr(as.character(attrs$published), 1, 4)))
  }

  list(
    title = title_vals,
    abstract = abstract_val,
    author = author_df,
    issued = list(`date-parts` = list(c(pub_year))),
    created = list(`date-parts` = list(c(pub_year))),
    `container-title` = if (!is.null(attrs$publisher)) attrs$publisher else character(0)
  )
}

fetch_metadata_work <- function(doi) {
  tryCatch(
    crossref_work(doi),
    error = function(e) {
      message("Crossref lookup failed; trying DataCite: ", doi)
      datacite_work(doi)
    }
  )
}

clean_text <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("")
  x <- as.character(x[[1]])
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

get_year <- function(msg) {
  year <- NA_integer_
  if (!is.null(msg$issued$`date-parts`) && length(msg$issued$`date-parts`)) {
    year <- suppressWarnings(as.integer(msg$issued$`date-parts`[[1]][1]))
  }
  if (is.na(year) && !is.null(msg$created$`date-parts`) && length(msg$created$`date-parts`)) {
    year <- suppressWarnings(as.integer(msg$created$`date-parts`[[1]][1]))
  }
  year
}

is_preprint_record <- function(doi, msg) {
  # 10.1101 is Cold Spring Harbor's prefix — only treat as preprint when the
  # path is purely numeric (bioRxiv/medRxiv style), not CSH journal DOIs
  # like Genome Research (gr.*), Genes & Dev (gad.*), etc.
  if (grepl("^10\\.1101/[0-9]", doi, ignore.case = TRUE)) return(TRUE)
  # 10.64898 is a newer bioRxiv DOI prefix (same date-based path format)
  if (grepl("^10\\.64898/[0-9]", doi, ignore.case = TRUE)) return(TRUE)
  ct <- tolower(paste(unlist(msg$`container-title`), collapse = " "))
  grepl("biorxiv|medrxiv|arxiv|preprint", ct)
}

normalize_author_rows <- function(author_df) {
  if (is.null(author_df) || !NROW(author_df)) return(list())

  lapply(seq_len(NROW(author_df)), function(i) {
    list(
      given = trimws(as.character(author_df$given[[i]])),
      family = trimws(as.character(author_df$family[[i]])),
      name = trimws(as.character(author_df$name[[i]]))
    )
  })
}

fetch_pub_record <- function(doi, timeout_sec = 20) {
  old_timeout <- getOption("timeout")
  options(timeout = timeout_sec)
  on.exit(options(timeout = old_timeout), add = TRUE)

  msg <- fetch_metadata_work(doi)

  list(
    doi = doi,
    title = clean_text(msg$title),
    abstract = {
      abs <- clean_text(msg$abstract)
      if (nzchar(abs)) abs else "Abstract unavailable."
    },
    year = {
      year <- get_year(msg)
      if (is.na(year)) 0L else as.integer(year)
    },
    is_preprint = is_preprint_record(doi, msg),
    container_title = clean_text(msg$`container-title`),
    authors = normalize_author_rows(msg$author)
  )
}

read_json_object <- function(path) {
  if (!file.exists(path)) return(list())
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

write_json_object <- function(path, obj) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(obj, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
}

initials_name <- function(author) {
  given <- trimws(author$given %||% "")
  family <- trimws(author$family %||% "")
  fallback_name <- trimws(author$name %||% "")

  if (nzchar(family)) {
    given_parts <- unlist(strsplit(given, "[[:space:]-]+"))
    given_parts <- given_parts[nzchar(given_parts)]
    initials <- if (length(given_parts)) paste0(substr(given_parts, 1, 1), ".", collapse = " ") else ""
    return(trimws(paste(initials, family)))
  }

  name_parts <- unlist(strsplit(fallback_name, "[[:space:]]+"))
  name_parts <- name_parts[nzchar(name_parts)]
  if (length(name_parts) >= 2L) {
    family_part <- name_parts[[length(name_parts)]]
    given_parts <- name_parts[-length(name_parts)]
    initials <- paste0(substr(given_parts, 1, 1), ".", collapse = " ")
    return(trimws(paste(initials, family_part)))
  }

  fallback_name
}

full_name <- function(author) {
  given <- trimws(author$given %||% "")
  family <- trimws(author$family %||% "")
  fallback_name <- trimws(author$name %||% "")
  name <- trimws(paste(given, family))
  if (!nzchar(name)) fallback_name else name
}

author_matches <- function(spec, author) {
  spec_norm <- trimws(as.character(spec))
  variants <- c(
    initials_name(author),
    full_name(author),
    trimws(author$family %||% ""),
    trimws(author$name %||% "")
  )
  spec_norm %in% variants
}

annotate_author <- function(author, override) {
  name <- initials_name(author)

  if (length(override$author_overrides) && nzchar(override$author_overrides[[name]] %||% "")) {
    name <- override$author_overrides[[name]]
  }

  if (length(override$first_authors) && any(vapply(override$first_authors, author_matches, logical(1), author = author))) {
    name <- paste0(name, "<sup>&ast;</sup>")
  }

  if (length(override$corresponding_authors) && any(vapply(override$corresponding_authors, author_matches, logical(1), author = author))) {
    name <- paste0(name, "<sup>&dagger;</sup>")
  }

  if (length(override$bold_authors) && any(vapply(override$bold_authors, author_matches, logical(1), author = author))) {
    name <- sprintf("**%s**", name)
  }

  name
}

format_author_list <- function(record, override) {
  if (length(override$author_display)) {
    authors <- unlist(override$author_display, use.names = FALSE)
  } else {
    authors <- vapply(record$authors, annotate_author, character(1), override = override)
  }

  authors <- authors[nzchar(authors)]
  if (!length(authors)) return("Author list unavailable")

  if (length(authors) <= 9L) {
    return(paste(authors, collapse = ", "))
  }

  bold_specs <- override$bold_authors %||% list()
  included_middle <- character(0)
  if (!length(override$author_display) && length(bold_specs)) {
    hidden_idx <- seq.int(5L, length(record$authors) - 4L)
    if (length(hidden_idx)) {
      hidden_authors <- record$authors[hidden_idx]
      hidden_labels <- authors[hidden_idx]
      keep_hidden <- vapply(seq_along(hidden_authors), function(i) {
        any(vapply(bold_specs, author_matches, logical(1), author = hidden_authors[[i]]))
      }, logical(1))
      included_middle <- hidden_labels[keep_hidden]
      included_middle <- unique(included_middle[nzchar(included_middle)])
    }
  }

  omitted_count <- length(authors) - 8L
  omitted_label <- if (length(included_middle)) {
    sprintf(
      "...[%d authors including %s]...",
      omitted_count,
      paste(included_middle, collapse = ", ")
    )
  } else {
    sprintf("...[%d authors]...", omitted_count)
  }

  shown <- c(
    authors[1:4],
    omitted_label,
    authors[(length(authors) - 3L):length(authors)]
  )
  paste(shown, collapse = ", ")
}

record_section <- function(record, override) {
  if (nzchar(override$section %||% "")) {
    return(as.character(override$section))
  }
  if (isTRUE(record$is_preprint)) "Preprints" else as.character(record$year)
}

record_order <- function(section, record, entry) {
  if (identical(section, "Preprints")) {
    return(list(section_rank = 0L, item_rank = -as.integer(record$year)))
  }

  section_num <- suppressWarnings(as.integer(section))
  if (is.na(section_num)) section_num <- 0L
  list(section_rank = 10000L - section_num, item_rank = -as.integer(record$year))
}

build_pub_row <- function(entry, record, override = list()) {
  title <- override$title %||% record$title
  abstract <- override$abstract %||% record$abstract
  section <- record_section(record, override)
  authors <- format_author_list(record, override)
  preprint_doi <- override$preprint_doi %||% entry$preprint
  title_link_doi <- override$title_doi %||% entry$primary
  preprint_link <- if (!is.na(preprint_doi) && nzchar(preprint_doi)) {
    sprintf(" [[preprint](https://doi.org/%s)]", preprint_doi)
  } else {
    ""
  }

  details <- paste0(
    "<details>\n",
    "<summary> [", title, "](https://doi.org/", title_link_doi, ")", preprint_link, "  \n",
    authors, "\n",
    "</summary>\n",
    "<p style=\"margin-left: 30px\">\n",
    if (isTRUE(override$hide_abstract)) "Abstract hidden." else abstract,
    "\n",
    "</p></details><p></p>"
  )

  ord <- record_order(section, record, entry)

  list(
    doi = entry$primary,
    section = section,
    section_rank = ord$section_rank,
    item_rank = ord$item_rank,
    details_html = details
  )
}

pubs_to_markdown <- function(rows) {
  if (!length(rows)) return("")

  section_names <- unique(vapply(rows, `[[`, character(1), "section"))
  out <- character(0)

  for (section in section_names) {
    subset <- rows[vapply(rows, function(x) identical(x$section, section), logical(1))]
    subset <- subset[order(vapply(subset, `[[`, integer(1), "item_rank"))]
    out <- c(out, sprintf("### %s", section), "")
    out <- c(out, vapply(subset, `[[`, character(1), "details_html"), "")
  }

  paste(out, collapse = "\n")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Parse the unified pubs.yml file.
# Returns list(entries = [...], overrides = list(doi = override, ...))
# where each entry has $primary and $preprint, and each override has
# $bold_authors (defaults merged with per-pub lab_authors), $first_authors,
# $corresponding_authors, and any other supported override fields.
parse_pubs_yaml <- function(pubs_file) {
  if (!file.exists(pubs_file)) {
    stop(sprintf("Publications YAML file not found: %s", pubs_file))
  }

  raw <- yaml::read_yaml(pubs_file)
  defaults <- raw$defaults %||% list()
  default_lab_authors <- unlist(defaults$lab_authors %||% list(), use.names = FALSE)

  seen <- new.env(parent = emptyenv())
  entries <- list()
  overrides <- list()

  for (pub in raw$publications %||% list()) {
    doi <- trimws(as.character(pub$doi %||% ""))
    if (!nzchar(doi)) doi <- trimws(as.character(pub$preprint_doi %||% ""))
    if (!nzchar(doi)) next
    if (exists(doi, envir = seen, inherits = FALSE)) next
    assign(doi, TRUE, envir = seen)

    preprint_raw <- pub$preprint_doi %||% NA_character_
    preprint <- trimws(as.character(preprint_raw))
    if (!nzchar(preprint) || identical(preprint, "NA")) preprint <- NA_character_

    entries[[length(entries) + 1L]] <- list(
      line = doi,
      primary = doi,
      preprint = preprint
    )

    # Merge default lab_authors with per-pub lab_authors to get bold_authors
    pub_lab <- unlist(pub$lab_authors %||% list(), use.names = FALSE)
    bold_authors <- unique(c(default_lab_authors, pub_lab))
    bold_authors <- bold_authors[nzchar(trimws(bold_authors))]

    overrides[[doi]] <- list(
      bold_authors        = if (length(bold_authors)) as.list(bold_authors) else NULL,
      first_authors       = pub$first_authors %||% NULL,
      corresponding_authors = pub$corresponding_authors %||% NULL,
      preprint_doi        = pub$preprint_doi %||% NULL,
      section             = pub$section %||% NULL,
      title               = pub$title %||% NULL,
      abstract            = pub$abstract %||% NULL,
      hide_abstract       = pub$hide_abstract %||% NULL,
      author_display      = pub$author_display %||% NULL,
      author_overrides    = pub$author_overrides %||% NULL,
      title_doi           = pub$title_doi %||% NULL
    )
  }

  list(entries = entries, overrides = overrides)
}
