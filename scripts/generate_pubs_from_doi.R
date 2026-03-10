generate_pubs_from_doi <- function(
  doi_file = "doi.txt",
  output_csv = "data/pubs.csv",
  timeout_sec = 20
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required to build pubs from DOI list.")
  }

  if (!file.exists(doi_file)) {
    stop(sprintf("DOI file not found: %s", doi_file))
  }

  # Read DOI list (ignore blanks and comments)
  raw_lines <- readLines(doi_file, warn = FALSE, encoding = "UTF-8")
  doi_lines <- trimws(raw_lines)
  dois <- doi_lines[nzchar(doi_lines) & !grepl("^#", doi_lines)]
  dois <- unique(dois)

  if (!length(dois)) {
    stop(sprintf("No DOIs found in %s", doi_file))
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

    # Normalize DataCite response into a Crossref-like structure used below.
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

  metadata_work <- function(doi) {
    tryCatch(
      crossref_work(doi),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("404", msg, fixed = TRUE)) {
          message("Crossref 404 for DOI; trying DataCite: ", doi)
          return(datacite_work(doi))
        }
        stop(e)
      }
    )
  }

  clean_text <- function(x) {
    if (length(x) == 0 || is.null(x) || is.na(x)) return("")
    x <- as.character(x[[1]])
    x <- gsub("<[^>]+>", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  author_string <- function(author_df) {
    if (is.null(author_df) || !NROW(author_df)) return("Author list unavailable")
    authors <- apply(author_df, 1, function(a) {
      given <- trimws(as.character(a[["given"]]))
      family <- trimws(as.character(a[["family"]]))
      name <- trimws(paste(given, family))
      if (!nzchar(name)) {
        nm <- trimws(as.character(a[["name"]]))
        name <- nm
      }
      if (grepl("Ross-Ibarra", name, fixed = TRUE)) {
        name <- sprintf("**%s**", name)
      }
      name
    })
    paste(authors[nzchar(authors)], collapse = ", ")
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

  is_preprint <- function(doi, msg) {
    if (grepl("^10\\.1101/", doi, ignore.case = TRUE)) return(TRUE)
    ct <- tolower(paste(unlist(msg$`container-title`), collapse = " "))
    grepl("biorxiv|medrxiv|arxiv|preprint", ct)
  }

  rows <- lapply(dois, function(doi) {
    msg <- metadata_work(doi)
    title <- clean_text(msg$title)
    abs <- clean_text(msg$abstract)
    if (!nzchar(abs)) abs <- "Abstract unavailable."
    authors <- author_string(msg$author)
    year <- get_year(msg)
    preprint <- is_preprint(doi, msg)
    doi_url <- sprintf("https://doi.org/%s", doi)

    if (preprint) {
      section <- "Preprints"
      details <- paste0(
        "<details>\n",
        "<summary> ", title, "\n",
        " [[preprint](", doi_url, ")]  \n",
        authors, "\n",
        "</summary>\n",
        "<p style=\"\"margin-left: 30px\"\">\n",
        abs, "\n",
        "</p></details><p></p>"
      )
      ord <- 0L
    } else {
      section <- as.character(year)
      details <- paste0(
        "<details>\n",
        "    <summary> [", title, "](", doi_url, ")\n ",
        authors, "\n\n",
        " </summary>  \n",
        "    <p style=\"\"margin-left: 30px\"\"> \n ",
        abs, "\n",
        " </p></details><p></p>"
      )
      ord <- ifelse(is.na(year), 9999L, as.integer(9999 - year))
    }

    data.frame(
      section = section,
      order = ord,
      details_html = details,
      year = ifelse(is.na(year), 0L, year),
      stringsAsFactors = FALSE
    )
  })

  pubs <- do.call(rbind, rows)

  pre <- pubs[pubs$section == "Preprints", , drop = FALSE]
  non_pre <- pubs[pubs$section != "Preprints", , drop = FALSE]

  if (NROW(pre)) {
    pre <- pre[order(pre$year, decreasing = TRUE), , drop = FALSE]
    pre$order <- seq_len(NROW(pre)) - 1L
  }

  if (NROW(non_pre)) {
    non_pre$section_num <- suppressWarnings(as.integer(non_pre$section))
    non_pre <- non_pre[order(non_pre$section_num, decreasing = TRUE, non_pre$year, decreasing = TRUE), , drop = FALSE]
    non_pre$order <- ave(non_pre$order, non_pre$section, FUN = function(x) seq_along(x) + 5L)
    non_pre$section_num <- NULL
  }

  final <- rbind(pre, non_pre)
  final$year <- NULL

  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  write.csv(final, output_csv, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
  invisible(final)
}

if (identical(environmentName(environment()), "R_GlobalEnv")) {
  generate_pubs_from_doi()
}
