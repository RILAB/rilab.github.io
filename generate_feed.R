month_nums <- c(
  January=1, February=2, March=3, April=4, May=5, June=6,
  July=7, August=8, September=9, October=10, November=11, December=12,
  Jan=1, Feb=2, Mar=3, Apr=4, Jun=6, Jul=7,
  Aug=8, Sep=9, Oct=10, Nov=11, Dec=12
)

xml_esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  s
}

lines <- readLines("POPCORN.Rmd", encoding = "UTF-8")

items      <- list()
cur_year   <- NA_integer_
cur_month  <- NA_integer_
cur_day    <- NA_integer_

for (raw in lines) {
  line <- trimws(raw)

  # ## 2026
  if (grepl("^## \\d{4}\\s*$", line)) {
    cur_year  <- as.integer(sub("^## ", "", line))
    next
  }

  # ### July
  if (grepl("^### ", line)) {
    mname <- trimws(sub("^### +", "", line))
    cur_month <- unname(month_nums[mname])
    next
  }

  # #### July 3  or  #### Mar 27  or  #### April 10 **DOUBLE FEATURE**
  if (grepl("^#### ", line)) {
    date_text <- trimws(sub("^#### +", "", line))
    date_text <- trimws(gsub("\\*.*", "", date_text))   # strip **...**
    parts <- strsplit(date_text, "\\s+")[[1]]
    if (length(parts) >= 2) {
      m <- unname(month_nums[parts[1]])
      d <- suppressWarnings(as.integer(gsub("[^0-9]", "", parts[2])))
      if (!is.na(m) && !is.na(d)) {
        cur_month <- m
        cur_day   <- d
      }
    }
    next
  }

  # Paper line: [Author et al YEAR](url) Title text
  if (grepl("^\\[", line) &&
      !is.na(cur_year) && !is.na(cur_month) && !is.na(cur_day)) {

    m <- regexec("^\\[(.+?)\\]\\(([^)]*)\\)\\s*(.*)", line)
    caps <- regmatches(line, m)[[1]]
    if (length(caps) != 4) next

    citation <- caps[2]
    url      <- caps[3]
    title    <- trimws(caps[4])

    # Fallback URL for mailto: or missing links
    if (url == "" || grepl("^mailto:", url)) {
      url <- "https://rilab.github.io/POPCORN.html"
    }

    date_obj <- tryCatch(
      as.Date(sprintf("%04d-%02d-%02d", cur_year, cur_month, cur_day)),
      error = function(e) NA
    )
    if (is.na(date_obj)) next
    if (date_obj > Sys.Date()) next   # skip future-scheduled papers

    pub_date <- format(date_obj, "%a, %d %b %Y 00:00:00 +0000")

    item_title <- if (nchar(title) > 0) paste0(citation, " — ", title) else citation

    items <- c(items, list(list(
      title    = item_title,
      link     = url,
      pubDate  = pub_date,
      date_obj = date_obj
    )))
  }
}

# Most recent 50 papers
items <- items[order(sapply(items, function(x) x$date_obj), decreasing = TRUE)]
items <- items[seq_len(min(50, length(items)))]

rss_items <- vapply(items, function(it) {
  paste0(
    "    <item>\n",
    "      <title>", xml_esc(it$title), "</title>\n",
    "      <link>",  xml_esc(it$link),  "</link>\n",
    "      <guid>",  xml_esc(it$link),  "</guid>\n",
    "      <pubDate>", it$pubDate, "</pubDate>\n",
    "    </item>"
  )
}, character(1))

rss <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  '<rss version="2.0">\n',
  '  <channel>\n',
  '    <title>POP:CORN Journal Club</title>\n',
  '    <link>https://rilab.github.io/POPCORN.html</link>\n',
  '    <description>Weekly paper discussion at the Ross-Ibarra Lab, UC Davis</description>\n',
  '    <language>en-us</language>\n',
  '    <lastBuildDate>', format(Sys.time(), "%a, %d %b %Y %H:%M:%S +0000", tz="UTC"), '</lastBuildDate>\n',
  paste(rss_items, collapse = "\n"), "\n",
  '  </channel>\n',
  '</rss>\n'
)

writeLines(rss, "feed.xml", useBytes = TRUE)
message("feed.xml written with ", length(items), " items")
