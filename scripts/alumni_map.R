#!/usr/bin/env Rscript
# Map of lab alumni: a point at each person's current location, with a label
# (name + current position) connected to the point by a leader line and
# colored by the role they held in the lab.
#
# Usage (from the repo root):
#   Rscript scripts/alumni_map.R                       # world map
#   Rscript scripts/alumni_map.R --region usa          # zoom to contiguous US
#   Rscript scripts/alumni_map.R --region europe
#   Rscript scripts/alumni_map.R --in data/alumni_map.tsv --out images/alumni_map
#   Rscript scripts/alumni_map.R --label-us            # world map, label US alumni too
#
# By default the world map labels only alumni outside the US; US alumni are
# drawn as points with a single count callout (pair it with --region usa).
# Figures are sized 16:9 for slides.
#
# Input TSV columns (data/alumni_map.tsv):
#   name         person's name
#   type         undergrad | grad | postdoc | other
#   position     current job title (e.g. "Asst. Professor")
#   institution  current institution (shown on the label)
#   location     free-text place used for geocoding when lat/lon are blank
#                (e.g. "Ames, Iowa, USA"); falls back to institution if blank
#   lat, lon     decimal degrees; if filled in, these are used as-is
#   include      yes/no; rows with "no" are skipped
#   notes        ignored by the script
#
# Missing coordinates are looked up with OpenStreetMap Nominatim and cached in
# data/alumni_geocode_cache.tsv, so each place is only queried once. Copy good
# coordinates into lat/lon in the TSV if a lookup lands in the wrong spot.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
  library(dplyr)
  library(readr)
})

args <- commandArgs(trailingOnly = TRUE)
opt <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i)) default else args[i + 1]
}
in_file    <- opt("--in", "data/alumni_map.tsv")
out_prefix <- opt("--out", "images/alumni_map")
region     <- opt("--region", "world")
label_us   <- "--label-us" %in% args
cache_file <- "data/alumni_geocode_cache.tsv"

type_levels <- c(undergrad = "Undergraduate", grad = "Graduate student",
                 postdoc = "Postdoc", other = "Staff / other")
type_colors <- c("Undergraduate" = "#1b9e77", "Graduate student" = "#d95f02",
                 "Postdoc" = "#7570b3", "Staff / other" = "#666666")

regions <- list(
  world  = list(xlim = c(-170, 180), ylim = c(-58, 80)),
  usa    = list(xlim = c(-125, -66), ylim = c(24, 50)),
  europe = list(xlim = c(-12, 32), ylim = c(35, 65))
)
if (!region %in% names(regions)) stop("--region must be one of: ", paste(names(regions), collapse = ", "))
bbox <- regions[[region]]

# ---- read data ----------------------------------------------------------------
alumni <- read_tsv(in_file, col_types = cols(.default = col_character()), na = "") %>%
  mutate(across(everything(), ~ coalesce(trimws(.x), ""))) %>%
  filter(tolower(include) != "no") %>%
  mutate(
    type = tolower(type),
    lat = suppressWarnings(as.numeric(lat)),
    lon = suppressWarnings(as.numeric(lon)),
    query = ifelse(location != "", location, institution)
  )

bad_type <- setdiff(unique(alumni$type), names(type_levels))
if (length(bad_type)) stop("Unknown type(s): ", paste(bad_type, collapse = ", "),
                           ". Use one of: ", paste(names(type_levels), collapse = ", "))

# ---- geocode missing coordinates ------------------------------------------------
geocode_osm <- function(q) {
  url <- paste0("https://nominatim.openstreetmap.org/search?format=json&limit=1&q=",
                utils::URLencode(q, reserved = TRUE))
  res <- httr::GET(url, httr::user_agent("rilab-alumni-map"))
  Sys.sleep(1)  # Nominatim usage policy: max 1 request/second
  hit <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  if (length(hit) == 0) return(c(NA_real_, NA_real_))
  as.numeric(c(hit$lat[1], hit$lon[1]))
}

cache <- if (file.exists(cache_file)) {
  read_tsv(cache_file, col_types = "cdd")
} else {
  tibble(query = character(), lat = double(), lon = double())
}

need <- alumni %>% filter(is.na(lat) | is.na(lon), query != "") %>%
  distinct(query) %>% anti_join(cache, by = "query")
if (nrow(need)) {
  message("Geocoding ", nrow(need), " place(s) via OpenStreetMap...")
  new <- need %>% rowwise() %>%
    mutate(ll = list(geocode_osm(query)), lat = ll[1], lon = ll[2]) %>%
    ungroup() %>% select(query, lat, lon)
  cache <- bind_rows(cache, new)
  write_tsv(cache, cache_file, na = "")
}

alumni <- alumni %>%
  left_join(cache, by = "query", suffix = c("", ".geo")) %>%
  mutate(lat = coalesce(lat, lat.geo), lon = coalesce(lon, lon.geo)) %>%
  select(-lat.geo, -lon.geo)

missing <- alumni %>% filter(is.na(lat) | is.na(lon))
if (nrow(missing)) {
  message("Skipping (no coordinates; fill in location or lat/lon): ",
          paste(missing$name, collapse = "; "))
}

plot_df <- alumni %>%
  filter(!is.na(lat), !is.na(lon),
         between(lon, bbox$xlim[1], bbox$xlim[2]),
         between(lat, bbox$ylim[1], bbox$ylim[2])) %>%
  mutate(
    type = factor(type_levels[type], levels = type_levels),
    current = trimws(paste(position, ifelse(position != "" & institution != "", ", ", ""), institution, sep = "")),
    label = ifelse(current != "", paste0(name, "\n", current), name),
    country = maps::map.where("world", lon, lat),
    in_us = coalesce(country == "USA",
                     between(lon, -125, -66) & between(lat, 24, 50))  # coastal points
  )

collapse_us <- region == "world" && !label_us
label_df <- if (collapse_us) filter(plot_df, !in_us) else plot_df
n_us <- sum(plot_df$in_us)

# ---- plot -----------------------------------------------------------------------
world <- map_data("world")
label_size <- if (collapse_us) 3 else 2.6

p <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group),
               fill = "grey92", color = "grey75", linewidth = 0.15) +
  geom_point(data = plot_df, aes(lon, lat, color = type), size = 1.8) +
  geom_text_repel(
    data = label_df, aes(lon, lat, label = label, color = type),
    size = label_size, lineheight = 0.9, fontface = "plain",
    box.padding = 0.5, point.padding = 0.2, min.segment.length = 0,
    segment.size = 0.3, segment.alpha = 0.7, force = 3, force_pull = 0.5,
    max.overlaps = Inf, max.time = 5, seed = 42
  ) +
  {if (collapse_us && n_us > 0)
    annotate("label", x = -98, y = 16, size = 4, label.size = 0.3,
             label = paste0(n_us, " alumni in the US"))} +
  scale_color_manual(values = type_colors, drop = TRUE, name = NULL) +
  coord_quickmap(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE) +
  theme_void(base_size = 14) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA))

dims <- c(13.33, 7.5)  # 16:9 slide
dir.create(dirname(out_prefix), showWarnings = FALSE, recursive = TRUE)
suffix <- if (region == "world") "" else paste0("_", region)
for (ext in c("png", "pdf")) {
  f <- paste0(out_prefix, suffix, ".", ext)
  ggsave(f, p, width = dims[1], height = dims[2], dpi = 300,
         device = ext)
  message("Wrote ", f)
}
