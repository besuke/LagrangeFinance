# Pipe-delimited text, with explicit units: rate decimal OR rate_pct percent.
import_market_data <- function(path, from = NULL, to = NULL, column_map = NULL, encoding = "UTF-8") {
  .ws_require(c("readr", "dplyr", "purrr", "tibble"))
  period <- .ws_range(from, to)
  x <- purrr::map_dfr(.ws_files(path, "\\.(txt|psv|csv)$"), function(file) {
    x <- readr::read_delim(file, delim = "|", col_types = readr::cols(.default = "c"),
      locale = readr::locale(encoding = encoding), trim_ws = TRUE,
      name_repair = "check_unique", show_col_types = FALSE, progress = FALSE)
    if (nrow(readr::problems(x))) stop("Malformed pipe-delimited input: ", file, call. = FALSE)
    x <- .ws_names(x, column_map)
    .ws_columns(x, c("marketdate", "currency", "instrument", "tenor"))
    if (!any(c("rate", "rate_pct") %in% names(x))) stop("Market needs rate or rate_pct.", call. = FALSE)
    x$marketdate <- .ws_date(x$marketdate, "marketdate")
    x$currency <- toupper(trimws(x$currency)); x$instrument <- trimws(x$instrument)
    x$tenor <- toupper(trimws(x$tenor))
    if ("rate" %in% names(x)) x$rate <- .ws_number(x$rate, "rate")
    if ("rate_pct" %in% names(x)) x$rate_pct <- .ws_number(x$rate_pct, "rate_pct")
    if (!"rate" %in% names(x)) x$rate <- x$rate_pct / 100
    if (!"rate_pct" %in% names(x)) x$rate_pct <- x$rate * 100
    if (anyNA(x$rate) || anyNA(x$rate_pct) || any(abs(x$rate * 100 - x$rate_pct) > 1e-8))
      stop("Missing or inconsistent market rates.", call. = FALSE)
    if (anyNA(x$marketdate) || any(.ws_blank(x$currency)) || any(.ws_blank(x$instrument)) || any(.ws_blank(x$tenor)))
      stop("Missing market key.", call. = FALSE)
    x$tenordate <- if ("tenordate" %in% names(x)) .ws_date(x$tenordate, "tenordate") else rep(as.Date(NA), nrow(x))
    x$market_source_file <- normalizePath(file, winslash = "/")
    x
  }) |> dplyr::filter(marketdate >= period$from, marketdate <= period$to)
  if (anyDuplicated(x[c("marketdate", "instrument", "tenor")])) stop("Duplicate marketdate + instrument + tenor.", call. = FALSE)
  x
}
