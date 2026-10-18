# Internal helpers shared by the worksheet import functions.
.ws_require <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "), call. = FALSE)
}
.ws_columns <- function(x, required) {
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(x)
}
.ws_names <- function(x, column_map = NULL) {
  if (!is.null(column_map)) {
    if (is.null(names(column_map)) || anyNA(column_map) || anyNA(names(column_map)) ||
        any(!nzchar(names(column_map))) || anyDuplicated(names(column_map)) || anyDuplicated(unname(column_map)))
      stop("column_map must be c(canonical_name = 'source name') with unique names.", call. = FALSE)
    .ws_columns(x, unname(column_map))
    names(x)[match(unname(column_map), names(x))] <- names(column_map)
  }
  if (anyDuplicated(names(x))) stop("Duplicate column names after mapping.", call. = FALSE)
  tibble::as_tibble(x)
}
.ws_files <- function(path, pattern) {
  if (!length(path) || anyNA(path)) stop("Specify input paths.", call. = FALSE)
  files <- purrr::map(path, function(p) {
    if (dir.exists(p)) list.files(p, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
    else if (file.exists(p) && grepl(pattern, p, ignore.case = TRUE)) p
    else stop("Missing file or unsupported extension: ", p, call. = FALSE)
  }) |> unlist(use.names = FALSE) |> unique() |> sort()
  files <- files[!grepl("^~\\$", basename(files))]
  if (!length(files)) stop("No matching input files.", call. = FALSE)
  files
}
.ws_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
.ws_isin <- function(x) toupper(trimws(as.character(x)))
.ws_date <- function(x, label) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
  text <- gsub("/", "-", trimws(as.character(x)), fixed = TRUE)
  blank <- .ws_blank(text)
  serial <- !blank & grepl("^[0-9]+([.][0-9]+)?$", text)
  out <- rep(as.Date(NA), length(text))
  out[serial] <- as.Date(as.numeric(text[serial]), origin = "1899-12-30")
  idx <- !blank & !serial
  valid <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text[idx])
  if (any(!valid)) stop("Use YYYY-MM-DD dates in ", label, call. = FALSE)
  out[idx] <- suppressWarnings(as.Date(text[idx], format = "%Y-%m-%d"))
  if (any(!blank & is.na(out))) stop("Invalid date in ", label, call. = FALSE)
  out
}
.ws_range <- function(from = NULL, to = NULL) {
  a <- if (is.null(from)) as.Date("1900-01-01") else .ws_date(from, "from")
  b <- if (is.null(to)) as.Date("9999-12-31") else .ws_date(to, "to")
  if (length(a) != 1L || length(b) != 1L || is.na(a) || is.na(b) || a > b)
    stop("from/to must be single dates with from <= to.", call. = FALSE)
  list(from = a, to = b)
}
.ws_number <- function(x, label) {
  text <- trimws(as.character(x)); blank <- .ws_blank(text)
  valid <- blank | grepl("^[+-]?([0-9]+|[0-9]{1,3}(,[0-9]{3})+)([.][0-9]+)?([eE][+-]?[0-9]+)?$", text)
  out <- suppressWarnings(as.numeric(gsub(",", "", text, fixed = TRUE)))
  out[blank] <- NA_real_
  if (any(!blank & (!valid | !is.finite(out)))) stop("Invalid number in ", label, call. = FALSE)
  out
}
.ws_offer_columns <- c("offer_date", "ISIN", "broker_names", "offer_price", "offer_size", "offer_settlementdate")
