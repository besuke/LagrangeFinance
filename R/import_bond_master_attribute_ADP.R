# Reads an Aladdin-exported data.frame RDS. No live API call.
import_bond_master_attribute_ADP <- function(path, ISIN = NULL, from = NULL, to = NULL,
                                            column_map = NULL, snapshot_date = NULL) {
  .ws_require(c("dplyr", "purrr", "tibble"))
  period <- .ws_range(from, to); files <- .ws_files(path, "\\.rds$")
  if (!is.null(snapshot_date) && length(files) != 1L)
    stop("snapshot_date can label only one undated RDS.", call. = FALSE)
  x <- purrr::map_dfr(files, function(file) {
    x <- readRDS(file)
    if (!is.data.frame(x)) stop("RDS must contain a data.frame/tibble: ", file, call. = FALSE)
    x <- .ws_names(x, column_map); .ws_columns(x, "ISIN")
    if (!"attribute_date" %in% names(x)) {
      if (is.null(snapshot_date)) stop("RDS needs attribute_date or explicit snapshot_date.", call. = FALSE)
      date <- .ws_date(snapshot_date, "snapshot_date")
      if (length(date) != 1L || is.na(date)) stop("snapshot_date must be one date.", call. = FALSE)
      x$attribute_date <- rep(date, nrow(x))
    } else if (!is.null(snapshot_date)) stop("RDS already has attribute_date; omit snapshot_date.", call. = FALSE)
    x$ISIN <- .ws_isin(x$ISIN)
    x$attribute_date <- .ws_date(x$attribute_date, "attribute_date")
    if (any(.ws_blank(x$ISIN)) || anyNA(x$attribute_date)) stop("Missing master key.", call. = FALSE)
    # Never allow a master export to replace any offer terms.
    x <- x[, !grepl("^offer_", names(x)) & names(x) != "broker_names", drop = FALSE]
    x$attribute_source_file <- normalizePath(file, winslash = "/")
    x
  }) |> dplyr::filter(attribute_date >= period$from, attribute_date <= period$to)
  if (!is.null(ISIN)) x <- dplyr::filter(x, .data$ISIN %in% .ws_isin(ISIN))
  if (anyDuplicated(x[c("ISIN", "attribute_date")])) stop("Duplicate ISIN + attribute_date in master.", call. = FALSE)
  x
}
