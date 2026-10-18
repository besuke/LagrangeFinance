# Replace stale attributes, retaining all offer rows and their original order.
merge_offer_bond_attributes <- function(offers, attributes, attribute_policy = c("exact", "previous")) {
  .ws_require(c("collapse", "dplyr", "purrr", "tibble"))
  attribute_policy <- match.arg(attribute_policy)
  .ws_columns(offers, .ws_offer_columns); .ws_columns(attributes, c("ISIN", "attribute_date"))
  offers$ISIN <- .ws_isin(offers$ISIN)
  offers$offer_date <- .ws_date(offers$offer_date, "offer_date")
  attributes$ISIN <- .ws_isin(attributes$ISIN)
  attributes$attribute_date <- .ws_date(attributes$attribute_date, "attribute_date")
  if (anyNA(offers$offer_date) || anyNA(attributes$attribute_date) || any(.ws_blank(attributes$ISIN)))
    stop("Missing date or master ISIN.", call. = FALSE)
  if (anyDuplicated(attributes[c("ISIN", "attribute_date")])) stop("Duplicate master keys.", call. = FALSE)
  base <- offers[, c(.ws_offer_columns, intersect(c("offer_source_file", "offer_source_sheet"), names(offers))), drop = FALSE]
  base$.worksheet_row <- seq_len(nrow(base))
  attrs <- attributes[, !grepl("^offer_", names(attributes)) & names(attributes) != "broker_names", drop = FALSE]
  reserved <- c(".worksheet_row", "market_quotes", "jpy_market_quotes", "validation_status", "validation_message", "validation_issues", "marketdate", "attribute_age_days")
  if (any(reserved %in% names(attrs))) stop("Master contains reserved worksheet column names.", call. = FALSE)
  left_join <- function(x, y) {
    # Uniqueness is checked explicitly so older collapse versions are supported.
    if (anyDuplicated(y$ISIN)) stop("Master selection must have one row per ISIN.", call. = FALSE)
    collapse::join(x, y, on = "ISIN", how = "left", multiple = FALSE, sort = FALSE) |> tibble::as_tibble()
  }
  out <- if (!nrow(base)) left_join(base, attrs[0, ]) else {
    purrr::map_dfr(unique(base$offer_date), function(day) {
      selected <- if (attribute_policy == "exact") dplyr::filter(attrs, attribute_date == day) else {
        attrs |> dplyr::filter(attribute_date <= day) |> dplyr::group_by(ISIN) |>
          dplyr::slice_max(attribute_date, n = 1L, with_ties = FALSE) |> dplyr::ungroup()
      }
      left_join(dplyr::filter(base, offer_date == day), selected)
    })
  }
  if (nrow(out) != nrow(base)) stop("Join changed offer row count.", call. = FALSE)
  out |> dplyr::arrange(.worksheet_row) |> dplyr::select(-.worksheet_row)
}
