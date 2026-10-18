# Excel import. Each call may target one sheet across one or several workbooks.
import_offer_sheet <- function(path, sheet = 1, from = NULL, to = NULL,
                               column_map = NULL, skip = 0L) {
  .ws_require(c("readxl", "dplyr", "purrr", "tibble"))
  period <- .ws_range(from, to)
  purrr::map_dfr(.ws_files(path, "\\.xlsx?$"), function(file) {
    x <- readxl::read_excel(file, sheet = sheet, skip = skip, col_types = "text",
                            .name_repair = "check_unique") |> .ws_names(column_map)
    .ws_columns(x, .ws_offer_columns)
    blank_row <- purrr::pmap_lgl(x[.ws_offer_columns], function(...) all(.ws_blank(c(...))))
    x <- x[!blank_row, ]
    x <- x |> dplyr::mutate(
      offer_date = .ws_date(offer_date, "offer_date"), ISIN = .ws_isin(ISIN),
      broker_names = trimws(broker_names), offer_price = .ws_number(offer_price, "offer_price"),
      offer_size = .ws_number(offer_size, "offer_size"),
      offer_settlementdate = .ws_date(offer_settlementdate, "offer_settlementdate"))
    if (anyNA(x$offer_date)) stop("Missing offer_date: cannot select historical period.", call. = FALSE)
    x |> dplyr::filter(offer_date >= period$from, offer_date <= period$to) |>
      dplyr::mutate(offer_source_file = normalizePath(file, winslash = "/"),
                    offer_source_sheet = as.character(sheet))
  })
}
