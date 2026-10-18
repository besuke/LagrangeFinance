

import_ois_curves <- function(
    path,
    currency,
    marketdate = NULL,
    quote = c("mid", "bid", "ask")
) {
  quote <- match.arg(quote)
  currency <- match.arg(
    toupper(currency),
    c("JPY", "USD", "EUR", "GBP", "AUD")
  )
  
  # Excel日付番号、YYYY-MM-DD、YYYY/MM/DD、ISO日時に対応
  parse_marketdate <- function(x) {
    x <- trimws(as.character(x))
    result <- rep(as.Date(NA), length(x))
    
    is_serial <- !is.na(x) &
      grepl("^[0-9]+([.][0-9]+)?$", x)
    
    result[is_serial] <- as.Date(
      as.numeric(x[is_serial]),
      origin = "1899-12-30"
    )
    
    is_text <- !is.na(x) & nzchar(x) & !is_serial
    
    result[is_text] <- as.Date(
      gsub("/", "-", substr(x[is_text], 1, 10)),
      format = "%Y-%m-%d"
    )
    
    result
  }
  
  tbl <- readxl::read_excel(
    path = path,
    sheet = currency,
    range = readxl::cell_cols("A:F"),
    col_types = c(
      "text", "text", "text",
      "numeric", "numeric", "numeric"
    )
  ) |>
    tibble::as_tibble() |>
    dplyr::rename(
      ask = `Ask Yield`,
      mid = `Mid Yield`,
      bid = `Bid Yield`
    ) |>
    dplyr::mutate(
      marketdate = parse_marketdate(.data$marketdate),
      currency = .env$currency,
      rate_pct = .data[[quote]],
      rate = .data$rate_pct / 100,
      instrument = dplyr::case_when(
        .data$ticker == "SONIO/N Index" ~ "SONIA_FIXING",
        .env$currency == "AUD" & !grepl("^ADSO", .data$ticker) ~ "AUD_SWAP_UNCONFIRMED",
        TRUE ~ paste0(c(JPY = "TONAR", USD = "SOFR", EUR = "ESTR",
                       GBP = "SONIA", AUD = "AONIA")[[.env$currency]], "_OIS")
      ),
      instrument_type = dplyr::case_when(
        .data$instrument == "SONIA_FIXING" ~ "overnight_fixing",
        .data$instrument == "AUD_SWAP_UNCONFIRMED" ~ "unconfirmed_swap",
        TRUE ~ "spot_ois"
      ),
      tenordate = as.Date(NA),
      source = "Bloomberg Excel (image transcription)"
    ) |>
    dplyr::select(
      marketdate, currency, tenor, ticker,
      instrument, instrument_type, tenordate,
      ask, mid, bid, rate_pct, rate, source
    )
  
  if (anyNA(tbl[c("marketdate", "tenor", "ticker")]) ||
      any(!is.finite(tbl$rate_pct))) {
    stop("日付・Tenor・Ticker・選択した気配値に欠損があります。")
  }
  
  if (!is.null(marketdate)) {
    selected_date <- parse_marketdate(marketdate)
    
    if (length(selected_date) != 1L || is.na(selected_date)) {
      stop("marketdateには有効な日付を1つ指定してください。")
    }
    
    tbl <- tbl |>
      dplyr::filter(.data$marketdate == .env$selected_date)
  }
  
  if (nrow(tbl) == 0L) {
    stop("指定条件のマーケットデータがありません。")
  }
  
  if (anyDuplicated(tbl[c("marketdate", "ticker", "tenor")])) {
    stop("marketdate・Ticker・Tenorが重複しています。")
  }
  
  tbl
}

