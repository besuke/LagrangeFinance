# Currency-dispatched OIS curves for compute_asset_swap().
# Input: observed spot OIS par quotes, NOT overnight fixings or IBOR swap rates.
# rate_pct is in percent (4.25 means 4.25%). No market data is fabricated.
# Source alongside R/bond_asset_swap.R and R/set_market_data_for_assetswap.R.

ois_currency_spec <- function(currency) {
  if (!is.character(currency) || length(currency) != 1L || is.na(currency)) {
    stop("currency must be one currency code.", call. = FALSE)
  }
  currency <- toupper(trimws(currency))
  registry <- list(
    JPY = list(index_name = "TONAR", constructor = "Tonar", settlement_days = 2L),
    USD = list(index_name = "SOFR", constructor = "Sofr", settlement_days = 2L),
    EUR = list(index_name = "ESTR", constructor = "Estr", settlement_days = 2L),
    GBP = list(index_name = "SONIA", constructor = "Sonia", settlement_days = 0L),
    AUD = list(index_name = "AONIA", constructor = "Aonia", settlement_days = 1L)
  )
  spec <- registry[[currency]]
  if (is.null(spec)) {
    stop("Unsupported currency: ", currency, ". Use JPY, USD, EUR, GBP or AUD.",
         call. = FALSE)
  }
  c(list(currency = currency), spec)
}

# A single dispatch table selects the QuantLib constructor, calendar and day count.
instance_overnight_index <- function(currency, curve_handle = NULL) {
  spec <- ois_currency_spec(currency)
  ns <- asNamespace("QuantLib")
  if (!exists(spec$constructor, ns, inherits = FALSE)) {
    stop("QuantLib R binding unavailable: ", spec$constructor, call. = FALSE)
  }
  constructor <- get(spec$constructor, ns, inherits = FALSE)
  if (is.null(curve_handle)) constructor() else constructor(curve_handle)
}

# Quote conventions must match the supplied market source.
# settlement_days is the OIS spot lag, distinct from the index fixing lag.
# payment_lag and payment_frequency apply to curve helpers, independently of ASW.
instance_swap_curve <- function(
  currency, date, quotes = NULL,
  curve_type = c("log_cubic_discount", "flat_forward"),
  settlement_days = NULL, payment_lag = 0L,
  payment_frequency = "Annual", extrapolate = FALSE
) {
  spec <- ois_currency_spec(currency)
  curve_type <- match.arg(curve_type)
  date <- as.Date(date)
  if (length(date) != 1L || is.na(date)) stop("Choose one evaluation date.")
  if (is.null(quotes)) {
    if (spec$currency != "JPY") {
      stop("Supply dated OIS quotes for ", spec$currency, "; no default data.")
    }
    if (!exists("instance_swap_curve_TONAR", mode = "function")) {
      stop("Source R/set_market_data_for_assetswap.R first.")
    }
    if (!missing(settlement_days) || !missing(payment_lag) ||
        !missing(payment_frequency) || !missing(curve_type) ||
        !missing(extrapolate)) {
      stop("For custom curve conventions, supply quotes explicitly.")
    }
    bundle <- instance_swap_curve_TONAR(as.character(date))
    bundle$currency <- spec$currency
    bundle$index_name <- spec$index_name
    return(bundle)
  }
  if (!is.data.frame(quotes) || !nrow(quotes) ||
      !all(c("date", "tenor", "rate_pct") %in% names(quotes))) {
    stop("quotes must have date, tenor and rate_pct columns.")
  }
  if (anyNA(quotes$date)) stop("Missing quote dates.")
  if (anyNA(as.Date(quotes$date))) stop("Invalid quote dates.")
  quotes <- quotes[as.Date(quotes$date) == date, , drop = FALSE]
  if (!nrow(quotes)) stop("No OIS quotes for the evaluation date.")
  if ("currency" %in% names(quotes) &&
      any(is.na(quotes$currency) | toupper(trimws(quotes$currency)) != spec$currency)) {
    stop("Quote currency does not match requested currency.")
  }
  tenor <- toupper(trimws(as.character(quotes$tenor)))
  if (anyNA(tenor) || any(!grepl("^[1-9][0-9]*[DWMY]$", tenor)) ||
      anyDuplicated(tenor)) stop("Invalid or duplicate OIS tenors.")
  if (!is.numeric(quotes$rate_pct) || any(!is.finite(quotes$rate_pct))) {
    stop("rate_pct must contain finite numeric percentage rates.")
  }
  if (is.null(settlement_days)) settlement_days <- spec$settlement_days
  valid_lag <- function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) &&
      x >= 0 && x == as.integer(x)
  }
  if (!valid_lag(settlement_days) || !valid_lag(payment_lag)) {
    stop("Settlement and payment lags must be nonnegative integers.")
  }
  ns <- asNamespace("QuantLib")
  frequency_name <- paste0("Frequency_", payment_frequency, "_get")
  if (length(frequency_name) != 1L || !exists(frequency_name, ns, inherits = FALSE)) {
    stop("Unsupported payment_frequency.")
  }
  frequency <- get(frequency_name, ns, inherits = FALSE)()
  eval_date <- QuantLib::DateParser_parseISO(as.character(date))
  QuantLib::Settings_setEvaluationDate(QuantLib::Settings_instance(), eval_date)
  index <- instance_overnight_index(spec$currency)
  calendar <- index$fixingCalendar()
  day_counter <- index$dayCounter()
  units <- c(D = "Days", W = "Weeks", M = "Months", Y = "Years")
  tenor_n <- as.integer(sub("[DWMY]$", "", tenor))
  tenor_unit <- unname(units[substring(tenor, nchar(tenor))])
  helpers <- Map(function(n, unit, rate) {
    QuantLib::OISRateHelper(
      as.integer(settlement_days), QuantLib::Period(n, unit),
      QuantLib::QuoteHandle(QuantLib::SimpleQuote(rate / 100)), index,
      QuantLib::YieldTermStructureHandle(), FALSE,
      as.integer(payment_lag), "ModifiedFollowing", frequency
    )
  }, tenor_n, tenor_unit, quotes$rate_pct)
  helper_vector <- QuantLib::RateHelperVector()
  invisible(Map(function(helper) helper_vector$push_back(helper), helpers))
  constructor <- switch(curve_type,
    log_cubic_discount = QuantLib::PiecewiseLogCubicDiscount,
    flat_forward = QuantLib::PiecewiseFlatForward
  )
  curve <- constructor(0L, calendar, helper_vector, day_counter)
  if (isTRUE(extrapolate)) QuantLib::TermStructure_enableExtrapolation(curve)
  # Force bootstrap errors to surface here rather than during bond valuation.
  df <- as.numeric(curve$discount(curve$maxDate()))
  if (length(df) != 1L || !is.finite(df) || df <= 0) {
    stop("Invalid terminal discount factor.")
  }
  handle <- QuantLib::YieldTermStructureHandle(curve)
  linked_index <- instance_overnight_index(spec$currency, handle)
  list(
    currency = spec$currency, index_name = spec$index_name,
    curve = curve, curve_handle = handle, overnight_index = linked_index,
    index = linked_index, evaluation_date = as.character(date),
    calendar = calendar, day_counter = day_counter,
    helpers = helpers, helper_vector = helper_vector, quotes = quotes,
    curve_type = curve_type, settlement_days = as.integer(settlement_days),
    payment_lag = as.integer(payment_lag), payment_frequency = payment_frequency
  )
}

