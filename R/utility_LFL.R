# utility_LFL.R
# LagrangeFinance utility functions

create_lagrange_theme_LFL <- function(base_size = 12) {
  ggthemes::theme_economist(base_size = base_size)
}

#' Add the LagrangeFinance ggplot theme
#'
#' @param plot_object A ggplot object.
#' @param base_size Base font size.
#' @param legend_position Legend position.
#'
#' @return A ggplot object.
add_lagrange_theme_LFL <- function(
    plot_object,
    base_size = 12,
    legend_position = "bottom"
) {
  if (!base::inherits(plot_object, "ggplot")) {
    base::stop(
      "plot_object must be a ggplot object.",
      call. = FALSE
    )
  }
  
  plot_object +
    create_lagrange_theme_LFL(
      base_size = base_size
    ) +
    ggplot2::theme(
      legend.position = legend_position
    )
}
#' Write a CSV file to the derived-data directory
#'
#' @param tbl_data A data frame to write.
#' @param file_name Output CSV file name.
#' @param derived_data_dir Derived-data directory. When omitted,
#'   the project simulation_data/derived directory is used.
#'
#' @return The normalized output path, invisibly.
write_derived_csv_file_LFL <- function(
    tbl_data,
    file_name,
    derived_data_dir = NULL
) {
  if (!is.data.frame(tbl_data)) {
    stop(
      "tbl_data must be a data frame.",
      call. = FALSE
    )
  }
  
  if (
    !is.character(file_name) ||
    length(file_name) != 1L ||
    !nzchar(file_name)
  ) {
    stop(
      "file_name must be one non-empty character value.",
      call. = FALSE
    )
  }
  
  if (
    !identical(
      tolower(tools::file_ext(file_name)),
      "csv"
    )
  ) {
    stop(
      "file_name must have a .csv extension.",
      call. = FALSE
    )
  }
  
  if (is.null(derived_data_dir)) {
    if (
      exists(
        "get_derived_data_dir_LFL",
        mode = "function"
      )
    ) {
      derived_data_dir <-
        get_derived_data_dir_LFL()
    } else {
      vec_project_candidate <- unique(c(
        Sys.getenv("QUARTO_PROJECT_DIR"),
        getwd(),
        dirname(getwd())
      ))
      
      vec_project_candidate <-
        vec_project_candidate[
          nzchar(vec_project_candidate)
        ]
      
      vec_project_match <-
        vec_project_candidate[
          file.exists(
            file.path(
              vec_project_candidate,
              "_quarto.yml"
            )
          )
        ]
      
      if (length(vec_project_match) == 0L) {
        stop(
          paste0(
            "LagrangeFinance project root ",
            "could not be identified."
          ),
          call. = FALSE
        )
      }
      
      derived_data_dir <- file.path(
        vec_project_match[[1]],
        "simulation_data",
        "derived"
      )
    }
  }
  
  if (
    !is.character(derived_data_dir) ||
    length(derived_data_dir) != 1L ||
    !nzchar(derived_data_dir)
  ) {
    stop(
      paste0(
        "derived_data_dir must be one ",
        "non-empty character value."
      ),
      call. = FALSE
    )
  }
  
  dir.create(
    derived_data_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  output_path <- file.path(
    derived_data_dir,
    file_name
  )
  
  readr::write_csv(
    tbl_data,
    output_path
  )
  
  if (!file.exists(output_path)) {
    stop(
      "CSV output was not created: ",
      output_path,
      call. = FALSE
    )
  }
  
  invisible(
    normalizePath(
      output_path,
      winslash = "/",
      mustWork = TRUE
    )
  )
}

set_random_seed_LFL <- function(seed = 20260726) {
  set.seed(seed)
  invisible(seed)
}


find_project_dir_LFL <- function() {
  candidates <- unique(c(
    Sys.getenv("QUARTO_PROJECT_DIR"),
    getwd(),
    dirname(getwd()),
    dirname(dirname(getwd()))
  ))
  candidates <- candidates[nzchar(candidates)]
  hit <- candidates[file.exists(file.path(candidates, "_quarto.yml"))]

  if (!length(hit))
    stop("LagrangeFinance project root was not found.", call. = FALSE)

  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

get_data_dir_LFL <- function() {
  file.path(find_project_dir_LFL(), "data")
}

get_simulation_data_dir_LFL <- function() {
  file.path(find_project_dir_LFL(), "simulation_data")
}

get_output_dir_LFL <- function() {
  file.path(find_project_dir_LFL(), "output")
}

get_figure_output_dir_LFL <- function() {
  file.path(get_output_dir_LFL(), "figure")
}

get_table_output_dir_LFL <- function() {
  file.path(get_output_dir_LFL(), "table")
}

create_output_dirs_LFL <- function() {
  dirs <- c(
    get_output_dir_LFL(),
    get_figure_output_dir_LFL(),
    get_table_output_dir_LFL()
  )

  purrr::walk(dirs, \(dir) {
    if (!dir.exists(dir))
      dir.create(dir, recursive = TRUE)
  })

  invisible(dirs)
}




read_csv_file_LFL <- function(file_name, ...) {
  readr::read_csv(
    find_data_file_LFL(file_name),
    show_col_types = FALSE,
    ...
  )
}

write_csv_file_LFL <- function(x, file_name, ...) {
  create_output_dirs_LFL()
  path <- file.path(get_table_output_dir_LFL(), file_name)
  readr::write_csv(x, path, ...)
  invisible(path)
}

read_parquet_file_LFL <- function(file_name, ...) {
  arrow::read_parquet(find_data_file_LFL(file_name), ...)
}

write_parquet_file_LFL <- function(x, file_name, ...) {
  create_output_dirs_LFL()
  path <- file.path(get_table_output_dir_LFL(), file_name)
  arrow::write_parquet(x, path, ...)
  invisible(path)
}

read_rds_file_LFL <- function(file_name) {
  readRDS(find_data_file_LFL(file_name))
}

write_rds_file_LFL <- function(x, file_name) {
  create_output_dirs_LFL()
  path <- file.path(get_output_dir_LFL(), file_name)
  saveRDS(x, path)
  invisible(path)
}

show_data_structure_LFL <- function(x) {
  dplyr::glimpse(x)
  invisible(x)
}

show_table_LFL <- function(x, n = 10) {
  x |>
    shorten_names_LFL() |>
    base::print(n = n)
  
  base::invisible(x)
}

shorten_names_LFL <- function(data) {
  data |>
    rlang::set_names(
      base::names(data) |>
        purrr::map_chr(
          \(name) {
            parts <- stringr::str_split(
              name,
              "_",
              simplify = FALSE
            )[[1]]
            
            if (base::length(parts) == 1) {
              name
            } else {
              base::paste0(
                stringr::str_sub(parts[1], 1, 3),
                "_",
                stringr::str_sub(
                  parts[base::length(parts)],
                  1,
                  5
                )
              )
            }
          }
        )
    )
}


show_table_head_LFL <- function(x, n = 6) {
  print(utils::head(x, n))
  invisible(x)
}

show_table_tail_LFL <- function(x, n = 6) {
  print(utils::tail(x, n))
  invisible(x)
}


show_plot_LFL <- function(plot) {
  plot |>
    add_lagrange_theme_LFL() |>
    base::print()
  
  base::invisible(plot)
}

show_session_info_LFL <- function() {
  utils::sessionInfo()
}
#' Compute a covariance table
#'
#' @param tbl_data A data frame containing numeric variables.
#' @param vec_variables Character vector of variable names.
#' @param annualization_factor Numeric annualization factor.
#'
#' @return A tibble containing covariance values.
compute_covariance_table_LFL <- function(
    tbl_data,
    vec_variables,
    annualization_factor = 1
) {
  stopifnot(
    is.data.frame(tbl_data),
    is.character(vec_variables),
    length(vec_variables) >= 1,
    all(vec_variables %in% names(tbl_data)),
    is.numeric(annualization_factor),
    length(annualization_factor) == 1
  )
  
  tidyr::expand_grid(
    variable_row = vec_variables,
    variable_column = vec_variables
  ) |>
    dplyr::mutate(
      covariance = purrr::map2_dbl(
        variable_row,
        variable_column,
        \(variable_row, variable_column) {
          stats::cov(
            tbl_data[[variable_row]],
            tbl_data[[variable_column]],
            use = "complete.obs"
          ) * annualization_factor
        }
      )
    )
}


#' Convert a covariance table to a matrix
#'
#' @param tbl_covariance A covariance table created by
#'   compute_covariance_table_LFL().
#'
#' @return A numeric covariance matrix.
convert_covariance_table_to_matrix_LFL <- function(
    tbl_covariance
) {
  stopifnot(
    is.data.frame(tbl_covariance),
    all(
      c(
        "variable_row",
        "variable_column",
        "covariance"
      ) %in% names(tbl_covariance)
    )
  )
  
  tbl_covariance |>
    tidyr::pivot_wider(
      names_from = variable_column,
      values_from = covariance
    ) |>
    tibble::column_to_rownames("variable_row") |>
    as.matrix()
}


#' Compute a mean-variance portfolio
#'
#' @param matrix_covariance Numeric covariance matrix.
#' @param vec_expected_returns Numeric expected-return vector.
#' @param target_return Numeric target portfolio return.
#'
#' @return A list returned by quadprog::solve.QP(), with additional
#'   portfolio statistics.
compute_mean_variance_portfolio_LFL <- function(
    matrix_covariance,
    vec_expected_returns,
    target_return
) {
  if (!requireNamespace("quadprog", quietly = TRUE)) {
    stop(
      "Package 'quadprog' is required.",
      call. = FALSE
    )
  }
  
  matrix_covariance <- as.matrix(matrix_covariance)
  vec_expected_returns <- as.numeric(vec_expected_returns)
  
  n_assets <- length(vec_expected_returns)
  
  if (
    nrow(matrix_covariance) != n_assets ||
    ncol(matrix_covariance) != n_assets
  ) {
    stop(
      "The covariance matrix dimensions must match the expected-return vector.",
      call. = FALSE
    )
  }
  
  if (
    anyNA(matrix_covariance) ||
    anyNA(vec_expected_returns)
  ) {
    stop(
      "The covariance matrix and expected-return vector must not contain NA values.",
      call. = FALSE
    )
  }
  
  matrix_covariance <- (
    matrix_covariance +
      t(matrix_covariance)
  ) / 2
  
  matrix_covariance <- matrix_covariance +
    diag(1e-8, n_assets)
  
  matrix_constraint <- cbind(
    vec_expected_returns,
    rep(1, n_assets)
  )
  
  vec_constraint <- c(
    target_return,
    1
  )
  
  list_optimization <- quadprog::solve.QP(
    Dmat = matrix_covariance,
    dvec = rep(0, n_assets),
    Amat = matrix_constraint,
    bvec = vec_constraint,
    meq = 2
  )
  
  vec_weight <- list_optimization$solution
  
  list_optimization$target_return <- target_return
  list_optimization$portfolio_return <- sum(
    vec_weight * vec_expected_returns
  )
  list_optimization$portfolio_variance <- as.numeric(
    t(vec_weight) %*%
      matrix_covariance %*%
      vec_weight
  )
  list_optimization$portfolio_risk <- sqrt(
    list_optimization$portfolio_variance
  )
  list_optimization$weight_sum <- sum(vec_weight)
  
  list_optimization
}


#' Create a portfolio-weight table
#'
#' @param list_optimization Result from
#'   compute_mean_variance_portfolio_LFL().
#' @param vec_asset_ids Asset identifiers.
#' @param target_return Numeric target return.
#'
#' @return A tibble containing portfolio weights.
create_portfolio_weight_table_LFL <- function(
    list_optimization,
    vec_asset_ids,
    target_return = list_optimization$target_return
) {
  vec_weight <- list_optimization$solution
  
  if (length(vec_weight) != length(vec_asset_ids)) {
    stop(
      "The number of asset IDs must match the number of portfolio weights.",
      call. = FALSE
    )
  }
  
  tibble::tibble(
    asset_ID = vec_asset_ids,
    weight = as.numeric(vec_weight),
    target_return = target_return
  )
}


#' Compute a portfolio summary
#'
#' @param list_optimization Result from
#'   compute_mean_variance_portfolio_LFL().
#' @param target_return Numeric target return.
#'
#' @return A one-row tibble containing portfolio statistics.
compute_portfolio_summary_LFL <- function(
    list_optimization,
    target_return = list_optimization$target_return
) {
  tibble::tibble(
    target_return = target_return,
    portfolio_return = list_optimization$portfolio_return,
    portfolio_variance = list_optimization$portfolio_variance,
    portfolio_risk = list_optimization$portfolio_risk,
    weight_sum = list_optimization$weight_sum
  )
}


#' Compute an efficient frontier
#'
#' @param matrix_covariance Numeric covariance matrix.
#' @param vec_expected_returns Numeric expected-return vector.
#' @param vec_target_returns Numeric vector of target returns.
#'
#' @return A tibble containing efficient-frontier results.
compute_efficient_frontier_LFL <- function(
    matrix_covariance,
    vec_expected_returns,
    vec_target_returns
) {
  purrr::map_dfr(
    vec_target_returns,
    \(target_return) {
      list_optimization <-
        compute_mean_variance_portfolio_LFL(
          matrix_covariance = matrix_covariance,
          vec_expected_returns = vec_expected_returns,
          target_return = target_return
        )
      
      tibble::tibble(
        target_return = target_return,
        portfolio_return =
          list_optimization$portfolio_return,
        portfolio_variance =
          list_optimization$portfolio_variance,
        portfolio_risk =
          list_optimization$portfolio_risk,
        weight_sum =
          list_optimization$weight_sum,
        list_weights = list(
          list_optimization$solution
        )
      )
    }
  )
}
#' Find a data file
#'
#' @param file_name File name to find.
#' @param search_dir Directory searched recursively.
#'
#' @return A normalized file path.
#' Find a data file
#'
#' @param file_name File name or relative path to find.
#' @param search_dir Directory or directories searched recursively.
#'
#' @return A normalized file path.
find_data_file_LFL <- function(
    file_name,
    search_dir = NULL
) {
  if (
    !is.character(file_name) ||
    length(file_name) != 1L ||
    !nzchar(file_name)
  ) {
    stop(
      "file_name must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (is.null(search_dir)) {
    search_dir <- unique(c(
      get_data_dir_LFL(),
      get_simulation_data_dir_LFL()
    ))
  }

  if (
    !is.character(search_dir) ||
    length(search_dir) == 0L
  ) {
    stop(
      "search_dir must be a non-empty character vector.",
      call. = FALSE
    )
  }

  search_dir <- search_dir[
    dir.exists(search_dir)
  ]

  if (length(search_dir) == 0L) {
    stop(
      "No data directory could be found.",
      call. = FALSE
    )
  }

  direct_candidates <- file.path(
    search_dir,
    file_name
  )

  direct_matches <- direct_candidates[
    file.exists(direct_candidates)
  ]

  if (length(direct_matches) >= 1L) {
    return(
      normalizePath(
        direct_matches[[1]],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  candidate_paths <- unlist(
    purrr::map(
      search_dir,
      \(dir) list.files(dir, recursive = TRUE, full.names = TRUE)
    ),
    use.names = FALSE
  )

  matching_paths <- candidate_paths[
    basename(candidate_paths) ==
      basename(file_name)
  ]

  if (length(matching_paths) == 0L) {
    stop(
      "File not found under ",
      paste(search_dir, collapse = ", "),
      ": ",
      file_name,
      call. = FALSE
    )
  }

  if (length(matching_paths) > 1L) {
    stop(
      "Multiple files were found for ",
      file_name,
      ": ",
      paste(matching_paths, collapse = ", "),
      call. = FALSE
    )
  }

  normalizePath(
    matching_paths[[1]],
    winslash = "/",
    mustWork = TRUE
  )
}
validate_required_columns_LFL <- function(
    tbl_data,
    vec_required_columns,
    data_name = "data"
) {
  if (!is.data.frame(tbl_data)) {
    stop(
      "tbl_data must be a data frame.",
      call. = FALSE
    )
  }
  
  if (
    !is.character(vec_required_columns) ||
    length(vec_required_columns) == 0L
  ) {
    stop(
      "vec_required_columns must be a non-empty character vector.",
      call. = FALSE
    )
  }
  
  vec_missing_columns <- setdiff(
    vec_required_columns,
    names(tbl_data)
  )
  
  if (length(vec_missing_columns) > 0L) {
    stop(
      data_name,
      " is missing required columns: ",
      paste(vec_missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(tbl_data)
}


#' Validate unique keys
#'
#' @param tbl_data A data frame.
#' @param vec_key_columns Columns defining a unique key.
#' @param data_name Name used in the error message.
#'
#' @return The input data invisibly.
validate_unique_keys_LFL <- function(
    tbl_data,
    vec_key_columns,
    data_name = "data"
) {
  validate_required_columns_LFL(
    tbl_data = tbl_data,
    vec_required_columns = vec_key_columns,
    data_name = data_name
  )
  
  tbl_duplicate_key <- tbl_data |>
    dplyr::count(
      dplyr::across(
        dplyr::all_of(vec_key_columns)
      ),
      name = "observations"
    ) |>
    dplyr::filter(observations > 1L)
  
  if (nrow(tbl_duplicate_key) > 0L) {
    stop(
      data_name,
      " contains duplicate keys for: ",
      paste(vec_key_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  invisible(tbl_data)
}


#' Summarize missing values
#'
#' @param tbl_data A data frame.
#' @param data_name Name of the data.
#'
#' @return A tibble containing missing-value counts.
summarize_missing_values_LFL <- function(
    tbl_data,
    data_name = "data"
) {
  if (!is.data.frame(tbl_data)) {
    stop(
      "tbl_data must be a data frame.",
      call. = FALSE
    )
  }
  
  tbl_data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        \(vec_value) sum(is.na(vec_value))
      )
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "missing_values"
    ) |>
    dplyr::mutate(
      data_name = data_name,
      observations = nrow(tbl_data),
      missing_ratio = dplyr::if_else(
        observations > 0L,
        missing_values / observations,
        NA_real_
      ),
      .before = variable
    )
}


#' Compute earnings surprise
#'
#' @param tbl_event Event data.
#' @param number_of_groups Number of earnings-surprise groups.
#'
#' @return Event data with earnings surprise and event strength.
compute_earnings_surprise_LFL <- function(
    tbl_event,
    number_of_groups = 5L
) {
  validate_required_columns_LFL(
    tbl_data = tbl_event,
    vec_required_columns = c(
      "event_date",
      "firm_ID",
      "earnings_forecast",
      "realized_earnings",
      "lagged_ME"
    ),
    data_name = "tbl_event"
  )
  
  if (
    !is.numeric(number_of_groups) ||
    length(number_of_groups) != 1L ||
    number_of_groups < 2
  ) {
    stop(
      "number_of_groups must be an integer greater than one.",
      call. = FALSE
    )
  }
  
  number_of_groups <- as.integer(number_of_groups)
  
  tbl_event |>
    dplyr::mutate(
      event_date = as.Date(event_date),
      firm_ID = as.integer(firm_ID),
      earnings_forecast = as.numeric(
        earnings_forecast
      ),
      realized_earnings = as.numeric(
        realized_earnings
      ),
      lagged_ME = as.numeric(lagged_ME)
    ) |>
    dplyr::arrange(
      event_date,
      firm_ID
    ) |>
    dplyr::mutate(
      event_ID = dplyr::row_number(),
      event_year = lubridate::year(event_date),
      earnings_surprise = dplyr::case_when(
        !is.finite(lagged_ME) ~ NA_real_,
        lagged_ME <= 0 ~ NA_real_,
        !is.finite(earnings_forecast) ~ NA_real_,
        !is.finite(realized_earnings) ~ NA_real_,
        TRUE ~ (
          realized_earnings -
            earnings_forecast
        ) / lagged_ME
      ),
      earnings_surprise = dplyr::if_else(
        is.finite(earnings_surprise),
        earnings_surprise,
        NA_real_
      )
    ) |>
    dplyr::group_by(event_year) |>
    dplyr::mutate(
      event_strength = dplyr::if_else(
        is.finite(earnings_surprise),
        dplyr::ntile(
          earnings_surprise,
          number_of_groups
        ),
        NA_integer_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      event_strength = factor(
        event_strength,
        levels = seq_len(number_of_groups),
        ordered = TRUE
      )
    )
}


#' Assign event day zero
#'
#' @param tbl_event Event data.
#' @param tbl_trading_calendar Trading-date table containing date_ID and date.
#'
#' @return Event data with the first trading day after the event date.
assign_event_day_zero_LFL <- function(
    tbl_event,
    tbl_trading_calendar
) {
  validate_required_columns_LFL(
    tbl_data = tbl_event,
    vec_required_columns = c(
      "event_ID",
      "event_date",
      "firm_ID"
    ),
    data_name = "tbl_event"
  )
  
  validate_required_columns_LFL(
    tbl_data = tbl_trading_calendar,
    vec_required_columns = c(
      "date_ID",
      "date"
    ),
    data_name = "tbl_trading_calendar"
  )
  
  tbl_trading_calendar <- tbl_trading_calendar |>
    dplyr::transmute(
      date_ID = as.integer(date_ID),
      date = as.Date(date)
    ) |>
    dplyr::arrange(date_ID)
  
  vec_trading_date <- tbl_trading_calendar$date
  
  tbl_result <- tbl_event |>
    dplyr::mutate(
      event_date = as.Date(event_date),
      event_trade_date_ID = findInterval(
        as.numeric(event_date),
        as.numeric(vec_trading_date)
      ) + 1L,
      event_trade_date_ID = dplyr::if_else(
        event_trade_date_ID <=
          nrow(tbl_trading_calendar),
        event_trade_date_ID,
        NA_integer_
      ),
      event_trade_date =
        vec_trading_date[event_trade_date_ID]
    )
  
  tbl_invalid_event <- tbl_result |>
    dplyr::filter(
      is.na(event_trade_date_ID) |
        is.na(event_trade_date)
    )
  
  if (nrow(tbl_invalid_event) > 0L) {
    stop(
      "Some events could not be assigned a subsequent trading day.",
      call. = FALSE
    )
  }
  
  tbl_result
}


#' Create an event-study panel
#'
#' @param tbl_event Event data containing event_trade_date_ID.
#' @param tbl_trading_calendar Trading calendar.
#' @param tbl_return_market Individual and market returns.
#' @param estimation_start First relative day.
#' @param event_end Last relative day.
#'
#' @return A complete event-study panel.
create_event_study_panel_LFL <- function(
    tbl_event,
    tbl_trading_calendar,
    tbl_return_market,
    estimation_start = -130L,
    event_end = 30L
) {
  validate_required_columns_LFL(
    tbl_data = tbl_event,
    vec_required_columns = c(
      "event_ID",
      "firm_ID",
      "event_date",
      "event_trade_date",
      "event_trade_date_ID",
      "event_year",
      "event_strength",
      "earnings_surprise"
    ),
    data_name = "tbl_event"
  )
  
  validate_required_columns_LFL(
    tbl_data = tbl_trading_calendar,
    vec_required_columns = c(
      "date_ID",
      "date"
    ),
    data_name = "tbl_trading_calendar"
  )
  
  validate_required_columns_LFL(
    tbl_data = tbl_return_market,
    vec_required_columns = c(
      "firm_ID",
      "date_ID",
      "R",
      "R_M"
    ),
    data_name = "tbl_return_market"
  )
  
  vec_relative_day <- seq(
    as.integer(estimation_start),
    as.integer(event_end)
  )
  
  tbl_valid_event <- tbl_event |>
    dplyr::filter(
      is.finite(earnings_surprise),
      !is.na(event_strength),
      !is.na(event_trade_date_ID)
    ) |>
    dplyr::select(
      event_ID,
      firm_ID,
      event_date,
      event_trade_date,
      event_trade_date_ID,
      event_year,
      event_strength,
      earnings_surprise
    )
  
  tbl_event_panel <- tbl_valid_event |>
    tidyr::crossing(
      relative_day = vec_relative_day
    ) |>
    dplyr::mutate(
      date_ID =
        event_trade_date_ID +
        relative_day
    ) |>
    dplyr::left_join(
      tbl_trading_calendar,
      by = "date_ID"
    ) |>
    dplyr::left_join(
      tbl_return_market,
      by = c(
        "firm_ID",
        "date_ID"
      )
    ) |>
    dplyr::arrange(
      event_ID,
      relative_day
    )
  
  expected_rows <-
    nrow(tbl_valid_event) *
    length(vec_relative_day)
  
  if (nrow(tbl_event_panel) != expected_rows) {
    stop(
      "The event-study panel has an unexpected number of rows.",
      call. = FALSE
    )
  }
  
  tbl_event_panel
}


#' Estimate event-specific market models
#'
#' @param tbl_event_panel Event-study panel.
#' @param estimation_start First estimation-window day.
#' @param estimation_end Last estimation-window day.
#' @param minimum_observations Minimum observations required.
#'
#' @return A tibble containing market-model parameters.
estimate_market_models_LFL <- function(
    tbl_event_panel,
    estimation_start = -130L,
    estimation_end = -31L,
    minimum_observations = 80L
) {
  validate_required_columns_LFL(
    tbl_data = tbl_event_panel,
    vec_required_columns = c(
      "event_ID",
      "firm_ID",
      "event_date",
      "event_trade_date",
      "event_year",
      "event_strength",
      "earnings_surprise",
      "relative_day",
      "R",
      "R_M"
    ),
    data_name = "tbl_event_panel"
  )
  
  tbl_estimation <- tbl_event_panel |>
    dplyr::filter(
      dplyr::between(
        relative_day,
        estimation_start,
        estimation_end
      ),
      is.finite(R),
      is.finite(R_M)
    ) |>
    dplyr::arrange(
      event_ID,
      relative_day
    )
  
  tbl_estimable_event <- tbl_estimation |>
    dplyr::group_by(event_ID) |>
    dplyr::summarise(
      estimation_observations = dplyr::n(),
      distinct_market_returns =
        dplyr::n_distinct(R_M),
      .groups = "drop"
    ) |>
    dplyr::filter(
      estimation_observations >=
        minimum_observations,
      distinct_market_returns > 1L
    )
  
  tbl_model <- tbl_estimation |>
    dplyr::semi_join(
      tbl_estimable_event,
      by = "event_ID"
    ) |>
    dplyr::group_by(event_ID) |>
    tidyr::nest() |>
    dplyr::mutate(
      list_model = purrr::map(
        data,
        \(tbl_group) {
          stats::lm(
            R ~ R_M,
            data = tbl_group
          )
        }
      ),
      list_coefficient = purrr::map(
        list_model,
        broom::tidy
      ),
      list_fit = purrr::map(
        list_model,
        broom::glance
      )
    ) |>
    dplyr::ungroup()
  
  tbl_coefficient <- tbl_model |>
    dplyr::select(
      event_ID,
      list_coefficient
    ) |>
    tidyr::unnest(list_coefficient) |>
    dplyr::select(
      event_ID,
      term,
      estimate
    ) |>
    tidyr::pivot_wider(
      names_from = term,
      values_from = estimate
    ) |>
    dplyr::rename(
      alpha = `(Intercept)`,
      beta = R_M
    )
  
  tbl_fit <- tbl_model |>
    dplyr::select(
      event_ID,
      list_fit
    ) |>
    tidyr::unnest(list_fit) |>
    dplyr::transmute(
      event_ID,
      sigma_AR = sigma,
      r_squared = r.squared,
      adjusted_r_squared = adj.r.squared,
      estimation_observations = nobs
    )
  
  tbl_event_panel |>
    dplyr::distinct(
      event_ID,
      firm_ID,
      event_date,
      event_trade_date,
      event_year,
      event_strength,
      earnings_surprise
    ) |>
    dplyr::inner_join(
      tbl_coefficient,
      by = "event_ID"
    ) |>
    dplyr::inner_join(
      tbl_fit,
      by = "event_ID"
    ) |>
    dplyr::arrange(event_ID)
}


#' Compute abnormal returns
#'
#' @param tbl_event_panel Event-study panel.
#' @param tbl_market_model Market-model parameters.
#' @param event_start First event-window day.
#' @param event_end Last event-window day.
#' @param require_complete_window Whether to require a complete event window.
#'
#' @return Event-window data containing normal return, AR, and CAR.
compute_abnormal_returns_LFL <- function(
    tbl_event_panel,
    tbl_market_model,
    event_start = -30L,
    event_end = 30L,
    require_complete_window = TRUE
) {
  validate_required_columns_LFL(
    tbl_data = tbl_market_model,
    vec_required_columns = c(
      "event_ID",
      "alpha",
      "beta",
      "sigma_AR",
      "r_squared",
      "estimation_observations"
    ),
    data_name = "tbl_market_model"
  )
  
  event_window_length <-
    as.integer(event_end) -
    as.integer(event_start) + 1L
  
  tbl_event_window <- tbl_event_panel |>
    dplyr::filter(
      dplyr::between(
        relative_day,
        event_start,
        event_end
      )
    ) |>
    dplyr::semi_join(
      tbl_market_model,
      by = "event_ID"
    )
  
  if (isTRUE(require_complete_window)) {
    tbl_complete_event <- tbl_event_window |>
      dplyr::group_by(event_ID) |>
      dplyr::summarise(
        valid_event_days = sum(
          is.finite(R) &
            is.finite(R_M)
        ),
        .groups = "drop"
      ) |>
      dplyr::filter(
        valid_event_days ==
          event_window_length
      )
    
    tbl_event_window <- tbl_event_window |>
      dplyr::semi_join(
        tbl_complete_event,
        by = "event_ID"
      )
  }
  
  tbl_result <- tbl_event_window |>
    dplyr::inner_join(
      tbl_market_model |>
        dplyr::select(
          event_ID,
          alpha,
          beta,
          sigma_AR,
          r_squared,
          adjusted_r_squared,
          estimation_observations
        ),
      by = "event_ID"
    ) |>
    dplyr::mutate(
      normal_return =
        alpha + beta * R_M,
      AR = R - normal_return
    ) |>
    dplyr::group_by(event_ID) |>
    dplyr::arrange(
      relative_day,
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      CAR = cumsum(AR)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      event_ID,
      relative_day
    )
  
  tbl_invalid_result <- tbl_result |>
    dplyr::filter(
      !is.finite(R) |
        !is.finite(R_M) |
        !is.finite(normal_return) |
        !is.finite(AR) |
        !is.finite(CAR)
    )
  
  if (nrow(tbl_invalid_result) > 0L) {
    stop(
      "The event-window results contain invalid AR or CAR values.",
      call. = FALSE
    )
  }
  
  tbl_result
}


#' Compute event-window returns
#'
#' @param tbl_abnormal_return Event-window abnormal-return data.
#' @param tbl_window Window definitions.
#'
#' @return Event-level cumulative abnormal returns.
compute_event_window_returns_LFL <- function(
    tbl_abnormal_return,
    tbl_window
) {
  validate_required_columns_LFL(
    tbl_data = tbl_abnormal_return,
    vec_required_columns = c(
      "event_ID",
      "firm_ID",
      "event_date",
      "event_trade_date",
      "event_year",
      "event_strength",
      "earnings_surprise",
      "relative_day",
      "AR"
    ),
    data_name = "tbl_abnormal_return"
  )
  
  validate_required_columns_LFL(
    tbl_data = tbl_window,
    vec_required_columns = c(
      "window_ID",
      "window_label",
      "start_day",
      "end_day"
    ),
    data_name = "tbl_window"
  )
  
  tbl_result <- purrr::pmap_dfr(
    tbl_window,
    \(window_ID,
      window_label,
      start_day,
      end_day) {
      tbl_abnormal_return |>
        dplyr::filter(
          dplyr::between(
            relative_day,
            start_day,
            end_day
          )
        ) |>
        dplyr::group_by(
          event_ID,
          firm_ID,
          event_date,
          event_trade_date,
          event_year,
          event_strength,
          earnings_surprise
        ) |>
        dplyr::summarise(
          CAR_window = sum(AR),
          observed_days = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          window_ID = .env$window_ID,
          window_label = .env$window_label,
          start_day = as.integer(
            .env$start_day
          ),
          end_day = as.integer(
            .env$end_day
          ),
          expected_days =
            as.integer(.env$end_day) -
            as.integer(.env$start_day) +
            1L,
          .before = CAR_window
        )
    }
  ) |>
    dplyr::arrange(
      window_ID,
      event_ID
    )
  
  tbl_invalid_window <- tbl_result |>
    dplyr::filter(
      observed_days != expected_days
    )
  
  if (nrow(tbl_invalid_window) > 0L) {
    stop(
      "Some events do not contain all required event-window days.",
      call. = FALSE
    )
  }
  
  tbl_result
}


#' Read a CSV file from the derived-data directory
#'
#' @param file_name CSV file name.
#' @param derived_data_dir Derived-data directory. When omitted,
#'   simulation_data/derived under the project root is used.
#' @param ... Additional arguments passed to readr::read_csv().
#'
#' @return A tibble.
read_derived_csv_file_LFL <- function(
    file_name,
    derived_data_dir = NULL,
    ...
) {
  if (
    !is.character(file_name) ||
    length(file_name) != 1L ||
    !nzchar(file_name)
  ) {
    stop(
      "file_name must be one non-empty character value.",
      call. = FALSE
    )
  }
  
  if (is.null(derived_data_dir)) {
    vec_project_dir_candidate <- unique(c(
      Sys.getenv("QUARTO_PROJECT_DIR"),
      getwd(),
      dirname(getwd())
    ))
    
    vec_project_dir_candidate <-
      vec_project_dir_candidate[
        nzchar(vec_project_dir_candidate)
      ]
    
    vec_project_dir <-
      vec_project_dir_candidate[
        file.exists(
          file.path(
            vec_project_dir_candidate,
            "_quarto.yml"
          )
        )
      ]
    
    if (length(vec_project_dir) == 0L) {
      stop(
        paste0(
          "LagrangeFinance project root could not be found from: ",
          getwd()
        ),
        call. = FALSE
      )
    }
    
    derived_data_dir <- file.path(
      vec_project_dir[[1]],
      "simulation_data",
      "derived"
    )
  }
  
  if (
    !is.character(derived_data_dir) ||
    length(derived_data_dir) != 1L ||
    !nzchar(derived_data_dir)
  ) {
    stop(
      "derived_data_dir must be one non-empty character value.",
      call. = FALSE
    )
  }
  
  file_path <- file.path(
    derived_data_dir,
    file_name
  )
  
  if (!file.exists(file_path)) {
    stop(
      "Derived CSV file was not found: ",
      normalizePath(
        file_path,
        winslash = "/",
        mustWork = FALSE
      ),
      call. = FALSE
    )
  }
  
  readr::read_csv(
    file = file_path,
    show_col_types = FALSE,
    ...
  )
}

#' Read a derived CSV file
#'
#' Compatibility wrapper for read_derived_csv_file_LFL().
#'
#' @param file_name CSV file name.
#' @param derived_data_dir Derived-data directory.
#' @param ... Additional arguments passed to readr::read_csv().
#'
#' @return A tibble.
read_derived_csv_LFL <- function(
    file_name,
    derived_data_dir = NULL,
    ...
) {
  read_derived_csv_file_LFL(
    file_name = file_name,
    derived_data_dir = derived_data_dir,
    ...
  )
}


#' Write a Parquet file to the derived-data directory
#'
#' @param tbl_data A data frame to write.
#' @param file_name Output Parquet file name or relative path.
#' @param derived_data_dir Derived-data directory. When omitted,
#'   data/derived under the project root is used.
#' @param ... Additional arguments passed to arrow::write_parquet().
#'
#' @return The normalized output path, invisibly.
write_derived_parquet_file_LFL <- function(
    tbl_data,
    file_name,
    derived_data_dir = NULL,
    ...
) {
  if (!is.data.frame(tbl_data)) {
    stop(
      "tbl_data must be a data frame.",
      call. = FALSE
    )
  }

  if (
    !is.character(file_name) ||
    length(file_name) != 1L ||
    !nzchar(file_name)
  ) {
    stop(
      "file_name must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (
    !identical(
      tolower(tools::file_ext(file_name)),
      "parquet"
    )
  ) {
    stop(
      "file_name must have a .parquet extension.",
      call. = FALSE
    )
  }

  if (is.null(derived_data_dir)) {
    derived_data_dir <- file.path(
      get_data_dir_LFL(),
      "derived"
    )
  }

  if (
    !is.character(derived_data_dir) ||
    length(derived_data_dir) != 1L ||
    !nzchar(derived_data_dir)
  ) {
    stop(
      "derived_data_dir must be one non-empty character value.",
      call. = FALSE
    )
  }

  output_path <- file.path(
    derived_data_dir,
    file_name
  )

  dir.create(
    dirname(output_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  arrow::write_parquet(
    tbl_data,
    sink = output_path,
    ...
  )

  if (!file.exists(output_path)) {
    stop(
      "Parquet output was not created: ",
      output_path,
      call. = FALSE
    )
  }

  invisible(
    normalizePath(
      output_path,
      winslash = "/",
      mustWork = TRUE
    )
  )
}
relative_path_LFL <- function(path) {
  project_dir <- find_project_dir_LFL()
  
  stringr::str_remove(
    base::normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    ),
    base::paste0(
      "^",
      stringr::fixed(
        base::paste0(project_dir, "/")
      )
    )
  )
}

#' Read a Parquet file from the derived-data directory
#'
#' @param file_name Parquet file name or relative path.
#' @param derived_data_dir Derived-data directory. When omitted,
#'   data/derived under the project root is used.
#' @param ... Additional arguments passed to arrow::read_parquet().
#'
#' @return A data frame.
read_derived_parquet_file_LFL <- function(
    file_name,
    derived_data_dir = NULL,
    ...
) {
  if (
    !is.character(file_name) ||
    length(file_name) != 1L ||
    !nzchar(file_name)
  ) {
    stop(
      "file_name must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (
    !identical(
      tolower(tools::file_ext(file_name)),
      "parquet"
    )
  ) {
    stop(
      "file_name must have a .parquet extension.",
      call. = FALSE
    )
  }

  if (is.null(derived_data_dir)) {
    derived_data_dir <- file.path(
      get_data_dir_LFL(),
      "derived"
    )
  }

  if (
    !is.character(derived_data_dir) ||
    length(derived_data_dir) != 1L ||
    !nzchar(derived_data_dir)
  ) {
    stop(
      "derived_data_dir must be one non-empty character value.",
      call. = FALSE
    )
  }

  file_path <- file.path(
    derived_data_dir,
    file_name
  )

  if (!file.exists(file_path)) {
    stop(
      "Derived Parquet file was not found: ",
      normalizePath(
        file_path,
        winslash = "/",
        mustWork = FALSE
      ),
      call. = FALSE
    )
  }

  arrow::read_parquet(
    file_path,
    ...
  )
}


# ---- Balance sheets from public filings ---------------------------------------------------------------
#
# Standalone balance sheets of Japanese securities companies (EDINET XBRL, the companies' public notices
# and the group's results presentations) and consolidated balance sheets of US companies (SEC EDGAR XBRL
# company facts), put into one form: one row per company and fiscal year, amounts in millions of the
# reporting currency.


#' Download a public file once, keeping an existing copy.
#'
#' @param url The file's URL.
#' @param destination The local path.
#' @param user_agent The User-Agent header sent with the request.
#' @param overwrite TRUE to download again over an existing copy.
#'
#' @return The normalized local path, invisibly.
download_public_file_LFL <- function(
    url,
    destination,
    user_agent = "LagrangeFinance-research",
    overwrite = FALSE
) {
  if (!is.character(url) || length(url) != 1L || !grepl("^https://", url)) {
    stop("url must be one https URL.", call. = FALSE)
  }

  if (file.exists(destination) && !overwrite) {
    return(invisible(normalizePath(destination, winslash = "/")))
  }

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(destination, ".part")
  status <- utils::download.file(
    url,
    temporary,
    mode = "wb",
    quiet = TRUE,
    method = "libcurl",
    headers = c("User-Agent" = user_agent)
  )

  if (!identical(status, 0L) || !file.exists(temporary) || file.size(temporary) == 0) {
    unlink(temporary)
    stop("Download failed: ", url, call. = FALSE)
  }

  file.rename(temporary, destination)
  invisible(normalizePath(destination, winslash = "/"))
}


#' The sources of the standalone balance sheets of Daiwa Securities, Nomura Securities and SBI Securities
#' for the fiscal years ending March 2022 to March 2026.
#'
#' - SBI Securities (EDINET code E03816) files annual securities reports for all five years; the
#'   standalone statement is read from the XBRL of each report.
#' - Daiwa Securities Co. Ltd. (E06228) filed annual securities reports up to the year ending March 2023
#'   (XBRL). For the later years its standalone balance sheet is published in the results presentation
#'   of Daiwa Securities Group Inc. on TDnet (page "大和証券貸借対照表", billions of yen).
#' - Nomura Securities Co., Ltd. (E03810) files no annual securities report; its public notices of the
#'   balance sheet (決算公告, millions of yen) are published on nomura.co.jp.
#'
#' @param raw_dir The folder of the downloaded files.
#'
#' @return A tibble with one row per company and fiscal year: `company`, `entity`, `edinet_code`,
#'   `fiscal_year_end` (the expected one), `source_type`, `document`, `url`, `path`.
get_securities_balance_sheet_sources_LFL <- function(
    raw_dir = file.path(get_data_dir_LFL(), "raw", "fundamentals", "securities_balance_sheets")
) {
  edinet_url <- function(document) {
    paste0("https://api.edinet-fsa.go.jp/api/v2/documents/", document, "?type=1")
  }
  tdnet_url <- function(document) {
    paste0("https://www2.jpx.co.jp/disc/86010/", document, ".pdf")
  }
  nomura_url <- function(document) {
    paste0("https://www.nomura.co.jp/guide/pdf/", document, ".pdf")
  }

  tibble::tribble(
    ~company,   ~entity,              ~edinet_code, ~fiscal_year_end, ~source_type,  ~document,
    "大和証券", "大和証券株式会社",   "E06228",     "2022-03-31",     "edinet_xbrl", "S100OK7U",
    "大和証券", "大和証券株式会社",   "E06228",     "2023-03-31",     "edinet_xbrl", "S100R85W",
    "大和証券", "大和証券株式会社",   "E06228",     "2024-03-31",     "tdnet_pdf",   "140120240425576701",
    "大和証券", "大和証券株式会社",   "E06228",     "2025-03-31",     "tdnet_pdf",   "140120250428525395",
    "大和証券", "大和証券株式会社",   "E06228",     "2026-03-31",     "tdnet_pdf",   "140120260427511611",
    "野村證券", "野村證券株式会社",   "E03810",     "2022-03-31",     "notice_pdf",  "20220701",
    "野村證券", "野村證券株式会社",   "E03810",     "2023-03-31",     "notice_pdf",  "20230629",
    "野村證券", "野村證券株式会社",   "E03810",     "2024-03-31",     "notice_pdf",  "20240701",
    "野村證券", "野村證券株式会社",   "E03810",     "2025-03-31",     "notice_pdf",  "20250701",
    "野村證券", "野村證券株式会社",   "E03810",     "2026-03-31",     "notice_pdf",  "20260701",
    "SBI証券",  "株式会社ＳＢＩ証券", "E03816",     "2022-03-31",     "edinet_xbrl", "S100OMLS",
    "SBI証券",  "株式会社ＳＢＩ証券", "E03816",     "2023-03-31",     "edinet_xbrl", "S100R7NM",
    "SBI証券",  "株式会社ＳＢＩ証券", "E03816",     "2024-03-31",     "edinet_xbrl", "S100TY3C",
    "SBI証券",  "株式会社ＳＢＩ証券", "E03816",     "2025-03-31",     "edinet_xbrl", "S100W8SP",
    "SBI証券",  "株式会社ＳＢＩ証券", "E03816",     "2026-03-31",     "edinet_xbrl", "S100YNPY"
  ) |>
    dplyr::mutate(
      fiscal_year_end = as.Date(fiscal_year_end),
      url = dplyr::case_when(
        source_type == "edinet_xbrl" ~ edinet_url(document),
        source_type == "tdnet_pdf" ~ tdnet_url(document),
        source_type == "notice_pdf" ~ nomura_url(document)
      ),
      folder = dplyr::case_when(
        source_type == "edinet_xbrl" ~ "edinet_xbrl",
        source_type == "tdnet_pdf" ~ "daiwa_tdnet",
        source_type == "notice_pdf" ~ "nomura"
      ),
      extension = dplyr::if_else(source_type == "edinet_xbrl", ".zip", ".pdf"),
      path = file.path(raw_dir, folder, paste0(document, extension))
    ) |>
    dplyr::select(-folder, -extension)
}


#' Download (once) the XBRL archive of an EDINET document with the EDINET API v2.
#'
#' The API needs a subscription key, read from the environment variable EDINET_API_KEY. The key is
#' never written to messages or files.
#'
#' @param document The EDINET document ID (S100....).
#' @param destination The local path of the ZIP archive.
#' @param api_key The EDINET API subscription key.
#' @param overwrite TRUE to download again over an existing copy.
#'
#' @return The normalized local path, invisibly.
download_edinet_xbrl_LFL <- function(
    document,
    destination,
    api_key = Sys.getenv("EDINET_API_KEY"),
    overwrite = FALSE
) {
  if (file.exists(destination) && !overwrite) {
    return(invisible(normalizePath(destination, winslash = "/")))
  }

  if (!nzchar(api_key)) {
    stop(
      "EDINET document ", document, " is not cached and EDINET_API_KEY is not set. ",
      "Set the EDINET API subscription key in ~/.Renviron.",
      call. = FALSE
    )
  }

  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(destination, ".part")
  response <- curl::curl_fetch_disk(
    paste0(
      "https://api.edinet-fsa.go.jp/api/v2/documents/", document,
      "?type=1&Subscription-Key=", utils::URLencode(api_key, reserved = TRUE)
    ),
    temporary
  )
  is_zip <- file.exists(temporary) &&
    identical(readBin(temporary, "raw", n = 2L), charToRaw("PK"))

  if (!identical(response$status_code, 200L) || !is_zip) {
    unlink(temporary)
    stop(
      "EDINET API did not return the XBRL archive of ", document,
      " (HTTP ", response$status_code, ").",
      call. = FALSE
    )
  }

  file.rename(temporary, destination)
  invisible(normalizePath(destination, winslash = "/"))
}


#' Read an XBRL instance document with xml2.
#'
#' Every child of the root with a `contextRef` attribute is a fact; every `context` element is a
#' context, with its instant (or period end) and whether it has a segment (dimensions).
#'
#' @param path The XBRL instance (.xbrl or .xml).
#'
#' @return A list: `facts` (`prefix`, `element`, `context`, `unit`, `decimals`, `value` as text,
#'   `measure` of the unit such as "iso4217:USD") and
#'   `contexts` (`context`, `instant`, `has_segment`).
read_xbrl_instance_LFL <- function(path) {
  document <- xml2::read_xml(path)
  fact_nodes <- xml2::xml_find_all(document, "/*/*[@contextRef]")
  context_nodes <- xml2::xml_find_all(document, "/*/*[local-name()='context']")

  facts <- tibble::tibble(
    name = xml2::xml_name(fact_nodes, xml2::xml_ns(document)),
    context = xml2::xml_attr(fact_nodes, "contextRef"),
    unit = xml2::xml_attr(fact_nodes, "unitRef"),
    decimals = xml2::xml_attr(fact_nodes, "decimals"),
    value = xml2::xml_text(fact_nodes)
  ) |>
    tidyr::separate_wider_delim(
      name,
      delim = ":",
      names = c("prefix", "element"),
      too_few = "align_end"
    ) |>
    dplyr::distinct()

  period_text <- function(xpath) {
    xml2::xml_text(xml2::xml_find_first(context_nodes, xpath))
  }

  unit_nodes <- xml2::xml_find_all(document, "/*/*[local-name()='unit']")
  units <- tibble::tibble(
    unit = xml2::xml_attr(unit_nodes, "id"),
    measure = xml2::xml_text(xml2::xml_find_first(unit_nodes, ".//*[local-name()='measure']"))
  )

  facts <- dplyr::left_join(facts, units, by = "unit")

  contexts <- tibble::tibble(
    context = xml2::xml_attr(context_nodes, "id"),
    instant = as.Date(dplyr::coalesce(
      period_text(".//*[local-name()='instant']"),
      period_text(".//*[local-name()='endDate']")
    )),
    has_segment = !is.na(period_text(".//*[local-name()='segment']")),
    dimensions = purrr::map_chr(
      context_nodes,
      \(node) {
        members <- xml2::xml_find_all(node, ".//*[local-name()='segment']/*")
        paste(
          paste0(xml2::xml_attr(members, "dimension"), "=", stringr::str_squish(xml2::xml_text(members))),
          collapse = "; "
        )
      }
    )
  )

  list(facts = facts, contexts = contexts)
}


#' Read all facts of the XBRL instance in an EDINET archive.
#'
#' The instance is the `.xbrl` file under `XBRL/PublicDoc/`; it is read with [read_xbrl_instance_LFL()].
#'
#' @param zip_path The EDINET ZIP archive.
#'
#' @return The `facts` of [read_xbrl_instance_LFL()].
read_edinet_xbrl_facts_LFL <- function(zip_path) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  instance <- members[stringr::str_detect(members, "^XBRL/PublicDoc/[^/]+\\.xbrl$")]

  if (length(instance) != 1L) {
    stop("Expected one XBRL instance in ", zip_path, ", found ", length(instance), ".", call. = FALSE)
  }

  exdir <- file.path(tempdir(), paste0("edinet_", tools::file_path_sans_ext(basename(zip_path))))
  utils::unzip(zip_path, files = instance, exdir = exdir, junkpaths = TRUE, overwrite = TRUE)

  read_xbrl_instance_LFL(file.path(exdir, basename(instance)))$facts
}


#' Extract the standalone balance sheet totals from the facts of an EDINET annual securities report.
#'
#' The standalone statement uses the context `CurrentYearInstant_NonConsolidatedMember` when the filer
#' also prepares consolidated statements, and `CurrentYearInstant` otherwise.
#'
#' @param facts The tibble of [read_edinet_xbrl_facts_LFL()].
#'
#' @return A tibble with `edinet_code`, `fiscal_year_end`, `item`, `value` (millions of yen) and
#'   `original_item` (the XBRL element).
extract_edinet_balance_sheet_LFL <- function(facts) {
  elements <- c(
    total_assets = "Assets",
    current_assets = "CurrentAssets",
    noncurrent_assets = "NoncurrentAssets",
    current_liabilities = "CurrentLiabilities",
    noncurrent_liabilities = "NoncurrentLiabilities",
    total_liabilities = "Liabilities",
    net_assets = "NetAssets"
  )

  dei <- function(element) {
    facts |>
      dplyr::filter(prefix == "jpdei_cor", .data$element == .env$element) |>
      dplyr::pull(value) |>
      utils::head(1L)
  }

  balance_sheet <- facts |>
    dplyr::filter(prefix == "jppfs_cor", element %in% elements, unit == "JPY")

  context <- if (any(balance_sheet$context == "CurrentYearInstant_NonConsolidatedMember")) {
    "CurrentYearInstant_NonConsolidatedMember"
  } else {
    "CurrentYearInstant"
  }

  tibble::tibble(item = names(elements), element = unname(elements)) |>
    dplyr::left_join(
      dplyr::filter(balance_sheet, .data$context == .env$context),
      by = "element"
    ) |>
    dplyr::transmute(
      edinet_code = dei("EDINETCodeDEI"),
      fiscal_year_end = as.Date(dei("CurrentFiscalYearEndDateDEI")),
      item,
      value = as.numeric(value) / 1e6,
      original_item = paste0("jppfs_cor:", element, " [", context, "]")
    )
}


# Amounts as printed: "1,234", "△408" (negative), a dash (none, 0).
.parse_amount_LFL <- function(token) {
  token <- stringr::str_replace_all(token, "[,，]", "")
  dplyr::case_when(
    stringr::str_detect(token, "^[－―−‐–-]$") ~ 0,
    stringr::str_detect(token, "^[△▲]") ~ -suppressWarnings(as.numeric(stringr::str_remove(token, "^[△▲]"))),
    TRUE ~ suppressWarnings(as.numeric(token))
  )
}


# The dates of balance sheet headings: "2026年3月31日" or "2026年3月末" (the last day of the month).
.parse_period_ends_LFL <- function(text) {
  match <- stringr::str_match_all(
    text,
    "(\\d{4})\\s*年\\s*(\\d{1,2})\\s*月\\s*(?:(\\d{1,2})\\s*日|末)"
  )[[1]]
  month_start <- lubridate::make_date(as.integer(match[, 2]), as.integer(match[, 3]), 1L)
  dplyr::if_else(
    is.na(match[, 4]),
    lubridate::ceiling_date(month_start, "month") - 1L,
    lubridate::make_date(as.integer(match[, 2]), as.integer(match[, 3]), as.integer(match[, 4]))
  )
}


#' Extract totals of a balance sheet printed in a PDF.
#'
#' The text from the first page that matches `page_pattern` (and `pages_after` following pages, up to
#' `end_pattern`) is normalized (NFKC: full-width digits and brackets become ASCII) and each label is
#' looked up as a word; its amount is the first number in the rest of its line. In a `two_period`
#' statement (an annual securities report: previous year, current year) the last number of the line and
#' the second date are the current ones.
#'
#' @param path The PDF.
#' @param page_pattern A regular expression that identifies the first page of the balance sheet.
#' @param labels A named character vector: item names and the regular expressions of their labels.
#' @param unit_multiplier The factor that converts the printed amounts into millions.
#' @param pages_after The number of pages after the first one that the statement may run over.
#' @param end_pattern A regular expression of the heading after the statement, or NULL.
#' @param two_period TRUE for a statement with previous- and current-year columns.
#'
#' @return A tibble with `fiscal_year_end`, `item`, `value` (millions), `original_item` (the label and
#'   the page).
extract_pdf_balance_sheet_LFL <- function(
    path,
    page_pattern,
    labels,
    unit_multiplier = 1,
    pages_after = 0L,
    end_pattern = NULL,
    two_period = FALSE
) {
  pages <- stringi::stri_trans_nfkc(pdftools::pdf_text(path))
  page <- which(stringr::str_detect(pages, page_pattern))[1]

  if (is.na(page)) {
    stop("No page matches '", page_pattern, "' in ", path, call. = FALSE)
  }

  text <- paste(pages[page:min(length(pages), page + pages_after)], collapse = "\n")
  text <- stringr::str_extract(text, paste0(page_pattern, "[\\s\\S]*"))

  if (!is.null(end_pattern)) {
    text <- stringr::str_remove(text, paste0(end_pattern, "[\\s\\S]*$"))
  }

  # the label as a word, then the rest of its line; amounts are the numbers in the rest of the line
  # (a note column such as "10,29" may come first, so the current year of a two-period statement is
  # the last number), and a lone dash is zero
  match <- purrr::map(
    labels,
    \(label) stringr::str_match(text, paste0("(?:^|\\s)(", label, ")(?=[ \\t])([^\\n]*)"))
  )
  amounts <- purrr::map_chr(
    match,
    \(m) {
      tokens <- stringr::str_extract_all(
        dplyr::coalesce(m[, 3], ""),
        "[△▲]?[0-9][0-9,]*|(?<=\\s)[−－―‐–-](?=\\s|$)"
      )[[1]]
      if (!length(tokens)) NA_character_ else if (two_period) tokens[[length(tokens)]] else tokens[[1]]
    }
  )
  matched_labels <- purrr::map_chr(match, \(m) m[, 2])
  period_ends <- .parse_period_ends_LFL(text)

  tibble::tibble(
    fiscal_year_end = period_ends[[if (two_period) 2L else 1L]],
    item = names(labels),
    value = .parse_amount_LFL(amounts) * unit_multiplier,
    original_item = dplyr::if_else(
      is.na(matched_labels),
      NA_character_,
      paste0(matched_labels, " (PDF p.", page, ")")
    )
  )
}


# Labels of Nomura Securities' public notices (millions of yen).
.nomura_notice_labels_LFL <- function() {
  c(
    total_assets = "資産合計",
    current_assets = "流動資産計",
    noncurrent_assets = "固定資産計",
    current_liabilities = "流動負債計",
    noncurrent_liabilities = "固定負債計",
    statutory_reserves = "特別法上の準備金計",
    total_liabilities = "負債合計",
    net_assets = "純資産合計"
  )
}


# Labels of the page "大和証券貸借対照表" in Daiwa Securities Group's results presentation (billions of
# yen; statutory reserves are not shown).
.daiwa_tdnet_labels_LFL <- function() {
  c(
    total_assets = "資産合計",
    current_assets = "流動資産",
    noncurrent_assets = "固定資産",
    current_liabilities = "流動負債",
    noncurrent_liabilities = "固定負債",
    total_liabilities = "負債合計",
    net_assets = "純資産"
  )
}


# Labels of the standalone balance sheet (【貸借対照表】) of an annual securities report (millions of yen;
# subtotals end in 合計 or 計).
.edinet_pdf_labels_LFL <- function() {
  c(
    total_assets = "資産合計",
    current_assets = "流動資産合?計",
    noncurrent_assets = "固定資産合?計",
    current_liabilities = "流動負債合?計",
    noncurrent_liabilities = "固定負債合?計",
    statutory_reserves = "特別法上の準備金合?計",
    total_liabilities = "負債合計",
    net_assets = "純資産合計"
  )
}


#' Download and read the balance sheet totals of one source of
#' [get_securities_balance_sheet_sources_LFL()].
#'
#' An EDINET document is read from its XBRL when the archive is cached or EDINET_API_KEY is set, and
#' otherwise from the PDF of the annual securities report that EDINET publishes without a key. The
#' fiscal year end found in the document must be the expected one, and an EDINET document must be filed
#' by the expected company.
#'
#' @param source_type "edinet_xbrl", "tdnet_pdf" or "notice_pdf".
#' @param document,url,path,entity,edinet_code,fiscal_year_end The columns of the source.
#'
#' @return A tibble with `item`, `value` (millions of yen), `original_item`, `amount_precision`
#'   (millions), `method` and `source_url`.
read_securities_balance_sheet_LFL <- function(
    source_type,
    document,
    url,
    path,
    entity,
    edinet_code,
    fiscal_year_end
) {
  use_xbrl <- source_type == "edinet_xbrl" &&
    (file.exists(path) || nzchar(Sys.getenv("EDINET_API_KEY")))
  method <- dplyr::case_when(
    use_xbrl ~ "edinet_xbrl",
    source_type == "edinet_xbrl" ~ "edinet_pdf",
    TRUE ~ source_type
  )
  edinet_pdf_url <- paste0("https://disclosure2dl.edinet-fsa.go.jp/searchdocument/pdf/", document, ".pdf")
  edinet_pdf_path <- file.path(dirname(dirname(path)), "edinet_pdf", paste0(document, ".pdf"))

  values <- switch(
    method,
    edinet_xbrl = {
      download_edinet_xbrl_LFL(document, path)
      extracted <- extract_edinet_balance_sheet_LFL(read_edinet_xbrl_facts_LFL(path))
      if (!identical(unique(extracted$edinet_code), edinet_code)) {
        stop("EDINET document ", document, " is not filed by ", edinet_code, ".", call. = FALSE)
      }
      dplyr::mutate(extracted, amount_precision = 1, source_url = paste0("https://disclosure2.edinet-fsa.go.jp/ (", document, ")"))
    },
    edinet_pdf = {
      download_public_file_LFL(edinet_pdf_url, edinet_pdf_path)
      cover <- stringi::stri_trans_nfkc(pdftools::pdf_text(edinet_pdf_path)[[1]])
      if (!stringr::str_detect(cover, stringr::fixed(stringi::stri_trans_nfkc(entity)))) {
        stop("EDINET document ", document, " is not filed by ", entity, ".", call. = FALSE)
      }
      extract_pdf_balance_sheet_LFL(
        edinet_pdf_path,
        "【貸借対照表】",
        .edinet_pdf_labels_LFL(),
        pages_after = 3L,
        end_pattern = "【損益計算書】",
        two_period = TRUE
      ) |>
        dplyr::mutate(amount_precision = 1, source_url = edinet_pdf_url)
    },
    tdnet_pdf = {
      download_public_file_LFL(url, path)
      extract_pdf_balance_sheet_LFL(path, "大和証券貸借対照表", .daiwa_tdnet_labels_LFL(), 1000) |>
        dplyr::mutate(amount_precision = 1000, source_url = url)
    },
    notice_pdf = {
      download_public_file_LFL(url, path)
      extract_pdf_balance_sheet_LFL(path, "野村[證証]券株式会社[\\s\\S]*貸\\s*借\\s*対\\s*照\\s*表", .nomura_notice_labels_LFL()) |>
        dplyr::mutate(amount_precision = 1, source_url = url)
    },
    stop("Unknown source_type: ", source_type, call. = FALSE)
  )

  found <- unique(values$fiscal_year_end)
  if (!identical(found, fiscal_year_end)) {
    stop(document, " is the balance sheet of ", format(found), ", not of ", format(fiscal_year_end), ".", call. = FALSE)
  }

  values |>
    dplyr::mutate(method = method) |>
    dplyr::select(item, value, original_item, amount_precision, method, source_url)
}


# Columns of the balance sheet comparison, in order.
.balance_sheet_items_LFL <- function() {
  c(
    "total_assets", "current_assets", "noncurrent_assets", "current_liabilities",
    "noncurrent_liabilities", "statutory_reserves", "total_liabilities", "net_assets"
  )
}


# Japanese names of the items.
.balance_sheet_item_names_LFL <- function() {
  c(
    total_assets = "総資産",
    current_assets = "流動資産",
    noncurrent_assets = "固定資産",
    current_liabilities = "流動負債",
    noncurrent_liabilities = "固定負債",
    statutory_reserves = "特別法上の準備金",
    total_liabilities = "負債合計",
    net_assets = "純資産"
  )
}


# Names of the methods of reading a source.
.source_method_names_LFL <- function() {
  c(
    edinet_xbrl = "EDINET 有価証券報告書 XBRL",
    edinet_pdf = "EDINET 有価証券報告書 PDF",
    tdnet_pdf = "TDnet 大和証券グループ本社 決算説明資料 PDF（十億円単位）",
    notice_pdf = "野村證券 決算公告 PDF",
    sec_xbrl = "SEC EDGAR 年次報告書 XBRL"
  )
}


#' Fill the items a source does not report, and record which ones were calculated.
#'
#' - 固定資産 = 総資産 − 流動資産
#' - 負債合計 = 総資産 − 純資産
#' - 固定負債 = 負債合計 − 流動負債 − 特別法上の準備金
#' - 特別法上の準備金 = 負債合計 − 流動負債 − 固定負債 (Japanese securities companies only;
#'   `reserves_apply = FALSE` leaves it NA)
#'
#' @param tbl_wide One row per company and fiscal year with the columns of
#'   `.balance_sheet_items_LFL()` (NA where not reported) and `reserves_apply`.
#'
#' @return `tbl_wide` with the items filled and `calculated_items` (the Japanese names of the
#'   calculated items, separated by "、"; "" when none).
complete_balance_sheet_LFL <- function(tbl_wide) {
  names_ja <- .balance_sheet_item_names_LFL()

  tbl_wide |>
    dplyr::mutate(
      calc_noncurrent_assets = is.na(noncurrent_assets),
      noncurrent_assets = dplyr::coalesce(noncurrent_assets, total_assets - current_assets),
      calc_total_liabilities = is.na(total_liabilities),
      total_liabilities = dplyr::coalesce(total_liabilities, total_assets - net_assets),
      calc_noncurrent_liabilities = is.na(noncurrent_liabilities),
      noncurrent_liabilities = dplyr::coalesce(
        noncurrent_liabilities,
        total_liabilities - current_liabilities - dplyr::coalesce(statutory_reserves, 0)
      ),
      calc_statutory_reserves = reserves_apply & is.na(statutory_reserves),
      statutory_reserves = dplyr::if_else(
        reserves_apply,
        dplyr::coalesce(statutory_reserves, total_liabilities - current_liabilities - noncurrent_liabilities),
        NA_real_
      ),
      calculated_items = purrr::pmap_chr(
        list(calc_noncurrent_assets, calc_total_liabilities, calc_noncurrent_liabilities, calc_statutory_reserves),
        \(...) {
          paste(
            names_ja[c("noncurrent_assets", "total_liabilities", "noncurrent_liabilities", "statutory_reserves")][c(...)],
            collapse = "、"
          )
        }
      )
    ) |>
    dplyr::select(-dplyr::starts_with("calc_"))
}


#' Read, check and combine the standalone balance sheets of the securities companies.
#'
#' @param sources The tibble of [get_securities_balance_sheet_sources_LFL()].
#'
#' @return A list: `balance_sheets` (one row per company and fiscal year in the form of
#'   [build_balance_sheet_comparison_LFL()]) and `original_items` (one row per company, fiscal year and
#'   item: the value and the label or XBRL element it was read from).
build_securities_balance_sheets_LFL <- function(sources = get_securities_balance_sheet_sources_LFL()) {
  items <- .balance_sheet_items_LFL()

  read <- sources |>
    dplyr::mutate(
      values = purrr::pmap(
        list(source_type, document, url, path, entity, edinet_code, fiscal_year_end),
        read_securities_balance_sheet_LFL
      )
    ) |>
    dplyr::select(company, entity, fiscal_year_end, document, values) |>
    tidyr::unnest(values) |>
    dplyr::filter(item %in% items)

  balance_sheets <- read |>
    dplyr::select(-original_item) |>
    dplyr::mutate(item = factor(item, levels = items)) |>
    tidyr::pivot_wider(names_from = item, values_from = value, names_expand = TRUE) |>
    dplyr::mutate(
      reserves_apply = TRUE,
      basis = "単体",
      currency = "JPY",
      unit = "百万円",
      source = unname(.source_method_names_LFL()[method])
    ) |>
    complete_balance_sheet_LFL()

  list(
    balance_sheets = balance_sheets,
    original_items = dplyr::select(read, company, fiscal_year_end, document, item, value, original_item)
  )
}


# ---- SEC EDGAR: annual reports (10-K, 20-F) in XBRL ---------------------------------------------------

#' The SEC central index keys of the GAFAM companies.
#'
#' @return A tibble of `company`, `entity` and `cik`.
get_gafam_companies_LFL <- function() {
  tibble::tribble(
    ~company,     ~entity,                   ~cik,
    "Alphabet",   "Alphabet Inc.",           1652044L,
    "Apple",      "Apple Inc.",              320193L,
    "Meta",       "Meta Platforms, Inc.",    1326801L,
    "Amazon",     "Amazon.com, Inc.",        1018724L,
    "Microsoft",  "Microsoft Corporation",   789019L
  )
}


# The User-Agent sent to SEC EDGAR, which asks automated clients for a contact.
.sec_user_agent_LFL <- function() {
  Sys.getenv("SEC_USER_AGENT", "LagrangeFinance research besuke.poripori@gmail.com")
}


#' Download (once) and read the XBRL company facts of a company from SEC EDGAR.
#'
#' @param cik The central index key.
#' @param raw_dir The folder of the downloaded JSON files.
#' @param overwrite TRUE to download again.
#'
#' @return The parsed JSON as a list.
get_sec_company_facts_LFL <- function(
    cik,
    raw_dir = file.path(get_data_dir_LFL(), "raw", "fundamentals", "sec_company_facts"),
    overwrite = FALSE
) {
  file_name <- sprintf("CIK%010d.json", as.integer(cik))
  path <- file.path(raw_dir, file_name)
  download_public_file_LFL(
    paste0("https://data.sec.gov/api/xbrl/companyfacts/", file_name),
    path,
    user_agent = .sec_user_agent_LFL(),
    overwrite = overwrite
  )
  jsonlite::read_json(path, simplifyVector = TRUE)
}


#' Find the annual reports of the last fiscal years in SEC company facts.
#'
#' The fiscal year ends are the instants of total assets (`Assets` in us-gaap or ifrs-full) reported in
#' annual reports (10-K or 20-F, `fp == "FY"`); each fiscal year end is matched with the first annual
#' report that reports it, which is the report of that fiscal year itself.
#'
#' @param facts The list of [get_sec_company_facts_LFL()].
#' @param years The number of fiscal years.
#'
#' @return A tibble with `fiscal_year_end`, `accession`, `form`, `filed`, `taxonomy`, `currency` and
#'   `total_assets` (as in company facts, for the cross-check).
find_sec_annual_reports_LFL <- function(facts, years = 5L) {
  taxonomy <- intersect(c("us-gaap", "ifrs-full"), names(facts$facts))[1]
  units <- facts$facts[[taxonomy]]$Assets$units

  if (is.null(units)) {
    stop("No total assets in the company facts of ", facts$entityName, ".", call. = FALSE)
  }

  purrr::imap(units, \(values, currency) dplyr::mutate(tibble::as_tibble(values), currency = currency)) |>
    purrr::list_rbind() |>
    dplyr::filter(form %in% c("10-K", "20-F"), fp == "FY") |>
    dplyr::transmute(
      fiscal_year_end = as.Date(end),
      accession = accn,
      form,
      filed = as.Date(filed),
      taxonomy = taxonomy,
      currency,
      total_assets = val
    ) |>
    dplyr::group_by(fiscal_year_end) |>
    dplyr::slice_min(filed, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(fiscal_year_end)) |>
    utils::head(years)
}


#' Download (once) the XBRL instance of an SEC filing.
#'
#' The filing index (`index.json`) lists the files of the filing; the instance is the file ending in
#' `_htm.xml` (extracted from inline XBRL) or, for older filings, the `.xml` file that is not a
#' linkbase or the filing summary.
#'
#' @param cik The central index key.
#' @param accession The accession number (0000320193-24-000123).
#' @param raw_dir The folder of the downloaded files.
#'
#' @return The local path of the instance.
get_sec_xbrl_instance_LFL <- function(
    cik,
    accession,
    raw_dir = file.path(get_data_dir_LFL(), "raw", "fundamentals", "sec_xbrl")
) {
  folder <- file.path(raw_dir, as.character(cik), accession)
  cached <- list.files(folder, pattern = "\\.xml$", full.names = TRUE)

  if (length(cached) == 1L) {
    return(cached)
  }

  base_url <- paste0(
    "https://www.sec.gov/Archives/edgar/data/", as.integer(cik), "/",
    stringr::str_remove_all(accession, "-"), "/"
  )
  index_path <- file.path(folder, "index.json")
  download_public_file_LFL(paste0(base_url, "index.json"), index_path, user_agent = .sec_user_agent_LFL())
  files <- jsonlite::read_json(index_path, simplifyVector = TRUE)$directory$item$name

  candidates <- files[
    stringr::str_detect(files, "\\.xml$") &
      !stringr::str_detect(files, "(_cal|_def|_lab|_pre)\\.xml$|^FilingSummary\\.xml$")
  ]
  instance <- c(candidates[stringr::str_detect(candidates, "_htm\\.xml$")], candidates)[1]

  if (is.na(instance)) {
    stop("No XBRL instance in SEC filing ", accession, ".", call. = FALSE)
  }

  Sys.sleep(0.2)
  path <- file.path(folder, instance)
  download_public_file_LFL(paste0(base_url, instance), path, user_agent = .sec_user_agent_LFL())
  unlink(index_path)
  path
}


#' The XBRL concepts of the balance sheet items, in order of preference, for us-gaap and ifrs-full.
#'
#' @return A tibble with `item`, `taxonomy` and `concepts` (a list of character vectors).
get_sec_concept_map_LFL <- function() {
  tibble::tribble(
    ~item,                     ~taxonomy,   ~concepts,
    "total_assets",            "us-gaap",   c("Assets"),
    "current_assets",          "us-gaap",   c("AssetsCurrent"),
    "noncurrent_assets",       "us-gaap",   c("AssetsNoncurrent"),
    "current_liabilities",     "us-gaap",   c("LiabilitiesCurrent"),
    "noncurrent_liabilities",  "us-gaap",   c("LiabilitiesNoncurrent"),
    "total_liabilities",       "us-gaap",   c("Liabilities"),
    "net_assets",              "us-gaap",   c("StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest", "StockholdersEquity"),
    "liabilities_and_equity",  "us-gaap",   c("LiabilitiesAndStockholdersEquity"),
    "property_plant_equipment","us-gaap",   c("PropertyPlantAndEquipmentNet"),
    "goodwill",                "us-gaap",   c("Goodwill"),
    "other_intangible_assets", "us-gaap",   c("IntangibleAssetsNetExcludingGoodwill", "FiniteLivedIntangibleAssetsNet"),
    "total_assets",            "ifrs-full", c("Assets"),
    "current_assets",          "ifrs-full", c("CurrentAssets"),
    "noncurrent_assets",       "ifrs-full", c("NoncurrentAssets"),
    "current_liabilities",     "ifrs-full", c("CurrentLiabilities"),
    "noncurrent_liabilities",  "ifrs-full", c("NoncurrentLiabilities"),
    "total_liabilities",       "ifrs-full", c("Liabilities"),
    "net_assets",              "ifrs-full", c("Equity"),
    "liabilities_and_equity",  "ifrs-full", c("EquityAndLiabilities"),
    "property_plant_equipment","ifrs-full", c("PropertyPlantAndEquipment", "PropertyPlantAndEquipmentIncludingRightofuseAssets"),
    "goodwill",                "ifrs-full", c("Goodwill"),
    "other_intangible_assets", "ifrs-full", c("IntangibleAssetsOtherThanGoodwill")
  )
}


#' Extract balance sheet items from an XBRL instance of an annual report.
#'
#' The values are the facts without dimensions (no segment) at the fiscal year end, in the reporting
#' currency; for each item the first concept of the concept map that is reported is used.
#'
#' @param instance The list of [read_xbrl_instance_LFL()].
#' @param fiscal_year_end The balance sheet date.
#' @param taxonomy "us-gaap" or "ifrs-full".
#' @param currency The reporting currency (USD, JPY, DKK...).
#' @param concept_map The tibble of [get_sec_concept_map_LFL()].
#'
#' @return A tibble with `item`, `value` (in units of the currency) and `original_item` (the concept).
extract_xbrl_balance_sheet_LFL <- function(
    instance,
    fiscal_year_end,
    taxonomy,
    currency,
    concept_map = get_sec_concept_map_LFL()
) {
  contexts <- instance$contexts |>
    dplyr::filter(!has_segment, instant == fiscal_year_end) |>
    dplyr::pull(context)

  facts <- instance$facts |>
    dplyr::filter(
      prefix == taxonomy,
      context %in% contexts,
      measure == paste0("iso4217:", currency)
    ) |>
    dplyr::distinct(element, value)

  concept_prefix <- taxonomy

  concept_map |>
    dplyr::filter(.data$taxonomy == .env$taxonomy) |>
    dplyr::mutate(
      found = purrr::map(
        concepts,
        \(concepts) {
          hit <- facts |>
            dplyr::filter(element %in% concepts) |>
            dplyr::mutate(rank = match(element, concepts)) |>
            dplyr::arrange(rank) |>
            utils::head(1L)
          tibble::tibble(
            value = if (nrow(hit)) as.numeric(hit$value) else NA_real_,
            original_item = if (nrow(hit)) paste0(concept_prefix, ":", hit$element) else NA_character_
          )
        }
      )
    ) |>
    tidyr::unnest(found) |>
    dplyr::select(item, value, original_item)
}


#' Split goodwill out of an IFRS intangible assets line that includes it.
#'
#' Some IFRS filers present goodwill within the balance sheet line of intangible assets and tag that
#' line `IntangibleAssetsOtherThanGoodwill` (Novo Nordisk up to 2023). When no undimensioned `Goodwill`
#' is reported, goodwill is taken from the note that allocates it to segments (the sum over
#' `ifrs-full:SegmentsAxis`) or, failing that, from the gross carrying amount less accumulated
#' impairment, and it is split out of the line only if the line equals that goodwill plus
#' the classes of other intangible assets in the note (`ClassesOfIntangibleAssetsOtherThanGoodwillAxis`,
#' without the internally generated subtotal). Otherwise the values are returned unchanged.
#'
#' @param values The tibble of [extract_xbrl_balance_sheet_LFL()].
#' @param instance The list of [read_xbrl_instance_LFL()].
#' @param fiscal_year_end The balance sheet date.
#' @param currency The reporting currency.
#'
#' @return `values` with `goodwill` and `other_intangible_assets` split where the note reconciles.
reconcile_ifrs_goodwill_LFL <- function(values, instance, fiscal_year_end, currency) {
  value_of <- function(name) values$value[values$item == name][1]
  line <- value_of("other_intangible_assets")

  if (!is.na(value_of("goodwill")) || is.na(line)) {
    return(values)
  }

  notes <- instance$facts |>
    dplyr::filter(prefix == "ifrs-full", measure == paste0("iso4217:", currency)) |>
    dplyr::inner_join(dplyr::filter(instance$contexts, instant == fiscal_year_end), by = "context") |>
    dplyr::distinct(element, dimensions, value) |>
    dplyr::mutate(value = as.numeric(value))
  carrying_axis <- "^ifrs-full:CarryingAmountAccumulatedDepreciationAmortisationAndImpairmentAndGrossCarryingAmountAxis="
  by_segment <- notes |>
    dplyr::filter(element == "Goodwill", stringr::str_detect(dimensions, "^ifrs-full:SegmentsAxis=[^;]+$"))
  by_carrying_amount <- notes |>
    dplyr::filter(
      element == "Goodwill",
      stringr::str_detect(dimensions, paste0(carrying_axis, "ifrs-full:(GrossCarryingAmountMember|AccumulatedImpairmentMember)$"))
    )
  goodwill <- if (nrow(by_segment)) by_segment else by_carrying_amount
  goodwill_source <- if (nrow(by_segment)) "セグメント別の合計" else "取得原価 − 減損損失累計額"
  classes <- notes |>
    dplyr::filter(
      element == "IntangibleAssetsOtherThanGoodwill",
      stringr::str_detect(dimensions, "^ifrs-full:ClassesOfIntangibleAssetsOtherThanGoodwillAxis=[^;]+$"),
      !stringr::str_detect(dimensions, "InternallyGeneratedMember")
    )

  if (!nrow(goodwill) || !nrow(classes) || abs(line - sum(goodwill$value) - sum(classes$value)) > 0.5e6) {
    return(values)
  }

  values |>
    dplyr::mutate(
      value = dplyr::case_when(
        item == "goodwill" ~ sum(goodwill$value),
        item == "other_intangible_assets" ~ line - sum(goodwill$value),
        TRUE ~ value
      ),
      original_item = dplyr::case_when(
        item == "goodwill" ~ paste0("ifrs-full:Goodwill（注記：", goodwill_source, "）"),
        item == "other_intangible_assets" ~ "ifrs-full:IntangibleAssetsOtherThanGoodwill − 注記ののれん（貸借対照表の無形資産はのれんを含む）",
        TRUE ~ original_item
      )
    )
}


#' Read the consolidated balance sheets of SEC filers from the XBRL instances of their annual reports.
#'
#' Company facts locate the annual report of each of the last fiscal years; each report's XBRL instance
#' is read with xml2, and its total assets must equal those in company facts.
#'
#' @param companies A tibble with `company`, `entity` and `cik`.
#' @param years The number of fiscal years.
#' @param concept_map The tibble of [get_sec_concept_map_LFL()].
#'
#' @return A list: `values` (one row per company, fiscal year and item: `value` in units of the
#'   currency, `original_item`) and `reports` (one row per company and fiscal year: `accession`,
#'   `form`, `filed`, `taxonomy`, `currency`, `source_url`).
read_sec_balance_sheets_LFL <- function(companies, years = 5L, concept_map = get_sec_concept_map_LFL()) {
  reports <- companies |>
    dplyr::mutate(
      reports = purrr::map(cik, \(cik) find_sec_annual_reports_LFL(get_sec_company_facts_LFL(cik), years = years))
    ) |>
    tidyr::unnest(reports) |>
    dplyr::mutate(
      source_url = paste0(
        "https://www.sec.gov/Archives/edgar/data/", cik, "/",
        stringr::str_remove_all(accession, "-"), "/"
      )
    )

  values <- reports |>
    dplyr::mutate(
      values = purrr::pmap(
        list(cik, accession, fiscal_year_end, taxonomy, currency),
        \(cik, accession, fiscal_year_end, taxonomy, currency) {
          instance <- read_xbrl_instance_LFL(get_sec_xbrl_instance_LFL(cik, accession))
          values <- extract_xbrl_balance_sheet_LFL(instance, fiscal_year_end, taxonomy, currency, concept_map)
          if (taxonomy == "ifrs-full") {
            values <- reconcile_ifrs_goodwill_LFL(values, instance, fiscal_year_end, currency)
          }
          values
        }
      )
    ) |>
    dplyr::select(company, fiscal_year_end, accession, currency, total_assets_facts = total_assets, values) |>
    tidyr::unnest(values)

  mismatch <- values |>
    dplyr::filter(item == "total_assets", is.na(value) | value != total_assets_facts)

  if (nrow(mismatch) > 0L) {
    stop(
      "Total assets in the XBRL instance differ from company facts: ",
      paste(mismatch$company, mismatch$fiscal_year_end, collapse = ", "),
      call. = FALSE
    )
  }

  list(
    values = dplyr::select(values, -total_assets_facts),
    reports = dplyr::select(reports, -total_assets)
  )
}


#' Build the consolidated balance sheets of the GAFAM companies.
#'
#' Noncurrent assets and liabilities that a company does not report are calculated (see
#' [complete_balance_sheet_LFL()]); so are total liabilities where they are not tagged (Amazon).
#' 純資産 is total equity including noncontrolling interests where reported, else stockholders' equity.
#' Total assets must equal total liabilities and equity.
#'
#' @param years The number of fiscal years.
#' @param companies The tibble of [get_gafam_companies_LFL()].
#'
#' @return A list: `balance_sheets` in the form of [build_balance_sheet_comparison_LFL()] and
#'   `original_items`.
build_gafam_balance_sheets_LFL <- function(years = 5L, companies = get_gafam_companies_LFL()) {
  items <- .balance_sheet_items_LFL()
  read <- read_sec_balance_sheets_LFL(companies, years = years)

  wide <- read$values |>
    dplyr::select(company, fiscal_year_end, item, value) |>
    dplyr::mutate(item = factor(item, levels = unique(get_sec_concept_map_LFL()$item))) |>
    tidyr::pivot_wider(names_from = item, values_from = value, names_expand = TRUE)

  unbalanced <- dplyr::filter(wide, is.na(liabilities_and_equity) | total_assets != liabilities_and_equity)
  if (nrow(unbalanced) > 0L) {
    stop("Total assets differ from liabilities and equity: ", paste(unbalanced$company, unbalanced$fiscal_year_end, collapse = ", "), call. = FALSE)
  }

  balance_sheets <- read$reports |>
    dplyr::select(company, entity, fiscal_year_end, document = accession, currency, source_url) |>
    dplyr::left_join(
      wide |>
        dplyr::select(company, fiscal_year_end, dplyr::any_of(items)) |>
        dplyr::mutate(dplyr::across(dplyr::any_of(items), \(value) value / 1e6)),
      by = c("company", "fiscal_year_end")
    ) |>
    dplyr::mutate(statutory_reserves = NA_real_) |>
    dplyr::mutate(
      reserves_apply = FALSE,
      basis = "連結",
      unit = paste0("百万", currency),
      amount_precision = 1e-6,
      method = "sec_xbrl",
      source = unname(.source_method_names_LFL()["sec_xbrl"])
    ) |>
    complete_balance_sheet_LFL()

  list(
    balance_sheets = balance_sheets,
    original_items = read$values |>
      dplyr::filter(item %in% items) |>
      dplyr::transmute(company, fiscal_year_end, document = accession, item, value = value / 1e6, original_item)
  )
}


#' Check a balance sheet comparison of [build_balance_sheet_comparison_LFL()].
#'
#' - each company has `years` fiscal years, one row each, with no missing amounts;
#' - 総資産 = 流動資産 + 固定資産;
#' - 総資産 = 流動負債 + 固定負債 + 特別法上の準備金 + 純資産.
#'
#' A difference of up to three units of the published precision (`amount_precision`, in millions) is
#' allowed for the rounding of the published amounts.
#'
#' @param tbl_balance_sheet The tibble of the balance sheets.
#' @param years The number of fiscal years per company.
#'
#' @return A tibble with one row per company: `periods`, `first_fiscal_year_end`,
#'   `last_fiscal_year_end`, `max_asset_difference`, `max_funding_difference` (millions) and `passed`.
validate_balance_sheet_comparison_LFL <- function(tbl_balance_sheet, years = 5L) {
  required <- c(
    "company", "entity", "fiscal_year_end", .balance_sheet_items_LFL(), "currency", "unit", "basis",
    "source", "amount_precision", "calculated_items"
  )
  validate_required_columns_LFL(tbl_balance_sheet, required, "tbl_balance_sheet")
  validate_unique_keys_LFL(tbl_balance_sheet, c("company", "fiscal_year_end"), "tbl_balance_sheet")

  amounts <- setdiff(.balance_sheet_items_LFL(), "statutory_reserves")
  missing <- tbl_balance_sheet |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(amounts), is.na))

  if (nrow(missing) > 0L) {
    stop(
      "Missing amounts: ",
      paste(missing$company, missing$fiscal_year_end, collapse = ", "),
      call. = FALSE
    )
  }

  checks <- tbl_balance_sheet |>
    dplyr::mutate(
      asset_difference = total_assets - current_assets - noncurrent_assets,
      funding_difference = total_assets - current_liabilities - noncurrent_liabilities -
        dplyr::coalesce(statutory_reserves, 0) - net_assets,
      tolerance = 3 * amount_precision + 1e-9
    ) |>
    dplyr::summarise(
      .by = company,
      periods = dplyr::n(),
      first_fiscal_year_end = min(fiscal_year_end),
      last_fiscal_year_end = max(fiscal_year_end),
      max_asset_difference = max(abs(asset_difference)),
      max_funding_difference = max(abs(funding_difference)),
      passed = periods == years &
        all(abs(asset_difference) <= tolerance) &
        all(abs(funding_difference) <= tolerance)
    )

  if (!all(checks$passed)) {
    stop(
      "Balance sheet checks failed for: ",
      paste(checks$company[!checks$passed], collapse = ", "),
      call. = FALSE
    )
  }

  checks
}


#' Build the balance sheet comparison of the three Japanese securities companies (standalone, yen) and
#' GAFAM (consolidated, US dollars) for their last five fiscal years.
#'
#' @param securities_sources The tibble of [get_securities_balance_sheet_sources_LFL()].
#' @param gafam_companies The tibble of [get_gafam_companies_LFL()].
#' @param years The number of fiscal years per company.
#'
#' @return A list: `balance_sheets` (one row per company and fiscal year with `company`, `entity`,
#'   `fiscal_year_end`, the amounts of `.balance_sheet_items_LFL()` in millions of `currency`,
#'   `currency`, `unit`, `basis` (単体 or 連結), `source`, `source_url`, `document`,
#'   `amount_precision` and `calculated_items`), `original_items` (one row per company, fiscal year
#'   and reported item: the value as read and the label or XBRL element in the source document) and
#'   `checks` (the result of [validate_balance_sheet_comparison_LFL()]).
build_balance_sheet_comparison_LFL <- function(
    securities_sources = get_securities_balance_sheet_sources_LFL(),
    gafam_companies = get_gafam_companies_LFL(),
    years = 5L
) {
  company_order <- c(unique(securities_sources$company), gafam_companies$company)
  parts <- list(
    build_securities_balance_sheets_LFL(securities_sources),
    build_gafam_balance_sheets_LFL(years = years, companies = gafam_companies)
  )
  arrange_companies <- function(tbl) {
    tbl |>
      dplyr::mutate(company = factor(company, levels = company_order)) |>
      dplyr::arrange(company, dplyr::desc(fiscal_year_end)) |>
      dplyr::mutate(company = as.character(company))
  }

  balance_sheets <- purrr::map(parts, "balance_sheets") |>
    purrr::list_rbind() |>
    arrange_companies() |>
    dplyr::select(
      company, entity, fiscal_year_end, dplyr::all_of(.balance_sheet_items_LFL()),
      currency, unit, basis, source, document, source_url, amount_precision, calculated_items
    )

  list(
    balance_sheets = balance_sheets,
    original_items = purrr::map(parts, "original_items") |>
      purrr::list_rbind() |>
      arrange_companies(),
    checks = validate_balance_sheet_comparison_LFL(balance_sheets, years = years)
  )
}


#' Format the balance sheet comparison for display.
#'
#' Amounts are shown in millions of the reporting currency with thousands separators; calculated
#' amounts carry a trailing "*". Japanese column names are used.
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_balance_sheet_comparison_LFL()].
#'
#' @return A tibble of character columns.
format_balance_sheet_table_LFL <- function(tbl_balance_sheet) {
  names_ja <- .balance_sheet_item_names_LFL()
  shown <- c(
    "total_assets", "current_assets", "noncurrent_assets", "current_liabilities",
    "noncurrent_liabilities", "statutory_reserves", "net_assets"
  )

  format_amount <- function(value, calculated_items, item) {
    text <- dplyr::if_else(
      is.na(value),
      "—",
      format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE)
    )
    calculated <- stringr::str_detect(calculated_items, stringr::fixed(names_ja[[item]]))
    dplyr::if_else(!is.na(value) & calculated, paste0(text, "*"), text)
  }

  tbl_balance_sheet |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(shown),
        \(value) format_amount(value, calculated_items, dplyr::cur_column())
      ),
      fiscal_year_end = format(fiscal_year_end, "%Y-%m-%d"),
      calculated_items = dplyr::if_else(nzchar(calculated_items), calculated_items, "—")
    ) |>
    dplyr::select(
      会社名 = company,
      決算日 = fiscal_year_end,
      総資産 = total_assets,
      流動資産 = current_assets,
      固定資産 = noncurrent_assets,
      流動負債 = current_liabilities,
      固定負債 = noncurrent_liabilities,
      特別法上の準備金 = statutory_reserves,
      純資産 = net_assets,
      通貨 = currency,
      単位 = unit,
      区分 = basis,
      出典 = source,
      算出項目 = calculated_items
    )
}


#' Create the display table of the balance sheet comparison.
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_balance_sheet_comparison_LFL()].
#' @param caption The table caption.
#'
#' @return A knitr_kable object.
create_balance_sheet_kable_LFL <- function(
    tbl_balance_sheet,
    caption = "貸借対照表の比較（* は算出値）"
) {
  formatted <- format_balance_sheet_table_LFL(tbl_balance_sheet)
  knitr::kable(
    formatted,
    caption = caption,
    align = c("l", "l", rep("r", 7L), rep("l", 5L))
  )
}


#' Create the display table of the source documents.
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_balance_sheet_comparison_LFL()].
#'
#' @return A knitr_kable object.
create_balance_sheet_source_kable_LFL <- function(tbl_balance_sheet) {
  tbl_balance_sheet |>
    dplyr::transmute(
      会社名 = company,
      法人名 = entity,
      決算日 = format(fiscal_year_end, "%Y-%m-%d"),
      出典 = source,
      書類 = document,
      URL = source_url
    ) |>
    knitr::kable(caption = "出典書類")
}


#' Create the display table of the checks.
#'
#' @param tbl_checks The `checks` of [build_balance_sheet_comparison_LFL()].
#'
#' @return A knitr_kable object.
create_balance_sheet_check_kable_LFL <- function(tbl_checks) {
  tbl_checks |>
    dplyr::transmute(
      会社名 = company,
      期数 = periods,
      最初の決算日 = format(first_fiscal_year_end, "%Y-%m-%d"),
      最後の決算日 = format(last_fiscal_year_end, "%Y-%m-%d"),
      資産側の最大差異 = round(max_asset_difference, 3),
      負債純資産側の最大差異 = round(max_funding_difference, 3),
      判定 = dplyr::if_else(passed, "合格", "不合格")
    ) |>
    knitr::kable(caption = "検証結果（差異は百万通貨単位）")
}


#' Create the display table of the original items: for each company and item, the label or XBRL
#' element of the source document that the value was read from.
#'
#' @param tbl_original_items The `original_items` of [build_balance_sheet_comparison_LFL()] or
#'   [build_application_balance_sheets_LFL()].
#' @param tbl_balance_sheet The matching `balance_sheets`.
#' @param names_ja The items shown, named by item with their Japanese names.
#'
#' @return A knitr_kable object ("（算出）": calculated, "—": not applicable).
create_original_item_kable_LFL <- function(
    tbl_original_items,
    tbl_balance_sheet,
    names_ja = .balance_sheet_item_names_LFL()
) {

  reported <- tbl_original_items |>
    dplyr::filter(!is.na(value), !is.na(original_item)) |>
    dplyr::mutate(original_item = stringr::str_remove(original_item, " \\(PDF p\\.[0-9]+\\)$")) |>
    dplyr::summarise(
      .by = c(company, item),
      original_item = paste(unique(original_item), collapse = " / ")
    )

  tbl_balance_sheet |>
    dplyr::select(company, dplyr::all_of(names(names_ja))) |>
    tidyr::pivot_longer(-company, names_to = "item", values_to = "value") |>
    dplyr::summarise(.by = c(company, item), applicable = any(!is.na(value))) |>
    dplyr::left_join(reported, by = c("company", "item")) |>
    dplyr::mutate(
      original_item = dplyr::case_when(
        !applicable ~ "—",
        is.na(original_item) ~ "（算出）",
        TRUE ~ original_item
      ),
      item = factor(unname(names_ja[item]), levels = unname(names_ja))
    ) |>
    dplyr::select(company, item, original_item) |>
    dplyr::arrange(item) |>
    tidyr::pivot_wider(names_from = item, values_from = original_item) |>
    dplyr::rename(会社名 = company) |>
    knitr::kable(caption = "勘定科目の対応（開示書類上の科目名・XBRL要素）")
}


#' Summarize how the composition of each balance sheet changed between the first and the last of the
#' fiscal years.
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_balance_sheet_comparison_LFL()].
#'
#' @return A tibble with one row per company: the first and last fiscal year ends, the growth of total
#'   assets, and the ratios of current assets, current liabilities and net assets to total assets in
#'   the first and the last year.
summarize_composition_change_LFL <- function(tbl_balance_sheet) {
  company_order <- unique(tbl_balance_sheet$company)

  tbl_balance_sheet |>
    dplyr::mutate(
      current_asset_ratio = current_assets / total_assets,
      current_liability_ratio = current_liabilities / total_assets,
      net_asset_ratio = net_assets / total_assets
    ) |>
    dplyr::arrange(fiscal_year_end) |>
    dplyr::summarise(
      .by = company,
      first_fiscal_year_end = dplyr::first(fiscal_year_end),
      last_fiscal_year_end = dplyr::last(fiscal_year_end),
      total_asset_growth = dplyr::last(total_assets) / dplyr::first(total_assets) - 1,
      current_asset_ratio_first = dplyr::first(current_asset_ratio),
      current_asset_ratio_last = dplyr::last(current_asset_ratio),
      current_liability_ratio_first = dplyr::first(current_liability_ratio),
      current_liability_ratio_last = dplyr::last(current_liability_ratio),
      net_asset_ratio_first = dplyr::first(net_asset_ratio),
      net_asset_ratio_last = dplyr::last(net_asset_ratio)
    ) |>
    dplyr::arrange(match(company, company_order))
}


#' Create the display table of [summarize_composition_change_LFL()].
#'
#' @param tbl_change The tibble of [summarize_composition_change_LFL()].
#'
#' @return A knitr_kable object.
create_composition_change_kable_LFL <- function(tbl_change) {
  percent <- scales::label_percent(accuracy = 0.1)
  arrow <- function(first, last) paste0(percent(first), " → ", percent(last))

  tbl_change |>
    dplyr::transmute(
      会社名 = company,
      期間 = paste0(format(first_fiscal_year_end, "%Y/%m"), "〜", format(last_fiscal_year_end, "%Y/%m")),
      総資産の増減 = percent(total_asset_growth),
      流動資産比率 = arrow(current_asset_ratio_first, current_asset_ratio_last),
      流動負債比率 = arrow(current_liability_ratio_first, current_liability_ratio_last),
      純資産比率 = arrow(net_asset_ratio_first, net_asset_ratio_last)
    ) |>
    knitr::kable(caption = "財務構成の変化（総資産比、最初の期→直近期）")
}


#' Compute the composition of each balance sheet as ratios to total assets.
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_balance_sheet_comparison_LFL()].
#'
#' @return A long tibble with `company`, `fiscal_year_end`, `side` (資産 or 負債・純資産),
#'   `component` (an ordered factor) and `ratio`.
compute_balance_sheet_composition_LFL <- function(tbl_balance_sheet) {
  names_ja <- .balance_sheet_item_names_LFL()
  components <- c(
    "current_assets", "noncurrent_assets", "current_liabilities", "noncurrent_liabilities",
    "statutory_reserves", "net_assets"
  )

  tbl_balance_sheet |>
    dplyr::select(company, fiscal_year_end, total_assets, dplyr::all_of(components)) |>
    tidyr::pivot_longer(dplyr::all_of(components), names_to = "item", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      side = dplyr::if_else(item %in% c("current_assets", "noncurrent_assets"), "資産", "負債・純資産"),
      component = factor(unname(names_ja[item]), levels = unname(names_ja[components])),
      company = factor(company, levels = unique(tbl_balance_sheet$company)),
      ratio = value / total_assets
    ) |>
    dplyr::select(company, fiscal_year_end, side, component, ratio)
}


#' Plot the composition of the balance sheets: one panel per company, one stacked bar per fiscal year.
#'
#' @param tbl_composition The tibble of [compute_balance_sheet_composition_LFL()].
#' @param side "負債・純資産" (funding structure) or "資産".
#' @param nrow The number of rows of panels.
#'
#' @return A ggplot object.
plot_balance_sheet_composition_LFL <- function(
    tbl_composition,
    side = c("負債・純資産", "資産"),
    nrow = 2L
) {
  side <- match.arg(side)
  palette <- c(
    流動資産 = "#2a78d6",
    固定資産 = "#eb6834",
    流動負債 = "#2a78d6",
    固定負債 = "#eb6834",
    特別法上の準備金 = "#1baf7a",
    純資産 = "#eda100"
  )

  tbl_composition |>
    dplyr::filter(.data$side == .env$side) |>
    dplyr::mutate(fiscal_year = format(fiscal_year_end, "%Y/%m")) |>
    ggplot2::ggplot(ggplot2::aes(x = fiscal_year, y = ratio, fill = component)) +
    ggplot2::geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
    ggplot2::facet_wrap(ggplot2::vars(company), nrow = nrow, scales = "free_x") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = palette, drop = TRUE) +
    ggplot2::labs(
      title = paste0(side, "の構成比（総資産比）"),
      x = "決算期",
      y = NULL,
      fill = NULL
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
}


# ---- Applications: IT and semiconductors, pharmaceuticals ---------------------------------------------
#
# Consolidated balance sheets of six more companies, read with the same functions: SEC filers from the
# XBRL instances of their annual reports, Japanese filers from the PDFs of their annual securities
# reports on EDINET, and Samsung Electronics from the audited consolidated statements on its IR site.


#' The companies of the application comparisons and their sources.
#'
#' - NVIDIA (CIK 1045810), Eli Lilly (CIK 59478), Novo Nordisk (CIK 353278, 20-F in IFRS): SEC EDGAR
#'   annual reports in XBRL.
#' - SoftBank Group (E02778) and Takeda (E00919): EDINET annual securities reports (consolidated IFRS
#'   statement of financial position, 連結財政状態計算書). Takeda's SEC registration has no 20-F for the
#'   year ending March 2026, so EDINET is used for all five years.
#' - Samsung Electronics: audited consolidated statements of financial position (K-IFRS, millions of
#'   won) published on its IR site, one PDF per year.
#'
#' @return A tibble with one row per company: `theme`, `company`, `entity`, `source_type`, `cik`,
#'   `edinet_code` and `documents` (EDINET document IDs or Samsung fiscal years, newest first).
get_application_companies_LFL <- function() {
  tibble::tribble(
    ~theme,       ~company,              ~entity,                          ~source_type,   ~cik,     ~edinet_code, ~documents,
    "IT・半導体", "サムスン電子",        "Samsung Electronics Co., Ltd.",  "samsung_pdf",  NA,       NA,           as.character(2025:2021),
    "IT・半導体", "ソフトバンクグループ", "ソフトバンクグループ株式会社",   "edinet_pdf",   NA,       "E02778",     c("S100YGH5", "S100W4HN", "S100TP3N", "S100QZOM", "S100OCUV"),
    "IT・半導体", "NVIDIA",              "NVIDIA Corporation",             "sec_xbrl",     1045810L, NA,           character(),
    "製薬",       "イーライリリー",      "Eli Lilly and Company",          "sec_xbrl",     59478L,   NA,           character(),
    "製薬",       "武田薬品工業",        "武田薬品工業株式会社",           "edinet_pdf",   NA,       "E00919",     c("S100YB5L", "S100W07G", "S100TNTP", "S100R3K7", "S100OBQE"),
    "製薬",       "ノボ ノルディスク",   "Novo Nordisk A/S",               "sec_xbrl",     353278L,  NA,           character()
  )
}


# Items of the application balance sheets, in order.
.application_items_LFL <- function() {
  c(
    "total_assets", "current_assets", "noncurrent_assets", "current_liabilities", "noncurrent_liabilities",
    "total_liabilities", "net_assets", "property_plant_equipment", "goodwill",
    "other_intangible_assets", "investments"
  )
}


# Japanese names of the application items shown in the table of original items.
.application_item_names_LFL <- function() {
  c(
    total_assets = "総資産",
    noncurrent_assets = "固定資産",
    net_assets = "純資産",
    property_plant_equipment = "有形固定資産",
    goodwill = "のれん",
    other_intangible_assets = "その他の無形資産",
    investments = "投資"
  )
}


#' The Parquet files of the balance sheet datasets, relative to data/derived.
#'
#' @return A named character vector: the name of each table and its file.
get_balance_sheet_dataset_files_LFL <- function() {
  folder <- file.path("fundamentals", "balance_sheets")
  c(
    balance_sheets = file.path(folder, "balance_sheets.parquet"),
    original_items = file.path(folder, "balance_sheet_original_items.parquet"),
    checks = file.path(folder, "balance_sheet_checks.parquet"),
    application_balance_sheets = file.path(folder, "application_balance_sheets.parquet"),
    application_original_items = file.path(folder, "application_original_items.parquet"),
    application_checks = file.path(folder, "application_checks.parquet")
  )
}


#' Write the balance sheet datasets to Parquet with arrow.
#'
#' @param comparison The list of [build_balance_sheet_comparison_LFL()].
#' @param application The list of [build_application_balance_sheets_LFL()].
#'
#' @return A tibble with one row per file: `table`, `path` (relative to the project), `rows`, `columns`.
write_balance_sheet_datasets_LFL <- function(comparison, application) {
  tables <- list(
    balance_sheets = comparison$balance_sheets,
    original_items = comparison$original_items,
    checks = comparison$checks,
    application_balance_sheets = application$balance_sheets,
    application_original_items = application$original_items,
    application_checks = application$checks
  )
  files <- get_balance_sheet_dataset_files_LFL()

  purrr::imap(
    tables,
    \(tbl_data, table) {
      path <- write_derived_parquet_file_LFL(tbl_data, files[[table]])
      tibble::tibble(table = table, path = relative_path_LFL(path), rows = nrow(tbl_data), columns = ncol(tbl_data))
    }
  ) |>
    purrr::list_rbind()
}


#' Read the balance sheet datasets from Parquet with arrow.
#'
#' @return A list: `comparison` and `application`, each with `balance_sheets`, `original_items` and
#'   `checks`, as returned by [build_balance_sheet_comparison_LFL()] and
#'   [build_application_balance_sheets_LFL()].
read_balance_sheet_datasets_LFL <- function() {
  tables <- purrr::map(
    get_balance_sheet_dataset_files_LFL(),
    \(file_name) tibble::as_tibble(read_derived_parquet_file_LFL(file_name))
  )

  list(
    comparison = list(
      balance_sheets = tables$balance_sheets,
      original_items = tables$original_items,
      checks = tables$checks
    ),
    application = list(
      balance_sheets = tables$application_balance_sheets,
      original_items = tables$application_original_items,
      checks = tables$application_checks
    )
  )
}


#' Fingerprint the balance sheet tables: the number of rows and companies, a hash of the amounts and a
#' hash of the sources, so that a table read back from Parquet can be compared with the one written.
#'
#' @param datasets A list with `comparison` and `application`, as returned by
#'   [read_balance_sheet_datasets_LFL()].
#'
#' @return A tibble with one row per balance sheet table: `table`, `rows`, `companies`,
#'   `amount_hash`, `source_hash`.
compute_balance_sheet_fingerprints_LFL <- function(datasets) {
  tables <- list(
    balance_sheets = list(data = datasets$comparison$balance_sheets, amounts = .balance_sheet_items_LFL()),
    application_balance_sheets = list(data = datasets$application$balance_sheets, amounts = .application_items_LFL())
  )
  hash_rows <- function(tbl) {
    tbl |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(value) dplyr::coalesce(format(value, digits = 15, trim = TRUE), "NA"))) |>
      tidyr::unite("row", dplyr::everything(), sep = "|") |>
      dplyr::pull(row) |>
      sort() |>
      rlang::hash()
  }

  purrr::imap(
    tables,
    \(spec, name) {
      keys <- c("company", "fiscal_year_end")
      tibble::tibble(
        table = name,
        rows = nrow(spec$data),
        companies = dplyr::n_distinct(spec$data$company),
        amount_hash = hash_rows(dplyr::select(spec$data, dplyr::all_of(c(keys, spec$amounts)))),
        source_hash = hash_rows(dplyr::select(spec$data, dplyr::all_of(c(keys, "source", "document", "source_url"))))
      )
    }
  ) |>
    purrr::list_rbind()
}


#' Load the balance sheet datasets: read the saved Parquet files with arrow, or, when they are missing
#' (the first run) or `rebuild = TRUE`, fetch and check the public disclosures, write the Parquet files
#' and read them back.
#'
#' The fingerprints of [compute_balance_sheet_fingerprints_LFL()] are saved with the data when it is
#' built. Each load compares the fingerprints of the data read from Parquet with the reference: the data
#' just built on the first run, the saved fingerprints afterwards. The numbers of rows must also be the
#' expected ones.
#'
#' @param securities_sources The tibble of [get_securities_balance_sheet_sources_LFL()].
#' @param gafam_companies The tibble of [get_gafam_companies_LFL()].
#' @param application_companies The tibble of [get_application_companies_LFL()].
#' @param years The number of fiscal years per company.
#' @param rebuild TRUE to fetch the disclosures again even if the Parquet files exist.
#' @param expected_rows The expected numbers of rows of the balance sheet tables.
#'
#' @return A list: `mode` ("built" or "read"), `files` (the Parquet files and their rows),
#'   `verification`, `comparison` and `application`.
load_balance_sheet_datasets_LFL <- function(
    securities_sources = get_securities_balance_sheet_sources_LFL(),
    gafam_companies = get_gafam_companies_LFL(),
    application_companies = get_application_companies_LFL(),
    years = 5L,
    rebuild = FALSE,
    expected_rows = c(balance_sheets = 40L, application_balance_sheets = 30L)
) {
  files <- get_balance_sheet_dataset_files_LFL()
  fingerprint_file <- file.path(dirname(files[[1]]), "balance_sheet_fingerprints.parquet")
  derived_dir <- file.path(get_data_dir_LFL(), "derived")
  saved <- file.exists(file.path(derived_dir, c(files, fingerprint_file)))
  build <- rebuild || !all(saved)

  reference <- if (build) {
    built <- list(
      comparison = build_balance_sheet_comparison_LFL(securities_sources, gafam_companies, years),
      application = build_application_balance_sheets_LFL(application_companies, years)
    )
    write_balance_sheet_datasets_LFL(built$comparison, built$application)
    fingerprints <- compute_balance_sheet_fingerprints_LFL(built)
    write_derived_parquet_file_LFL(fingerprints, fingerprint_file)
    fingerprints
  } else {
    tibble::as_tibble(read_derived_parquet_file_LFL(fingerprint_file))
  }

  datasets <- read_balance_sheet_datasets_LFL()
  read_back <- compute_balance_sheet_fingerprints_LFL(datasets)

  verification <- reference |>
    dplyr::inner_join(read_back, by = "table", suffix = c("_reference", "_read")) |>
    dplyr::mutate(
      expected_rows = unname(expected_rows[table]),
      amounts_match = amount_hash_reference == amount_hash_read,
      sources_match = source_hash_reference == source_hash_read,
      passed = rows_read == expected_rows & rows_reference == rows_read &
        companies_reference == companies_read & amounts_match & sources_match
    ) |>
    dplyr::select(
      table, expected_rows, rows_reference, rows_read, companies_reference, companies_read,
      amounts_match, sources_match, passed
    )

  if (nrow(verification) != length(expected_rows) || !all(verification$passed)) {
    stop("The balance sheet data read from Parquet differs from the reference.", call. = FALSE)
  }

  file_rows <- purrr::imap(
    c(files, fingerprints = fingerprint_file),
    \(file_name, table) {
      path <- file.path(derived_dir, file_name)
      tibble::tibble(
        table = table,
        path = relative_path_LFL(path),
        rows = nrow(arrow::open_dataset(path)),
        columns = length(arrow::open_dataset(path)$schema$names)
      )
    }
  ) |>
    purrr::list_rbind()

  list(
    mode = if (build) "built" else "read",
    files = file_rows,
    verification = verification,
    comparison = datasets$comparison,
    application = datasets$application
  )
}


#' Create the display table of the verification of [load_balance_sheet_datasets_LFL()].
#'
#' @param loaded The list of [load_balance_sheet_datasets_LFL()].
#'
#' @return A knitr_kable object.
create_dataset_verification_kable_LFL <- function(loaded) {
  reference <- if (loaded$mode == "built") "取得直後のデータ" else "保存時の指紋"
  yes_no <- function(value) dplyr::if_else(value, "一致", "不一致")

  loaded$verification |>
    dplyr::transmute(
      テーブル = table,
      期待行数 = expected_rows,
      基準の行数 = rows_reference,
      読込行数 = rows_read,
      会社数 = companies_read,
      主要数値 = yes_no(amounts_match),
      出典 = yes_no(sources_match),
      判定 = dplyr::if_else(passed, "合格", "不合格")
    ) |>
    knitr::kable(
      caption = paste0(
        "Parquetから読み戻したデータの照合（実行モード：",
        if (loaded$mode == "built") "公開開示から取得してParquetを生成" else "保存済みParquetを読込み",
        "、基準：", reference, "）"
      )
    )
}


#' Query a balance sheet dataset with arrow before collecting it into R.
#'
#' The Parquet file is opened as an arrow Dataset; filtering, the ratios and the aggregation by company
#' run in arrow, and only the result is collected.
#'
#' @param table "balance_sheets" or "application_balance_sheets".
#'
#' @return A tibble with one row per company: `basis`, `currency`, `periods`, the first and last fiscal
#'   year ends, and the average ratios of noncurrent assets and net assets to total assets.
query_balance_sheet_dataset_LFL <- function(table = c("balance_sheets", "application_balance_sheets")) {
  table <- match.arg(table)
  path <- file.path(get_data_dir_LFL(), "derived", get_balance_sheet_dataset_files_LFL()[[table]])

  arrow::open_dataset(path) |>
    dplyr::filter(!is.na(total_assets), total_assets > 0) |>
    dplyr::mutate(
      noncurrent_ratio = noncurrent_assets / total_assets,
      net_asset_ratio = net_assets / total_assets
    ) |>
    dplyr::group_by(company, basis, currency) |>
    dplyr::summarise(
      periods = dplyr::n(),
      first_fiscal_year_end = min(fiscal_year_end),
      last_fiscal_year_end = max(fiscal_year_end),
      mean_noncurrent_ratio = mean(noncurrent_ratio),
      mean_net_asset_ratio = mean(net_asset_ratio),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::arrange(currency, company)
}


#' Create the display table of [write_balance_sheet_datasets_LFL()].
#'
#' @param tbl_files The tibble of [write_balance_sheet_datasets_LFL()].
#'
#' @return A knitr_kable object.
create_dataset_file_kable_LFL <- function(tbl_files) {
  tbl_files |>
    dplyr::transmute(テーブル = table, ファイル = path, 行数 = rows, 列数 = columns) |>
    knitr::kable(caption = "arrowで書き出したParquetファイル")
}


#' Create the display table of [query_balance_sheet_dataset_LFL()].
#'
#' @param tbl_query The tibble of [query_balance_sheet_dataset_LFL()].
#'
#' @return A knitr_kable object.
create_dataset_query_kable_LFL <- function(tbl_query) {
  percent <- scales::label_percent(accuracy = 0.1)

  tbl_query |>
    dplyr::transmute(
      会社名 = company,
      区分 = basis,
      通貨 = currency,
      期数 = periods,
      期間 = paste0(format(first_fiscal_year_end, "%Y/%m"), "〜", format(last_fiscal_year_end, "%Y/%m")),
      平均固定資産比率 = percent(mean_noncurrent_ratio),
      平均純資産比率 = percent(mean_net_asset_ratio)
    ) |>
    knitr::kable(caption = "arrowのDatasetで集計した結果（5期平均）")
}


#' Create the display table of the original items of the application comparisons.
#'
#' @param application The list of [build_application_balance_sheets_LFL()].
#'
#' @return A knitr_kable object.
create_application_original_item_kable_LFL <- function(application) {
  create_original_item_kable_LFL(
    application$original_items,
    application$balance_sheets,
    names_ja = .application_item_names_LFL()
  )
}


# Labels of a consolidated IFRS statement of financial position in an annual securities report.
.edinet_ifrs_pdf_labels_LFL <- function() {
  c(
    total_assets = "資産合計",
    current_assets = "流動資産合計",
    noncurrent_assets = "非流動資産合計",
    current_liabilities = "流動負債合計",
    noncurrent_liabilities = "非流動負債合計",
    total_liabilities = "負債合計",
    net_assets = "資本合計",
    property_plant_equipment = "有形固定資産",
    goodwill = "のれん",
    other_intangible_assets = "無形資産",
    equity_method_investments = "持分法で会計処理されている投資",
    # SVFの投資(FVTPL), SVFからの投資(FVTPL), FVTPLで会計処理されているSVF1およびSVF2からの投資
    fund_investments = "\\S*SVF\\S*投資(?:\\(FVTPL\\))?",
    asset_management_investments = "資産運用子会社からの投資",
    investment_securities = "投資有価証券"
  )
}


#' Read the consolidated IFRS statement of financial position from the PDF of an EDINET annual
#' securities report.
#'
#' @param document The EDINET document ID.
#' @param entity The filer's name, which must be on the cover.
#' @param raw_dir The folder of the downloaded PDFs.
#'
#' @return A tibble with `fiscal_year_end`, `item`, `value` (millions of yen), `original_item`,
#'   `source_url`. Investments are the sum of equity-method investments, SVF investments, investments from
#'   asset management subsidiaries and investment securities where reported.
read_edinet_ifrs_pdf_LFL <- function(
    document,
    entity,
    raw_dir = file.path(get_data_dir_LFL(), "raw", "fundamentals", "edinet_pdf")
) {
  url <- paste0("https://disclosure2dl.edinet-fsa.go.jp/searchdocument/pdf/", document, ".pdf")
  path <- file.path(raw_dir, paste0(document, ".pdf"))
  download_public_file_LFL(url, path)

  cover <- stringi::stri_trans_nfkc(pdftools::pdf_text(path)[[1]])
  if (!stringr::str_detect(cover, stringr::fixed(stringi::stri_trans_nfkc(entity)))) {
    stop("EDINET document ", document, " is not filed by ", entity, ".", call. = FALSE)
  }

  values <- extract_pdf_balance_sheet_LFL(
    path,
    "【連結財政状態計算書】",
    .edinet_ifrs_pdf_labels_LFL(),
    pages_after = 2L,
    end_pattern = "【連結(損益|包括利益)",
    two_period = TRUE
  )
  investment_items <- c("equity_method_investments", "fund_investments", "asset_management_investments", "investment_securities")
  investments <- values |>
    dplyr::filter(item %in% investment_items, !is.na(value))

  values |>
    dplyr::filter(!item %in% investment_items) |>
    dplyr::bind_rows(
      tibble::tibble(
        fiscal_year_end = values$fiscal_year_end[[1]],
        item = "investments",
        value = if (nrow(investments)) sum(investments$value) else NA_real_,
        original_item = if (nrow(investments)) paste(investments$original_item, collapse = " + ") else NA_character_
      )
    ) |>
    dplyr::mutate(source_url = url)
}


#' Read Samsung Electronics' audited consolidated statement of financial position (K-IFRS) for one year.
#'
#' The PDF has four amount columns (KRW current year, KRW previous year, USD current year, USD previous
#' year); the current-year won amount is the fourth number from the end of a line. Subtotals have no
#' label: current assets is the amount line just before "Non-current assets", non-current assets the one
#' just before "Total assets", and likewise for liabilities. Labels that wrap over two lines carry their
#' amounts on one of the two lines. Intangible assets include goodwill; investments are investments in
#' associates and joint ventures plus non-current financial assets at fair value.
#'
#' @param year The fiscal year (December year end).
#' @param raw_dir The folder of the downloaded PDFs.
#'
#' @return A tibble with `fiscal_year_end`, `item`, `value` (millions of won), `original_item`,
#'   `source_url`.
read_samsung_balance_sheet_LFL <- function(
    year,
    raw_dir = file.path(get_data_dir_LFL(), "raw", "fundamentals", "samsung_ir")
) {
  url <- paste0(
    "https://images.samsung.com/is/content/samsung/assets/global/ir/docs/",
    year, "_con_quarter04_bs.pdf"
  )
  path <- file.path(raw_dir, paste0(year, "_con_quarter04_bs.pdf"))
  download_public_file_LFL(url, path)

  text <- paste(pdftools::pdf_text(path), collapse = "\n")

  if (!stringr::str_detect(text, "Samsung Electronics Co., Ltd.") ||
      !stringr::str_detect(text, "CONSOLIDATED STATEMENTS? OF FINANCIAL POSITION")) {
    stop(path, " is not Samsung Electronics' consolidated statement of financial position.", call. = FALSE)
  }

  lines <- tibble::tibble(line = stringr::str_split(text, "\n")[[1]]) |>
    dplyr::mutate(
      index = dplyr::row_number(),
      tokens = stringr::str_extract_all(line, "\\(?[0-9]{1,3}(?:,[0-9]{3})+\\)?|\\(?[0-9]+\\)?|(?<=\\s)-(?=\\s|$)"),
      amount = purrr::map_chr(tokens, \(tokens) if (length(tokens) >= 4L) tokens[[length(tokens) - 3L]] else NA_character_),
      label = stringr::str_squish(stringr::str_remove_all(line, "[0-9,()\\-]+"))
    )

  value_of <- function(token) {
    negative <- stringr::str_detect(token, "^\\(")
    number <- as.numeric(stringr::str_remove_all(dplyr::if_else(token == "-", "0", token), "[,()]"))
    dplyr::if_else(negative, -number, number)
  }
  first_line <- function(pattern, after = 0L) {
    lines$index[stringr::str_detect(lines$line, pattern) & lines$index > after][1]
  }
  # the amount of a labelled line, or of the line before it when the label wraps
  labelled <- function(index) {
    dplyr::coalesce(lines$amount[index], lines$amount[index - 1L])
  }
  # the amount line without a label just before a line
  subtotal_before <- function(index) {
    hit <- lines$index[lines$index < index & !is.na(lines$amount) & !nzchar(lines$label)]
    lines$amount[max(hit)]
  }

  noncurrent_start <- first_line("^\\s*Non-current assets")
  total_assets_line <- first_line("^\\s*Total assets")
  noncurrent_liabilities_line <- first_line("^\\s*Non-current liabilities")
  total_liabilities_line <- first_line("^\\s*Total liabilities\\s{2,}")
  fvoci_line <- first_line("through other comprehensive income", noncurrent_start)
  fvpl_line <- first_line("through profit or loss", noncurrent_start)
  associates_line <- first_line("Investments? in associates", noncurrent_start)
  year_line <- first_line("[0-9]{4}\\s+[0-9]{4}\\s+[0-9]{4}", first_line("December 31"))

  investments <- c(labelled(associates_line), labelled(fvoci_line), labelled(fvpl_line))

  if (anyNA(investments)) {
    stop("Investments could not be read from ", path, call. = FALSE)
  }

  tibble::tribble(
    ~item,                      ~token,                                                    ~original_item,
    "total_assets",             lines$amount[total_assets_line],                           "Total assets",
    "current_assets",           subtotal_before(noncurrent_start),                         "Current assets (subtotal)",
    "noncurrent_assets",        subtotal_before(total_assets_line),                        "Non-current assets (subtotal)",
    "current_liabilities",      subtotal_before(noncurrent_liabilities_line),              "Current liabilities (subtotal)",
    "noncurrent_liabilities",   subtotal_before(total_liabilities_line),                   "Non-current liabilities (subtotal)",
    "total_liabilities",        lines$amount[total_liabilities_line],                      "Total liabilities",
    "net_assets",               lines$amount[first_line("^\\s*Total equity")],             "Total equity",
    "property_plant_equipment", labelled(first_line("Property, plant and equipment", noncurrent_start)), "Property, plant and equipment",
    "other_intangible_assets",  labelled(first_line("Intangible assets", noncurrent_start)), "Intangible assets (goodwill included)"
  ) |>
    dplyr::mutate(value = value_of(token)) |>
    dplyr::select(-token) |>
    dplyr::bind_rows(
      tibble::tibble(
        item = "investments",
        value = sum(value_of(investments)),
        original_item = "Investments in associates and joint ventures + Financial assets at fair value (non-current)"
      )
    ) |>
    dplyr::mutate(
      fiscal_year_end = lubridate::make_date(
        as.integer(stringr::str_extract(lines$line[year_line], "[0-9]{4}")), 12L, 31L
      ),
      original_item = paste0(original_item, " (PDF)"),
      source_url = url,
      .before = 1L
    )
}


#' Build the consolidated balance sheets of the application companies for their last fiscal years.
#'
#' Amounts are in millions of the reporting currency. Items a source does not report are calculated as
#' in [complete_balance_sheet_LFL()] and recorded in `calculated_items`; the checks of
#' [validate_balance_sheet_comparison_LFL()] apply.
#'
#' @param companies The tibble of [get_application_companies_LFL()].
#' @param years The number of fiscal years.
#'
#' @return A list: `balance_sheets` (one row per company and fiscal year), `original_items` and `checks`.
build_application_balance_sheets_LFL <- function(companies = get_application_companies_LFL(), years = 5L) {
  items <- .application_items_LFL()
  source_names <- c(
    sec_xbrl = "SEC EDGAR 年次報告書 XBRL",
    edinet_pdf = "EDINET 有価証券報告書 PDF（連結財政状態計算書）",
    samsung_pdf = "Samsung Electronics IR 監査済連結財政状態計算書 PDF"
  )

  sec_companies <- dplyr::filter(companies, source_type == "sec_xbrl")
  sec <- read_sec_balance_sheets_LFL(dplyr::select(sec_companies, company, entity, cik), years = years)
  sec_values <- sec$values |>
    dplyr::left_join(
      dplyr::select(sec$reports, company, fiscal_year_end, source_url),
      by = c("company", "fiscal_year_end")
    ) |>
    dplyr::transmute(
      company, fiscal_year_end, document = accession, currency, source_type = "sec_xbrl",
      item, value = value / 1e6, original_item, source_url, amount_precision = 1e-6
    )

  edinet_values <- companies |>
    dplyr::filter(source_type == "edinet_pdf") |>
    dplyr::select(company, entity, document = documents) |>
    tidyr::unnest_longer(document) |>
    dplyr::slice_head(n = years, by = company) |>
    dplyr::mutate(values = purrr::map2(document, entity, read_edinet_ifrs_pdf_LFL)) |>
    dplyr::select(-entity) |>
    tidyr::unnest(values) |>
    dplyr::mutate(currency = "JPY", source_type = "edinet_pdf", amount_precision = 1)

  samsung_values <- companies |>
    dplyr::filter(source_type == "samsung_pdf") |>
    dplyr::select(company, document = documents) |>
    tidyr::unnest_longer(document) |>
    dplyr::slice_head(n = years, by = company) |>
    dplyr::mutate(values = purrr::map(document, read_samsung_balance_sheet_LFL)) |>
    tidyr::unnest(values) |>
    dplyr::mutate(currency = "KRW", source_type = "samsung_pdf", amount_precision = 1)

  values <- dplyr::bind_rows(sec_values, edinet_values, samsung_values) |>
    dplyr::filter(item %in% items)

  # goodwill split out of the intangible assets line with the note (reconcile_ifrs_goodwill_LFL())
  note_split <- values |>
    dplyr::filter(item == "goodwill", stringr::str_detect(dplyr::coalesce(original_item, ""), "注記")) |>
    dplyr::distinct(company, fiscal_year_end) |>
    dplyr::mutate(note_split = "のれん・その他の無形資産（注記で分割）")

  balance_sheets <- values |>
    dplyr::select(-original_item) |>
    dplyr::mutate(item = factor(item, levels = items)) |>
    tidyr::pivot_wider(names_from = item, values_from = value, names_expand = TRUE) |>
    dplyr::left_join(dplyr::select(companies, theme, company, entity), by = "company") |>
    dplyr::mutate(
      statutory_reserves = NA_real_,
      reserves_apply = FALSE,
      basis = "連結",
      unit = paste0("百万", currency),
      source = unname(source_names[source_type])
    ) |>
    complete_balance_sheet_LFL() |>
    dplyr::left_join(note_split, by = c("company", "fiscal_year_end")) |>
    dplyr::mutate(
      calculated_items = dplyr::case_when(
        is.na(note_split) ~ calculated_items,
        nzchar(calculated_items) ~ paste0(calculated_items, "、", note_split),
        TRUE ~ note_split
      )
    ) |>
    dplyr::select(-note_split) |>
    dplyr::mutate(company = factor(company, levels = companies$company)) |>
    dplyr::arrange(company, dplyr::desc(fiscal_year_end)) |>
    dplyr::mutate(company = as.character(company)) |>
    dplyr::relocate(theme, company, entity, fiscal_year_end)

  list(
    balance_sheets = balance_sheets,
    original_items = dplyr::select(values, company, fiscal_year_end, document, item, value, original_item),
    checks = validate_balance_sheet_comparison_LFL(balance_sheets, years = years)
  )
}


#' Compute the ratios of the application comparisons to total assets.
#'
#' - 固定資産比率 = 固定資産 / 総資産, 純資産比率 = 純資産 / 総資産
#' - 固定資産の内訳: 有形固定資産 (the line as presented), のれん・無形資産, 投資, その他の固定資産 (the rest)
#' - のれん比率, その他の無形資産比率 (Samsung reports goodwill within intangible assets)
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_application_balance_sheets_LFL()].
#'
#' @return A tibble with one row per company and fiscal year and the ratios as columns.
compute_application_ratios_LFL <- function(tbl_balance_sheet) {
  zero <- function(value) dplyr::coalesce(value, 0)

  tbl_balance_sheet |>
    dplyr::transmute(
      theme,
      company = factor(company, levels = unique(company)),
      fiscal_year_end,
      currency,
      noncurrent_ratio = noncurrent_assets / total_assets,
      net_asset_ratio = net_assets / total_assets,
      tangible_ratio = zero(property_plant_equipment) / total_assets,
      intangible_ratio = (zero(goodwill) + zero(other_intangible_assets)) / total_assets,
      investment_ratio = zero(investments) / total_assets,
      other_noncurrent_ratio = noncurrent_ratio - tangible_ratio - intangible_ratio - investment_ratio,
      goodwill_ratio = goodwill / total_assets,
      other_intangible_ratio = other_intangible_assets / total_assets
    )
}


#' Create the display table of an application comparison.
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_application_balance_sheets_LFL()].
#' @param theme "IT・半導体" or "製薬".
#'
#' @return A knitr_kable object (amounts in millions of the reporting currency; "*": calculated).
create_application_kable_LFL <- function(tbl_balance_sheet, theme = c("IT・半導体", "製薬")) {
  theme <- match.arg(theme)
  amount <- function(value) dplyr::if_else(is.na(value), "—", format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE))
  percent <- scales::label_percent(accuracy = 0.1)
  rows <- dplyr::filter(tbl_balance_sheet, .data$theme == .env$theme)
  ratios <- compute_application_ratios_LFL(rows)

  common <- rows |>
    dplyr::transmute(
      会社名 = company,
      決算日 = format(fiscal_year_end, "%Y-%m-%d"),
      通貨 = currency,
      単位 = unit,
      総資産 = amount(total_assets),
      固定資産 = paste0(amount(noncurrent_assets), dplyr::if_else(stringr::str_detect(calculated_items, "固定資産"), "*", ""))
    )

  specific <- if (theme == "IT・半導体") {
    rows |>
      dplyr::transmute(
        純資産 = amount(net_assets),
        有形固定資産 = amount(property_plant_equipment),
        のれん = amount(goodwill),
        無形資産 = amount(other_intangible_assets),
        投資 = amount(investments),
        固定資産比率 = percent(ratios$noncurrent_ratio),
        純資産比率 = percent(ratios$net_asset_ratio)
      )
  } else {
    note <- dplyr::if_else(stringr::str_detect(rows$calculated_items, "注記"), "†", "")
    rows |>
      dplyr::transmute(
        のれん = paste0(amount(goodwill), note),
        その他の無形資産 = paste0(amount(other_intangible_assets), note),
        のれん比率 = percent(ratios$goodwill_ratio),
        その他の無形資産比率 = percent(ratios$other_intangible_ratio)
      )
  }

  dplyr::bind_cols(common, specific, tibble::tibble(出典 = rows$source, 書類 = rows$document)) |>
    knitr::kable(
      caption = paste0(
        theme, "：連結貸借対照表（報告通貨・百万単位、* は算出値",
        if (theme == "製薬") "、† は注記によりのれんを分割した値" else "",
        "）"
      )
    )
}


#' Compute the financial indicators of the application companies from their balance sheets.
#'
#' Ratios do not depend on the reporting currency, so the companies can be compared directly:
#' total assets indexed to the first fiscal year (= 100), the current ratio (current assets / current
#' liabilities), the debt-to-equity ratio (total liabilities / net assets), and the ratios of net
#' assets, noncurrent assets, property, plant and equipment, goodwill and intangible assets, and
#' investments to total assets.
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_application_balance_sheets_LFL()].
#'
#' @return A long tibble with `theme`, `company`, `fiscal_year_end`, `metric` (an ordered factor with
#'   the unit in its name) and `value`.
compute_application_metrics_LFL <- function(tbl_balance_sheet) {
  zero <- function(value) dplyr::coalesce(value, 0)
  metrics <- c(
    "総資産（初年度=100）", "流動比率（%）", "負債資本倍率（倍）", "純資産比率（%）",
    "固定資産比率（%）", "有形固定資産比率（%）", "のれん・無形（%）", "投資比率（%）"
  )

  tbl_balance_sheet |>
    dplyr::arrange(fiscal_year_end) |>
    dplyr::mutate(
      .by = company,
      "総資産（初年度=100）" = 100 * total_assets / dplyr::first(total_assets)
    ) |>
    dplyr::mutate(
      company = factor(company, levels = unique(tbl_balance_sheet$company)),
      "流動比率（%）" = 100 * current_assets / current_liabilities,
      "負債資本倍率（倍）" = total_liabilities / net_assets,
      "純資産比率（%）" = 100 * net_assets / total_assets,
      "固定資産比率（%）" = 100 * noncurrent_assets / total_assets,
      "有形固定資産比率（%）" = 100 * zero(property_plant_equipment) / total_assets,
      "のれん・無形（%）" = 100 * (zero(goodwill) + zero(other_intangible_assets)) / total_assets,
      "投資比率（%）" = 100 * zero(investments) / total_assets
    ) |>
    dplyr::select(theme, company, fiscal_year_end, dplyr::all_of(metrics)) |>
    tidyr::pivot_longer(dplyr::all_of(metrics), names_to = "metric", values_to = "value") |>
    dplyr::mutate(metric = factor(metric, levels = metrics))
}


#' Plot the financial indicators of the three companies of a theme, one panel per indicator.
#'
#' @param tbl_metrics The tibble of [compute_application_metrics_LFL()].
#' @param theme "IT・半導体" or "製薬".
#'
#' @return A ggplot object.
plot_application_metrics_LFL <- function(tbl_metrics, theme = c("IT・半導体", "製薬")) {
  theme <- match.arg(theme)

  tbl_metrics |>
    dplyr::filter(.data$theme == .env$theme) |>
    ggplot2::ggplot(ggplot2::aes(x = fiscal_year_end, y = value, colour = company)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(ggplot2::vars(metric), nrow = 2L, scales = "free_y") +
    ggplot2::scale_x_date(date_labels = "%Y") +
    ggplot2::scale_colour_manual(values = c("#2a78d6", "#eb6834", "#1baf7a")) +
    ggplot2::labs(title = paste0(theme, "：財務指標"), x = NULL, y = NULL, colour = NULL)
}


#' Plot the balance sheet composition of the three companies of a theme.
#'
#' @param tbl_balance_sheet The `balance_sheets` of [build_application_balance_sheets_LFL()].
#' @param theme "IT・半導体" or "製薬".
#' @param side "資産" or "負債・純資産".
#'
#' @return A ggplot object.
plot_application_composition_LFL <- function(
    tbl_balance_sheet,
    theme = c("IT・半導体", "製薬"),
    side = c("資産", "負債・純資産")
) {
  theme <- match.arg(theme)
  side <- match.arg(side)

  tbl_balance_sheet |>
    dplyr::filter(.data$theme == .env$theme) |>
    compute_balance_sheet_composition_LFL() |>
    plot_balance_sheet_composition_LFL(side = side, nrow = 1L) +
    ggplot2::labs(title = paste0(theme, "：", side, "の構成比（総資産比）"))
}


#' Plot the breakdown of noncurrent assets as ratios to total assets, one panel per company.
#'
#' @param tbl_ratios The tibble of [compute_application_ratios_LFL()].
#' @param theme "IT・半導体" or "製薬".
#'
#' @return A ggplot object.
plot_noncurrent_breakdown_LFL <- function(tbl_ratios, theme = c("IT・半導体", "製薬")) {
  theme <- match.arg(theme)
  components <- if (theme == "IT・半導体") {
    c(
      "有形固定資産" = "tangible_ratio",
      "のれん・無形資産" = "intangible_ratio",
      "投資" = "investment_ratio",
      "その他の固定資産" = "other_noncurrent_ratio"
    )
  } else {
    c("のれん" = "goodwill_ratio", "その他の無形資産" = "other_intangible_ratio")
  }
  palette <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")[seq_along(components)]
  title <- if (theme == "IT・半導体") "固定資産の内訳（総資産比）" else "のれんとその他の無形資産（総資産比）"

  tbl_ratios |>
    dplyr::filter(.data$theme == .env$theme) |>
    dplyr::select(company, fiscal_year_end, dplyr::all_of(components)) |>
    tidyr::pivot_longer(dplyr::all_of(names(components)), names_to = "component", values_to = "ratio") |>
    dplyr::filter(!is.na(ratio)) |>
    dplyr::mutate(
      component = factor(component, levels = names(components)),
      fiscal_year = format(fiscal_year_end, "%Y/%m")
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = fiscal_year, y = ratio, fill = component)) +
    ggplot2::geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
    ggplot2::facet_wrap(ggplot2::vars(company), scales = "free_x") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = stats::setNames(palette, names(components))) +
    ggplot2::labs(title = title, x = "決算期", y = NULL, fill = NULL)
}


.set_lagrange_knitr_options_LFL <- function() {
  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    warning = FALSE,
    message = FALSE
  )
  
  invisible(TRUE)
}


.load_lagrange_packages_LFL <- function() {
  packages <- c(
    "QuantLib",
    "tidyverse",
    "broom",
    "lubridate",
    "slider",
    "ggthemes",
    "gganimate",
    "gifski",
    "glue",
    "scales",
    "arrow",
    "dbplyr",
    "future",
    "furrr",
    "PortfolioAnalytics",
    "PerformanceAnalytics",
    "ROI",
    "ROI.plugin.quadprog",
    "quadprog",
    "xts",
    "zoo",
    "tidymodels",
    "glmnet",
    "torch",
    "GaussQuant",
    "nloptr"
  )
  missing_packages <- packages |>
    purrr::keep(
      \(package_name) {
        length(
          find.package(
            package_name,
            quiet = TRUE
          )
        ) == 0L
      }
    )
  
  if (length(missing_packages) > 0L) {
    stop(
      "必要なパッケージを読み込めません。",
      call. = FALSE
    )
  }
  
  suppressWarnings(
    suppressMessages(
      purrr::walk(
        packages,
        \(package_name) {
          library(
            package_name,
            character.only = TRUE
          )
        }
      )
    )
  )
  
  suppressWarnings(
    suppressPackageStartupMessages(
      purrr::walk(
        packages,
        \(package_name) {
          library(
            package_name,
            character.only = TRUE
          )
        }
      )
    )
  )
  
  invisible(TRUE)
}


.initialize_lagrange_LFL <- function() {
  .set_lagrange_knitr_options_LFL()
  .load_lagrange_packages_LFL()
  
  ggplot2::theme_set(
    create_lagrange_theme_LFL()
  )
  
  base::options(
    pillar.sigfig = 5,
    scipen = 5
  )

  set_random_seed_LFL()
  
  invisible(TRUE)
}


.initialize_lagrange_LFL()
