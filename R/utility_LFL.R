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
  if (!inherits(plot_object, "ggplot")) {
    stop(
      "plot_object must be a ggplot object.",
      call. = FALSE
    )
  }
  
  plot_object +
    ggplot2::theme_classic(
      base_size = base_size
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(
          b = 10
        )
      ),
      plot.subtitle = ggplot2::element_text(
        margin = ggplot2::margin(
          b = 10
        )
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        margin = ggplot2::margin(
          t = 10
        )
      ),
      axis.title = ggplot2::element_text(
        face = "bold"
      ),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(
        face = "bold"
      ),
      panel.grid.major.y =
        ggplot2::element_line(
          linewidth = 0.25,
          linetype = "dotted"
        ),
      panel.grid.minor =
        ggplot2::element_blank()
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

load_part1_packages_LFL <- function() {
  pkgs <- c(
    "tidyverse", "dbplyr", "arrow", "future", "furrr",
    "ggthemes", "glue", "scales", "slider", "broom", "lubridate"
  )

  invisible(lapply(pkgs, require, character.only = TRUE))
  ggplot2::theme_set(create_lagrange_theme_LFL())
  options(pillar.sigfig = 5, tibble.width = Inf, scipen = 999)
  set_random_seed_LFL()
  invisible(TRUE)
}

load_part2_packages_LFL <- function() {
  pkgs <- c(
    "tidyverse", "GaussQuant", "QuantLib", "dbplyr",
    "arrow", "future", "furrr", "ggthemes", "glue", "scales"
  )

  invisible(lapply(pkgs, require, character.only = TRUE))
  ggplot2::theme_set(create_lagrange_theme_LFL())
  options(pillar.sigfig = 5, tibble.width = Inf, scipen = 999)
  set_random_seed_LFL()
  invisible(TRUE)
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
  print(x, n = n)
  invisible(x)
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
  print(plot)
  invisible(plot)
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
    lapply(
      search_dir,
      list.files,
      recursive = TRUE,
      full.names = TRUE
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
