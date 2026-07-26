############################################################
# LagrangeFinance Utility Functions
############################################################
add_lagrange_theme_LFL <- function(ggplot, base_size = 12) {
  ggplot +
    ggthemes::theme_economist(base_size = base_size) +
    ggplot2::scale_y_continuous(position = "right") +
    ggthemes::scale_colour_economist() +
    ggthemes::scale_fill_economist()
}
set_random_seed_LFL <- function(seed = 20260726) {
  set.seed(seed)
  invisible(seed)
}


load_packages_LFL <- function(packages) {
  purrr::walk(packages, \(package)
  suppressPackageStartupMessages(
    library(package, character.only = TRUE)
  ))
  invisible(packages)
}

load_part1_packages_LFL <- function() {
  packages <- c(
    "tidyverse", "dbplyr", "arrow", "future", "furrr", "ggthemes",
    "glue", "scales", "slider", "broom", "lubridate"
  )

  load_packages_LFL(packages)
  options(pillar.sigfig = 5, tibble.width = Inf, scipen = 999)
  set_random_seed_LFL()
  invisible(packages)
}

load_part2_packages_LFL <- function() {
  packages <- c(
    "tidyverse", "GaussQuant", "QuantLib", "dbplyr", "arrow",
    "future", "furrr", "ggthemes", "glue", "scales"
  )

  load_packages_LFL(packages)
  options(pillar.sigfig = 5, tibble.width = Inf, scipen = 999)
  set_random_seed_LFL()
  invisible(packages)
}

find_project_dir_LFL <- function() {
  current_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(
    Sys.getenv("QUARTO_PROJECT_DIR"),
    current_dir,
    dirname(current_dir),
    dirname(dirname(current_dir)),
    dirname(dirname(dirname(current_dir)))
  ))
  candidates <- candidates[nzchar(candidates)]
  project_dirs <- candidates[file.exists(file.path(candidates, "_quarto.yml"))]

  if (!length(project_dirs)) {
    stop("LagrangeFinance project root was not found.", call. = FALSE)
  }

  normalizePath(project_dirs[[1]], winslash = "/", mustWork = TRUE)
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
  output_dirs <- c(
    get_output_dir_LFL(),
    get_figure_output_dir_LFL(),
    get_table_output_dir_LFL()
  )

  purrr::walk(output_dirs, \(output_dir) {
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  })

  invisible(output_dirs)
}

find_data_file_LFL <- function(file_name) {
  file_paths <- list.files(get_simulation_data_dir_LFL(), recursive = TRUE, full.names = TRUE)
  matched_paths <- file_paths[basename(file_paths) == file_name]

  if (!length(matched_paths)) {
    stop(file_name, " was not found in simulation_data.", call. = FALSE)
  }

  if (length(matched_paths) > 1) {
    warning(file_name, " matched multiple files; the first match is used.", call. = FALSE)
  }

  normalizePath(matched_paths[[1]], winslash = "/", mustWork = TRUE)
}

read_csv_file_LFL <- function(file_name, ...) {
  readr::read_csv(find_data_file_LFL(file_name), show_col_types = FALSE, ...)
}

write_csv_file_LFL <- function(tbl_data, file_name, ...) {
  create_output_dirs_LFL()
  file_path <- file.path(get_table_output_dir_LFL(), file_name)
  readr::write_csv(tbl_data, file_path, ...)
  invisible(file_path)
}

read_parquet_file_LFL <- function(file_name, ...) {
  arrow::read_parquet(find_data_file_LFL(file_name), ...)
}

write_parquet_file_LFL <- function(tbl_data, file_name, ...) {
  create_output_dirs_LFL()
  file_path <- file.path(get_table_output_dir_LFL(), file_name)
  arrow::write_parquet(tbl_data, file_path, ...)
  invisible(file_path)
}

read_rds_file_LFL <- function(file_name) {
  readRDS(find_data_file_LFL(file_name))
}

write_rds_file_LFL <- function(object, file_name) {
  create_output_dirs_LFL()
  file_path <- file.path(get_output_dir_LFL(), file_name)
  saveRDS(object, file_path)
  invisible(file_path)
}

show_data_structure_LFL <- function(object) {
  dplyr::glimpse(object)
  invisible(object)
}

show_table_LFL <- function(tbl_data, n = 10) {
  print(tbl_data, n = n)
  invisible(tbl_data)
}

show_table_head_LFL <- function(tbl_data, n = 6) {
  print(utils::head(tbl_data, n))
  invisible(tbl_data)
}

show_table_tail_LFL <- function(tbl_data, n = 6) {
  print(utils::tail(tbl_data, n))
  invisible(tbl_data)
}

show_plot_LFL <- function(ggplot) {
  print(ggplot)
  invisible(ggplot)
}

show_session_info_LFL <- function() {
  utils::sessionInfo()
}
