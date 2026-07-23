files <- c(
  "part1_empirical/ch03_stock_data_time_series.qmd",
  "part1_empirical/ch04_factor_models.qmd",
  "part1_empirical/ch05_asset_pricing.qmd",
  "part1_empirical/ch06_event_study.qmd"
)

chapters <- c(3L, 4L, 5L, 6L)

number_h2 <- function(path, chapter) {
  lines <- readLines(
    path,
    encoding = "UTF-8",
    warn = FALSE
  )

  state <- new.env(parent = emptyenv())
  state$in_fence <- FALSE
  state$section <- 0L

  output <- vapply(
    lines,
    function(line) {
      if (grepl("^\\s*(```|~~~)", line, perl = TRUE)) {
        state$in_fence <- !state$in_fence
        return(line)
      }

      if (
        !state$in_fence &&
        grepl("^##\\s+(?!#)", line, perl = TRUE)
      ) {
        state$section <- state$section + 1L

        heading <- sub(
          "^##\\s+",
          "",
          line,
          perl = TRUE
        )

        heading <- sub(
          "^[0-9]+(?:\\.[0-9]+)*\\s+",
          "",
          heading,
          perl = TRUE
        )

        return(
          sprintf(
            "## %d.%d %s",
            chapter,
            state$section,
            heading
          )
        )
      }

      line
    },
    character(1),
    USE.NAMES = FALSE
  )

  connection <- file(
    path,
    open = "w",
    encoding = "UTF-8"
  )

  writeLines(
    output,
    connection,
    useBytes = TRUE
  )

  close(connection)
}

invisible(
  Map(
    number_h2,
    files,
    chapters
  )
)
