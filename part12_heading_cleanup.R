# Part I / II heading cleanup batch
#
# Policy
#   1. Remove Markdown headings at H4-H6 outside code fences.
#   2. Remove H3 headings that are merely "まとめ" or data/result save/export headings.
#   3. Keep ALL body text, code, equations, figures, tables, chunk labels and order.
#   4. Keep all H2 headings (including chapter-level "まとめ").
#   5. Do NOT touch Part III.
#
# Run from:
#   C:/AnalyticFin/Projects/LagrangeFinance
#
# Dry run first:
#   source("part12_heading_cleanup.R")
# Then, after reviewing the report:
#   APPLY_CHANGES <- TRUE
#   source("part12_heading_cleanup.R")

apply_changes <- exists("APPLY_CHANGES", inherits = FALSE) &&
  isTRUE(APPLY_CHANGES)

dirs <- c("part1_empirical", "part2_quantive_riemann")

files <- unlist(
  lapply(
    dirs,
    function(dir) {
      list.files(
        dir,
        pattern = "\\.qmd$",
        full.names = TRUE,
        recursive = FALSE
      )
    }
  ),
  use.names = FALSE
)

if (length(files) == 0L) {
  stop("No Part I / II QMD files found. Run from the LagrangeFinance project root.")
}

# H3 headings removed only when they are clearly operational/non-pedagogical.
# Numbering such as "### I-6.9.4 ..." is stripped before matching.
is_disposable_h3 <- function(line) {
  if (!grepl("^###\\s+", line)) return(FALSE)

  title <- sub("^###\\s+", "", line)
  title <- sub("^[IVX]+-[0-9]+(?:\\.[0-9]+)*\\s+", "", title)

  # Exact/near-exact summary labels.
  if (grepl(
    "^(まとめ|小まとめ|章のまとめ|節のまとめ|本節のまとめ|本章のまとめ)$",
    title
  )) return(TRUE)

  # Save/export/write headings. Require a save-like verb so that headings
  # merely discussing "データ" or "結果" are never removed.
  save_verb <- "(保存|書き出し|出力|エクスポート)"
  save_object <- "(データ|中間データ|分析データ|結果|分析結果|計算結果|推定結果|集計結果|出力結果|ファイル|オブジェクト|テーブル|表)"

  grepl(
    paste0("^", save_object, ".*", save_verb, "(する)?$|^", save_verb, ".*", save_object, "$"),
    title
  )
}

scan_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  remove <- rep(FALSE, length(lines))
  reason <- rep(NA_character_, length(lines))

  in_fence <- FALSE

  for (i in seq_along(lines)) {
    line <- lines[[i]]

    if (grepl("^\\s*(```|~~~)", line)) {
      in_fence <- !in_fence
      next
    }

    if (in_fence) next

    if (grepl("^#{4,6}\\s+", line)) {
      remove[[i]] <- TRUE
      reason[[i]] <- "H4-H6"
      next
    }

    if (is_disposable_h3(line)) {
      remove[[i]] <- TRUE
      reason[[i]] <- "H3-summary/save"
    }
  }

  data.frame(
    file = path,
    line = which(remove),
    reason = reason[remove],
    heading = lines[remove],
    stringsAsFactors = FALSE
  )
}

report_list <- lapply(files, scan_file)
report <- do.call(rbind, report_list)

if (is.null(report) || nrow(report) == 0L) {
  cat("\nNo headings matched the cleanup rules.\n")
  cat("No files changed.\n")
  quit(save = "no")
}

cat("\n=== Part I / II heading cleanup report ===\n")
cat("Mode:", if (apply_changes) "APPLY" else "DRY RUN", "\n")
cat("Files scanned:", length(files), "\n")
cat("Headings matched:", nrow(report), "\n\n")

for (path in unique(report$file)) {
  x <- report[report$file == path, , drop = FALSE]
  cat("\n[", path, "]\n", sep = "")
  for (j in seq_len(nrow(x))) {
    cat(
      sprintf(
        "  L%-5d %-16s %s\n",
        x$line[[j]],
        paste0("[", x$reason[[j]], "]"),
        x$heading[[j]]
      )
    )
  }
}

if (!apply_changes) {
  cat("\n=== DRY RUN ONLY ===\n")
  cat("No files were modified.\n")
  cat("Review the headings above.\n")
  cat("To apply exactly these rules:\n\n")
  cat("  APPLY_CHANGES <- TRUE\n")
  cat('  source("part12_heading_cleanup.R")\n\n')
} else {
  for (path in unique(report$file)) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    x <- report[report$file == path, , drop = FALSE]
    keep <- rep(TRUE, length(lines))
    keep[x$line] <- FALSE

    before_h2 <- lines[grepl("^##\\s+", lines)]
    before_labels <- lines[grepl("^#\\|\\s*label:", lines)]

    new_lines <- lines[keep]

    after_h2 <- new_lines[grepl("^##\\s+", new_lines)]
    after_labels <- new_lines[grepl("^#\\|\\s*label:", new_lines)]

    # Safety checks: H2 and chunk labels must be byte-for-byte identical.
    stopifnot(identical(before_h2, after_h2))
    stopifnot(identical(before_labels, after_labels))

    writeLines(new_lines, path, useBytes = TRUE)
  }

  cat("\n=== APPLY COMPLETE ===\n")
  cat("Only the headings listed above were removed.\n")
  cat("All H2 headings: unchanged\n")
  cat("Chunk labels: unchanged\n")
  cat("Body/code/equations/tables/figures: unchanged\n")
  cat("Part III: untouched\n")
  cat("AUDIT OK\n")
}
