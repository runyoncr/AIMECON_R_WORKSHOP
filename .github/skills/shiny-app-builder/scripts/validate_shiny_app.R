args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: Rscript validate_shiny_app.R <source.qmd> <app-directory>")
}

source_page <- normalizePath(args[[1L]], mustWork = TRUE)
app_dir <- normalizePath(args[[2L]], mustWork = TRUE)
app_file <- file.path(app_dir, "app.R")
readme_file <- file.path(app_dir, "README.md")

stopifnot(file.exists(app_file), file.exists(readme_file))
if (!grepl("^example-.*[.]qmd$", basename(source_page))) {
  stop("The source must be an example-*.qmd page.")
}

r_files <- list.files(app_dir, pattern = "[.]R$", recursive = TRUE, full.names = TRUE)
if (!length(r_files)) stop("The app contains no R files.")
invisible(lapply(r_files, parse))

app_text <- paste(readLines(app_file, warn = FALSE), collapse = "\n")
page_text <- paste(readLines(source_page, warn = FALSE), collapse = "\n")

required_patterns <- c(
  "app_default_state\\s*<-\\s*function",
  "SHINY_APP_VALIDATE",
  "shinyApp\\s*\\("
)
missing_patterns <- required_patterns[!vapply(
  required_patterns,
  grepl,
  logical(1),
  x = app_text,
  perl = TRUE
)]
if (length(missing_patterns)) {
  stop("app.R is missing required validation hooks: ", paste(missing_patterns, collapse = ", "))
}

start_count <- lengths(regmatches(page_text, gregexpr("<!-- shiny-app-builder:start -->", page_text, fixed = TRUE)))
end_count <- lengths(regmatches(page_text, gregexpr("<!-- shiny-app-builder:end -->", page_text, fixed = TRUE)))
if (!identical(start_count, 1L) || !identical(end_count, 1L)) {
  stop("The source page must contain exactly one Shiny extension marker pair.")
}

package_calls <- regmatches(
  app_text,
  gregexpr("(?:library|require)\\s*\\(\\s*[\"']?([A-Za-z][A-Za-z0-9.]*)", app_text, perl = TRUE)
)[[1L]]
packages <- unique(sub(".*\\(\\s*[\"']?", "", package_calls))
packages <- packages[nzchar(packages)]

forbidden <- c(
  "ANTHROPIC_API_KEY", "OPENAI_API_KEY", "call_claude",
  "httr::", "httr2::", "curl::", "download[.]file\\s*\\(",
  "https?://", "fileInput\\s*\\(", "shinyapps[.]io", "rsconnect::"
)
hits <- forbidden[vapply(forbidden, grepl, logical(1), x = app_text, perl = TRUE, ignore.case = TRUE)]
if (length(hits)) stop("Forbidden API, network, upload, or deployment pattern(s): ", paste(hits, collapse = ", "))

absolute_path <- "(?:[A-Za-z]:[/\\\\]|/Users/|/home/)"
if (grepl(absolute_path, app_text, perl = TRUE)) stop("app.R contains an absolute local path.")

transient_pattern <- "(?:^|[/\\\\])(?:[.]Renviron|[.]Rhistory|[.]RData|[.]Rproj[.]user|__pycache__|[.]quarto)(?:$|[/\\\\])"
app_files <- list.files(app_dir, recursive = TRUE, all.files = TRUE, full.names = FALSE, no.. = TRUE)
transient <- app_files[grepl(transient_pattern, app_files, perl = TRUE, ignore.case = TRUE)]
if (length(transient)) stop("Transient or sensitive files found: ", paste(transient, collapse = ", "))

old_validate <- Sys.getenv("SHINY_APP_VALIDATE", unset = NA_character_)
on.exit({
  if (is.na(old_validate)) Sys.unsetenv("SHINY_APP_VALIDATE") else Sys.setenv(SHINY_APP_VALIDATE = old_validate)
}, add = TRUE)
Sys.setenv(SHINY_APP_VALIDATE = "true")
app_env <- new.env(parent = globalenv())
sys.source(app_file, envir = app_env)
default_state <- app_env$app_default_state()
if (!is.list(default_state) || !length(default_state)) stop("app_default_state() must return a non-empty list.")

cat("Validated app:", basename(app_dir), "\n")
cat("R files parsed:", length(r_files), "\n")
cat("Packages:", if (length(packages)) paste(packages, collapse = ", ") else "none", "\n")
cat("Default-state fields:", paste(names(default_state), collapse = ", "), "\n")
