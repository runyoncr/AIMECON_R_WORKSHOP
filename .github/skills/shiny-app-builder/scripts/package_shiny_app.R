args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop("Usage: Rscript package_shiny_app.R <app-directory> <output.zip>")
}

app_dir <- normalizePath(args[[1L]], mustWork = TRUE)
output_zip <- normalizePath(args[[2L]], mustWork = FALSE)
required <- file.path(app_dir, c("app.R", "README.md"))
if (!all(file.exists(required))) stop("Validation prerequisites are missing: app.R and README.md are required.")
if (!requireNamespace("zip", quietly = TRUE)) stop("The installed 'zip' package is required for deterministic packaging.")

files <- list.files(app_dir, recursive = TRUE, all.files = TRUE, full.names = FALSE, no.. = TRUE)
forbidden_pattern <- paste0(
  "(?:^|[/\\\\])(?:[.]Renviron|[.]Rhistory|[.]RData|[.]Rproj[.]user|",
  "[.]git|[.]quarto|__pycache__|cache|logs?)(?:$|[/\\\\])|[.]log$"
)
forbidden <- files[grepl(forbidden_pattern, files, perl = TRUE, ignore.case = TRUE)]
if (length(forbidden)) stop("Refusing to package transient or sensitive files: ", paste(forbidden, collapse = ", "))

if (file.exists(output_zip)) unlink(output_zip)
dir.create(dirname(output_zip), recursive = TRUE, showWarnings = FALSE)
zip::zipr(output_zip, files = basename(app_dir), root = dirname(app_dir), include_directories = TRUE)

manifest <- zip::zip_list(output_zip)$filename
expected_prefix <- paste0(basename(app_dir), "/")
if (!all(startsWith(manifest, expected_prefix))) stop("Archive entries do not share the expected top-level folder.")

cat("Created:", output_zip, "\n")
cat(paste0(" - ", manifest), sep = "\n")
cat("\n")
