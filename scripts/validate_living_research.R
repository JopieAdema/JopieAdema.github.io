args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript scripts/validate_living_research.R <project-directory>")

project <- normalizePath(args[[1]], mustWork = TRUE)
required <- file.path(project, c("index.qmd", "metadata.json", "update.R"))
missing <- required[!file.exists(required)]
if (length(missing)) stop("Missing required files: ", paste(basename(missing), collapse = ", "))

metadata <- jsonlite::read_json(file.path(project, "metadata.json"), simplifyVector = TRUE)
keys <- c("slug", "title", "summary", "status", "data_vintage", "last_successful_update", "source_urls")
absent <- setdiff(keys, names(metadata))
if (length(absent)) stop("metadata.json lacks: ", paste(absent, collapse = ", "))
if (!grepl("^[a-z0-9]+(-[a-z0-9]+)*$", metadata$slug)) stop("Invalid metadata slug: ", metadata$slug)

qmd <- paste(readLines(file.path(project, "index.qmd"), warn = FALSE), collapse = "\n")
headings <- c("Setting and research idea", "Research design", "Preliminary results", "Conclusion")
absent_headings <- headings[!vapply(headings, grepl, logical(1), x = qmd, fixed = TRUE)]
if (length(absent_headings)) stop("index.qmd lacks sections: ", paste(absent_headings, collapse = ", "))

message("Living Research validation passed: ", metadata$slug)
