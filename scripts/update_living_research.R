root <- normalizePath("living-research", mustWork = TRUE)
entries <- list.dirs(root, recursive = FALSE, full.names = TRUE)
projects <- entries[!startsWith(basename(entries), "_") & file.exists(file.path(entries, "metadata.json"))]

if (!length(projects)) {
  message("No active Living Research notes yet; nothing to update.")
  quit(status = 0L)
}

catalog <- vector("list", length(projects))
for (i in seq_along(projects)) {
  project <- projects[[i]]
  previous_directory <- getwd()
  setwd(project)
  tryCatch(
    source("update.R", local = new.env(parent = globalenv())),
    finally = setwd(previous_directory)
  )
  status <- system2("Rscript", c("scripts/validate_living_research.R", shQuote(project)))
  if (status != 0L) stop("Validation failed for ", basename(project))

  metadata <- jsonlite::read_json(file.path(project, "metadata.json"), simplifyVector = TRUE)
  destination <- file.path("assets", "live-research", metadata$slug)
  staging <- tempfile(pattern = paste0(metadata$slug, "-"))
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

  status <- system2("quarto", c("render", shQuote(file.path(project, "index.qmd")), "--output-dir", shQuote(staging)))
  if (status != 0L || !file.exists(file.path(staging, "index.html"))) stop("Render failed for ", metadata$slug)

  if (dir.exists(destination)) unlink(destination, recursive = TRUE, force = TRUE)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(staging, destination)) stop("Could not promote rendered output for ", metadata$slug)
  catalog[[i]] <- metadata
}

dir.create("_data", showWarnings = FALSE)
jsonlite::write_json(catalog, "_data/living_research.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
message("Updated ", length(projects), " Living Research note(s).")
