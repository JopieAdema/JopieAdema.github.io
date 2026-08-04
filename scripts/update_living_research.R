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
  destination <- file.path("assets", "living-research", metadata$slug)
  staging <- tempfile(pattern = paste0(metadata$slug, "-"))
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

  # Render in place (next to index.qmd) rather than via --output-dir: quarto
  # nests --output-dir output under the source directory's own name for a
  # standalone (non-project) file render, which broke the flat staging layout
  # this script expects. embed-resources: true in living-research/_quarto.yml
  # makes index.html fully self-contained, so a single file copy suffices.
  rendered_html <- file.path(project, "index.html")
  if (file.exists(rendered_html)) file.remove(rendered_html)
  status <- system2("quarto", c("render", shQuote(file.path(project, "index.qmd"))))
  if (status != 0L || !file.exists(rendered_html)) stop("Render failed for ", metadata$slug)
  file.copy(rendered_html, file.path(staging, "index.html"))
  file.remove(rendered_html)

  if (dir.exists(destination)) unlink(destination, recursive = TRUE, force = TRUE)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(staging, destination)) stop("Could not promote rendered output for ", metadata$slug)
  catalog[[i]] <- metadata
}

dir.create("_data", showWarnings = FALSE)
jsonlite::write_json(catalog, "_data/living_research.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
message("Updated ", length(projects), " Living Research note(s).")
