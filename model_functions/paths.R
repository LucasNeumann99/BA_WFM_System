get_pipeline_paths <- function() {
  cfg_path <- here::here("config", "paths.json")
  output_base <- "output"
  results_base <- "results"

  if (file.exists(cfg_path)) {
    cfg <- jsonlite::fromJSON(cfg_path)
    if (!is.null(cfg$output_base) && nzchar(cfg$output_base)) {
      output_base <- cfg$output_base
    }
    if (!is.null(cfg$results_base) && nzchar(cfg$results_base)) {
      results_base <- cfg$results_base
    }
  }

  list(
    output = normalizePath(here::here(output_base), winslash = "/", mustWork = FALSE),
    results = normalizePath(here::here(results_base), winslash = "/", mustWork = FALSE)
  )
}
