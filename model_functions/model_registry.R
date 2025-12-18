# Helpers to manage which GLM model set is active (for Shiny/ops use).
# Default is the final NegBin models with mixed lag/baseline teams.

library(here)

# Returns the full path to the active GLM model RDS.
get_active_glm_model_path <- function(default = "final_glm_negbin_by_team.rds") {
  active_file  <- here("models", "active_glm_model.txt")
  default_path <- here("models", default)

  if (!file.exists(active_file)) return(default_path)

  active_name <- readLines(active_file, warn = FALSE)
  active_name <- trimws(active_name[active_name != ""])[1]

  if (is.na(active_name) || active_name == "") return(default_path)

  candidate <- here("models", active_name)
  if (file.exists(candidate)) return(candidate)

  warning(
    "Active GLM model file not found: ", candidate,
    ". Falling back to default: ", default_path
  )
  default_path
}

# Loads the active GLM model list (named by team).
load_active_glm_models <- function(default = "final_glm_negbin_by_team.rds") {
  readRDS(get_active_glm_model_path(default))
}

# Sets a new active GLM model RDS (must already exist in models/).
set_active_glm_model <- function(model_file) {
  model_path <- here("models", model_file)
  if (!file.exists(model_path)) {
    stop("Model does not exist: ", model_path)
  }

  active_file <- here("models", "active_glm_model.txt")
  writeLines(model_file, active_file)
  invisible(model_path)
}

# Lists available GLM model RDS files.
list_available_glm_models <- function() {
  list.files(here("models"), pattern = "glm.*\\.rds$", full.names = FALSE)
}
