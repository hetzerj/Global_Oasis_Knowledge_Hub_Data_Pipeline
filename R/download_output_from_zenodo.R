# ==============================================================================
# download_output_from_zenodo.R
#
# Download the latest matched_reviewed_refs.parquet file from a Zenodo or
# Zenodo Sandbox concept record.
#
# This script is intended to run before run_monthly_update.R.
#
# Required environment variable:
#   ZENODO_CONCEPT_RECORD_ID
#
# Optional environment variables:
#   ZENODO_SANDBOX  "true" or "false", default: "true"
#   OUTPUT_DIR      output directory, default: output
# ==============================================================================


# ==============================================================================
# Configuration
# ==============================================================================

concept_record_id <- Sys.getenv("ZENODO_CONCEPT_RECORD_ID")
use_sandbox <- tolower(Sys.getenv("ZENODO_SANDBOX", "true")) %in% c("true", "1", "yes")
output_dir <- Sys.getenv("OUTPUT_DIR", "output")

target_file <- "matched_reviewed_refs.parquet"

base_url <- if (use_sandbox) {
  "https://sandbox.zenodo.org/api"
} else {
  "https://zenodo.org/api"
}

user_agent <- Sys.getenv(
  "ZENODO_USER_AGENT",
  "Global-Oasis-Knowledge-Hub-Data-Pipeline/0.1"
)


# ==============================================================================
# Safety checks
# ==============================================================================

if (concept_record_id == "") {
  stop(
    "ZENODO_CONCEPT_RECORD_ID is missing. ",
    "Set it to the Zenodo concept record ID."
  )
}

if (!fs::dir_exists(output_dir)) {
  message("Output directory does not exist. Creating: ", output_dir)
  fs::dir_create(output_dir)
} else {
  message("Output directory already exists: ", output_dir)
}


# ==============================================================================
# Resolve latest version from concept record
# ==============================================================================

latest_url <- paste0(
  base_url,
  "/records/",
  concept_record_id,
  "/versions/latest"
)

message("Resolving latest Zenodo version from concept record:")
message(latest_url)

resp <- httr2::request(latest_url) |>
  httr2::req_user_agent(user_agent) |>
  httr2::req_headers("Accept" = "application/json") |>
  httr2::req_error(is_error = function(resp) FALSE) |>
  httr2::req_perform()

status <- httr2::resp_status(resp)

if (status >= 300) {
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  stop(
    "Could not resolve latest Zenodo version from concept record.\n",
    "HTTP status: ", status, "\n",
    "Response body:\n", body
  )
}

record <- httr2::resp_body_json(resp, simplifyVector = FALSE)

message("Latest Zenodo version record ID: ", record$id)
message("Latest Zenodo version title: ", record$metadata$title)


# ==============================================================================
# Find target file
# ==============================================================================

files <- record$files

if (length(files) == 0) {
  stop("The latest Zenodo record contains no files.")
}

file_names <- vapply(files, function(x) x$key, character(1))

if (!target_file %in% file_names) {
  stop(
    "Could not find ", target_file, " in latest Zenodo record.\n",
    "Available files:\n",
    paste(file_names, collapse = "\n")
  )
}

target_index <- which(file_names == target_file)[1]
download_url <- files[[target_index]]$links$self

destination <- file.path(output_dir, target_file)


# ==============================================================================
# Download target file
# ==============================================================================

message("Downloading file: ", target_file)
message("Destination: ", destination)

download_resp <- httr2::request(download_url) |>
  httr2::req_user_agent(user_agent) |>
  httr2::req_error(is_error = function(resp) FALSE) |>
  httr2::req_perform(path = destination)

download_status <- httr2::resp_status(download_resp)

if (download_status >= 300) {
  body <- tryCatch(httr2::resp_body_string(download_resp), error = function(e) "")
  stop(
    "Download failed.\n",
    "HTTP status: ", download_status, "\n",
    "Response body:\n", body
  )
}

if (!file.exists(destination)) {
  stop("Download finished, but file was not found at: ", destination)
}

message("Download complete.")
message("File saved to: ", destination)