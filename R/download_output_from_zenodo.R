# Download the latest matched_reviewed_refs.parquet file from a Zenodo or Zenodo Sandbox record.
# This script is suppose to run in order to update monthly with run_monthly_update.R


record_id <- Sys.getenv("ZENODO_RECORD_ID")
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

if (record_id == "") {
  stop(
    "ZENODO_RECORD_ID is missing. ",
    "Set it to the Zenodo record ID or concept record ID."
  )
}

if (!fs::dir_exists(output_dir)) {
  fs::dir_create(output_dir)
}


# ==============================================================================
# Read Zenodo record metadata
# ==============================================================================

record_url <- paste0(base_url, "/records/", record_id)

message("Reading Zenodo record metadata:")
message(record_url)

resp <- httr2::request(record_url) |>
  httr2::req_user_agent(user_agent) |>
  httr2::req_headers("Accept" = "application/json") |>
  httr2::req_error(is_error = function(resp) FALSE) |>
  httr2::req_perform()

status <- httr2::resp_status(resp)

if (status >= 300) {
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  stop(
    "Could not read Zenodo record metadata.\n",
    "HTTP status: ", status, "\n",
    "Response body:\n", body
  )
}

record <- httr2::resp_body_json(resp, simplifyVector = FALSE)


# ==============================================================================
# Find target file
# ==============================================================================

files <- record$files

if (length(files) == 0) {
  stop("The Zenodo record contains no files.")
}

file_names <- vapply(files, function(x) x$key, character(1))

if (!target_file %in% file_names) {
  stop(
    "Could not find ", target_file, " in Zenodo record.\n",
    "Available files:\n",
    paste(file_names, collapse = "\n")
  )
}

target_index <- which(file_names == target_file)[1]
download_url <- files[[target_index]]$links$self

destination <- file.path(output_dir, target_file)

message("Downloading file: ", target_file)
message("Destination: ", destination)

download_resp <- httr2::request(download_url) |>
  httr2::req_user_agent(user_agent) |>
  httr2::req_error(is_error = function(resp) FALSE) |>
  httr2::req_perform(path = destination)

download_status <- httr2::resp_status(download_resp)

if (download_status >= 300) {
  stop(
    "Download failed.\n",
    "HTTP status: ", download_status
  )
}

message("Download complete.")
message("File saved to: ", destination)