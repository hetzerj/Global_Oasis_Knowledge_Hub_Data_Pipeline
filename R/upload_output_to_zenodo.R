# Upload generated pipeline output files to Zenodo or Zenodo Sandbox.
#
# This script is intended to be called after either:
#   1. R/run_full_data_pipeline.R
#   2. R/run_monthly_update.R
#
# The script:
#   1. Creates a new Zenodo draft deposition, or reuses an existing one.
#   2. Updates the Zenodo metadata.
#   3. Uploads selected files from the output folder.
#
# Required environment variable:
#   ZENODO_TOKEN
#
# ==============================================================================
# Version helper functions
# ==============================================================================

read_current_version <- function(version_file) {
  if (!file.exists(version_file)) {
    message("VERSION file does not exist. No previous version found.")
    return(NA_character_)
  }
  
  version <- trimws(readLines(version_file, warn = FALSE)[1])
  
  if (length(version) == 0 || is.na(version) || version == "") {
    message("VERSION file is empty. No previous version found.")
    return(NA_character_)
  }
  
  version
}


parse_version <- function(version) {
  version_clean <- sub("^v", "", version)
  parts <- strsplit(version_clean, "\\.")[[1]]
  
  if (length(parts) != 2) {
    stop(
      "Version must follow the format v0.0, v0.1, v1.0, etc. ",
      "Current value: ", version
    )
  }
  
  major <- suppressWarnings(as.integer(parts[1]))
  minor <- suppressWarnings(as.integer(parts[2]))
  
  if (is.na(major) || is.na(minor)) {
    stop("Could not parse version: ", version)
  }
  
  list(major = major, minor = minor)
}


calculate_next_version <- function(current_version, pipeline_type) {
  if (is.na(current_version)) {
    return("v0.0")
  }
  
  parsed <- parse_version(current_version)
  
  if (pipeline_type == "full") {
    next_major <- parsed$major + 1
    next_minor <- 0
  } else if (pipeline_type == "monthly") {
    next_major <- parsed$major
    next_minor <- parsed$minor + 1
  } else {
    stop(
      "Unknown PIPELINE_TYPE: ", pipeline_type, ". ",
      "Use either 'full' or 'monthly'."
    )
  }
  
  paste0("v", next_major, ".", next_minor)
}


write_version_file <- function(version_file, version) {
  writeLines(version, version_file)
  message("Updated VERSION file to: ", version)
}


# ==============================================================================
# User and environment configuration
# ==============================================================================

zenodo_token <- Sys.getenv("ZENODO_TOKEN")

use_sandbox <- tolower(Sys.getenv("ZENODO_SANDBOX", "true")) %in% c("true", "1", "yes")
publish_record <- tolower(Sys.getenv("ZENODO_PUBLISH", "false")) %in% c("true", "1", "yes")

deposition_id <- Sys.getenv("ZENODO_DEPOSITION_ID", unset = NA)
output_dir <- Sys.getenv("OUTPUT_DIR", "output")

version_file <- Sys.getenv("VERSION_FILE", "VERSION")
pipeline_type <- Sys.getenv("PIPELINE_TYPE", "full")

base_url <- if (use_sandbox) {
  "https://sandbox.zenodo.org/api"
} else {
  "https://zenodo.org/api"
}

user_agent <- Sys.getenv(
  "ZENODO_USER_AGENT",
  "Global-Oasis-Knowledge-Hub-Data-Pipeline/0.1"
)

current_version <- read_current_version(version_file)

dataset_version <- Sys.getenv(
  "DATA_VERSION",
  unset = calculate_next_version(current_version, pipeline_type)
)

message("Current version: ", ifelse(is.na(current_version), "none", current_version))
message("Pipeline type: ", pipeline_type)
message("Next dataset version: ", dataset_version)


# ==============================================================================
# File selection
# ==============================================================================

excluded_files <- c(
  "expanded_works.json"
)

if (!fs::dir_exists(output_dir)) {
  stop("Output folder does not exist: ", output_dir)
}

files_to_upload <- fs::dir_ls(output_dir, type = "file", recurse = TRUE)

files_to_upload <- files_to_upload[
  !basename(files_to_upload) %in% excluded_files
]

if (length(files_to_upload) == 0) {
  stop("No files found for upload in output folder: ", output_dir)
}


# ==============================================================================
# Safety checks
# ==============================================================================

if (zenodo_token == "") {
  stop(
    "ZENODO_TOKEN is missing. Set it first, for example:\n",
    "Sys.setenv(ZENODO_TOKEN = 'your_sandbox_token')"
  )
}

message("Zenodo base URL: ", base_url)
message("Sandbox mode: ", use_sandbox)
message("Publish record: ", publish_record)
message("Output directory: ", output_dir)
message("Files found: ", length(files_to_upload))


# ==============================================================================
# Zenodo API helper functions
# ==============================================================================

zenodo_request <- function(url) {
  httr2::request(url) |>
    httr2::req_auth_bearer_token(zenodo_token) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_headers("Accept" = "application/json")
}


check_response <- function(resp, action = "request") {
  status <- httr2::resp_status(resp)
  
  if (status >= 300) {
    body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    stop(
      "Zenodo API error during ", action, ".\n",
      "HTTP status: ", status, "\n",
      "Response body:\n", body
    )
  }
  
  invisible(resp)
}


create_new_deposition <- function() {
  message("Creating a new Zenodo draft deposition.")
  
  resp <- zenodo_request(paste0(base_url, "/deposit/depositions")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_raw("{}") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  
  status <- httr2::resp_status(resp)
  body <- httr2::resp_body_string(resp)
  
  if (status >= 300) {
    stop(
      "Zenodo API error while creating new deposition.\n",
      "HTTP status: ", status, "\n",
      "Response body:\n", body
    )
  }
  
  jsonlite::fromJSON(body, simplifyVector = FALSE)
}


get_deposition <- function(id) {
  message("Reading existing Zenodo deposition: ", id)
  
  resp <- zenodo_request(paste0(base_url, "/deposit/depositions/", id)) |>
    httr2::req_perform()
  
  check_response(resp, "reading deposition")
  
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}


update_metadata <- function(dep) {
  message("Updating Zenodo metadata.")
  
  metadata <- list(
    metadata = list(
      title = paste0("Global Oasis Knowledge Hub Data ", dataset_version),
      upload_type = "dataset",
      description = paste(
        "Generated data products from the Global Oasis Knowledge Hub data pipeline.",
        "Pipeline type:", pipeline_type,
        "Version:", dataset_version
      ),
      creators = list(
        list(
          name = "Hetzer, Jessica",
          affiliation = "Senckenberg Biodiversity and Climate Research Centre"
        )
      ),
      access_right = "open",
      license = "cc-by-4.0",
      version = dataset_version,
      keywords = list(
        "oases",
        "knowledge graph",
        "OpenAlex",
        "bibliographic data",
        "Global Oasis Knowledge Hub"
      )
    )
  )
  
  resp <- zenodo_request(dep$links$self) |>
    httr2::req_method("PUT") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(metadata, auto_unbox = TRUE) |>
    httr2::req_perform()
  
  check_response(resp, "updating metadata")
  
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}


delete_existing_files <- function(dep) {
  if (length(dep$files) == 0) {
    message("No existing files to delete.")
    return(invisible(NULL))
  }
  
  message("Deleting existing files from draft.")
  
  for (f in dep$files) {
    delete_url <- f$links$self
    
    message("Deleting: ", f$filename)
    
    resp <- zenodo_request(delete_url) |>
      httr2::req_method("DELETE") |>
      httr2::req_perform()
    
    check_response(resp, paste0("deleting file ", f$filename))
  }
  
  invisible(NULL)
}


upload_file_to_deposition <- function(deposition_id, file_path, output_dir) {
  relative_name <- fs::path_rel(file_path, start = output_dir)
  relative_name <- gsub("\\\\", "/", relative_name)
  
  upload_name <- basename(relative_name)
  upload_url <- paste0(base_url, "/deposit/depositions/", deposition_id, "/files")
  
  file_size_mb <- as.numeric(fs::file_info(file_path)$size) / 1024^2
  
  message("Uploading file: ", relative_name)
  message("File size: ", round(file_size_mb, 2), " MB")
  
  resp <- httr::POST(
    url = upload_url,
    query = list(access_token = zenodo_token),
    body = list(
      name = upload_name,
      file = httr::upload_file(file_path)
    ),
    encode = "multipart"
  )
  
  status <- httr::status_code(resp)
  body <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  message("Upload status: ", status)
  
  if (status >= 300) {
    stop(
      "Zenodo file upload error.\n",
      "File: ", relative_name, "\n",
      "HTTP status: ", status, "\n",
      "Response body:\n", body
    )
  }
  
  invisible(resp)
}


publish_deposition <- function(dep) {
  message("Publishing Zenodo deposition.")
  
  resp <- zenodo_request(dep$links$publish) |>
    httr2::req_method("POST") |>
    httr2::req_perform()
  
  check_response(resp, "publishing deposition")
  
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}


# ==============================================================================
# Main workflow
# ==============================================================================

if (is.na(deposition_id) || deposition_id == "") {
  dep <- create_new_deposition()
} else {
  dep <- get_deposition(deposition_id)
}

message("Deposition ID: ", dep$id)
message("HTML URL: ", dep$links$html)

dep <- update_metadata(dep)

delete_existing_files(dep)

for (file_path in files_to_upload) {
  upload_file_to_deposition(dep$id, file_path, output_dir)
}

message("Upload complete.")

write_version_file(version_file, dataset_version)

if (publish_record) {
  published <- publish_deposition(dep)
  message("Published record: ", published$links$html)
} else {
  message("Record was not published.")
  message("Inspect the draft here:")
  message(dep$links$html)
}

message("Done.")