# This script does three main things:
#  1) Read the manually curated review table from the folder input
#  2) Match each reviewed reference to an OpenAlex work (prefered search by DOI, then title)
#  3) Use oa_snowball() on the matched OpenAlex IDs to build nodes + edges
#
# -------------------------------------------------------------------
# 1) Read the reviewed references table
# -------------------------------------------------------------------

load_reviewed_references <- function(path = "input/references_reviewed_v0.0.csv") {
  readr::read_tsv(path, show_col_types = FALSE)
}

# -------------------------------------------------------------------
# 2) Match the reviewed references to OpenAlex IDs
# -------------------------------------------------------------------

match_reviewed_references_openalex <- function(reviewed_refs) {
  # For each row in the review table, try to find a corresponding OpenAlex work ID
  # The logic is:
  #   1. If a DOI exists, try to match by DOI.
  #   2. If DOI search fails or is missing, try to match by title.
  # Returns a character vector where each element is either an OpenAlex work ID or NA if no match
  
  # Helper: clean titles to avoid weird control characters messing with search
  clean_title <- function(title) {
    title <- iconv(title, to = "UTF-8", sub = "")   
    title <- gsub("[[:cntrl:]]", "", title)         
    title <- gsub("[,\"\']", "", title)            
    title <- gsub("\\s+", " ", title)               
    trimws(title)                                 
  }
  
  # Helper: find an OpenAlex work by title
  match_by_title <- function(title) {
    cleaned <- clean_title(title)
    tryCatch(
      openalexR::oa_fetch(entity = "works", title.search = cleaned),
      error = function(e) NULL
    )
  }
  
  # Helper: find an OpenAlex work by DOI
  match_by_doi <- function(doi) {
    tryCatch(
      openalexR::oa_fetch(entity = "works", doi = doi),
      error = function(e) NULL
    )
  }
  
  # Number of rows in the review table
  nrow_reviewed_refs <- nrow(reviewed_refs)
  
  # Initialize result vector with NAs
  matches <- rep(NA_character_, nrow_reviewed_refs)
  
  # Progress bar for visual feedback in long runs
  pb <- txtProgressBar(min = 0, max = nrow_reviewed_refs, style = 3)
  
  for (i in seq_len(nrow_reviewed_refs)) {
    search_type     <- ""
    openalex_result <- NULL
    
    doi   <- reviewed_refs$DOI[i]
    title <- reviewed_refs$Title[i]
    
    # 1) Try to match by DOI (if available)
    if (!is.null(doi) && !is.na(doi) && doi != "") {
      search_type     <- "DOI"
      openalex_result <- match_by_doi(doi)
    }
    
    # 2) If DOI search fails or there is no DOI, try by title (if available)
    if (is.null(openalex_result) || length(openalex_result) == 0) {
      if (!is.null(title) && !is.na(title) && title != "") {
        search_type     <- "Title"
        openalex_result <- match_by_title(title)
      }
    }
    
    # 3) Save match if available
    if (!is.null(openalex_result) && length(openalex_result) > 0) {
      matches[i] <- as.character(openalex_result[["id"]][[1]])
      message("Found OpenAlex ID ", matches[i], " with ", search_type)
    } else {
      # No match found – keep NA and print an informative message
      if (!is.null(title) && !is.na(title) && title != "") {
        message("No OpenAlex ID found for: ", as.character(title))
      } else {
        message("No OpenAlex ID found: unknown title, position ", i)
      }
    }
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  return(matches)
}

# -------------------------------------------------------------------
# 3) Create a knowledge-network from the matched IDs 
# -------------------------------------------------------------------

enrich_openalex_metadata <- function(matches, batch_size = 25, verbose = TRUE) {
  # Given a vector of OpenAlex work IDs, this function:
  #   - cleans and de-duplicates the IDs
  #   - splits them into batches
  #   - runs oa_snowball() per batch
  #   - combines and de-duplicates nodes and edges
  #
  # Arguments:
  #   matches: character vector of OpenAlex identifiers
  #   batch_size: how many IDs to send in one oa_snowball() call
  #   verbose: whether to print progress messages
  #
  # Returns:
  #   A list with:
  #     $nodes: combined node table
  #     $edges: combined edge table
  
  # 1) Clean and de-duplicate identifiers
  identifiers <- unique(matches)
  identifiers <- identifiers[!is.na(identifiers)]                # drop NAs
  identifiers <- sub("https://openalex.org/", "", identifiers)   # strip base URL if present
  
  # If nothing left after cleaning, return empty tables
  if (length(identifiers) == 0) {
    if (verbose) message("No valid OpenAlex IDs provided to enrich_openalex_metadata().")
    return(list(
      nodes = data.frame(),
      edges = data.frame()
    ))
  }
  
  # 2) Split into batches for oa_snowball() calls
  batches <- split(identifiers, ceiling(seq_along(identifiers) / batch_size))
  
  # 3) Run oa_snowball on each batch 
  res <- lapply(seq_along(batches), function(i) {
    if (verbose) {
      message("Batch ", i, "/", length(batches))
    }
    
    tryCatch(
      openalexR::oa_snowball(
        identifier = batches[[i]],
        verbose    = verbose
      ),
      error = function(e) {
        message("Batch ", i, " failed: ", conditionMessage(e))
        NULL
      }
    )
  })
  
  # Remove failed batches (NULLs)
  res <- Filter(Negate(is.null), res)
  
  # If all batches failed, return empty tables instead of erroring in rbind
  if (length(res) == 0) {
    if (verbose) message("All oa_snowball() calls failed. Returning empty nodes/edges.")
    return(list(
      nodes = data.frame(),
      edges = data.frame()
    ))
  }
  
  # 4) Combine nodes and edges across all successful batches
  nodes <- do.call(rbind, lapply(res, `[[`, "nodes"))
  edges <- do.call(rbind, lapply(res, `[[`, "edges"))
  
  # 5) De-duplicate
  #    - nodes: unique by node ID
  #    - edges: unique by from–to pair
  nodes <- nodes[!duplicated(nodes$id), , drop = FALSE]
  edges <- edges[!duplicated(paste(edges$from, edges$to)), , drop = FALSE]
  date_of_generation  <- Sys.time() 
  return(list(nodes = nodes, edges = edges, date_of_generation = date_of_generation))
}



