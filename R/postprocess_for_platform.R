# Functions for preparing OpenAlex network outputs for platform use

# Add abbreviated author labels to the node table
add_author_abbr <- function(oa_network) {
  oa_network$nodes$authors_short <- vapply(
    oa_network$nodes$authorships,
    function(x) {
      if (is.null(x) || !is.data.frame(x) || nrow(x) == 0) {
        return(NA_character_)
      }
      
      first_name <- x$display_name[1]
      
      if (is.na(first_name) || !nzchar(first_name)) {
        return(NA_character_)
      }
      
      if (nrow(x) > 1) {
        paste(first_name, "et al.")
      } else {
        first_name
      }
    },
    FUN.VALUE = character(1)
  )
  
  return(oa_network)
}

# Transform the network node table to a flat data frame for export
transform_expanded_works_to_dataframe <- function(expanded_works_raw) {
  expanded_works <- add_author_abbr(expanded_works_raw)
  
  df <- data.frame(
    oa_ID = expanded_works$nodes$id,
    Authors = expanded_works$nodes$authors_short,
    Year = expanded_works$nodes$publication_year,
    Title = expanded_works$nodes$title,
    Type = expanded_works$nodes$type,
    Open_access = expanded_works$nodes$is_oa,
    URL = expanded_works$nodes$landing_page_url,
    Source = expanded_works$nodes$source_display_name,
    stringsAsFactors = FALSE
  )
  
  return(df)
}