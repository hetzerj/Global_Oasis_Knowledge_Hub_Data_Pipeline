# Full pipeline run for loading the reviewed reference base, connect OpenAlex records, expanding to knowledge network and rebuilding the Global Oasis Knowledge Hub outputs 
# This script:
# 1. loads reviewed references from the input directory
# 2. matches them to OpenAlex records
# 3. enriches the matched records with citation network metadata
# 4. exports intermediate and final outputs for downstream use

source("R/postprocess_for_platform.R")

# create output directory
dir.create("output", showWarnings = FALSE, recursive = TRUE)


# Load reviewed references and match them to OpenAlex
reviewed <- load_reviewed_references()
matched <- match_reviewed_references_openalex(reviewed)

# Store full and shortened OpenAlex identifiers
reviewed$OpenAlex_ID <- matched
reviewed$OpenAlex_ID_short <- sub("https://openalex.org/", "", reviewed$OpenAlex_ID)

# Export matched reviewed references
arrow::write_parquet(
  reviewed,
  "output/matched_reviewed_refs.parquet",
  compression = "zstd"
)

# Enrich matched references with linked citation network metadata
enriched <- enrich_openalex_metadata(matched)

# Export full enriched network object as JSON
jsonlite::write_json(
  enriched,
  "output/expanded_works.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  dataframe = "rows",
  null = "null",
  na = "null"
)

# Export network edges in compressed parquet format
arrow::write_parquet(
  enriched$edges,
  "output/expanded_works_edges.parquet",
  compression = "zstd"
)

# Transform network nodes to simplified tabular format 
enriched_df <- transform_expanded_works_to_dataframe(enriched)

# Export network nodes in compressed parquet format
arrow::write_parquet(
  enriched_df,
  "output/expanded_works_nodes.parquet",
  compression = "zstd"
)
