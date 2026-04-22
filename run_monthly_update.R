# Monthly pipeline run for updating derived OpenAlex network outputs.
# This script:
# 1. loads previously matched reviewed references
# 2. enriches them with linked OpenAlex metadata
# 3. exports updated network files for downstream use

source("R/enrich_reviewed_references_openalex.R")
source("R/postprocess_for_platform.R")

# Create output directory 
dir.create("output", showWarnings = FALSE, recursive = TRUE)

# Load previously matched reviewed references
matched <- arrow::read_parquet("output/matched_reviewed_refs.parquet")

# Enrich matched references with linked citation network metadata
enriched <- enrich_openalex_metadata(matched)

# Export enriched network as JSON
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

# Transform network nodes to tabular format 
enriched_df <- transform_expanded_works_to_dataframe(enriched)

# Export network nodes in compressed parquet format
arrow::write_parquet(
  enriched_df,
  "output/expanded_works_nodes.parquet",
  compression = "zstd"
)