# R Functions for DLD8 Processing

These R scripts mirror the app's folder-based processing flow:
- Scan a folder for `.dld8` files
- Read and parse each file (BSON)
- Extract metadata and marks
- Return in-memory data structures for downstream processing

## Dependencies
- `jsonlite`
- Either `bson` or `mongolite` (for BSON parsing)

## Usage
```r
source("R/processing.R")

source_folder <- "C:/data/dld8_runs"

project <- create_dld8_project(
  source_folder = source_folder,
  exclude_non_experimental = TRUE  # default: TRUE
)

print(project$dld8_files)
print(project$excluded_dld8_files)
```

## Export data frames
```r
marks_df <- extract_marks_df(project)
metadata_df <- extract_metadata_df(project)

# Include non-experimental files if desired
marks_all <- extract_marks_df(project, include_non_experimental = TRUE)
metadata_all <- extract_metadata_df(project, include_non_experimental = TRUE)
```

## Flux/FCR tables (long or wide)
```r
tables_long <- extract_flux_tables(project, format = "long")
tables_wide <- extract_flux_tables(project, format = "wide")

# Choose how mark columns are determined
tables_present <- extract_flux_tables(project, format = "wide", mark_order_source = "present")
tables_protocol <- extract_flux_tables(project, format = "wide", mark_order_source = "protocol")
tables_fallback <- extract_flux_tables(project, format = "wide", mark_order_source = "protocol_then_present")

# Group by protocol (returns tables split into lists by protocol string)
tables_by_protocol <- extract_flux_tables(project, format = "wide", group_by_protocol = TRUE)
```

## Override baseline/reference markers
```r
overrides <- data.frame(
  rel_path = c("run1.dld8"),
  chamber = c("chamber_a"),
  baseline = c("ce1"),
  reference = c("4G"),
  stringsAsFactors = FALSE
)

project <- add_marker_overrides(project, overrides)
tables_wide <- extract_flux_tables(project, format = "wide")
```

## Override by protocol (applies to all files)
```r
proto_overrides <- data.frame(
  protocol = c("D003"),
  baseline = c("ce1"),
  reference = c("4G"),
  stringsAsFactors = FALSE
)

project <- add_marker_overrides(project, proto_overrides)
```

## Validate effective markers (defaults + overrides)
```r
validation <- validate_effective_markers(project)
validation[validation$baseline_ok == FALSE | validation$reference_ok == FALSE, ]
```

## Check for file changes
```r
changes <- check_project_changes(project)
changes$changed
changes$added
changes$removed

# Update the cache snapshot
changes <- check_project_changes(project, update = TRUE)
project <- changes$project
```

## Debug: print present marks
```r
debug_print_present_marks(project)
```
