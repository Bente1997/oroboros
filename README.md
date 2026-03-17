# oroboros R Functions (DLD8)

This package replaces the Excel Templates for calculations and allows for bulk analysis of Oroboros DatLab 8 (.dld8) files. 

What it does:
- Scan a folder for `.dld8` files
- Parse each file (BSON)
- Extract metadata and marks
- Compute flux / FCR tables
- Optionally apply baseline/reference overrides
- Track file changes

## Dependencies
- `jsonlite`
- Either `bson` or `mongolite` 

## Usage

### Initiating a project
Define the `source_folder` where your dld8 data lives. Note: the script searches **recursively**. 
```r
library(oroboros)

source_folder <- "C:/data/dld8_runs"

project <- create_dld8_project(
  source_folder = source_folder,
  exclude_non_experimental = TRUE  # default: TRUE. Excludes cleaning/background protocols.
)

print(project$dld8_files) # Nr. of experimental dld8 files found in your directory
print(project$excluded_dld8_files) # Nr. of dld8 files excluded (cleaning/background files)

# the returned object already holds a cache of parsed data in
# `project$parsed_dld8`; subsequent helpers reuse that cache instead
# of reading every file again.
```

### Marks and file information

- `extract_marks_df` results in a long-format dataframe with information on each mark. E.g. `markName`, `RespiratoryState`, `calibrationVolume`.
- `metadata_df` results in a wide format dataframe with each row an entity (`file` x `chamber`) with its metadata. E.g. `sampleCode`, `sampleNumber`, etc.

```r
marks_df <- extract_marks_df(project)
metadata_df <- extract_metadata_df(project)

# Include non-experimental files if desired
marks_all <- extract_marks_df(project, include_non_experimental = TRUE)
metadata_all <- extract_metadata_df(project, include_non_experimental = TRUE)
```

### Baseline/reference markers
In order for your exports to have baseline correction, you will need to define the baseline and reference marker. First, validate whether all entities have a baseline/reference marker already (derived from protocol data).


```r
validation <- validate_effective_markers(project)
validation[validation$baseline_ok == FALSE | validation$reference_ok == FALSE, ] 
```

If the marks are not set or you wish to change them, you can do so by defining them per **file/chamber** OR per protocol. 



```r

# ----- By file/chamber
overrides <- data.frame(
  rel_path = c("run1.dld8"),
  chamber = c("chamber_a"),
  baseline = c("ce1"),
  reference = c("4G"),
  stringsAsFactors = FALSE
)


project <- add_marker_overrides(project, overrides)


# ----- By protocol
proto_overrides <- data.frame(
  protocol = c("D003"),
  baseline = c("ce1"),
  reference = c("4G"),
  stringsAsFactors = FALSE
)

project <- add_marker_overrides(project, proto_overrides)

```

Make sure to check again:

```r
validation <- validate_effective_markers(project)
validation[validation$baseline_ok == FALSE | validation$reference_ok == FALSE, ] 
```





### Flux/FCR tables (long or wide)
`extract_flux_tables` results in a list of dataframes, either named by metric or by protocol (`group_by_protocol = FALSE`).

**Metrics:**

- `flux_per_volume`: Flux per Volume
- `specific_flux`: Specific Flux
- `specific_flux_bc`: Specific Flux (baseline corrected)
- `fcr`: Flux Control Ratio               
- `fcr_bc`: Flux Control Ratio (baseline corrected)

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



### Check for file changes

- `check_project_changes` Checks whether the folder used to initiate the project has any changes.
- `changes$changed`: Existing files in the project that have been altered.
- `changes$added`: New files in the source folder.
- `changes$removed`: Files that are now missing from the source folder.

```r
changes <- check_project_changes(project)
changes$changed
changes$added
changes$removed
```

It is important that one updates the project structure if any changes occur:


```r
# Update the cache snapshot
changes <- check_project_changes(project, update = TRUE)
project <- changes$project
```


