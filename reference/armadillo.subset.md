# Describes data available to subset and makes subset

This automates the process of:

1.  Checking what data is available to create subsets

2.  Make the subset

## Usage

``` r
armadillo.subset(
  input_source = NULL,
  subset_def = NULL,
  source_project = NULL,
  source_folder = NULL,
  source_table = NULL,
  target_project = NULL,
  target_folder = NULL,
  target_table = NULL,
  target_vars = NULL,
  new_project = NULL,
  dry_run = NULL,
  strict = FALSE
)
```

## Arguments

- input_source:

  Character specifying how information about the target view is
  provided: choose 'subset_def' if providing a subset definition object,
  or 'arguments' if providing information directly.

- subset_def:

  R object containing subset definition created by
  [`armadillo.subset_definition()`](https://molgenis.github.io/molgenis-r-armadillo/reference/armadillo.subset_definition.md).
  Compulsory if input_source = 'subset_def'

- source_project:

  project from which to subset data

- source_folder:

  folder from which to subset data. Compulsory if input_source =
  'arguments'.

- source_table:

  table from which to subset data. Compulsory if input_source =
  'arguments'.

- target_project:

  project to upload subset to. Will be created if it doesn't exist.

- target_folder:

  folder to upload subset to. Will be created if it doesn't exist.
  Compulsory if input_source = 'arguments'.

- target_table:

  table to upload subset to. Compulsory if input_source = 'arguments'.

- target_vars:

  variables from \`source_table\` to include in the view. Compulsory if
  input_source = 'arguments'.

- new_project:

  Deprecated: use `target_project` instead

- dry_run:

  Defunct: previously enabgled dry-run to check which variables are
  missing

- strict:

  Boolean specifying whether to create subset if one or more target
  variables do not exist in the target data. Option FALSE will throw and
  error, option TRUE (default) creates subset and return a warning

## Value

missing variables provided in the subset definition

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.subset(
  source_project = "gecko",
  target_project = "study1",
  subset_def = local_subset
)
} # }
```
