# Builds an R object containing info required to make subsets

Builds an R object containing info required to make subsets

## Usage

``` r
armadillo.subset_definition(reference_csv = NULL, vars = NULL)
```

## Arguments

- reference_csv:

  `.csv` file containing details of the variable to subset. Must contain
  5 columns: 'source_folder' specifying the folder from which to subset,
  'souce_table' specifying the table from which to subset,
  'target_folder' specifying the folder in which to create the subset
  'target_table' specifying the name of the subset and 'variable'
  specifying the variable(s) to include in the subset. Note that
  'source_project' and 'target_project' are specified as arguments to
  \`armadillo.subset\`.

- vars:

  Deprecated: use `reference_csv` instead

## Value

A dataframe containing variables that is used for input in the
[`armadillo.subset()`](https://molgenis.github.io/molgenis-r-armadillo/reference/armadillo.subset.md)
method

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.subset_definition(
  reference_csv = "C:/tmp/vars.csv"
)
} # }
```
