# Load a table from a project

Load a table from a project

## Usage

``` r
armadillo.load_table(project, folder, name)
```

## Arguments

- project:

  study or collection variables

- folder:

  the folder containing the table

- name:

  name of the table

## Value

the contents of the table file, as data frame

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.load_table(
  project = "gecko",
  folder = "core_all",
  name = "lc_core_1"
)
} # }
```
