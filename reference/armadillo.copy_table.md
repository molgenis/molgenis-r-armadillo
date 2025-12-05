# Copy table

Copy table

## Usage

``` r
armadillo.copy_table(
  project,
  folder,
  name,
  new_folder = folder,
  new_name = name
)
```

## Arguments

- project:

  study or other variable collection

- folder:

  the folder containing the table

- name:

  specific table for copy action

- new_folder:

  name of the folder in which to place the copy, defaults to folder

- new_name:

  name of the copy, defaults to name

## Value

the response from the server

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.copy_table(
  project = "gecko",
  folder = "core_all",
  name = "table1",
  new_folder = "core_all_v2",
)
} # }
```
