# Move the table

Move the table

## Usage

``` r
armadillo.move_table(
  project,
  folder,
  name,
  new_folder = folder,
  new_name = name
)
```

## Arguments

- project:

  a study or collection of variables

- folder:

  the folder containing the table to move

- name:

  a table to move

- new_folder:

  the folder to move the table to, defaults to folder

- new_name:

  use to rename the file, defaults to name

## Value

NULL, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.move_table(
  project = "gecko",
  folder = "core_all",
  name = "table1",
  new_folder = "core_all_v2",
)
} # }
```
