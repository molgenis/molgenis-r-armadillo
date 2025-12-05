# Move the resource

Move the resource

## Usage

``` r
armadillo.move_resource(
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

  the folder containing the resource to move

- name:

  a resource to move

- new_folder:

  the folder to move the resource to, defaults to folder

- new_name:

  use to rename the file, defaults to name

## Value

NULL, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.move_resource(
  project = "gecko",
  folder = "core_all",
  name = "table1",
  new_folder = "core_all_v2",
)
} # }
```
