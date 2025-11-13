# Create a project for a variable collection

Create a project for a variable collection

## Usage

``` r
armadillo.create_project(
  project_name = NULL,
  users = NULL,
  overwrite_existing = "choose"
)
```

## Arguments

- project_name:

  The name of the project to create. The project name

  - cannot be empty.

  - must be no more than 56 characters.

  - cannot end with a `-`.

  - must consist of lowercase letters and numbers.

- users:

  A list collection of the users that should have access to the project

- overwrite_existing:

  Character, specifying action to take if project still exists: 'choose'
  (default) displays a menu giving the option to overwrite or not, 'yes'
  overwrites the existing project and 'no' exists the function with a
  message.

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.create_project("gecko")
} # }
```
