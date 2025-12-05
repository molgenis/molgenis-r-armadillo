# Login

Interactively obtains an id token and uses it to create a session token
for an Armadillo Service

## Usage

``` r
armadillo.login(armadillo)
```

## Arguments

- armadillo:

  URL of the Armadillo server,

## Value

the id token

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.login(
  "https://armadillo.dev.molgenis.org"
)
armadillo.login("http://localhost:8080")
} # }
```
