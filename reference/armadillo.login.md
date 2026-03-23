# Login

Interactively obtains an access token and uses it to create a session
token for an Armadillo Service

## Usage

``` r
armadillo.login(armadillo)
```

## Arguments

- armadillo:

  URL of the Armadillo server,

## Value

the access token

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.login(
  "https://armadillo.dev.molgenis.org"
)
armadillo.login("http://localhost:8080")
} # }
```
