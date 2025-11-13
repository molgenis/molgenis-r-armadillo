# Login with username / password (meant for dev and test environments)

Login with username / password (meant for dev and test environments)

## Usage

``` r
armadillo.login_basic(armadillo, username, password)
```

## Arguments

- armadillo:

  URL of the Armadillo server

- username:

  the username

- password:

  the password

## Examples

``` r
if (FALSE) { # \dontrun{
armadillo.login(
  "https://armadillo.dev.molgenis.org", "admin", "admin"
)
armadillo.login("http://localhost:8080", "admin", "admin")
} # }
```
