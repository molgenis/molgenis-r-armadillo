.onLoad <- function(libname, pkgname) { #nolint
  op <- options()
  defaults <- list(
    MolgenisArmadillo.s3.use_https = TRUE
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])

  invisible()
}
