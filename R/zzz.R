.onLoad <- function(libname, pkgname) {
  op <- options()
  op.MolgenisArmadillo <- list(
    MolgenisArmadillo.s3.use_https = TRUE
  )
  toset <- !(names(op.MolgenisArmadillo) %in% names(op))
  if (any(toset)) options(op.MolgenisArmadillo[toset])

  invisible()
}
